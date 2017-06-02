###########
# Untrack #
###########

struct Untrack{F} <: Function
    func::F
end

@inline Untrack(u::Untrack) = u

@inline (u::Untrack{<:Any})(a) = u.func(untrack(a))
@inline (u::Untrack{<:Any})(a, b) = u.func(untrack(a), untrack(b))
@inline (u::Untrack{<:Any})(a, b, c) = u.func(untrack(a), untrack(b), untrack(c))
@inline (u::Untrack{<:Any})(a, b, c, d) = u.func(untrack(a), untrack(b), untrack(c), untrack(d))
@inline (u::Untrack{<:Any})(a, b, c, d, e) = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline (u::Untrack{<:Any})(a, b, c, d, e, others...) = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

##########
# Record #
##########

struct Record{G<:AbstractGenre,F} <: Function
    genre::G
    func::F
end

@inline function (r::Record{ValueGenre})(output, input)
    output_node = track(output, r.genre)
    output_node.parent = FunctionNode(r.genre, r.func, output_node, input, nothing)
    return output_node
end

@inline function (r::Record{ValueGenre})(output, input::Tuple{Type{T}}) where {T}
    return track(output, r.genre)
end

###########
# Execute #
###########

struct Execute{G<:AbstractGenre,F} <: Function
    genre::G
    func::F
end

# This doesn't specialize on DataType arguments naively, so we have to force specialization
# by unrolling access + type assertions via a generated function. This is pretty annoying
# since the naive method is a one-liner otherwise...
# @generated function (e::Execute{<:AbstractGenre,<:Any})(input...)
#     typed_input = [:(input[$i]::$(input[i])) for i in 1:nfields(input)]
#     return quote
#         $(Expr(:meta, :inline))
#         r = Record(e.genre, e.func)
#         output = Untrack(e.func)($(typed_input...))
#         return call_record(r, output, input)
#     end
# end

@inline function (e::Execute)(input...)
    record = Record(e.genre, e.func)
    output = specialized_call(Untrack(e.func), input...)
    return call_record(record, output, input)
end

@inline call_record(r::Record, output::NTuple{N}, input::Tuple, args...) where {N} = NTuple{N}(r(o, input, args...) for o in output)
@inline call_record(r::Record, output, input::Tuple, args...) = call_record(trackability(output), r, output, input, args...)
@inline call_record(::TrackabilityTrait, r::Record, output, input::Tuple, args...) = r(output, input, args...)
@inline call_record(::NotTrackable, r::Record, output, input::Tuple, args...) = output

#############
# Intercept #
#############

struct Intercept{F} <: Function
    func::F
end

@inline Intercept(i::Intercept) = i

# As we did with `Execute`, we must for force specialization via a generated
# function in order to avoid performance penalties for DataType arguments.
# @generated function (i::Intercept{<:Any})(input...)
#     typed_input = [:(input[$i]::$(input[i])) for i in 1:nfields(input)]
#     return quote
#         $(Expr(:meta, :inline))
#         genre = promote_genre($(typed_input...))
#         return Execute(genre, i.func)($(typed_input...))
#     end
# end

@inline function (i::Intercept)(input...)
    genre = specialized_call(promote_genre, input...)
    return specialized_call(Execute(genre, i.func), input...)
end

#=
works for the following formats:
- `@intercept(f)(args...)`
- `@intercept f(args...) = ...`
- `@intercept function f(args...) ... end`
- `@intercept f = (args...) -> ...`
=#
macro intercept(expr)
    if isa(expr, Expr) && (expr.head == :(=) || expr.head == :function)
        lhs = expr.args[1]
        if isa(lhs, Expr) && lhs.head == :call # named function definition site
            name_and_types = lhs.args[1]
            if isa(name_and_types, Expr) && name_and_types.head == :curly
                old_name = name_and_types.args[1]
                hidden_name = Symbol("#cassette_hidden_$(old_name)")
                name_and_types.args[1] = hidden_name
            elseif isa(name_and_types, Symbol)
                old_name = name_and_types
                hidden_name = Symbol("#cassette_hidden_$(old_name)")
                lhs.args[1] = hidden_name
            else
                error("failed to apply Cassette.@intercept to expression $(expr); potentially malformed function signature?")
            end
            result = quote
                $expr
                if !(isdefined($(Expr(:quote, old_name))))
                    const $(old_name) = $(Intercept)($(hidden_name))
                end
            end
        elseif isa(lhs, Symbol) # variable assignment site
            expr.args[2] = :($(Intercept)($(expr.args[2])))
            result = expr
        else
            error("failed to apply Cassette.@intercept to expression $expr")
        end
    else # call site
        result = :($(Intercept)($expr))
    end
    return esc(result)
end

########################
# Instruction Wrappers #
########################

struct ForwardWrapper{F<:FunctionNode} <: Function
    instruction::F
end

(w::ForwardWrapper)() = (execute!(ForwardMode(), w.instruction); nothing)

struct ReverseWrapper{F<:FunctionNode} <: Function
    instruction::F
end

(w::ReverseWrapper)() = (execute!(ReverseMode(), w.instruction); nothing)

########
# Tape #
########

const ExecutionWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct Tape
    instructions::Vector{FunctionNode}
    forward::Vector{ExecutionWrapper}
    reverse::Vector{ExecutionWrapper}
    function Tape(instructions::Vector{FunctionNode})
        forward = [ExecutionWrapper(ForwardWrapper(instructions[i])) for i in 1:length(instructions)]
        reverse = [ExecutionWrapper(ReverseWrapper(instructions[i])) for i in length(instructions):-1:1]
        return new(instructions, forward, reverse)
    end
end

function Tape(output::ValueNode)
    instructions = Vector{FunctionNode}()
    walkback(output) do node, hasparent
        hasparent && push!(instructions, node.parent)
    end
    return Tape(reverse!(instructions))
end

#############
# Execution #
#############

abstract type ExecutionMode end

struct ForwardMode <: ExecutionMode end
struct ReverseMode <: ExecutionMode end

execute!(::ForwardMode, t::Tape) = (for f! in t.forward; f!(); end; nothing)
execute!(::ReverseMode, t::Tape) = (for f! in t.reverse; f!(); end; nothing)

function execute!(::ForwardMode, n::FunctionNode{ValueGenre,<:Any,<:RealNode,<:Tuple})
    n.output.value += Untrack(n.func)(n.input...)
    return nothing
end
