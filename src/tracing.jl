#############
# TraceCall #
#############

struct TraceCall{G<:AbstractGenre,F,w}
    callable::F
end

@inline unwrap(t::TraceCall) = t.callable

@inline (t::TraceCall{G})(input...) where {G} = perform_tracecall(behavior(t, input...), t, input...)

# TraceBehavior trait #
#---------------------#

struct Recurse   end
struct Intercept end
struct Skip      end

struct TraceBehavior{G<:AbstractGenre,F} end

@inline TraceBehavior(::TraceCall{G,F}) where {G,F} = TraceBehavior{G,F}()

@generated function behavior(t::TraceCall{G,F}, input...) where {G,F}
    if F.name.module === Core
        return :($(Expr(:meta, :inline)); Intercept())
    else
        return :($(Expr(:meta, :inline)); TraceBehavior(t)(input...))
    end
end

# perform_tracecall #
#-------------------#

@inline perform_tracecall(::Skip, t::TraceCall, input...) = unwrapcall(t, input...)
@inline perform_tracecall(::Intercept, t::TraceCall{G}, input...) where {G} = Play{G}(unwrap(t))(input...)
@inline perform_tracecall(::Recurse, t::TraceCall{G,F,w}, input...) where {G,F,w} = Trace{G,F,w}(unwrap(t))(input...)

########################################
# CodeInfo Retrieval/Pruning Utilities #
########################################

#=
Historically, `code_lowered(f, types)` requires `f` to be the function instance. That
interface is just a holdover from the days where `typeof(f) === Function`; nowadays, the
function type + argument type signature is a unique identifier of a method. Thus, we can do
the following, which can be called (kind of unsafely) from a generated function.
=#
methods_by_type_sig(::Type{T}) where {T<:Tuple} = Base._methods_by_ftype(T, -1, typemax(UInt))

code_info_from_method_info(method_info) = (Base.uncompressed_ast(method_info[3]), method_info[2])

function wrap_subcalls_with_intercept!(lines::Vector, ::Type{G}, w) where {G<:AbstractGenre}
    for line in lines
        isa(line, Expr) && wrap_subcalls_with_intercept!(line, G, w)
    end
    return lines
end

function wrap_subcalls_with_intercept!(ast::Expr, ::Type{G}, w) where {G<:AbstractGenre}
    if ast.head == :call && (ast.args[1] != GlobalRef(Core, :apply_type))
        f = ast.args[1]
        ast.args[1] = Expr(:call, Expr(:call, GlobalRef(Core, :apply_type), Cassette.TraceCall, G, :(typeof($f)), w), f)
        child_indices = 2:length(ast.args)
    else
        child_indices = 1:length(ast.args)
    end
    for i in child_indices
        child = ast.args[i]
        if isa(child, Expr)
            wrap_subcalls_with_intercept!(child, G, w)
        end
    end
    return ast
end

function replace_static_parameters!(lines::Vector, static_params)
    for i in eachindex(lines)
        line = lines[i]
        if isa(line, Expr)
            if line.head == :static_parameter
                lines[i] = static_params[line.args[1]]
            else
                replace_static_parameters!(line.args, static_params)
            end
        end
    end
end

function intercepted_code_info!(code_info::CodeInfo, static_params, ::Type{G}, w) where {G<:AbstractGenre}
    wrap_subcalls_with_intercept!(code_info.code, G, w)
    replace_static_parameters!(code_info.code, static_params)
    return code_info
end

trace_world_counter() = ccall(:jl_get_world_counter, UInt, ())

#########
# Trace #
#########

struct Trace{G<:AbstractGenre,F,w}
    callable::F
    @inline Trace{G,F,w}(callable::F) where {G,F,w} = new{G,F,w}(callable)
    @inline Trace{G}(callable::F) where {G,F} = Trace{G,F,trace_world_counter()}(callable)
end

@inline unwrap(t::Trace) = t.callable

@inline (t::Trace)(input...) = typedcall(exec_trace, t, input...)

debug_trace(f, input...) = Trace{VoidGenre,typeof(f),typemin(UInt)}(f)(input...)

function println_trace_debug(::Type{Trace{G,F,w}}, expr, argtypes) where {G,F,w}
    println("-----------------------------------------------------------")
    println("Debug Info For Trace{$G,$F}")
    println("Argument Types: ", argtypes)
    println(expr)
    println("-----------------------------------------------------------")
end

# TODO: Just make a single n-ary version of `Trace(...)`. The difficulty here comes from
# matching the generator's splatted tuple argument with the "pre-splatted" arguments
# represented as numbered slots in the generated CodeInfo.

function _generated_trace_body(::Type{Trace{G,F,w}}, varsyms, argtypes...) where {G,F,w}
    method_list = methods_by_type_sig(Tuple{F,unwrap.(argtypes)...})
    if length(method_list) != 1
        # assumes that `t` and `G` are valid variables in caller scope
        return quote
            $(Expr(:meta, :inline))
            Play{G}(unwrap(t))($(varsyms...))
        end
    else
        code_info, static_params = code_info_from_method_info(method_list[])
        for i in 1:length(varsyms)
            code_info.slotnames[i+1] = varsyms[i]
        end
        return intercepted_code_info!(code_info, static_params, G, w)
    end
end

for N in 0:15
    vars = [Symbol("x_$i") for i in 1:N]
    quoted_vars = Expr(:tuple, [Expr(:quote, v) for v in vars]...)
    @eval begin
        @generated function exec_trace(t::Trace{G,F,w}, $(vars...)) where {G,F,w}
            expr = _generated_trace_body(t, $(quoted_vars), $(vars...))
            w === typemin(UInt) && println_trace_debug(t, expr, [$(vars...)])
            return expr
        end
    end
end
