from typing import Optional, Any
from error import Error, Pos
import bbr_ast as ast

# NOTE: for example the arguments of the function have the following value
class Mok:
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}unknown_at_comptime\n"
UNKNOWN_VALUE_AT_COMPTIME = Mok()

class Context:
    def __init__(self, prev: Context):
        self.prev_ctx: Context = prev
        self.vars: list[ast.Var] = []
        if prev is not None:
            self._copy_vars()

    def _copy_vars(self):
        for var in self.prev_ctx.vars:
            self.append_var(var)
    
    def get_var_named(self, name: str) -> Optional[ast.Var]:
        for var in self.vars:
            if var.decl.name == name: return var
        return None

    def append_var(self, var: ast.Var) -> Optional[Error]:
        if self.get_var_named(var.decl.name) is not None:
            return Error(f"there is already a variable named `{var.decl.name}` in the context", var.decl.start)
        self.vars.append(var)


def _get_defualt_value(base_type: ast.TypeKind, start: Pos) -> Any:
    match base_type:
        case ast.TypeKind.i8|ast.TypeKind.i16|ast.TypeKind.i32|ast.TypeKind.i64: return ast.IntLiteralRepr(0, start)
        case ast.TypeKind.u8|ast.TypeKind.u16|ast.TypeKind.u32|ast.TypeKind.u64: return ast.IntLiteralRepr(0, start)
    raise NotImplementedError

def _resolve_var_promise(promise: ast.VarPromise, ctx: Context) -> ast.Var | Error:
    if (var := ctx.get_var_named(promise.name)) is None:
        return Error(f"unknown variable `{promise.name}`", promise.start)
    return var

def _check_dirv_cinc(cinc: ast.CincDirectiveRepr) -> Optional[Error]:
    raise NotImplementedError

def _check_dirv_call(call: ast.DirectiveCallStatement) -> Optional[Error]:
    raise NotImplementedError

def _check_decl_statement(decl: ast.DeclRepr, ctx: Context) -> Optional[Error]:
    return ctx.append_var(ast.Var(decl, _get_defualt_value(decl.type.base_type, decl.start)))

def _check_assign_statement(assign: ast.AssignStatement, ctx: Context) -> Optional[Error]:
    # NOTE: pretend that assign.assignable is always VarPromise
    assert(type(assign.assignable) == ast.VarPromise)
    if type(actual_var := _resolve_var_promise(assign.assignable, ctx)) == Error:
        return actual_var
    assign.assignable = actual_var

    if type(assign.expr) == ast.VarPromise:
        if type(actual_var := _resolve_var_promise(assign.expr, ctx)) == Error:
            return actual_var
        assign.expr = actual_var
    else:
        return _check_expression(assign.expr, ctx)
def _check_ret_statement(ret: ast.RetStatement, ctx: Context) -> Optional[Error]:
    if type(ret.expr) == ast.VarPromise:
        if type(actual_var := _resolve_var_promise(ret.expr, ctx)) == Error:
            return actual_var
        ret.expr = actual_var
    else:
        return _check_expression(ret.expr, ctx)

def _check_for_statement(_for: ast.ForStatement, ctx: Context) -> Optional[Error]:
    new_ctx = Context(ctx)
    if _for.init is not None:
        if type(_for.init) == ast.DeclRepr:
            if type(err := _check_decl_statement(_for.init, new_ctx)) == Error: return err
        elif type(_for.init) == ast.Var:
            if type(err := _check_init_var(_for.init, new_ctx)) == Error: return err
    if _for.cond is not None:
        if type(err := _check_math_expr(_for.cond, new_ctx)) == Error: return err
    if _for.inc is not None:
        if type(err := _check_math_expr(_for.inc, new_ctx)) == Error: return err
    
    return _check_body(_for.body, new_ctx)

def _check_statement(state: ast.ValidStatement, ctx: Context) -> Optional[Error]:
    match type(state):
        case ast.RetStatement: return _check_ret_statement(state, ctx)
        # TODO: check for statement
        case ast.ForStatement: return _check_for_statement(state, ctx)
        case ast.C_CallStatement: return _check_c_call_expr(state.c_call_repr, ctx)
        case ast.CallStatement: return _check_call_expr(state.call_repr, ctx)
        # TODO: check for statement
        case ast.DirectiveCallStatement: return
        case ast.InitStatement: return _check_init_var(state.var, ctx)
        case ast.DeclStatement: return _check_decl_statement(state.decl, ctx)
        case ast.AssignStatement: return _check_assign_statement(state, ctx)
    raise NotImplementedError

def _check_body(body: ast.BodyRepr, new_ctx: Context) -> Optional[Error]:
    # NOTE: maybe it's _check_body's responsibility to create the ctx, but i haven't decided yet
    #       so it is passed externally and it is the responsibility of the calling function
    for state in body.statements:
        if type(err := _check_statement(state, new_ctx)) == Error: return err
    new_ctx = new_ctx.prev_ctx

def _check_math_expr(math: ast.MathRepr, ctx: Context) -> Optional[Error]:
    c = 0
    for i, v in enumerate(math.seq):
        match type(v):
            case ast.IntLiteralRepr|ast.FloatLiteralRepr|ast.CallRepr|ast.Var:
                c += 1
            case ast.VarPromise:
                if type(actual_var := _resolve_var_promise(v, ctx)) == Error:
                    return actual_var
                c += 1
                math.seq[i] = actual_var
            case ast.UnaryOpRepr:
                c -= 1
                if c < 0:
                    return Error(f"not enough values on stack to perform unary operation `{v.type.name}`, expects at least 1 value", v.start)
                c += 1
            case ast.BinaryOpRepr:
                c -= 2
                if c < 0:
                    return Error(f"not enough values on stack to perform binary operation `{v.type.name}`, expects at least 2 values", v.start)
                c += 1
            case ast.BuiltinRepr:
                if v.type not in [ast.BuiltinKind.eq, ast.BuiltinKind.neq,
                                ast.BuiltinKind.lt, ast.BuiltinKind.nlt,
                                ast.BuiltinKind.gt, ast.BuiltinKind.ngt]:
                    return Error(f"mathable may be only comparison related built-ins, like `@lt`, not `@{v.type.name}`", v.start)
                c -= 2
                if c < 0:
                    return Error(f"not enough values on stack to perform comparison operation `{v.type.name}`, expects at least 2 values", v.start)
                c += 1

    if c != 1:
        return Error(f"arithmetic operation must result 1 value, got {c} - that's undefined behavior", math.seq[-1].start)

# TODO: check types
def _check_call_expr(call: ast.CallRepr, ctx: Context) -> Optional[Error]:
    if type(call.callable) == ast.VarPromise:
        if type(actual_var := _resolve_var_promise(call.callable, ctx)) == Error:
            actual_var
        call.callable = actual_var
    if type(call.callable.decl.type.details) != ast.FnSignatureRepr:
        return Error(f"you can call variables with `fn` type, not `{call.callable.decl.type.typename}`", call.callable.start)
    fn_repr: ast.FnSignatureRepr = call.callable.decl.type.details
    if (fn_len := len(fn_repr.args)) != (call_len := len(call.args)):
        return Error(f"you try to call function with {fn_len} arguments passing {call_len}", call.start)
    gen = (i for i in range(len(fn_repr.args)))
    for i, in_fn, in_call in zip(gen, fn_repr.args, call.args):
        if type(in_call) == ast.VarPromise:
            if type(actual_var := _resolve_var_promise(in_call, ctx)) == Error:
                return actual_var
            call.args[i] = actual_var
        elif type(in_call) == ast.UseDefualtRepr and in_fn.by_defualt_val is None:
            return Error(f"you try to use defualt value of argument without defualt value", in_call.start)
        # TODO: check types

def _check_c_call_expr(c_call: ast.C_CallRepr, ctx: Context) -> Optional[Error]:
    for i, in_call in enumerate(c_call.args):
        if type(in_call) == ast.VarPromise:
            if type(actual_var := _resolve_var_promise(in_call, ctx)) == Error:
                return actual_var
            c_call.args[i] = actual_var
        else:
            if type(err := _check_expression(in_call, ctx)) == Error:
                return err

def _check_expression(expr: ast.MinorExpression, ctx: Context) -> Optional[Error]:
    match type(expr):
        case ast.MathRepr: return _check_math_expr(expr, ctx)
        case ast.IntLiteralRepr | ast.FloatLiteralRepr | ast.StringLiteralRepr : return
        case ast.C_CallRepr: return _check_c_call_expr(expr, ctx)
        case ast.CallRepr: return _check_call_expr(expr, ctx)
        case ast.Var: return
        case ast.VarPromise: raise NotImplementedError
        # NOTE: it's caller responsibility to replace VarPromise with Var's
        case ast.BuiltinRepr: raise NotImplementedError

    raise NotImplementedError

def _check_var_value(var_value: ast.ValidVarValue, ctx: Context) -> Optional[Error]:
    if type(var_value) == ast.BodyRepr: 
        return _check_body(var_value, ctx)
    return _check_expression(var_value, ctx)
    
def _check_init_var(var: ast.Var|ast.VarPromise, ctx: Context) -> Optional[Error]:
    if type(var) == ast.VarPromise:
        if type(actual_var := _resolve_var_promise(var, ctx)) == Error:
            return actual_var
        if type(err := ctx.append_var(actual_var)) == Error: return err
    else:
        if type(err := ctx.append_var(var)) == Error: return err
    
    if type(var.value) == ast.VarPromise:
        if type(actual_var := _resolve_var_promise(var.value, ctx)) == Error:
            return actual_var
        var.value = actual_var
    if var.decl.type.base_type == ast.TypeKind.fn:
        fn_repr: ast.FnSignatureRepr = var.decl.type.details 
        ctx = Context(ctx)
        for fn_decl in fn_repr.args:
            ctx.append_var(ast.Var(fn_decl.decl, UNKNOWN_VALUE_AT_COMPTIME))
    return _check_var_value(var.value, ctx)


def check(_ast: ast.AST) -> list[Error]:
    ctx = Context(None)
    # TODO: check directives
    errs: list[Error] = []
    for var in _ast.vars:
        if type(err := _check_init_var(var, ctx)) == Error:
            errs.append(err)
    print(_ast)
    return errs
    
