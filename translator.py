from typing import Optional, Any
from io import Writer
from sys import stdout
from enum import Enum, auto
from error import Error, Pos
from lexer import Token, Tokenizer, TokType, Keyword, read
from parser import parse
from checker import check
from feed import TokenFeed
import bbr_ast as ast

# TODO: get rid of ast.TypePromiseRepr
#       it's checker responsibility to remove them
#       but they stay in the code around ast.TypeRepr as mok


class C_Code(str):
    def __init__(self, line: Optional[str]):
        if line is None:
            self.lines: list[str] = []
        else:
            self.lines: list[str] = [line]
        self._lift: int = 0
    
    def lift(self, space_count: int) -> None:
        self._lift += space_count
    
    def __str__(self) -> str:
        return (" "*self._lift).join(self.lines)

type_lookup = {
    ast.TypeKind.i64: "int64_t",
    ast.TypeKind.i32: "int32_t",
    ast.TypeKind.i16: "int16_t",
    ast.TypeKind.i8 : "int64_t",

    ast.TypeKind.u64: "uint64_t",
    ast.TypeKind.u32: "uint32_t",
    ast.TypeKind.u16: "uint16_t",
    ast.TypeKind.u8 : "uint64_t",
}

binop_lookup = {
    ast.BinOpType.ADD: "+",
    ast.BinOpType.SUB: "-",
    ast.BinOpType.MUL: "*",
    ast.BinOpType.DIV: "/",
    ast.BinOpType.REM: "%",
}

unop_lookup = {
    ast.UnaryOpType.DEC: "--",
    ast.UnaryOpType.INC: "++",
}

builtin_lookup = {
    ast.BuiltinKind.eq : "==",
    ast.BuiltinKind.neq: "!=",
    ast.BuiltinKind.gt : ">" ,
    ast.BuiltinKind.lt : "<" ,
    ast.BuiltinKind.ngt: ">=",
    ast.BuiltinKind.nlt: "<=",
}

def _get_defualt_value(base_type: ast.TypeKind) -> str:
    match base_type:
        case ast.TypeKind.i8|ast.TypeKind.i16|ast.TypeKind.i32|ast.TypeKind.i64: return "0"
        case ast.TypeKind.u8|ast.TypeKind.u16|ast.TypeKind.u32|ast.TypeKind.u64: return "0"
    raise NotImplementedError


def _get_type_name(t: ast.TypeRepr) -> str:
    if type(t.details) == ast.UserDefinedTypeData:
        return t.details.this_name
    return type_lookup[t.base_type]

def _get_str_arg(arg: ast.ValidCallArg) -> str:
    match type(arg):
        case ast.Var: return arg.decl.name
        case ast.CallRepr: return _transt_call_expr(arg)
        case ast.IntLiteralRepr: return str(arg.value)
        case ast.StringLiteralRepr: return f"\"{arg.value}\""
    raise NotImplementedError(type(arg))

def _get_str_mathable(mathable: ast.ValidMathableType) -> str:
    match type(mathable):
        case ast.Var: return mathable.decl.name
        case ast.CallRepr: return _transt_call_expr(mathable)
        case ast.IntLiteralRepr: return str(mathable.value)
        case ast.BinaryOpRepr: return binop_lookup[mathable.type]
        case ast.UnaryOpRepr: return unop_lookup[mathable.type]
        case ast.BuiltinRepr: return builtin_lookup[mathable.type]
    raise NotImplementedError(type(mathable))

def _transt_math_expr(math: ast.MathRepr) -> str:
    stack: list[str] = []

    for mathable in math.seq:
        if   type(mathable) in [ast.IntLiteralRepr, ast.FloatLiteralRepr]:
            stack.append(_get_str_mathable(mathable))
        elif type(mathable) in [ast.Var]:
            stack.append(_get_str_mathable(mathable))
        elif type(mathable) == ast.BinaryOpRepr:
            b = stack.pop()
            a = stack.pop()
            stack.append(f"({a}{_get_str_mathable(mathable)}{b})")
        elif type(mathable) == ast.UnaryOpRepr:
            a = stack.pop()
            stack.append(f"({a}{_get_str_mathable(mathable)})")
        elif type(mathable) == ast.BuiltinRepr:
            b = stack.pop()
            a = stack.pop()
            stack.append(f"({a}{_get_str_mathable(mathable)}{b})")
        else:
            raise NotImplementedError
    
    return stack.pop()

def _transt_decl_statement(state: ast.DeclStatement) -> str:
    return f"{_get_type_name(state.decl.type)} {state.decl.name} = {_get_defualt_value(state.decl.type.base_type)};\n"

def _transt_call_expr(call: ast.CallRepr) -> str:
    args = []
    for i, arg in enumerate(call.args):
        if type(arg) == ast.UseDefualtRepr:
            args.append(_get_str_arg(call.callable.decl.type.details.args[i].by_defualt_val))
        else:
            args.append(_get_str_arg(arg))
    args = ", ".join(args)
    
    return f"{call.callable.decl.name}({args})"

def _transt_c_call_expr(c_call: ast.C_CallRepr) -> str:
    args = ", ".join([_get_str_arg(arg) for arg in c_call.args])
    return f"{c_call.callable}({args})"

def _transt_expr(expr: ast.MinorExpression) -> str:
    if expr is None: return ""
    match type(expr):
        case ast.IntLiteralRepr: return str(expr.value)
        case ast.StringLiteralRepr: return expr.value
        case ast.CallRepr: return _transt_call_expr(expr)
        case ast.C_CallRepr: return _transt_c_call_expr(expr)
        case ast.MathRepr: return _transt_math_expr(expr)
        # TODO: replace with _transt_var
        case ast.Var: return expr.decl.name
        case ast.DeclRepr: return f"{_get_type_name(expr.type)} {expr.name} = {_get_defualt_value(expr.type.base_type)}"
    raise NotImplementedError

def _transt_assign_statement(assign: ast.AssignStatement) -> str:
    return f"{assign.assignable.decl.name} = {_transt_expr(assign.expr)};\n"

def _transt_init_statement(init: ast.InitStatement) -> str:
    return f"{_get_type_name(init.var.decl.type)} {init.var.decl.name} = {_transt_expr(init.var.value)};\n"

def _transt_ret_statement(ret: ast.RetStatement) -> str:
    return f"return {_transt_expr(ret.expr)};\n"

def _transt_for_statement(_for: ast.ForStatement) -> str:
    init = _transt_expr(_for.init)
    cond = _transt_math_expr(_for.cond)
    inc  = _transt_math_expr(_for.inc)
    return f"for ({init}; {cond}; {inc}) {{\n{_transt_body(_for.body)}}}\n"


def _transt_body(body: ast.BodyRepr) -> str:
    statements = []
    for state in body.statements:
        match type(state):
            case ast.RetStatement: statements.append(_transt_ret_statement(state))
            case ast.ForStatement: statements.append(_transt_for_statement(state))
            case ast.CallStatement: statements.append(f"{_transt_call_expr(state.call_repr)};\n")
            case ast.C_CallStatement: statements.append(f"{_transt_c_call_expr(state.c_call_repr)};\n")
            case ast.DeclStatement: statements.append(_transt_decl_statement(state))
            case ast.AssignStatement: statements.append(_transt_assign_statement(state))
            case ast.InitStatement: statements.append(_transt_init_statement(state))
    return "    ".join([""]+statements)

def _transt_fn_def(name: str, fn_repr: ast.FnSignatureRepr, body: ast.BodyRepr) -> str:
    ret = "void" if fn_repr.ret is None else _get_type_name(fn_repr.ret)
    args = ", ".join([f"{_get_type_name(arg.decl.type)} {arg.decl.name}" for arg in fn_repr.args])
    return f"{ret} {name}({args}) {{\n{_transt_body(body)}}}\n"


def _transt_typedef(name: str, decl_type: ast.TypeRepr) -> str:
    return f"typedef {_get_type_name(decl_type)} {name};\n"

def _transt_var(var: ast.Var) -> str:
    match type(var.value):
        case ast.BodyRepr: return _transt_fn_def(var.decl.name, var.decl.type.details, var.value)
        case ast.TypeRepr: return _transt_typedef(var.decl.name, var.value)

def _transt_cinc_directive(cinc: ast.CincDirectiveRepr) -> str:
    return f"#include {cinc.path}\n"

def _transt_directive(dirv_call: ast.DirectiveCallStatement) -> str:
    dirv = dirv_call.dirv
    if type(dirv) == ast.CincDirectiveRepr: return _transt_cinc_directive(dirv)

def get_builtin() -> str:
    buf = f"#include <stdint.h>\n"
    return buf

def translate_to(_ast: ast.AST, ostream: Writer):
    ostream.write(get_builtin())
    for dirv in _ast.directives:
        ostream.write(_transt_directive(dirv))
    
    # TODO: reg var beforehand to make them independent of the order of the declaration
    for var in _ast.vars:
        ostream.write(_transt_var(var))

def test_translate():
    toks = read(
        """
        foo fn(a i32 = -1) i32 := {
            ret a;
        }

        fib fn(n u64) u64 := {
            a, c u64;
            b u64 := 1;
            for i u64; <i n @lt>; <i++> {
                c = b;
                b = <a b +>;
                a = c;
            }
            ret a;
        }

        main fn() := {
            fib(10);
            foo(_);
            foo(100);
        }
        """
    )
    _ast = parse(toks)
    if type(_ast) == Error:
        print(_ast)
        return
    if len(errs := check(_ast)) > 0:
        print(*errs)
        return
    print(_ast)
    translate_to(_ast, stdout)
    stdout.close()

if __name__ == "__main__":
    test_translate()
