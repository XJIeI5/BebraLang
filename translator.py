from typing import Optional, Any
from io import Writer
from sys import stdout
from enum import Enum, auto
from lexer import Token, Tokenizer, TokType, Keyword, read
from parser import parse
from error import Error, Pos
from feed import TokenFeed
import bbr_ast as ast

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

def _get_str_mathable(mathable: ast.ValidMathableType) -> str:
    match type(mathable):
        case ast.Var: return mathable.decl.name
        case ast.CallRepr: return _transt_call_expr(mathable)
        case ast.IntLiteralRepr: return str(mathable.value)
        case ast.BinaryOpRepr: return binop_lookup[mathable.type]
        case ast.UnaryOpRepr: return unop_lookup[mathable.type]

def _transt_math_expr(math: ast.MathRepr) -> str:
    stack: list[str] = []

    for mathable in math.seq:
        if   type(mathable) in [ast.IntLiteralRepr, ast.FloatLiteralRepr]:
            stack.append(_get_str_mathable(mathable))
        elif type(mathable) == ast.Var:
            stack.append(_get_str_mathable(mathable))
        elif type(mathable) == ast.BinaryOpRepr:
            b = stack.pop()
            a = stack.pop()
            stack.append(f"({a}{_get_str_mathable(mathable)}{b})")
        elif type(mathable) == ast.UnaryOpRepr:
            a = stack.pop()
            stack.append(f"({a}{_get_str_mathable(mathable)})")
    
    return stack.pop()

def _transt_call_expr(call: ast.CallRepr) -> str:
    args = ", ".join([_get_str_arg(arg) for arg in call.args])
    return f"{call.callable.decl.name}({args})"

def _transt_c_call_expr(c_call: ast.C_CallRepr) -> str:
    args = ", ".join([_get_str_arg(arg) for arg in c_call.args])
    return f"{c_call.callable}({args})"

def _transt_expr(expr: ast.CompositeExpression|ast.MinorExpression) -> str:
    match type(expr):
        case ast.IntLiteralRepr: return str(expr.value)
        case ast.StringLiteralRepr: return expr.value
        case ast.CallRepr: return _transt_call_expr(expr)
        case ast.C_CallRepr: return _transt_c_call_expr(expr)
        case ast.MathRepr: return _transt_math_expr(expr)
    return ""

def _transt_ret_statement(ret: ast.RetStatement) -> str:
    return f"return {_transt_expr(ret.expr)};\n"

def _transt_body(body: ast.BodyRepr) -> str:
    statements = []
    for state in body.statements:
        match type(state):
            case ast.RetStatement: statements.append(_transt_ret_statement(state))
            case ast.CallStatement: statements.append(f"{_transt_call_expr(state.call_repr)};")
            case ast.C_CallStatement: statements.append(f"{_transt_c_call_expr(state.c_call_repr)};")
    return "\n    ".join(statements)

def _transt_fn_def(name: str, fn_repr: ast.FnSignatureRepr, body: ast.BodyRepr) -> str:
    ret = "void" if fn_repr.ret is None else _get_type_name(fn_repr.ret)
    args = ", ".join([f"{_get_type_name(arg.type)} {arg.name}" for arg in fn_repr.args])
    return f"{ret} {name}({args}) {{\n{_transt_body(body)}}}"


def _transt_typedef(name: str, decl_type: ast.TypeRepr) -> str:
    return f"typedef {_get_type_name(decl_type)} {name};\n"

def _transt_var(var: ast.Var) -> str:
    match type(var.value):
        case ast.BodyRepr: return _transt_fn_def(var.decl.name, var.decl.type.details, var.value)
        case ast.TypeRepr: return _transt_typedef(var.decl.name, var.value)

def _transt_cinc_directive(cinc: ast.CincDirectiveRepr) -> str:
    return f"#include {cinc.path}\n"

def _transt_directive(dirv: ast.DirectiveRepr) -> str:
    if type(dirv) == ast.CincDirectiveRepr: return _transt_cinc_directive(dirv)

def translate_to(_ast: ast.AST, ostream: Writer):
    for dirv in _ast.directives:
        ostream.write(_transt_directive(dirv))
    
    # TODO: reg var beforehand to make them independent of the order of the declaration
    for var in _ast.vars:
        ostream.write(_transt_var(var))

def test_translate():
    _ast = parse(read("""
                      #cinc "<stdio.h>";

                      main fn() := {
                        >printf("Hello, Bebralang!\n");
                      }
                      """))
    if type(_ast) == Error:
        print(_ast)
        return
    translate_to(_ast, stdout)
    stdout.close()

if __name__ == "__main__":
    test_translate()
