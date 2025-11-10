from typing import Optional, Any
from enum import Enum, auto
from lexer import Token, Tokenizer, TokType, Keyword, read
from error import Error, Pos
from feed import TokenFeed
import bbr_ast as ast

def _parse_directive(feed: TokenFeed) -> ast.DirectiveRepr | Error:
    if type(start := feed.consume_and_check(TokType.HASH, "directive call should begin with hashtag `#`, not `{}`")) == Error:
        return start
    command = feed.consume()
    match command.val:
        case "cinc":
            if type(arg := feed.consume_and_check(TokType.STR_LIT, "#cinc directive expects string literal path to .h file, not `{}`")) == Error:
                return arg
            if type(t := feed.consume_and_check(TokType.SEMI, "directive call should end with semicolon `;`, not `{}`")) == Error:
                return t
            return ast.CincDirectiveRepr(arg.val, start.pos)


def _parse_decl(feed: TokenFeed, ctx: ast.Context) -> ast.DeclRepr | Error:
    if type(name := feed.consume_and_check(TokType.VAR, "declaration should start with string literal, like `res_10`, not `{}`")) == Error:
        return name
    if type(decl_type := _parse_type(feed, ctx)) == Error:
        return decl_type
    return ast.DeclRepr(name.val, decl_type, name.pos)

def _parse_fn_signature_repr_type(feed: TokenFeed, ctx: ast.Context) -> ast.FnSignatureRepr | Error:
    if type(start := feed.consume_and_check(TokType.VAR, "function declaration should start with string literal, like `fn`, not `{}`")) == Error:
        return start
    if start.val != Keyword.fn.value:
        return Error(f"function declaration should start with `fn` keyword, not `{start.val}`", start.pos)
    if type(t := feed.consume_and_check(TokType.OP_PAR, "function declaration should contain information about arguments which starts with paren `(`, not `{}`")) == Error:
        return t
    
    decls = []
    maybe_cl_par = feed.peek(0)
    while maybe_cl_par.type != TokType.CL_PAR:
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par.type == TokType.VAR:
            if type(decl := _parse_decl(feed, ctx)) == Error:
                return decl
            decls.append(decl)
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par.type != TokType.CL_PAR and maybe_cl_par.type != TokType.COM:
            return Error(f"arguments in function declaration should be separated by comma `,` not `{maybe_cl_par.val}`", maybe_cl_par.pos)
        if maybe_cl_par.type == TokType.COM:
            feed.consume()

    # TODO: read arguments
    if type(t := feed.consume_and_check(TokType.CL_PAR, "function declaration should contain information about arguments which ends with paren `)`, not `{}`")) == Error:
        return t
    
    ret_type = None
    if (maybe_ret := feed.peek(0)).type == TokType.VAR:
        ret_type = _parse_type(feed, ctx)
        if type(ret_type) == Error: return ret_type 

    return ast.FnSignatureRepr(decls, ret_type, start.pos)

def _parse_type(feed: TokenFeed, ctx: ast.Context) -> ast.TypeRepr | Error:
    start = feed.peek(0)
    if start.type != TokType.VAR: return Error(f"type declaration should be string literal, like `i32`, not `{start.val}`", start.pos)
    match start.val:
        case "i64":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.i64, None, start.pos)
        case "i32":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.i32, None, start.pos)
        case "i16":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.i16, None, start.pos)
        case "i8" :
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.i8 , None, start.pos)

        case "u64":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.u64, None, start.pos)
        case "u32":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.u32, None, start.pos)
        case "u16":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.u16, None, start.pos)
        case "u8" :
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.u8 , None, start.pos)

        case "type":
            feed.consume()
            return ast.TypeRepr(ast.TypeKind.type, None, start.pos)
        case "fn"  :
            if type(fn_repr := _parse_fn_signature_repr_type(feed, ctx)) == Error:
                return fn_repr
            return ast.TypeRepr(ast.TypeKind.fn, fn_repr, start.pos)
    
    # NOTE: we are looking for user-defined var in the ctx
    if (v := ctx.get_var_named(start.val)) is not None:
        feed.consume()
        return ast.TypeRepr(v.value.base_type, ast.UserDefinedTypeData(v.value, v.decl.name), start.pos)

    return Error(f"unknown type `{start.val}`", start.pos)

def _parse_int_literal(feed: TokenFeed, base_type: ast.TypeKind) -> ast.IntLiteralRepr | Error:
    if type(base := feed.consume_and_check(TokType.INT_LIT, "int literal should consist of digits, like `123`, not `{}`")) == Error:
        return base
    if (byte_count := ast.get_byte_count(base_type)) is None:
        return Error(f"int literal can not be assigned to `{base_type.name}`", base.start)
    
    # TODO: assignement and type-convertion check
    return ast.IntLiteralRepr(byte_count, int(base.val), base.pos)

def _parse_bin_op(feed: TokenFeed) -> ast.BinaryOpRepr | Error:
    op = feed.consume()
    match op.type:
        case TokType.PLUS  : return ast.BinaryOpRepr(ast.BinOpType.ADD, op.pos)
        case TokType.MINS  : return ast.BinaryOpRepr(ast.BinOpType.SUB, op.pos)
        case TokType.ASTRKS: return ast.BinaryOpRepr(ast.BinOpType.MUL, op.pos)
        case TokType.DIV   : return ast.BinaryOpRepr(ast.BinOpType.DIV, op.pos)
        case TokType.PERC  : return ast.BinaryOpRepr(ast.BinOpType.MOD, op.pos)
    return Error(f"unknown binary operation `{op.val}`", op.pos)

def _parse_unary_op(feed: TokenFeed) -> ast.UnaryOpRepr | Error:
    op = feed.consume()
    match op.type:
        case TokType.DEC: return ast.UnaryOpRepr(ast.UnaryOpType.DEC, op.pos)
        case TokType.INC: return ast.UnaryOpRepr(ast.UnaryOpType.INC, op.pos)
    return Error(f"unknown binary operation `{op.val}`", op.pos)

# TODO: add ctx
def _parse_math(feed: TokenFeed, base_type: ast.TypeKind) -> ast.MathRepr | Error:
    if (start := feed.consume_and_check(TokType.OP_TRI, "math expression should start with open triangle `<`, not `{}`")) == Error:
        return start
    
    seq = []
    maybe_cl_tri = feed.peek(0)
    # TODO: add call support
    while maybe_cl_tri.type != TokType.CL_TRI:
        maybe_cl_tri = feed.peek(0)
        match maybe_cl_tri.type:
            case TokType.INT_LIT:
                if type(lit := _parse_int_literal(feed, base_type)) == Error:
                    return lit
                seq.append(lit)
            case TokType.PLUS|TokType.MINS|TokType.ASTRKS|TokType.DIV|TokType.PERC:
                if type(op := _parse_bin_op(feed)) == Error:
                    return op
                seq.append(op)
            case TokType.INC|TokType.DEC:
                if type(op := _parse_unary_op(feed)) == Error:
                    return op
                seq.append(op)
            case _:
                return Error(f"unknown mathable expression `{maybe_cl_tri.val}`", maybe_cl_tri.pos)
        maybe_cl_tri = feed.peek(0)


    if (end := feed.consume_and_check(TokType.CL_TRI, "math expression should end with close triangle `>`, not `{}`")) == Error:
        return end
    return ast.MathRepr(seq)

# TODO: add ctx
def _parse_call(feed: TokenFeed):
    raise NotImplementedError

# TODO: add ctx
def _parse_minor_expression(feed: TokenFeed, base_type: ast.TypeKind) -> ast.MinorExpression | Error:
    base = feed.peek(0)
    match base.type:
        case TokType.INT_LIT: return _parse_int_literal(feed, base_type)
        case TokType.OP_TRI:  return _parse_math(feed, base_type)

# TODO: add ctx
def _parse_composite_expression(feed: TokenFeed) -> ast.CompositeExpression | Error:
    base = feed.peek(0)
    action = feed.peek(1)
    match action.type:
        case TokType.OP_PAR: return _parse_call(feed)
    return Error(f"unknow composite expression `{base.val}{action.val}`", base.pos)

# TODO: add ctx
def _parse_ret_statement(feed: TokenFeed, ret_type: ast.TypeRepr) -> ast.RetStatement | Error:
    if type(start := feed.consume_and_check(TokType.RET, "return statement should start with `ret` keyword, not `{}`")) == Error:
        return start
    if ret_type is None and (t := feed.peek(0)).type != TokType.SEMI:
        return Error(f"function declared with no return value, but you returning `{t.val}`", t.pos)
    if ret_type is None:
        if type(t := feed.consume_and_check(TokType.SEMI, "return statement should end with semicolon `;`, not `{}`")) == Error:
            return t
        return ast.RetStatement(None, start.pos)

    if type(expr := _parse_composite_expression(feed)) == Error:
        if type(expr := _parse_minor_expression(feed, ret_type.base_type)):
            if not feed.len():
                return Error(f"unknown expression which starts with `{feed.peek(0).val}`", start.pos)
        if type(t := feed.consume_and_check(TokType.SEMI, "return statement should end with semicolon `;`, not `{}`")) == Error:
            return t
    return ast.RetStatement(expr, start.pos)

# TODO: add ctx
def _parse_statement(feed: TokenFeed, fn_repr: ast.FnSignatureRepr) -> ast.ValidStatement | Error:
    base = feed.peek(0)
    match base.type:
        # TODO: get ret type from ctx
        case TokType.RET: return _parse_ret_statement(feed, fn_repr.ret)
    return _parse_composite_expression(feed)


# TODO: add ctx
def _parse_body(feed: TokenFeed, callee_decl: ast.DeclRepr) -> ast.BodyRepr | Error:
    if type(start := feed.consume_and_check(TokType.OP_CBR, "body should begin with curly brace `{`, not `{}`")) == Error:
        return start
    
    statements = []
    maybe_cl_cbr = feed.peek(0)
    while maybe_cl_cbr.type != TokType.CL_CBR:
        if type(state := _parse_statement(feed, callee_decl.type.details)) == Error:
            return state
        statements.append(state)
        maybe_cl_cbr = feed.peek(0)

    if type(end := feed.consume_and_check(TokType.CL_CBR, "body should end with curly brace `}`, not `{}`")) == Error:
        return end
    return ast.BodyRepr(statements, start.pos)

def _parse_assignment(feed: TokenFeed, ctx: ast.Context) -> ast.Var | Error:
    if type(start := feed.consume_and_check(TokType.VAR, "name of variable should be string literal, like `res10`, not `{}`")) == Error:
        return start
    if type(var_type := _parse_type(feed, ctx)) == Error:
        return var_type
    if type(t := feed.consume_and_check(TokType.DDICK, "variable is assigned with double-dick operator `:=`, not `{}`")) == Error:
        return t
    base = feed.peek(0)
    # TODO: add type checking
    decl = ast.DeclRepr(start.val, var_type, start.pos)

    match base.type:
        case TokType.OP_CBR: # NOTE: ... := {}
            if type(body := _parse_body(feed, decl)) == Error:
                return body
            return ast.Var(decl, body)

        case TokType.INT_LIT: # NOTE: ... := 50;
            if type(int_lit := _parse_int_literal(feed, decl.type.base_type)) == Error:
                return int_lit
            if type(t := feed.consume_and_check(TokType.SEMI, "assigning int literal should end with semicolon `;`, not `{}`")) == Error:
                return t
            return ast.Var(decl, int_lit)

        case TokType.VAR: # NOTE: ... := i32;
            if type(ancestor_type := _parse_type(feed, ctx)) == Error:
                return ancestor_type
            if type(t := feed.consume_and_check(TokType.SEMI, "assigning type should end with semicolon `;`, not `{}`")) == Error:
                return t    
            var = ast.Var(decl, ast.TypeRepr(ancestor_type.base_type, ancestor_type.details, ancestor_type.start))
            if type(err := ctx.append_var(var)) == Error: return err
            return var
    return Error(f"unknown way to assign variable to `{base.val}`", base.pos)

def _parse_highest_level(feed: TokenFeed, ctx: ast.Context) -> ast.DirectiveRepr | ast.Var | Error:
    base = feed.peek(0)
    if base.type == TokType.HASH:
        return _parse_directive(feed)
    if base.type == TokType.VAR:
        return _parse_assignment(feed, ctx)

def parse(toks: list[Token]) -> ast.AST | Error:
    feed: TokenFeed = TokenFeed(toks)
    dirvs = []
    vars = []
    ctx = ast.Context(None)
    while feed.len():
        v = _parse_highest_level(feed, ctx)
        if   type(v) == ast.Var: vars.append(v)
        elif isinstance(v, ast.DirectiveRepr): dirvs.append(v)
        elif type(v) == ast.Error: return v
    return ast.AST(dirvs, vars)

def test_parse():
    print(parse(read("add i32 := 60;")))
    print(parse(read("myint type := i32;")))
    a = parse(read("""
                   myint type := i32;
                   test fn(a myint, b myint) myint := { ret 5; }
                   """))
    print(a)
    pass


if __name__ == "__main__":
    test_parse()
