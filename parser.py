from typing import Optional, Any
from enum import Enum, auto
from lexer import Token, Tokenizer, TokType, Keyword, read
from error import Error, Pos
from feed import TokenFeed
import bbr_ast as ast

def _parse_directive(feed: TokenFeed) -> ast.DirectiveCallStatement | Error:
    if type(start := feed.consume_and_check(TokType.HASH, "directive call should begin with hashtag `#`, not `{}`")) == Error:
        return start
    command = feed.consume()
    match command.val:
        case "cinc":
            if type(arg := feed.consume_and_check(TokType.STR_LIT, "#cinc directive expects string literal path to .h file, not `{}`")) == Error:
                return arg
            if type(t := feed.consume_and_check(TokType.SEMI, "directive call should end with semicolon `;`, not `{}`")) == Error:
                return t
            cinc = ast.CincDirectiveRepr(arg.val, start.pos)
            return ast.DirectiveCallStatement(cinc, start.pos)


def _parse_decls(feed: TokenFeed) -> list[ast.DeclRepr] | Error:
    if type(name := feed.consume_and_check(TokType.VAR, "declaration should start with string literal, like `res_10`, not `{}`")) == Error:
        return name
    if (maybe_comma := feed.peek(0)).type == TokType.COM:
        feed.consume()
        if type(decls := _parse_decls(feed)) == Error:
            return decls
        decls.append(ast.DeclRepr(name.val, decls[0].type, name.pos))
        return decls
    if type(decl_type := _parse_type(feed)) == Error:
        return decl_type
    return [ast.DeclRepr(name.val, decl_type, name.pos)]

# TODO: wrap errors
def _parse_fn_decls(feed: TokenFeed) -> list[ast.FnDeclRepr] | Error:
    if type(decls := _parse_decls(feed)) == Error:
        return decls
    if (maybe_eq := feed.peek(0)).type == TokType.EQ:
        feed.consume()
        if type(expr := _parse_expression(feed)) == Error: return expr
        return [ast.FnDeclRepr(decl, expr) for decl in decls]
    return [ast.FnDeclRepr(decl, None) for decl in decls]

def _parse_fn_signature_repr_type(feed: TokenFeed) -> ast.FnSignatureRepr | Error:
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
            if type(new_decls := _parse_fn_decls(feed)) == Error:
                return new_decls
            decls.extend(new_decls)
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par.type != TokType.CL_PAR and maybe_cl_par.type != TokType.COM:
            return Error(f"arguments in function declaration should be separated by comma `,` not `{maybe_cl_par.val}`", maybe_cl_par.pos)
        if maybe_cl_par.type == TokType.COM:
            feed.consume()
        maybe_cl_par = feed.peek(0)

    # TODO: read arguments 
    # UPD : what is this even means?
    if type(t := feed.consume_and_check(TokType.CL_PAR, "function declaration should contain information about arguments which ends with paren `)`, not `{}`")) == Error:
        return t
    
    ret_type = None
    if (maybe_ret := feed.peek(0)).type == TokType.VAR:
        ret_type = _parse_type(feed)
        if type(ret_type) == Error: return ret_type 

    return ast.FnSignatureRepr(decls, ret_type, start.pos)

def _parse_ptr_repr(feed: TokenFeed) -> ast.PtrRepr | Error:
    if type(start := feed.consume_and_check(TokType.ASTRKS, "pointer declaration should start with asterisk `*`, not `{}`")) == Error:
        return start
    if type(t := _parse_type(feed)) == Error:
        return t
    return ast.PtrRepr(t, start.pos)

def _parse_type(feed: TokenFeed) -> ast.Type | Error:
    start = feed.peek(0)
    if start.type == TokType.ASTRKS:
        if type(ptr_repr := _parse_ptr_repr(feed)) == Error:
            return ptr_repr
        return ast.TypeRepr(ast.TypeKind.ptr, ptr_repr, start.pos)
    
    if start.type != TokType.VAR: return Error(f"type declaration should be string literal, like `i32`, not `{start.val}`", start.pos)
    
    if start.val == "fn":
        if type(fn_repr := _parse_fn_signature_repr_type(feed)) == Error:
            return fn_repr
        return ast.TypeRepr(ast.TypeKind.fn, fn_repr, start.pos)
    
    def match_type(typename: str) -> Optional[ast.TypeRepr]:
        match typename:
            case "i64": return ast.TypeRepr(ast.TypeKind.i64, None, start.pos)
            case "i32": return ast.TypeRepr(ast.TypeKind.i32, None, start.pos)
            case "i16": return ast.TypeRepr(ast.TypeKind.i16, None, start.pos)
            case "i8" : return ast.TypeRepr(ast.TypeKind.i8 , None, start.pos)

            case "u64": return ast.TypeRepr(ast.TypeKind.u64, None, start.pos)
            case "u32": return ast.TypeRepr(ast.TypeKind.u32, None, start.pos)
            case "u16": return ast.TypeRepr(ast.TypeKind.u16, None, start.pos)
            case "u8" : return ast.TypeRepr(ast.TypeKind.u8 , None, start.pos)

            case "type": return ast.TypeRepr(ast.TypeKind.type, None, start.pos)
        return None
    
    if (t := match_type(start.val)) is not None:
        feed.consume()
        return t
    
    promise = feed.consume()
    return ast.TypePromiseRepr(promise.val, promise.pos)

def _parse_int_literal(feed: TokenFeed) -> ast.IntLiteralRepr | Error:
    if type(base := feed.consume_and_check(TokType.INT_LIT, "int literal should consist of digits, like `123`, not `{}`")) == Error:
        return base
    try:
        int(base.val)
    except ValueError:
        return Error(f"compile-time integer literal consists of digits, like `123`, nit `{base.val}`", base.pos)
    return ast.IntLiteralRepr(int(base.val), base.pos)

def _parse_str_literal(feed: TokenFeed) -> ast.StringLiteralRepr | Error:
    if type(base := feed.consume_and_check(TokType.STR_LIT, "string literal should begin with quotation, like `\"123\"`, not `{}`")) == Error:
        return base
    return ast.StringLiteralRepr(base.val, base.pos)

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
        case TokType.DEC      : return ast.UnaryOpRepr(ast.UnaryOpType.DEC , op.pos)
        case TokType.INC      : return ast.UnaryOpRepr(ast.UnaryOpType.INC , op.pos)
        case TokType.MATH_PfrV: return ast.UnaryOpRepr(ast.UnaryOpType.PfrV, op.pos)
        case TokType.MATH_VfrP: return ast.UnaryOpRepr(ast.UnaryOpType.VfrP, op.pos)
    return Error(f"unknown binary operation `{op.val}`", op.pos)

def _parse_math(feed: TokenFeed) -> ast.MathRepr | Error:
    if (start := feed.consume_and_check(TokType.OP_TRI, "math expression should start with open triangle `<`, not `{}`")) == Error:
        return start
    
    seq = []
    maybe_cl_tri = feed.peek(0)

    # TODO: add call support
    while maybe_cl_tri.type != TokType.CL_TRI:
        maybe_cl_tri = feed.peek(0)
        match maybe_cl_tri.type:
            case TokType.INT_LIT:
                if type(lit := _parse_int_literal(feed)) == Error: return lit
                seq.append(lit)
            case TokType.PLUS|TokType.MINS|TokType.ASTRKS|TokType.DIV|TokType.PERC:
                if type(op := _parse_bin_op(feed)) == Error: return op
                seq.append(op)
            case TokType.INC|TokType.DEC|TokType.MATH_VfrP|TokType.MATH_PfrV:
                if type(op := _parse_unary_op(feed)) == Error: return op
                seq.append(op)
            case TokType.AT:
                if type(builtin := _parse_builtin(feed)) == Error: return builtin
                seq.append(builtin)
            case TokType.VAR: # TODO: _parse_expression
                feed.consume()
                seq.append(ast.VarPromise(maybe_cl_tri.val, maybe_cl_tri.pos))
            case _:
                return Error(f"unknown mathable expression `{maybe_cl_tri.val}`", maybe_cl_tri.pos)
        maybe_cl_tri = feed.peek(0)


    if (end := feed.consume_and_check(TokType.CL_TRI, "math expression should end with close triangle `>`, not `{}`")) == Error:
        return end
    return ast.MathRepr(seq, start.pos)

def _parse_call(feed: TokenFeed) -> ast.CallRepr | Error:
    if type(start := feed.consume_and_check(TokType.VAR, "you should call using string literal, like `dump(@sink)`, not `{}`")) == Error:
        return start
    callable = ast.VarPromise(start.val, start.pos)

    if type(t := feed.consume_and_check(TokType.OP_PAR, "call expression should contain open paren `(` to specify passed arguments, not `{}`")) == Error:
        return t
    
    args = []
    maybe_cl_par = feed.peek(0)
    while maybe_cl_par.type != TokType.CL_PAR:
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par.type == TokType.UNDR:
            args.append(ast.UseDefualtRepr(maybe_cl_par.pos))
            feed.consume()
        else:
            if type(expr := _parse_expression(feed)) == Error:
                return expr
            args.append(expr)
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par.type != TokType.CL_PAR and maybe_cl_par.type != TokType.COM:
            return Error(f"arguments in function call should be separated by comma `,` not `{maybe_cl_par.val}`", maybe_cl_par.pos)
        if maybe_cl_par.type == TokType.COM:
            feed.consume()
        maybe_cl_par = feed.peek(0)

    if type(t := feed.consume_and_check(TokType.CL_PAR, "call expression should contain close paren `)` to specify end of passed arguments, not `{}`")) == Error:
        return t
    
    return ast.CallRepr(callable, args, start.pos)

# TODO: check if generated function exists
def _parse_c_call(feed: TokenFeed) -> ast.ValidExpression | Error:
    if type(start := feed.consume_and_check(TokType.CL_TRI, "c-call performed with close triangle `>` in front of function name, like `>printf();`, not `{}`")) == Error:
        return start
    if type(callable := feed.consume_and_check(TokType.VAR, "you should call using string literal, like `>print(\"Hello, World!\n\")`, not `>{}`")) == Error:
        return callable
    if type(t := feed.consume_and_check(TokType.OP_PAR, "call expression should contain open paren `(` to specify passed arguments, not `{}`")) == Error:
        return t
    
    args = []
    maybe_cl_par = feed.peek(0)
    while maybe_cl_par.type != TokType.CL_PAR:
        maybe_cl_par = feed.peek(0)
        if type(expr := _parse_expression(feed)) == Error:
            return expr
        args.append(expr)
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par.type != TokType.CL_PAR and maybe_cl_par.type != TokType.COM:
            return Error(f"arguments in function call should be separated by comma `,` not `{maybe_cl_par.val}`", maybe_cl_par.pos)
        if maybe_cl_par.type == TokType.COM:
            feed.consume()
        maybe_cl_par = feed.peek(0)

    if type(t := feed.consume_and_check(TokType.CL_PAR, "call expression should contain close paren `)` to specify end of passed arguments, not `{}`")) == Error:
        return t
    
    return ast.C_CallRepr(callable.val, args, start.pos)

def _parse_builtin(feed: TokenFeed) -> ast.BuiltinRepr | Error:
    if type(start := feed.consume_and_check(TokType.AT, "built-in expression should starts with at `@`, not `{}`")) == Error:
        return start
    if type(value := feed.consume_and_check(TokType.VAR, "built-in expression should be string literal, like `lt`, not `{}`")) == Error:
        return value
    if (kind := ast.get_builtin_kind(value.val)) is None:
        return Error(f"unknown built-in `{value.val}`", value.pos)
    return ast.BuiltinRepr(kind, start.pos)

# TODO: **ptr
def _parse_val_from_ptr(feed: TokenFeed) -> ast.ValFromPtrRepr | Error:
    if type(start := feed.consume_and_check(TokType.ASTRKS, "extracting the value from the pointer should start with asterisk `*`, not `{}`")) == Error:
        return start
    if (name := feed.consume_and_check(TokType.VAR, "pointer name should be string literal, like `ptr`, not `{}`")) == Error:
        return name
    return ast.ValFromPtrRepr(ast.VarPromise(name.val, name.pos), start.pos)

# TODO: &&val
def _parse_ptr_from_val(feed: TokenFeed) -> ast.PtrFromValRepr | Error:
    if type(start := feed.consume_and_check(TokType.UMPERD, "getting the pointer to the value should start with umpersand `&`, not `{}`")) == Error:
        return start
    if (name := feed.consume_and_check(TokType.VAR, "value name should be string literal, like `ptr`, not `{}`")) == Error:
        return name
    return ast.PtrFromValRepr(ast.VarPromise(name.val, name.pos), start.pos)

def _parse_expression(feed: TokenFeed) -> ast.ValidExpression | Error:
    base = feed.peek(0)
    action = feed.peek(1)

    match action.type:
        case TokType.OP_PAR: return _parse_call(feed)
    match base.type:
        case TokType.AT     : return _parse_builtin(feed)
        case TokType.STR_LIT: return _parse_str_literal(feed)
        case TokType.INT_LIT: return _parse_int_literal(feed)
        case TokType.OP_TRI : return _parse_math(feed)
        case TokType.CL_TRI : return _parse_c_call(feed)
        case TokType.VAR    :
            feed.consume()
            return ast.VarPromise(base.val, base.pos)
        case TokType.ASTRKS : return _parse_val_from_ptr(feed)
        case TokType.UMPERD : return _parse_ptr_from_val(feed)
    
    return Error(f"unknown minor expression `{base.val}{action.val}`", base.pos)

def _parse_expression_as_statements(feed: TokenFeed) -> list[ast.ValidStatement] | Error:
    base = feed.peek(0)
    action = feed.peek(1)
        
    match action.type:
        case TokType.OP_PAR: 
            if type(expr := _parse_call(feed)) == Error: return expr
            if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
            return [ast.CallStatement(expr, expr.start)]
    match base.type:
        case TokType.OP_TRI:
            if type(expr := _parse_math(feed)) == Error: return expr
            if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
            return [ast.MathStatement(expr, expr.start)]
        case TokType.CL_TRI:
            if type(expr := _parse_c_call(feed)) == Error: return expr
            if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
            return [ast.C_CallStatement(expr, expr.start)]
        case TokType.VAR:
            return _parse_var_statements(feed)

    return Error(f"unknow composite expression `{base.val}{action.val}`", base.pos)

def _parse_decl_statements(feed: TokenFeed) -> list[ast.DeclStatement] | list[ast.InitStatement] | Error:
    if type(decls := _parse_decls(feed)) == Error:
        return decls
    if (maybe_ddick := feed.peek(0)).type == TokType.DDICK:
        feed.consume()
        if type(expr := _parse_expression(feed)) == Error:
            return expr
        if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
        return [ast.InitStatement(ast.Var(decl, expr), decl.start) for decl in decls]
    if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
    return [ast.DeclStatement(decl, decl.start) for decl in decls]

def _parse_var_statements(feed: TokenFeed) -> list[ast.DeclStatement]|list[ast.InitStatement]|list[ast.AssignStatement]|Error:
    start = feed.peek(0)
    if start.type != TokType.VAR:
        return Error(f"action-with-variable statement should begin with string literal, like `res_10`, not `{start.val}`", start.pos)
    if (maybe_eq := feed.peek(1)).type == TokType.EQ:
        feed.consume() # NOTE: consume start
        feed.consume() #       consume maybe_eq
        if type(expr := _parse_expression(feed)) == Error: return expr
        if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
        # TODO: a = b = 10;
        return [ast.AssignStatement(ast.VarPromise(start.val, start.pos), expr, start.pos)]
    return _parse_decl_statements(feed)

def _parse_ret_statement(feed: TokenFeed) -> ast.RetStatement | Error:
    if type(start := feed.consume_and_check(TokType.RET, "return statement should start with `ret` keyword, not `{}`")) == Error:
        return start
    if (maybe_semi := feed.peek(0)).type == TokType.SEMI:
        feed.consume()
        return ast.RetStatement(None, start.pos)

    if type(expr := _parse_expression(feed)) == Error:
        if not feed.len():
            return Error(f"unknown expression which starts with `{feed.peek(0).val}`", start.pos)
        return expr
    if type(t := feed.consume_and_check(TokType.SEMI, "return statement should end with semicolon `;`, not `{}`")) == Error:
        return t
    return ast.RetStatement(expr, start.pos)

def _parse_for_statement(feed: TokenFeed) -> ast.ForStatement | Error:
    if type(start := feed.consume_and_check(TokType.FOR, "return statement should start with `ret` keyword, not `{}`")) == Error:
        return start

    def _parse_init(feed: TokenFeed) -> Optional[ast.DeclRepr|ast.Var] | Error:
        if (maybe_semi := feed.peek(0)).type == TokType.SEMI:
            feed.consume()
            return None
        if type(decls := _parse_decl_statements(feed)) == Error:
            return decls
        if (l := len(decls)) > 1:
            return Error(f"for loop's init expects only 1 variable to be declarable, not {l}", decls[2].pos)
        match type(v := decls[0]):
            case ast.DeclStatement: return v.decl
            case ast.InitStatement: return v.var
        
    def _parse_cond(feed: TokenFeed) -> Optional[ast.MathRepr] | Error:
        if (maybe_semi := feed.peek(0)).type == TokType.SEMI:
            feed.consume()
            return None
        if type(cond := _parse_math(feed)) == Error: return cond
        if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error: return t
        return cond

    def _parse_inc(feed: TokenFeed) -> Optional[ast.MathRepr] | Error:
        if (maybe_semi := feed.peek(0)).type == TokType.SEMI:
            feed.consume()
            return None
        return _parse_math(feed)

    if type(init := _parse_init(feed)) == Error: return init
    if type(cond := _parse_cond(feed)) == Error: return cond
    if type(inc := _parse_inc(feed)) == Error: return inc
    # TODO: wrap error with "for loop always wants body"
    if type(body := _parse_body(feed)) == Error: return body
    return ast.ForStatement(init, cond, inc, body, start.pos)

def _parse_statements(feed: TokenFeed) -> list[ast.ValidStatement] | Error:
    base = feed.peek(0)
    match base.type:
        case TokType.RET: return [_parse_ret_statement(feed)]
        case TokType.FOR: return [_parse_for_statement(feed)]
    return _parse_expression_as_statements(feed)

def _parse_body(feed: TokenFeed) -> ast.BodyRepr | Error:
    if type(start := feed.consume_and_check(TokType.OP_CBR, "body should begin with curly brace `{`, not `{}`")) == Error:
        return start
    
    statements = []
    maybe_cl_cbr = feed.peek(0)
    while maybe_cl_cbr.type != TokType.CL_CBR:
        if type(states := _parse_statements(feed)) == Error:
            return states
        statements.extend(states)
        maybe_cl_cbr = feed.peek(0)

    if type(end := feed.consume_and_check(TokType.CL_CBR, "body should end with curly brace `}`, not `{}`")) == Error:
        return end
    return ast.BodyRepr(statements, start.pos)

def _parse_assignment(feed: TokenFeed) -> ast.Var | Error:
    if type(start := feed.consume_and_check(TokType.VAR, "name of variable should be string literal, like `res10`, not `{}`")) == Error:
        return start
    if type(var_type := _parse_type(feed)) == Error:
        return var_type
    if type(t := feed.consume_and_check(TokType.DDICK, "variable is assigned with double-dick operator `:=`, not `{}`")) == Error:
        return t
    base = feed.peek(0)
    # TODO: add type checking
    decl = ast.DeclRepr(start.val, var_type, start.pos)

    match base.type:
        case TokType.OP_CBR: # NOTE: ... := {}
            if type(body := _parse_body(feed)) == Error:
                return body
            return ast.Var(decl, body)

        case TokType.INT_LIT: # NOTE: ... := 50;
            if type(int_lit := _parse_int_literal(feed)) == Error:
                return int_lit
            if type(t := feed.consume_and_check(TokType.SEMI, "assigning int literal should end with semicolon `;`, not `{}`")) == Error:
                return t
            return ast.Var(decl, int_lit)

        case TokType.VAR: # NOTE: ... := i32;
            if type(ancestor_type := _parse_type(feed)) == Error:
                return ancestor_type
            if type(t := feed.consume_and_check(TokType.SEMI, "assigning type should end with semicolon `;`, not `{}`")) == Error:
                return t    
            var = ast.Var(decl, ast.TypeRepr(ancestor_type.base_type, ancestor_type.details, ancestor_type.start))
            return var
    return Error(f"unknown way to assign variable to `{base.val}`", base.pos)

def _parse_highest_level(feed: TokenFeed) -> ast.DirectiveCallStatement | ast.Var | Error:
    base = feed.peek(0)
    if base.type == TokType.HASH:
        return _parse_directive(feed)
    if base.type == TokType.VAR:
        return _parse_assignment(feed)
    return Error(f"on highest context of a program there are only two possible expressions: assignment (`a i32 := 69;`) and directive call (`#cinc \"<stdio.h>\"`)", base.pos)

def parse(toks: list[Token]) -> ast.AST | Error:
    feed: TokenFeed = TokenFeed(toks)
    dirvs = []
    vars = []
    while feed.len():
        v = _parse_highest_level(feed)
        if type(v) == ast.Var:
            vars.append(v)
        elif type(v) == ast.DirectiveCallStatement:
            dirvs.append(v)
        elif type(v) == ast.Error:
            return v
    return ast.AST(dirvs, vars)

def test_parse():
    r = read("add i32 := 60;")
    print(parse(r))

    r = read("myint type := i32;")
    print(parse(r))

    r = read(
        """
        myint type := i32;
        test fn(a myint, b myint) myint := { ret 5; }
        """
    )
    print(parse(r))
    
    r = read(
        """
        echo fn(a i32) i32 := { ret a; }
        foo fn() := { echo(echo(5)); }
        """
    )
    print(a := parse(r))

    r = read(
        """
        main fn() := { >printf("Hello, Bebralang!"); }
        """
    )
    print(a := parse(r))
    
    r = read(
        """
        foo fn(a i32) := { ret a; }
        main fn() := { foo(""); }
        """
    )
    print(a := parse(r))

    r = read(
        """
        foo fn(a, b i32 = 5) := { ret; }
        """
    )
    print(a := parse(r))

    r = read(
        """
        foo fn(a, b i32 = 5) := {
            c i32;
            c = <a b +>;
        }
        """
    )
    print(a := parse(r))

    r = read(
        # TODO: for { }
        """
        foo fn() := {
            for i i32 := 1; <i 10 @lt>; <i++> { }
            for ; <i 10 @lt>; <i++> { }
            for ;; <i++> { }
        }
        """
    )
    print(a := parse(r))

    r = read(
        """
        foo fn() := {
            call(_, 10, _);
            call(10, 10, 10);
            call(_, _, _);
        }
        """
    )
    print(a := parse(r))

    r = read(
        """
        foo fn() := {
            i *i32;
            call(*i);
            call(&i);
            call(<i *_ ++>);
        }
        """
    )
    print(a := parse(r))

# NOTE: all type checks should be done at type-checking stage
#       all other checks also
# TODO: remove checks from parser

if __name__ == "__main__":
    test_parse()
