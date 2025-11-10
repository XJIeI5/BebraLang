from typing import Optional, Any
from enum import Enum, auto
from lexer import Token, Tokenizer, TokType, read
from error import Error, Pos

class TypeKind(Enum):
    i64 = auto()
    i32 = auto()
    i16 = auto()
    i8  = auto()

    u64 = auto()
    u32 = auto()
    u16 = auto()
    u8  = auto()

    f32  = auto()
    f64  = auto()
    f128 = auto()

    error  = auto()
    math   = auto()

    type   = auto()
    struct = auto()
    enum   = auto()
    fn     = auto()
    dynarr = auto()
    ptr    = auto()

class Type:
    pass

class Decl:
    def __init__(self, name: str, type: Type):
        self.name: str
        self.type: Type


class TypeDetails:
    def __init__(self, base_type: Type):
        self.base_type: Type = base_type

class PtrDetails(TypeDetails):
    def __init__(self, base_type: Type):
        self.base_type: Type = base_type

class DynArrDetails(TypeDetails):
    def __init__(self, base_type: Type):
        self.base_type: Type = base_type

class FnDetails(TypeDetails):
    def __init__(self, ret: Type, args: list[Decl]):
        self.ret: Type = ret
        self.args: list[Decl] = args

class EnumDetails(TypeDetails):
    def __init__(self, base_type: Type, names: list[str]):
        self.base_type: Type = base_type
        self.names: list[str] = names

class StructDetails(TypeDetails):
    def __init__(self, decls: list[Decl]):
        self.args_types: list[Decl] = decls

class ErrorDetails(TypeDetails):
    def __init__(self, decls: list[Decl]):
        self.args_types: list[Decl] = decls

class SinkDetails(TypeDetails):
    def __init__(self, base_type: Type):
        self.base_type: Type = base_type

class MathDetails(TypeDetails):
    def __init__(self, seq: list[Token|Any]):
        # NOTE: this means that <get_one() 2 +> is valid
        # TODO: add type restrictions
        self.seq: list[Token|Any] = seq

class Type:
    def __init__(self, kind: TypeKind, details: Optional[TypeDetails] = None):
        self.kind: TypeKind = kind
        self.details: Optional[TypeDetails] = details

class Var:
    def __init__(self, decl: Decl, value: Any):
        self.decl: Decl = decl
        self.value: Any = value

class Context:
    def __init__(self):
        self.vars: list[Var] = []

class TokenFeed:
    def __init__(self, toks: list[Token]):
        self.toks: list[Token] = toks
        self.cur: int = 0
    
    def peek(self, offset: int) -> Optional[Token]:
        if self.cur+offset >= len(self.toks):
            return None
        return self.toks[self.cur+offset]

    def consume(self) -> Optional[Token]:
        if self.cur >= len(self.toks):
            return None
        self.cur += 1
        return self.toks[self.cur-1]

    def consume_and_check(self, toktype: TokType, err_msg: str) -> Optional[Token] | Error:
        tok = self.consume() 
        if tok and tok.type != toktype:
            return Error(err_msg.format(f"{tok.name}"), tok.pos)
        return tok

def parse_return(feed: TokenFeed, ctx: Context):
    if type(t := feed.consume_and_check(TokType.RET, "return expression should start with keyword `ret`, not `{}`")) == Error:
        return t
    parse_expr(feed, ctx)
    if type(t := feed.consume_and_check(TokType.SEMI, "return exrpression should end with semicolon `;`, not `{}`")) == Error:
        return t

def parse_call(feed: TokenFeed, ctx: Context):
    pass

def parse_expr(feed: TokenFeed, ctx: Context):
    base = feed.peek(0)
    action = feed.peek(1)
    match base.type:
        case TokType.OP_TRI: parse_ariphmetics(feed, ctx)
    match action.type:
        case TokType.DOT: parse_call(feed, ctx)
        case TokType.VAR: parse_decl(feed, ctx)

def parse_statement(feed: TokenFeed, ctx: Context):
    base = feed.peek(0)
    match base.type:
        case TokType.RET: parse_return(feed, ctx)
        case _:           
            parse_expr(feed, ctx)
            if type(t := feed.consume_and_check(TokType.SEMI, "statement should end with semicolon `;`, not `{}`")) == Error:
                return t
    

def parse_body(feed: TokenFeed, ctx: Context):
    # TODO: add new context
    start = feed.consume_and_check(TokType.OP_CBR, "new context should begin with open curly bracket `{`, not `{}`")
    if type(start) == Error:
        return start

    maybe_cl_cbr = feed.peek(0)
    while maybe_cl_cbr.type != TokType.CL_CBR:
        parse_statement(feed, ctx)
        maybe_cl_cbr = feed.peek(0)
        if maybe_cl_cbr == None:
            return Error(f"context never closed", start.pos)
    
    if type(t := feed.consume_and_check(TokType.CL_CBR, "new context should end with close curly bracket `}`, not `{}`")) == Error:
        return t

def parse_fn(feed: TokenFeed, ctx: Context) -> FnDetails | Error:
    start = feed.consume_and_check(TokType.OP_PAR, "function declaration should begin with arguments declaration forwarded with open paren `(`, not `{}`")
    if type(start) == Error:
        return start
    
    fn_det = FnDetails(None, [])
    maybe_cl_par = feed.peek(0)
    while maybe_cl_par.type != TokType.CL_PAR:
        fn_det.args.append(parse_decl(feed, ctx))
        maybe_cl_par = feed.peek(0)
        if maybe_cl_par == None:
            return Error(f"function arguments declaration never closed", start.pos)
    
    if type(t := feed.consume_and_check(TokType.CL_PAR, "function declaration should contain arguments declaration ended with close paren `)`, not `{}`")) == Error:
        return t
    
    ret_type = parse_type(feed, ctx)
    fn_det.ret = ret_type
    return fn_det

def parse_type(feed: TokenFeed, ctx: Context) -> Type | Error:
    type = feed.consume_and_check(TokType.VAR, "type must be var, like `i32`, not `{}`")
    match type.val:
        case "i64" : return Type(TypeKind.i64 )
        case "i32" : return Type(TypeKind.i32 )
        case "i16" : return Type(TypeKind.i16 )
        case "i8"  : return Type(TypeKind.i8  )
        case "u64" : return Type(TypeKind.u64 )
        case "u32" : return Type(TypeKind.u32 )
        case "u16" : return Type(TypeKind.u16 )
        case "u8"  : return Type(TypeKind.u8  )
        case "f128": return Type(TypeKind.f128)
        case "f64" : return Type(TypeKind.f64 )
        case "f32" : return Type(TypeKind.f32 )
        case "fn"  : return Type(TypeKind.fn, parse_fn(feed, ctx))

def parse_decl(feed: TokenFeed, ctx: Context) -> Var:
    name = feed.consume_and_check(TokType.VAR, "declaration name must be var, like `res`, not `{}`")
    type = parse_type(feed, ctx)
    feed.consume_and_check(TokType.COLON, "declaration assignment is performed using double-dick operator `:=`, not `{}`")
    feed.consume_and_check(TokType.EQ, "declaration assignment is performed using double-dick operator `:=`, not `{}`")
    body = parse_body(feed, ctx)
    return Var(Decl(name.val, type), body)
    

def parse_ariphmetics(feed: TokenFeed, ctx: Context) -> MathDetails | Error:
    start = feed.consume_and_check(TokType.OP_TRI, "ariphetic operations should start with open triangle `<`, not `{}`")
    if type(start) == Error:
        return t
    
    maybe_cl_tri = feed.peek(0)
    math_det = MathDetails([])
    while maybe_cl_tri.type != TokType.CL_TRI:
        tok = feed.consume()
        if tok and tok.type == TokType.VAR:
            if type(v := parse_expr(feed, ctx)) == Error:
                return Error
            math_det.seq.append(v)
        if tok and tok.type in [TokType.INT_LIT, TokType.FLOAT_LIT, \
                                TokType.PLUS, TokType.MINS, TokType.PERC, TokType.ASTRKS, TokType.DIV]:
            math_det.seq.append(tok)
        
        maybe_cl_tri = feed.peek(0)
        if maybe_cl_tri == None:
            return Error(f"context never closed", start.pos)
    
    if type(t := feed.consume_and_check(TokType.CL_TRI, "ariphetic operations should end with close triangle `>`, not `{}`")) == Error:
        return t
    return math_det


def test_parse():
    parse_ariphmetics(TokenFeed(read("<1 2 +>")), Context())
    ctx = Context()
    parse_expr(TokenFeed(read("main fn() := { ret 0; }")), ctx)
    print(ctx)

if __name__ == "__main__":
    test_parse()