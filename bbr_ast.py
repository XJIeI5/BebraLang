from typing import Optional, Any
from enum import Enum, auto
from lexer import TokType
from error import Error, Pos

# NOTE: Var with IMPOSSIBLE value raises exception

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

    error  = auto() # NOTE: ?
    math   = auto() # NOTE: ?

    type   = auto()
    struct = auto()
    enum   = auto()
    fn     = auto()
    dynarr = auto()
    ptr    = auto()

class BuiltinKind(Enum):
    lt  = auto() # `<`  less than 
    gt  = auto() # `>`  greater than
    nlt = auto() # `<=` non-strict less than
    ngt = auto() # `>=` non-strict greater than
    eq  = auto() # `==` equal
    neq = auto() # `!=` not equal

def get_builtin_kind(v: str) -> Optional[BuiltinKind]:
    match v:
        case "lt" : return BuiltinKind.lt
        case "gt" : return BuiltinKind.gt
        case "nlt": return BuiltinKind.nlt
        case "ngt": return BuiltinKind.ngt
        case "eq" : return BuiltinKind.eq
        case "neq": return BuiltinKind.neq
    return None

def get_byte_count(kind: TypeKind) -> Optional[int]:
    if kind in [TypeKind.f128]                           : return 16
    if kind in [TypeKind.i64, TypeKind.u64, TypeKind.f64]: return 8
    if kind in [TypeKind.i32, TypeKind.u32, TypeKind.f32]: return 4
    if kind in [TypeKind.i16, TypeKind.u16]              : return 2
    if kind in [TypeKind.i8 , TypeKind.u8 ]              : return 1
    if kind in [TypeKind.ptr]                            : return 4 # NOTE: for 32-bit systems
    # TODO: add size for dynarr
    return None

class BinOpType(Enum):
    ADD = auto() # `+`
    SUB = auto() # `-`
    DIV = auto() # `/`
    MUL = auto() # `*`
    REM = auto() # `%`

class UnaryOpType(Enum):
    DEC = auto() # `--`
    INC = auto() # `++`


class DetailsData:
    def __init__(self): pass

class IntLiteralRepr:
    def __init__(self, value: int, start: Pos):
        self.value: int = value
        self.start: Pos = start

    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}intlit: {self.value}\n"

class FloatLiteralRepr:
    def __init__(self, value: float, start: Pos):
        self.value: float = value
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}floatlit: {self.value}\n"

class StringLiteralRepr:
    def __init__(self, value: str, start: Pos):
        self.value: str = value.replace("\n", "\\n").replace("\t", "\\t").replace("\r", "\\r").replace("\b", "\\b")
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}strlit: {self.value}\n"

class BinaryOpRepr:
    def __init__(self, binopTypeRepr: BinOpType, start: Pos):
        self.type: BinOpType = binopTypeRepr
        self.start: Pos = start

    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}binary {self.type.name}\n"

class UnaryOpRepr:
    def __init__(self, unaryopTypeRepr: UnaryOpType, start: Pos):
        self.type: UnaryOpType = unaryopTypeRepr
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}unary {self.type.name}\n"


class TypeRepr:
    def __init__(self, base_type: TypeKind, details: Optional[DetailsData], start: Pos):
        self.base_type: TypeKind = base_type
        self.details: Optional[DetailsData] = details
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}type {self.base_type.name}\n"
        if self.details is not None:
            buf += self.details._tostr(sub+2, "details = ")
        return buf

class TypePromiseRepr:
    def __init__(self, typename: str, start: Pos):
        self.typename: str = typename
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}type(promise) {self.typename}\n"
        return buf

class VarPromise:
    def __init__(self, name: str, start: Pos):
        self.name: str = name
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}var(promise) {self.name}\n"

Type = TypePromiseRepr | TypeRepr

class MathRepr: pass
class CallRepr: pass
class C_CallRepr: pass
class Var: pass
class BuiltinRepr: pass
MinorExpression = MathRepr|IntLiteralRepr|FloatLiteralRepr|StringLiteralRepr|CallRepr|C_CallRepr|Var|VarPromise|BuiltinRepr

class DeclRepr:
    def __init__(self, name: str, decl_type: Type, start: Pos):
        self.name: str = name
        self.type: Type = decl_type
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}decl {self.name}:\n"
        buf += self.type._tostr(sub+2, "type = ")
        return buf


class FnDeclRepr:
    def __init__(self, decl: DeclRepr, by_defualt_val: Optional[MinorExpression]):
        self.decl: DeclRepr = decl
        self.by_defualt_val: Optional[MinorExpression] = by_defualt_val
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}fndecl:\n"
        if self.by_defualt_val is not None:
            buf += self.by_defualt_val._tostr(sub+2, "defualt = ")
        buf += self.decl._tostr(sub+2, "decl = ")
        return buf
        
        
# TODO: remake
class FnSignatureData:
    def __init__(self, args: list[Type], ret: Optional[Type]):
        self.args: list[Type] = args
        self.ret: Optional[Type] = ret

class FnSignatureRepr(DetailsData):
    def __init__(self, args: list[FnDeclRepr], ret: Optional[Type], start: Pos):
        self.args: list[FnDeclRepr] = args
        self.ret: Optional[Type] = ret
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}fnsig:\n"
        if self.ret is not None:
            buf += self.ret._tostr(sub+2, "ret = ")
        for arg in self.args:
            buf += arg._tostr(sub+2, "arg = ")
        return buf

class DynArrRepr:
    def __init__(self, ancestor_type: Type, start: Pos):
        self.ancestor_type: Type = ancestor_type
        self.start: Pos = start



class CincDirectiveRepr:
    def __init__(self, path: str, start: Pos):
        self.path: str = path
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}#cinc \"{self.path}\"\n"

ValidDirective = CincDirectiveRepr

# NOTE: ret excepts calls as well as values ( ret get_five(); / ret 5; )
#       but we can't say, that just a int literal is expression itself.
#       One solution: use math expressions to maintain consistancy in AST types and parsing ( ret <5>; / ret 5; )
#       BUT why do only returned numbers should be enclosed with <> ? ( a i32 = 5; / ret <5>; )
#       WAS ACTUAL to implementation: ValidExpression = CallRepr|DirectiveRepr|MathRepr

class UseDefualtRepr:
    def __init__(self, start: Pos):
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}use_defualt\n"






class CallStatement:
    def __init__(self, call_repr: CallRepr, start: Pos):
        self.call_repr: CallRepr = call_repr
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}call(statement):\n"
        buf += self.call_repr._tostr(sub+2, "call = ")
        return buf

class C_CallStatement:
    def __init__(self, c_call_repr: C_CallRepr, start: Pos):
        self.c_call_repr: C_CallRepr = c_call_repr
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}c_call(statement):\n"
        buf += self.c_call_repr._tostr(sub+2, "c_call = ")
        return buf

class DeclStatement:
    def __init__(self, decl: DeclRepr, start: Pos):
        self.decl: DeclRepr = decl
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}decl(statement):\n"
        buf += self.decl._tostr(sub+2, "decl = ")
        return buf

class InitStatement:
    def __init__(self, var: Var, start: Pos):
        self.var: Var = var
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}init(statement):\n"
        buf += self.var._tostr(sub+2, "var = ")
        return buf

class AssignStatement:
    def __init__(self, assignable: Var|VarPromise, expr: MinorExpression, start: Pos):
        self.assignable: Var|VarPromise = assignable
        self.expr: MinorExpression = expr
        self.start: Pos = start

    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}assign(statement):\n"
        buf += self.assignable._tostr(sub+2, "assign to = ")
        buf += self.expr._tostr(sub+2, "expr = ")
        return buf

class DirectiveCallStatement:
    def __init__(self, dirv: ValidDirective, start: Pos):
        self.dirv: ValidDirective = dirv
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}dircall(statement):\n"
        buf += self.dirv._tostr(sub+2, "dirv = ")
        return buf

class RetStatement:
    def __init__(self, expr: MinorExpression|None, start: Pos):
        self.expr: MinorExpression|None = expr
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}ret:\n"
        if self.expr is not None:
            buf += self.expr._tostr(sub+2, "expr = ")
        return buf

# TODO: make it throught inheritance
ValidStatement = CallStatement|C_CallStatement|RetStatement|DirectiveCallStatement|InitStatement|DeclStatement
class BodyRepr:
    def __init__(self, statements: list[ValidStatement], start: Pos):
        self.statements: list[ValidStatement] = statements
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}body:\n"
        for state in self.statements:
            buf += state._tostr(sub+2, "state = ")
        return buf

class ForStatement:
    def __init__(self, init: Optional[DeclRepr|Var], cond: Optional[MathRepr], inc: Optional[MathRepr], body: BodyRepr, start: Pos):
        self.init: Optional[DeclRepr|Var] = init
        self.cond: Optional[MathRepr] = cond
        self.inc: Optional[MathRepr] = inc
        self.body: BodyRepr = body
        self.start: Pos = start

    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}for:\n"
        if self.init is not None:
            buf += self.init._tostr(sub+2, "init = ")
        if self.cond is not None:
            buf += self.cond._tostr(sub+2, "cond = ")
        if self.inc is not None:
            buf += self.inc._tostr(sub+2, "inc = ")
        buf += self.body._tostr(sub+2, "body = ")
        return buf




class BuiltinRepr:
    def __init__(self, kind: BuiltinKind, start: Pos):
        self.type: BuiltinKind = kind
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        return f"{" "*sub}{tag}builtin @{self.type.name}\n"

ValidVarValue = MinorExpression|BodyRepr
# TODO: rename to AssignRepr for consistancy
class Var:
    def __init__(self, decl: DeclRepr, value: ValidVarValue):
        self.decl: DeclRepr = decl
        self._value: ValidVarValue = value
    
    @property
    def value(self) -> ValidVarValue:
        return self._value

    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}var {self.decl.name}:\n"
        buf += self.decl.type._tostr(sub+2, "decl = ")
        buf += self.value._tostr(sub+2, "val = ")
        return buf

ValidCallArg = IntLiteralRepr|FloatLiteralRepr|StringLiteralRepr|CallRepr|Var|BuiltinRepr|UseDefualtRepr
class CallRepr:
    def __init__(self, callable: Var|VarPromise, args: list[ValidCallArg], start: Pos):
        self.callable: Var|VarPromise = callable
        self.args: list[ValidCallArg] = args
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}call:\n"
        buf += self.callable._tostr(sub+2, "callable = ")
        for arg in self.args:
            buf += arg._tostr(sub+2, "arg = ")
        return buf
# TODO: check call is valid

class C_CallRepr:
    def __init__(self, callable: str, args: list[ValidCallArg], start: Pos):
        self.callable: str = callable
        self.args: list[ValidCallArg] = args
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}c_call:\n"
        buf += f"{" "*sub}var: {self.callable}\n"
        for arg in self.args:
            buf += arg._tostr(sub+2, "arg = ")
        return buf

class UserDefinedTypeData(DetailsData):
    def __init__(self, ancestor_type: Type, this_name: str):
        self.ancestor_type: Type = ancestor_type
        self.this_name: str = this_name
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}userdefined {self.this_name}:\n"
        buf += self.ancestor_type._tostr(sub+2, "ancestor = ")
        return buf

# NOTE: it is expected that BuiltinRepr only represents `>` `<` `>=` `<=` `==` `!=`
#       in context of mathable type
ValidMathableType = IntLiteralRepr|FloatLiteralRepr|BinaryOpRepr|UnaryOpRepr|CallRepr|Var|VarPromise|BuiltinRepr
class MathRepr:
    def __init__(self, seq: list[ValidMathableType]):
        # NOTE: this means that <get_one() 2 +> is valid
        self.seq: list[ValidMathableType] = seq
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}math:\n"
        for mathable in self.seq:
            buf += mathable._tostr(sub+2, "mathable = ")
        return buf

class PtrToValRepr:
    def __init__(self, var: Var, start: Pos):
        self.var: Var = var
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        raise NotImplementedError

class ValToPtrRepr:
    def __init__(self, var: Var, start: Pos):
        self.var: Var = var
        self.start: Pos = start

    def _tostr(self, sub: int, tag: str) -> str:
        raise NotImplementedError

class PtrRepr(DetailsData):
    def __init__(self, ancestor_type: Type, start: Pos):
        self.ancestor_type: Type = ancestor_type
        self.start: Pos = start
    
    def _tostr(self, sub: int, tag: str) -> str:
        buf = f"{" "*sub}{tag}ptr:\n"
        buf += self.ancestor_type._tostr(sub+2, "ancestor = ")
        return buf

class AST:
    def __init__(self, directives: list[ValidDirective], vars: list[Var]):
        self.directives: list[ValidDirective] = directives
        self.vars: list[Var] = vars
    
    def __str__(self) -> str:
        sub = 2
        buf = "AST:\n"
        buf += "Directives:\n"
        for dirv in self.directives:
            buf += dirv._tostr(sub, "")
        buf += "Vars:\n"
        for var in self.vars:
            buf += var._tostr(sub, "")
        return buf

