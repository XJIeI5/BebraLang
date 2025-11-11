from typing import Optional, Any
from enum import Enum, auto
from lexer import TokType
from error import Error, Pos

# NOTE: Var with IMPOSSIBLE value raises exception
IMPOSSIBLE = object()

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
    def __init__(self, byte_count: int, value: int, start: Pos):
        self.byte_count: int = byte_count
        self.value: int = value
        self.start: Pos = start

class FloatLiteralRepr:
    def __init__(self, byte_count: int, value: float, start: Pos):
        self.byte_count: int = byte_count
        self.value: float = value
        self.start: Pos = start

class StringLiteralRepr:
    def __init__(self, value: str, start: Pos):
        self.value: str = value.replace("\n", "\\n").replace("\t", "\\t").replace("\r", "\\r").replace("\b", "\\b")
        self.start: Pos = start

class BinaryOpRepr:
    def __init__(self, binopTypeRepr: BinOpType, start: Pos):
        self.type: BinOpType = binopTypeRepr
        self.start: Pos = start

class UnaryOpRepr:
    def __init__(self, unaryopTypeRepr: UnaryOpType, start: Pos):
        self.type: UnaryOpType = unaryopTypeRepr
        self.start: Pos = start



class TypeRepr:
    def __init__(self, base_type: TypeKind, details: Optional[DetailsData], start: Pos):
        self.base_type: TypeKind = base_type
        self.details: Optional[DetailsData] = details
        self.start: Pos = start

class DeclRepr:
    def __init__(self, name: str, decl_type: TypeRepr, start: Pos):
        self.name: str = name
        self.type: TypeRepr = decl_type
        self.start: Pos = start

class FnSignatureData:
    def __init__(self, args: list[TypeRepr], ret: Optional[TypeRepr]):
        self.args: list[TypeRepr] = args
        self.ret: Optional[TypeRepr] = ret

class FnSignatureRepr(DetailsData):
    def __init__(self, args: list[DeclRepr], ret: Optional[TypeRepr], start: Pos):
        self.args: list[DeclRepr] = args
        self.ret: Optional[TypeRepr] = ret
        self.start: Pos = start

class FnDeclRepr:
    def __init__(self, decls: list[DeclRepr], ret: TypeRepr, start: Pos):
        self.decls: list[DeclRepr] = decls
        self.ret: TypeRepr = ret
        self.start: Pos = start


class PtrRepr:
    def __init__(self, base_type: TypeRepr, start: Pos):
        self.base_type: TypeRepr = base_type
        self.start: Pos = start

class DynArrRepr:
    def __init__(self, base_type: TypeRepr, start: Pos):
        self.base_type: TypeRepr = base_type
        self.start: Pos = start


class DirectiveRepr:
    def __init__(self): pass

class CincDirectiveRepr(DirectiveRepr):
    def __init__(self, path: str, start: Pos):
        self.path: str = path
        self.start: Pos = start

# NOTE: ret excepts calls as well as values ( ret get_five(); / ret 5; )
#       but we can't say, that just a int literal is expression itself.
#       One solution: use math expressions to maintain consistancy in AST types and parsing ( ret <5>; / ret 5; )
#       BUT why do only returned numbers should be enclosed with <> ? ( a i32 = 5; / ret <5>; )
#       WAS ACTUAL to implementation: ValidExpression = CallRepr|DirectiveRepr|MathRepr

class MathRepr: pass
class CallRepr: pass
class C_CallRepr: pass
class Var: pass
MinorExpression = MathRepr|IntLiteralRepr|FloatingPointError|StringLiteralRepr|CallRepr|C_CallRepr|Var

class CallStatement:
    def __init__(self, call_repr: CallRepr, start: Pos):
        self.call_repr: CallRepr = call_repr
        self.start: Pos = start

class C_CallStatement:
    def __init__(self, c_call_repr: C_CallRepr, start: Pos):
        self.c_call_repr: C_CallRepr = c_call_repr
        self.start: Pos = start

class DirectiveCallStatement:
    def __init__(self, dirv: DirectiveRepr, start: Pos):
        self.dirv: DirectiveRepr = dirv
        self.start: Pos = start

class RetStatement:
    def __init__(self, expr: MinorExpression|None, start: Pos):
        self.expr: MinorExpression|None = expr
        self.start: Pos = start

ValidStatement = CallStatement|C_CallStatement|RetStatement|DirectiveCallStatement
class BodyRepr:
    def __init__(self, statements: list[ValidStatement], start: Pos):
        self.statements: list[ValidStatement] = statements
        self.start: Pos = start
        

class Var:
    def __init__(self, decl: DeclRepr, value: Any):
        self.decl: DeclRepr = decl
        self._value: Any = value
    
    @property
    def value(self) -> Any:
        assert(self._value != IMPOSSIBLE)
        return self._value

ValidCallArg = IntLiteralRepr|FloatLiteralRepr|StringLiteralRepr|CallRepr|Var
class CallRepr:
    def __init__(self, callable: Var, args: list[ValidCallArg], start: Pos):
        self.callable: Var = callable
        self.args: list[ValidCallArg] = args
        self.start: Pos = start
# TODO: check call is valid

class C_CallRepr:
    def __init__(self, callable: str, args: list[ValidCallArg], start: Pos):
        self.callable: str = callable
        self.args: list[ValidCallArg] = args
        self.start: Pos = start

class UserDefinedTypeData(DetailsData):
    def __init__(self, ancestor_type: TypeRepr, this_name: str):
        self.ancestor_type: TypeRepr = ancestor_type
        self.this_name: str = this_name

ValidMathableType = IntLiteralRepr|FloatLiteralRepr|BinaryOpRepr|UnaryOpRepr|CallRepr|Var
class MathRepr:
    def __init__(self, seq: list[ValidMathableType]):
        # NOTE: this means that <get_one() 2 +> is valid
        self.seq: list[ValidMathableType] = seq

class Context:
    def __init__(self, prev: Context):
        self.prev_ctx: Context = prev
        self.vars: list[Var] = []
        if prev is not None:
            self._copy_vars()

    def _copy_vars(self):
        for var in self.prev_ctx.vars:
            self.append_var(var)
    
    def get_var_named(self, name: str) -> Optional[Var]:
        for var in self.vars:
            if var.decl.name == name: return var
        return None

    def append_var(self, var: Var) -> Optional[Error]:
        if self.get_var_named(var.decl.name) is not None:
            return Error(f"there is already a variable named `{var.decl.name}` in the context", var.decl.start)
        self.vars.append(var)

class AST:
    def __init__(self, directives: list[DirectiveRepr], vars: list[Var]):
        self.directives: list[DirectiveRepr] = directives
        self.vars: list[Var] = vars
