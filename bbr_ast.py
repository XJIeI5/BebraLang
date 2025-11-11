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

class CallRepr:
    def __init__(self):
        self.callable: Var
        self.args: list[Var]
        self.start: Pos
# TODO: check call is valid

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
CompositeExpression = CallRepr|DirectiveRepr
MinorExpression = MathRepr|IntLiteralRepr

class RetStatement:
    def __init__(self, expr: CompositeExpression|MinorExpression|None, start: Pos):
        self.expr: CompositeExpression|MinorExpression|None = expr
        self.start: Pos = start

ValidStatement = CompositeExpression|RetStatement
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

class UserDefinedTypeData(DetailsData):
    def __init__(self, ancestor_type: TypeRepr, this_name: str):
        self.ancestor_type: TypeRepr = ancestor_type
        self.this_name: str = this_name

# NOTE: DeclRepr is valid mathable becuase of  
ValidMathableType = IntLiteralRepr|FloatLiteralRepr|BinaryOpRepr|UnaryOpRepr|CallRepr|Var
class MathRepr:
    def __init__(self, seq: list[ValidMathableType]):
        # NOTE: this means that <get_one() 2 +> is valid
        self.seq: list[ValidMathableType] = seq

class Context:
    def __init__(self, prev: Context):
        self.prev_ctx: Context = prev
        self.vars: list[Var] = []

    def get_var_named(self, name: str) -> Optional[Var]:
        for var in self.vars:
            if var.decl.name == name: return var
        return None

    def append_var(self, var: Var) -> Optional[Error]:
        if self.get_var_named(var.decl.name) is not None:
            return Error(f"there is already a variable named `{var.decl.name}` in the context")
        self.vars.append(var)

class AST:
    def __init__(self, directives: list[DirectiveRepr], vars: list[Var]):
        self.directives: list[DirectiveRepr] = directives
        self.vars: list[Var] = vars
