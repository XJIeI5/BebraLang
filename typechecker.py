from typing import Optional
from error import Error, Pos
import bbr_ast as ast

def check_valid_mathable_seq(seq: list[ast.ValidMathableType]) -> Optional[Error]:
    c = 0
    for v in seq:
        if type(v) in [ast.IntLiteralRepr, ast.FloatLiteralRepr, ast.CallRepr]: c += 1
        elif type(v) == ast.UnaryOpRepr:
            c -= 1
            if c < 0:
                return Error(f"not enough values on stack to perform unary operation `{v.type.name}`, expects at least 1 value", v.start)
            c += 1
        elif type(v) == ast.BinaryOpRepr:
            c -= 2
            if c < 0:
                return Error(f"not enough values on stack to perform binary operation `{v.type.name}`, expects at least 2 values", v.start)
            c += 1
    if c != 1:
        return Error(f"arithmetic operation must result 1 value, got {c} - that's undefined behavior", seq[-1].start)
    return None