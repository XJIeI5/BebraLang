from typing import Optional
from enum import Enum, auto
from error import Error, Pos
import re

class ErrorMsg(Enum):
    REDUNDANT_DOT = "redundant dot `.` in float declaration"
    NOT_CLOSED_STR = "not closed string literal"
    EOF = "end of file"

class Keyword(Enum):
    fn = "fn"

class TokType(Enum):
    PLUS   = auto()  # + 
    MINS   = auto()  # -
    ASTRKS = auto()  # *
    DIV    = auto()  # /
    PERC   = auto()  # %
    EQ     = auto()  # =

    EXCL   = auto()  # !
    QUEST  = auto()  # ?
    DOT    = auto()  # .
    COM    = auto()  # ,
    UMPERD = auto()  # &
    HASH   = auto()  # #
    AT     = auto()  # @
    QUOT   = auto()  # "
    COLON  = auto()  # :
    SEMI   = auto()  # ;
    UNDR   = auto()  # _

    DDICK = auto() # :=
    INC   = auto() # ++
    DEC   = auto() # --

    OP_PAR = auto()  # (
    OP_BR  = auto()  # [
    OP_CBR = auto()  # {
    OP_TRI = auto()  # <
    CL_PAR = auto()  # )
    CL_BR  = auto()  # ]
    CL_CBR = auto()  # }
    CL_TRI = auto()  # >

    RET = auto()  # ret
    FOR = auto()  # for

    VAR       = auto()  # somevar
    INT_LIT   = auto()  # 420
    FLOAT_LIT = auto()  # 420.69
    STR_LIT   = auto()  # "somevar"

    @staticmethod
    def longest_name_len() -> int:
        return max([len(v.name) for v in TokType])

class Token:
    def __init__(self, _type: TokType, value: str, pos: Pos):
        self.type: TokType = _type
        self.val: str = value
        self.pos: Pos = pos
    
    def __str__(self):
        space_count = TokType.longest_name_len()-len(self.type.name)
        return f"{self.type.name}:{space_count*" "} {self.val}"


def get_from_rune(v: str, pos: Pos) -> Optional[Token]:
    match v:
        case "+": return Token(TokType.PLUS,   v, pos)
        case "-": return Token(TokType.MINS,   v, pos)
        case "*": return Token(TokType.ASTRKS, v, pos)
        case "/": return Token(TokType.DIV,    v, pos)
        case "%": return Token(TokType.PERC,   v, pos)
        case "=": return Token(TokType.EQ,     v, pos)

        case "!": return Token(TokType.EXCL,   v, pos)
        case "?": return Token(TokType.QUEST,  v, pos)
        case ".": return Token(TokType.DOT,    v, pos)
        case ",": return Token(TokType.COM,    v, pos)
        case "&": return Token(TokType.UMPERD, v, pos)
        case "#": return Token(TokType.HASH,   v, pos)
        case "@": return Token(TokType.AT,     v, pos)
        case ":": return Token(TokType.COLON,  v, pos)
        case ";": return Token(TokType.SEMI,   v, pos)
        case "_": return Token(TokType.UNDR,   v, pos)

        case "(": return Token(TokType.OP_PAR, v, pos)
        case "[": return Token(TokType.OP_BR,  v, pos)
        case "{": return Token(TokType.OP_CBR, v, pos)
        case "<": return Token(TokType.OP_TRI, v, pos)
        case ")": return Token(TokType.CL_PAR, v, pos)
        case "]": return Token(TokType.CL_BR,  v, pos)
        case "}": return Token(TokType.CL_CBR, v, pos)
        case ">": return Token(TokType.CL_TRI, v, pos)
    return None

def get_from_two_runes(v: str, pos: Pos) -> Optional[Token]:
    match v:
        case ":=": return Token(TokType.DDICK, v, pos)
        case "++": return Token(TokType.INC  , v, pos)
        case "--": return Token(TokType.DEC  , v, pos)
    return None

class Tokenizer:
    def __init__(self, tape: str):
        self.tape: str = tape
        self.cur: int = 0

    def _get_pos(self, pos: int) -> Pos:
        res = Pos(0, 0)
        for r in self.tape[:pos+1]:
            if r == "\n":
                res.row += 1
                res.col = 0
            else:
                res.col += 1
        return res
    
    # NOTE: expects that self.tape[start_ind] is \"
    def _continue_parsing_string_lit(self, start_ind: int) -> Token | Error:
        quot_close = None
        try:
            quot_close = self.tape.index("\"", start_ind+1)
        except ValueError:
            return Error(ErrorMsg.NOT_CLOSED_STR, self._get_pos(start_ind))
        
        string_val = self.tape[start_ind+1:quot_close]
        return Token(TokType.STR_LIT, string_val, self._get_pos(start_ind))

    # NOTE: expects that self.tape[start_ind] is digit
    def _continue_parsing_number_lit(self, start_ind: int, num_is_float: bool = False) -> Token | Error:
        _is_float = False
        buf = self.tape[start_ind]
        for offset, r in enumerate(self.tape[start_ind+1:]):
            if r.isdigit():
                buf += r
            elif r == "." and _is_float:
                return Error(ErrorMsg.REDUNDANT_DOT, self._get_pos(start_ind+offset))
            elif r == ".":
                _is_float = True
                buf += r
            else:
                break
        toktype = TokType.FLOAT_LIT if _is_float else TokType.INT_LIT
        if num_is_float:
            if toktype == TokType.FLOAT_LIT:
                return Error(ErrorMsg.REDUNDANT_DOT, self._get_pos(self.tape.index(".", start_ind+1)))
            toktype = TokType.FLOAT_LIT
        return Token(toktype, buf, self._get_pos(start_ind))
    
    def _check_on_keywords(self, v: str) -> TokType:
        match v:
            case "ret": return TokType.RET
            case "for": return TokType.FOR
        return TokType.VAR
    
    # NOTE: expects that self.tape[start_ind] is alpha
    def _continue_parsing_var(self, start_ind: int) -> Token:
        buf = self.tape[start_ind]
        for r in self.tape[start_ind+1:]:
            if r.isalpha() or r.isdigit() or r == "_":
                buf += r
            else:
                break
        return Token(self._check_on_keywords(buf), buf, self._get_pos(start_ind))


    def consume_tok(self) -> Token | Error:
        if self.cur >= len(self.tape):
            return Error(ErrorMsg.EOF, self._get_pos(len(self.tape)-1))
        r = self.tape[self.cur]
        if self.cur+1 < len(self.tape) and r == "-" and (next := self.tape[self.cur+1]) and ((num_is_float := next == ".") or next.isdigit()):
            self.cur += 1
            res = self._continue_parsing_number_lit(self.cur, num_is_float=num_is_float)
            if type(res) == Token:
                self.cur += len(res.val)
                res.val = f"-{res.val}"
                return res
            else:
                self.cur -= 1
        if self.cur+1 < len(self.tape) and r == "_" and (next := self.tape[self.cur+1]) and (next.isdigit() or next.isalpha()):
            res = self._continue_parsing_var(self.cur)
            self.cur += len(res.val)
            return res
        if r == ".":
            res = self._continue_parsing_number_lit(self.cur, num_is_float=True)
            if type(res) == Token:
                self.cur += len(res.val)
                res.type = TokType.FLOAT_LIT
            return res
        if r.isspace():
            self.cur += 1
            return self.consume_tok()
        if self.cur+1 < len(self.tape) and (tok := get_from_two_runes(r+self.tape[self.cur+1], self._get_pos(self.cur))) != None:
            self.cur += 2
            return tok
        if (tok := get_from_rune(r, self._get_pos(self.cur))) != None:
            self.cur += 1
            return tok
        if r == "\"":
            res = self._continue_parsing_string_lit(self.cur)
            if type(res) ==  Token:
                self.cur += len(res.val)+2 # to consume \" and set cur to next rune
            return res
        if r.isdigit():
            res = self._continue_parsing_number_lit(self.cur)
            if type(res) == Token:
                self.cur += len(res.val)
            return res
        if r.isalpha():
            res = self._continue_parsing_var(self.cur)
            self.cur += len(res.val)
            return res
        return Error(f"unknown token `{self.tape[self.cur]}`", self._get_pos(self.cur))


    def pass_word() -> None:
        return
    
    def memorize() -> None:
        return
    
    def remember() -> None:
        return


def read(tape: str) -> list[Token] | Error:
    tape = " ".join(tape.split(" "))
    runner = Tokenizer(tape)
    buf = []
    while type((tok := runner.consume_tok())) != Error:
        buf.append(tok)
    
    # NOTE: last time tok is assigned was in loop, so it's Error type
    if tok.msg != ErrorMsg.EOF:
        return tok
    return buf 

def test_tokenizer():
    toks = read("first_num := arr[<i 1 +>];")
    print(*(toks+["-------"]), sep="\n")

    toks = read("fn sum(a int, b int) int { ret 0; }")
    print(*(toks+["-------"]), sep="\n")

    toks = read("#cinc \"stdio.h\"\n\nfn main() {}")
    print(*(toks+["-------"]), sep="\n")

    toks = read("if v < .01 { ret error; }")
    print(*(toks+["-------"]), sep="\n")

    toks = read(":= : ++ _1num")
    if type(toks) == Error: print(toks)
    print(*(toks+["-------"]), sep="\n")

    toks = read("-5 -.5")
    print(*(toks+["-------"]), sep="\n")


if __name__ == "__main__":
    test_tokenizer()
