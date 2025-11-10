from typing import Optional
from error import Error
from lexer import Token, TokType

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
            return Error(err_msg.format(f"{tok.val}"), tok.pos)
        return tok
    
    def len(self) -> int:
        return len(self.toks)-self.cur
