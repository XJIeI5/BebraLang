from typing import Optional, TypeVar

class Pos: 
    def __init__(self, row: int, col: int):
        self.row: int = row
        self.col: int = col
    
    def __str__(self) -> str:
        return f"r{self.row}, c{self.col}"


class Error:
    def __init__(self, msg: str, pos: Pos, file: Optional[str] = None):
        self.msg: str = msg
        self.pos: Pos = pos 
        self.file: Optional[str] = file
    
    def __str__(self) -> str:
        return f"ERROR at {"" if self.file == None else self.file + " "}{self.pos}: {self.msg}"
