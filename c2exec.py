import os
import argparse
from subprocess import run
from error import Error
from lexer import read
from parser import parse
from translator import translate_to

def compile():
    parser = argparse.ArgumentParser(description="flip a switch by setting a flag")
    parser.add_argument("filename")
    parser.add_argument("-o", "--output", default="out", help="specify output's file name")
    parser.add_argument("-c", action="store_true", help="result would generated c-code")
    args = parser.parse_args()

    with open(args.filename, mode="r", encoding="utf-8") as f:
        if type(toks := read(f.read())) == Error:
            print(toks)
            return
    if type(_ast := parse(toks)) == Error:
        print(_ast)
        return
    
    with open(f"{args.output}.c", mode="w", encoding="utf-8") as f:
        translate_to(_ast, f)

    if args.c:
        return
    res = run(["gcc", f"{args.output}.c", "-o", args.output])
    if res.stderr is not None:
        print(res.stderr)
    os.remove(f"{args.output}.c")
    

if __name__ == "__main__":
    compile()
