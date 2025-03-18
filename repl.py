import sys
from datetime import datetime
from lexer import Lexer
from parser import Parser
from evaluator import Evaluator


class Repl:
    def __init__(self, debug: int = 0):
        self.program_contents: str = ""
        self.debug = debug
        self.evaluator = Evaluator(debug=debug)

    def run_file(self, file_path: str) -> None:
        with open(file_path, "r") as f:
            file_contents = f.read()
        self.program_contents = file_contents
        self.run(self.program_contents)

    def run_prompt(self):
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        print(f"Toy Lang 0.1.5 (default, {now})")
        print("Type (exit) to quit the console")
        while True:
            line = input(">>> ")
            if not line: continue
            if line.strip() == "(exit)": sys.exit(0)
            try:
                self.run(line)
            except Exception as e:
               print(f"{e}") 

    def run(self, source) -> None:
        lexer = Lexer(source)
        lexer.scan_tokens()

        parser = Parser(lexer.get_tokens())
        parser.parse_tokens()
        # print(parser.get_ast())

        for node in parser.get_ast():
            result = self.evaluator.evaluate(node)
            print(result)
