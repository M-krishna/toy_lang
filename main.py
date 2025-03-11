#!/usr/bin/env python3

import sys
import copy
from datetime import datetime
from enum import Enum
from typing import List, Callable, Optional, Dict, Any


class TokenType(Enum):
    LPAREN = "lparen"
    RPAREN = "rparen"
    QUOTE = "quote"
    DOT = "dot"
    NUMBER = "number"
    STRING = "string"
    IDENTIFIER = "identifier"
    EQUAL = "equal"
    STAR = "star"
    PLUS = "plus"
    MINUS = "minus"
    KEYWORD = "keyword"
    WHITESPACE = "whitespace"
    SEMICOLON = "semicolon"
    UNDERSCORE = "underscore"
    COMMA = "comma"
    DEFINE = "define"
    LAMBDA = "lambda"
    EOF = "eof"


class Token:
    def __init__(self, tt: TokenType, lexeme: str, literal: object = None, line: int = 0):
        self.tt = tt
        self.lexeme = lexeme
        self.literal = literal
        self.line = line
    
    def __repr__(self):
        return f"(Token type: {self.tt}, Lexeme: {self.lexeme})"


#################### ENVIRONMENT CLASS ####################
class Environment:
    def __init__(self, bindings: Optional[Dict[str, Any]] = None, outer_scope: "Environment" = None):
        self.bindings = bindings if bindings is not None else {}
        self.outer_scope: Environment = outer_scope # outer env for nested scopes

    def lookup(self, name: str) -> Any:
        if name in self.bindings:
            return self.bindings.get(name)
        elif self.outer_scope is not None:
            return self.outer_scope.lookup(name)
        else:
            raise NameError(f"Undefined symbol: {name}")

    def define(self, name: str, value: str) -> None:
        # Define a new variable in the current environment
        self.bindings.update({ name: value })

    def set(self, name: str, value: str) -> None:
        if name in self.bindings:
            self.bindings.update({ name: value })
        elif self.outer_scope is not None:
            self.outer_scope.set(name, value)
        else:
            raise NameError(f"Undefined symbol: {name}")

    def copy(self) -> "Environment":
        return copy.deepcopy(self)

#################### END OF ENVIRONMENT CLASS ####################

class Lexer:
    def __init__(self, source: str):
        self.source: str = source
        self.start_position: int = 0
        self.current_position: int = 0
        self.tokens: List[Token] = []
        self.line: int = 0

    def scan_tokens(self):
        while not self.is_at_end():
            self.start_position = self.current_position
            self.scan_token()
        self.add_token(TokenType.EOF.name, "")

    def scan_token(self):
        c: str = self.advance()

        keywords: dict = {
            "(": lambda : self.add_token(TokenType.LPAREN.name, c),
            ")": lambda : self.add_token(TokenType.RPAREN.name, c),
            "=": lambda : self.add_token(TokenType.EQUAL.name, c),
            "*": lambda : self.add_token(TokenType.STAR.name, c),
            "+": lambda : self.add_token(TokenType.PLUS.name, c),
            "-": lambda : self.add_token(TokenType.MINUS.name, c),
            "'": lambda : self.add_token(TokenType.QUOTE.name, c),
            "_": lambda : self.add_token(TokenType.UNDERSCORE.name, c),
            ",": lambda : self.add_token(TokenType.COMMA.name, c),
            ";": lambda : self.handle_semicolon(),
        }

        fn: Callable = keywords.get(c, lambda: self.default_case(c))
        try:
            fn()
        except:
            raise Exception("Something went wrong during lexical analysis")

    def handle_semicolon(self):
        while (self.peek() != "\n" and not self.is_at_end()):
            self.advance()

    def default_case(self, c: str):
        if self.is_digit(c):
            self.number()
        elif self.is_alpha(c):
            self.identifier()
        elif self.is_whitespace(c):
            pass
        elif self.is_string(c):
            self.handle_string()
        else:
            raise SyntaxError(f"Unknown character {c}")

    def number(self):
        while self.is_digit(self.peek()):
            self.advance()
        text: str = self.source[self.start_position:self.current_position]
        self.add_token(TokenType.NUMBER.name, text)

    def identifier(self):
        while self.is_alphanumeric(self.peek()):
            self.advance()
        text: str = self.source[self.start_position:self.current_position]
        if text == TokenType.LAMBDA.value:
            self.add_token(TokenType.LAMBDA.name, text)
        else:
            self.add_token(TokenType.IDENTIFIER.name, text)

    def handle_string(self):
        while not self.is_at_end() and not self.is_string(self.peek()):
            self.advance()
        
        if self.is_at_end():
            raise SyntaxError(f"Unterminated string at line {self.line}")

        # consume the closing quote(")
        self.advance()
        
        text: str = self.source[self.start_position+1:self.current_position-1]
        self.add_token(TokenType.STRING.name, text)

    def add_token(self, token_type: TokenType, lexeme: str):
        token: Token = Token(
            token_type, lexeme
        )
        self.tokens.append(token)

    ################# HELPER FUNCTIONS #######################
    def is_digit(self, c: str) -> bool:
        return (c >= "0") and (c <= "9")

    def is_alpha(self, c: str) -> bool:
        return (c >= "a" and c <= "z") or (c >= "A" and c <= "Z") or c == "_"

    def is_alphanumeric(self, c: str) -> bool:
        return self.is_digit(c) or self.is_alpha(c)

    def is_string(self, c: str) -> bool:
        return c == '"'

    def is_whitespace(self, c: str) -> bool:
        SPACE: str = " "
        TABSPACE: str = "\t"
        NEWLINE: str = "\n"

        whitespace = [SPACE, TABSPACE, NEWLINE]
        return c in whitespace

    def advance(self) -> str:
        current_chr: str = self.peek()
        if current_chr == "\n": self.line += 1
        self.current_position += 1
        return current_chr

    def peek(self) -> str:
        if self.is_at_end(): return '\0' # null terminator
        return self.source[self.current_position]

    def is_at_end(self) -> bool:
        return self.current_position >= len(self.source)

    def get_tokens(self) -> List[Token]:
        return self.tokens
    ################# END OF HELPER FUNCTIONS ################

############# AST REPRESENTATION #############
class Node:
    pass

class AtomNode(Node):
    def __init__(self, token_type: TokenType, value: str):
        self.type = token_type
        self.value = value

    def __repr__(self):
        return f"AtomNode({self.type}, {self.value})"

class ListNode(Node):
    def __init__(self, list_elements):
        self.list_elements = list_elements

    def __repr__(self):
        return f"ListNode({self.list_elements})"

class LambdaNode(Node):
    def __init__(self, params, body):
        self.params = params
        self.body = body

    def __repr__(self):
        return f"LambdaNode({self.params}, {self.body})"

class ClosureNode(Node):
    def __init__(self, params, body, env: Environment):
        self.params = params
        self.body = body
        self.env: Environment = env

    def __call__(self, *args):
        # Check the length of the args
        if len(args) != len(self.params):
            raise TypeError(f"Expected {len(self.params)} arguments, get {len(args)}")
        # Create a new environment by extending the captured one
        new_env: Environment = Environment(bindings=dict(zip(self.params, args)), outer_scope=self.env)
        # Evaluate the body in the new environment
        return Evaluator().evaluate(self.body, env=new_env)

    def __repr__(self):
        return f"ClosureNode({self.params}, {self.body}, env={self.env.bindings})"


class BeginNode(Node):
    def __init__(self, expressions: List):
        self.expressions: List = expressions

    def __repr__(self):
        return f"BeginNode({self.expressions})"

############# END OF AST REPRESENTATION ######

############# PARSER #################
class Parser:
    ATOM_LIST: List[str] = [
        TokenType.NUMBER.name,
        TokenType.STRING.name,
        TokenType.IDENTIFIER.name,
        TokenType.PLUS.name,
        TokenType.MINUS.name,
        TokenType.STAR.name
    ]

    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.current_position = 0
        self.ast = []

    def parse_tokens(self):
        while not self.is_at_end():
            self.ast.append(self.parse_expressions())

    def parse_expressions(self):
        current_token: Token = self.peek()

        if current_token.tt in self.ATOM_LIST:
            return self.parse_atom()
        elif current_token.tt == TokenType.QUOTE.name:
            return self.parse_quote()
        elif current_token.tt == TokenType.LPAREN.name:
            return self.parse_application()
        else:
            self.advance()

    def parse_atom(self):
        current_token: Token = self.peek()
        atom_node: AtomNode = AtomNode(
            current_token.tt, current_token.lexeme
        )
        self.advance() # consume the token
        return atom_node

    def parse_quote(self):
        self.advance() # consume the quote(')
        quoted_expr = self.parse_expressions() # Parse the following expression
        return ListNode([
            AtomNode(TokenType.QUOTE.name, "'"),
            quoted_expr
        ])

    def parse_application(self):
        # Application is of type `( expressions )`
        if not self.match(TokenType.LPAREN.name):
            raise SyntaxError(f"Expected '(', found {self.peek()}")

        # Check if its a Lambda or not
        # If its a lambda, then return a LambdaNode
        current_token: Token = self.peek()
        if current_token.lexeme == TokenType.LAMBDA.value:
            return self.parse_lambda()

        elements: List = []
        while not self.is_at_end() and self.peek().tt != TokenType.RPAREN.name:
            node: Node = self.parse_expressions()
            elements.append(node)

        if self.is_at_end():
            raise SyntaxError(f"Expected ')', but reached end of tokens")

        self.advance() # consume the RPAREN
        return ListNode(elements)

    def parse_lambda(self):
        # Lambda node takes: params(one or more), body(one or more) and environment
        self.advance() # consume the lambda token

        # Two forms of params
        # Single -> Don't need to be wrapped in paranthesis
        # Multiple -> Must be wrapped in paranthesis

        # check if its a open paranthesis('(') or an identifier
        param = self.advance()
        
        params = []
        if param.tt == TokenType.LPAREN.name:
            # either comma separated values of a single value
            params = self.parse_params()
        else:
            # It must be a single identifier
            if param.tt != TokenType.IDENTIFIER.name:
                raise SyntaxError(f"Expected identifier for lambda parameter, got: {param.lexeme}")
            params = [AtomNode(param.tt, param.lexeme)]

        # Parse the body of the lambda function
        body_expression = []
        while self.peek().tt != TokenType.RPAREN.name:
            body_expression.append(self.parse_expressions())

        # Consume the closing parenthesis
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected '(', got: {self.peek()}")
        return LambdaNode(params, BeginNode(body_expression))
    
    def parse_params(self):
        params = []
        while not self.is_at_end() and self.peek().tt != TokenType.RPAREN.name:
            token: Token = self.advance()
            if token.tt != TokenType.IDENTIFIER.name and token.tt != TokenType.COMMA.name:
                raise SyntaxError(f"Expected identifier in parameter list, got: {token.lexeme}")
            params.append(token)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected '(', got: {self.peek()}")
        
        return params

    ############### HELPER FUNCTIONS ###############
    def match(self, expected_token_type: TokenType) -> bool:
        if self.peek().tt == expected_token_type:
            self.advance()
            return True
        return False

    def advance(self) -> Token:
        current_token: Token = self.peek()
        self.current_position += 1
        return current_token

    def peek(self) -> Token:
        return self.tokens[self.current_position]

    def is_at_end(self) -> bool:
        return self.current_position >= len(self.tokens) or self.tokens[self.current_position].tt == TokenType.EOF.name
    
    def get_ast(self) -> List:
        return self.ast
    ############### END OF HELPER FUNCTIONS ###############
############# END OF PARSER #################

############## EVALUATOR ################
class Evaluator:

    def __init__(self, debug: int = 0):
        self.operators: dict = {
            "+": lambda *args: self.addition(*args),
            "-": lambda a, *args: self.subtraction(a, *args),
            "*": lambda *args: self.multiply(*args),
            "/": lambda a, *args: self.divide(a, *args)
        }
        self.debug: int = debug
        # Global environment
        self.env: Environment = Environment(bindings={})
        self.env.bindings.update(self.operators)

    def evaluate(self, ast: Node, depth: int = 0, env: Environment = None):

        # Use the provided env or use the default one(global env)
        if env is None: env = self.env
        indent = " " * depth
        if isinstance(ast, AtomNode):
            if self.debug:
                print(f"{indent}Evaluting atom: {ast}")
            if ast.type in [TokenType.NUMBER.name, TokenType.STRING.name]:
                return ast.value
            elif ast.type == TokenType.IDENTIFIER.name:
                # Check the program environment to see if its present or not
                value = env.lookup(ast.value)
                if not value:
                    raise Exception(f"{ast.value} is not defined")
                return value
            else:
                # It must be an operator (for now)
                # Check if the operator function is available in the enviroment
                fn: Callable = env.lookup(ast.value)
                if self.debug:
                    print(f"{indent}Atom Result: {fn}")
                return fn
        elif isinstance(ast, LambdaNode):
            if self.debug:
                print(f"Creating closure for lambda: {ast}")
            # There might be more than one param (IDENTIFIERS)
            values = [param.lexeme for param in ast.params if param.tt == TokenType.IDENTIFIER.name]
            return ClosureNode(values, ast.body, env=env.copy())
        elif isinstance(ast, BeginNode):
            if self.debug:
                print(f"{indent}Evaluating begin node: {ast}")
            result = None
            for expression in ast.expressions:
                result = self.evaluate(expression, env=env)
            return result
        elif isinstance(ast, ListNode):
            # we can safely assume that the 1st element is always an operator
            if self.debug:
                print(f"{indent}Evaluating list: {ast}")

            # Check if its a quoted(') expression.
            if len(ast.list_elements) >= 2 and isinstance(ast.list_elements[0], AtomNode) and ast.list_elements[0].value == "'":
                # simply return the quoted expression value without evaluating it
                return ast.list_elements[1:]
            elif len(ast.list_elements) >= 2 and isinstance(ast.list_elements[0], AtomNode) and ast.list_elements[0].value == TokenType.DEFINE.value:
                # This is for the "define" feature
                # check whether it has 2 arguments
                rest = ast.list_elements[1:]
                if not len(rest) == 2:
                    raise SyntaxError(f"Identifier or Expression missing for define")
                # If the first argument is not an identifier, throw an error
                first_arg: AtomNode = rest[0]
                if not first_arg.type == TokenType.IDENTIFIER.name:
                    raise SyntaxError(f"Expected an identifier, got {first_arg}")
                # Evaluate the second argument
                result = self.evaluate(rest[1], env=env)
                # Save the result to the environment
                env.define(first_arg.value, result)
                return result
            else:
                func = self.evaluate(ast.list_elements[0], env=env)
                args = [self.evaluate(arg, env=env) for arg in ast.list_elements[1:]]
            
            if self.debug:
                print(f"{indent}Applying function: {func.__name__ if hasattr(func, '__name__') else func} to args: {args}")
            result = func(*args)
            
            if self.debug:
                print(f"{indent}List result: {result}")
            return result
        else:
            raise SyntaxError(f"Unknown node type: {ast}")
    
    def addition(self, *args):
        result = 0
        for v in args:
            result += int(v)
        return result
            
    def subtraction(self, a, *args):
        a = int(a)
        if not args:
            return -a
        return a - self.addition(*args)

    def multiply(self, *args):
        result = 1
        for n in args:
            result *= int(n)
        return result

    def divide(self, a, *args):
        if args:
            return a / self.multiply(*args)
        else:
            return a
############## END OF EVALUATOR #########
arguments = sys.argv
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
        print(f"Toy Lang 0.0.1 (default, {now})")
        print("Type (exit) to quit the console")
        while True:
            print(">>> ", end="", flush=True)
            line = sys.stdin.readline()
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

        for node in parser.get_ast():
            result = self.evaluator.evaluate(node)
            print(result)


if __name__ == "__main__":
    arg_len = len(arguments)
    repl = Repl()
    if arg_len > 2:
        print(f"Usage: ./main.py [script]")
        sys.exit(64)
    elif arg_len == 2:
        repl.run_file(arguments[1])
    else:
        repl.run_prompt()
