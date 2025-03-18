from token_type import TokenType
from token_internal import Token
from typing import List, Callable

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
            "/": lambda : self.add_token(TokenType.SLASH.name, c),
            "'": lambda : self.add_token(TokenType.QUOTE.name, c),
            "_": lambda : self.add_token(TokenType.UNDERSCORE.name, c),
            ",": lambda : self.add_token(TokenType.COMMA.name, c),
            ">": lambda : self.add_token(TokenType.GREATER_THAN_EQUAL.name if self.match("=") else TokenType.GREATER_THAN.name),
            "<": lambda : self.add_token(TokenType.LESS_THAN_EQUAL.name if self.match("=") else TokenType.LESS_THAN.name),
            ";": lambda : self.handle_semicolon(),
            "#": lambda : self.handle_boolean()
        }

        fn: Callable = keywords.get(c, lambda: self.default_case(c))
        try:
            fn()
        except Exception as e:
            raise Exception(f"Something went wrong during lexical analysis: {e}")

    def handle_semicolon(self):
        while (self.peek() != "\n" and not self.is_at_end()):
            self.advance()

    def handle_boolean(self):
        if not (self.match('t') or self.match('f')):
            raise SyntaxError(f"Expected 't' or 'f', got: {self.peek()}")
        self.add_token(TokenType.BOOLEAN.name)

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
        elif text == TokenType.BEGIN.value:
            self.add_token(TokenType.BEGIN.name, text)
        elif text == TokenType.IF.value:
            self.add_token(TokenType.IF.name, text)
        elif text == TokenType.NOT.value:
            self.add_token(TokenType.NOT.name, text)
        elif text == TokenType.AND.value:
            self.add_token(TokenType.AND.name, text)
        elif text == TokenType.OR.value:
            self.add_token(TokenType.OR.name, text)
        elif text == TokenType.LIST.value:
            self.add_token(TokenType.LIST.name, text)
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

    def add_token(self, token_type: TokenType, lexeme: str = None):
        if lexeme is None:
            lexeme: str = self.source[self.start_position:self.current_position]
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

    def match(self, expected_token: str) -> bool:
        if self.peek() != expected_token:
            return False
        self.advance()
        return True

    def peek(self) -> str:
        if self.is_at_end(): return '\0' # null terminator
        return self.source[self.current_position]

    def is_at_end(self) -> bool:
        return self.current_position >= len(self.source)

    def get_tokens(self) -> List[Token]:
        return self.tokens
    ################# END OF HELPER FUNCTIONS ################
