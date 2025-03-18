from token_type import TokenType


class Token:
    def __init__(self, tt: TokenType, lexeme: str, literal: object = None, line: int = 0):
        self.tt = tt
        self.lexeme = lexeme
        self.literal = literal
        self.line = line
    
    def __repr__(self):
        return f"(Token type: {self.tt}, Lexeme: {self.lexeme})"