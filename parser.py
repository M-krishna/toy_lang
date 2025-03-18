from token_type import TokenType
from token_internal import Token
from typing import List
from ast_internal import *


class Parser:
    ATOM_LIST: List[str] = [
        TokenType.NUMBER.name,
        TokenType.STRING.name,
        TokenType.IDENTIFIER.name,
        TokenType.PLUS.name,
        TokenType.MINUS.name,
        TokenType.STAR.name,
        TokenType.SLASH.name,
        TokenType.LESS_THAN.name,
        TokenType.LESS_THAN_EQUAL.name,
        TokenType.GREATER_THAN.name,
        TokenType.GREATER_THAN_EQUAL.name
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
        elif current_token.tt == TokenType.BOOLEAN.name:
            return self.parse_boolean()
        elif current_token.tt == TokenType.LPAREN.name:
            return self.parse_application()
        else:
            self.advance()

    def parse_atom(self):
        current_token: Token = self.peek()
        atom_node: AtomNode = AtomNode(
            current_token.tt, int(current_token.lexeme) if current_token.tt == TokenType.NUMBER.name else current_token.lexeme
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

        # Check if its a begin or not
        # If its a begin, then return a BeginNode
        if current_token.lexeme == TokenType.BEGIN.value:
            return self.parse_begin()

        # check if its a Plus(+) token
        if current_token.lexeme == "+":
            return self.parse_plus()

        if current_token.lexeme == "-":
            return self.parse_minus()

        if current_token.lexeme == "*":
            return self.parse_multiply()

        if current_token.lexeme == "/":
            return self.parse_divide()

        if current_token.lexeme == "<" or current_token.lexeme == "<=":
            return self.parse_less_than()

        if current_token.lexeme == ">" or current_token.lexeme == ">=":
            return self.parse_greater_than()

        if current_token.lexeme == TokenType.IF.value:
            return self.parse_if_expression()

        if current_token.lexeme == "=":
            return self.parse_equal()

        if current_token.lexeme == TokenType.NOT.value:
            return self.parse_not()

        if current_token.lexeme == TokenType.AND.value:
            return self.parse_and()

        if current_token.lexeme == TokenType.OR.value:
            return self.parse_or()

        if current_token.lexeme == TokenType.LIST.value:
            return self.parse_list()

        if current_token.lexeme == TokenType.CAR.value:
            return self.parse_car()

        if current_token.lexeme == TokenType.CDR.value:
            return self.parse_cdr()

        if current_token.lexeme == TokenType.CONS.value:
            return self.parse_cons()

        if current_token.lexeme == TokenType.LET.value:
            return self.parse_let()

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
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return LambdaNode(params, BeginNode(body_expression))
    
    def parse_params(self):
        params = []
        while not self.is_at_end() and self.peek().tt != TokenType.RPAREN.name:
            token: Token = self.advance()
            if token.tt != TokenType.IDENTIFIER.name and token.tt != TokenType.COMMA.name:
                raise SyntaxError(f"Expected identifier in parameter list, got: {token.lexeme}")
            params.append(token)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        
        return params

    def parse_begin(self):
        self.advance() # consume the begin token

        expressions = []
        while self.peek().tt != TokenType.RPAREN.name:
            expressions.append(self.parse_expressions())
        
        # Consume the closing paranthesis
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return BeginNode(expressions=expressions)

    def parse_plus(self):
        self.advance() # consume the plus operator

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return PlusNode(operands)

    def parse_minus(self):
        self.advance() # consume the "-" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return MinusNode(operands)

    def parse_multiply(self):
        self.advance() # consume the "*" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)

        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return MultiplyNode(operands)
    
    def parse_divide(self):
        self.advance() # consume the "/" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return DivideNode(operands) 

    def parse_less_than(self):
        token: Token = self.advance() # consume "<" or "<=" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")

        return LessThanNode(operands) if token.tt == TokenType.LESS_THAN.name else LessThanEqualNode(operands)

    def parse_greater_than(self):
        token: Token = self.advance() # consume ">" or ">=" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)

        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        
        return GreaterThanNode(operands) if token.tt == TokenType.GREATER_THAN.name else GreaterThanEqualNode(operands)

    def parse_if_expression(self):
        self.advance() # consume the "if" token

        condition = self.parse_expressions()
        if condition is None:
            raise SyntaxError(f"Expected conditional statement, got nothing")

        true_case = self.parse_expressions()
        if true_case is None:
            raise SyntaxError(f"Expected true clause, got nothing")

        false_case = self.parse_expressions()
        if false_case is None:
            raise SyntaxError(f"Expected false clause, got nothing")

        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return IfNode(condition, true_case, false_case)

    def parse_boolean(self):
        current_token: Token = self.advance()

        BOOLEANS = ["#t", "#f"]
        if current_token.lexeme not in BOOLEANS:
            raise SyntaxError(f"Expected: #t or #f, got: {self.peek()}")
        return BooleanNode(True if current_token.lexeme == "#t" else False)

    def parse_equal(self):
        self.advance() # consume the "=" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)

        if len(operands) == 0:
            raise SyntaxError("Expected atleast 1 argument, got 0")
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return EqualNode(operands)

    def parse_not(self):
        self.advance() # consume the "not" token
        # The not operator must have only one operand
        # The operand can be either 0 or 1 / #t or #f / "" or "string"
        # Which means the token type must be either NUMBER or BOOLEAN or STRING
        operand = self.parse_expressions()
        if not operand:
            raise SyntaxError("Expected 1 argument, got 0")
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return NotNode(operand)

    def parse_and(self):
        self.advance() # consume the "and" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return AndNode(operands)

    def parse_or(self):
        self.advance() # consume the "or" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return OrNode(operands)

    def parse_list(self) -> ConsCell:
        self.advance() # consume the "list" token

        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)

        # Build the ConsCell list using the operands list(either recursively or iteratively)
        cons_cell: ConsCell | EmptyListNode = self.build_cons_cell(operands)

        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return cons_cell
    
    def build_cons_cell(self, operands) -> ConsCell | EmptyListNode:
        if len(operands) == 0:
            return EmptyListNode()
        return ConsCell(operands[0], self.build_cons_cell(operands[1:]))

    def parse_car(self):
        self.advance() # consume the "car" token

        _list: ConsCell | EmptyListNode = self.parse_expressions()
        if isinstance(_list, EmptyListNode):
            raise SyntaxError(f"Expected a list with atleast one value, got 0")
        
        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return CarNode(_list)

    def parse_cdr(self):
        self.advance() # consume the "cdr" token

        _list: ConsCell | EmptyListNode = self.parse_expressions()
        if isinstance(_list, EmptyListNode):
            raise SyntaxError(f"Expected a list with atleast one value, got 0")

        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return CdrNode(_list)

    def parse_cons(self):
        self.advance() # consume the "cons" token

        # operands can be of any type
        operands = []
        while self.peek().tt != TokenType.RPAREN.name:
            operand = self.parse_expressions()
            operands.append(operand)
        
        if len(operands) != 2: # cons will take only 2 operands (car and cdr)
            raise SyntaxError(f"the expected number of arguments does not match the given number. Expected: 2 arguments")

        cons_cell: ConsCell = ConsCell(operands[0], operands[1])

        if not self.match(TokenType.RPAREN.name):
            raise SyntaxError(f"Expected ')', got: {self.peek()}")
        return cons_cell

    def parse_let(self):
        self.advance() # consume the "let" token

        bindings = self.parse_expressions() # A list of pairs [(pattern, Expression)]
        pairs = []

        # We have two forms, a shorthand form and a standard form
        # Shorthand form => (let (x 10) (* x 3)) => this returns a ListNode([<nodes>])
        # Standard form => (let ((x 10)) (* x 10)) => this returns a ListNode([ListNode([<nodes>])])

        if bindings.list_elements and isinstance(bindings.list_elements, ListNode):
            # Standard form
            for binding in bindings.list_elements:
                if len(binding.list_elements) != 2:
                    raise SyntaxError(f"Each binding must have exactly 2 elements")
                var_node: Node = binding.list_elements[0]
                expr_node: Node = binding.list_elements[1]
                pairs.append((var_node, expr_node))
        else:
            # Shorthand form
            if len(bindings.list_elements) != 2:
                raise SyntaxError(f"Expected exactly 2 elements for a binding in shorthand form")
            pairs.append((bindings.list_elements[0], bindings.list_elements[1]))

        print(f"pairs: {pairs}")
        # body = self.parse_body()
        return LetNode(1, 2)

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