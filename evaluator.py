from environment import Environment
from ast_internal import *
from typing import Callable

class Evaluator:

    def __init__(self, debug: int = 0):
        self.operators: dict = {
            "+": lambda *args: self.addition(*args),
            "-": lambda a, *args: self.subtraction(a, *args),
            "*": lambda *args: self.multiply(*args),
            "/": lambda a, *args: self.divide(a, *args),
            "<": lambda *args: self.less_than(*args),
            "<=": lambda *args: self.less_than_equal(*args),
            ">": lambda *args: self.greater_than(*args),
            ">=": lambda *args: self.greater_than_equal(*args),
            "#t": True,
            "#f": False,
            "=": lambda *args: self.equality(*args),
            "display": lambda a: self.display(a)
        }
        self.debug: int = debug
        # Global environment
        self.env: Environment = Environment(bindings={})
        self.env.bindings.update(self.operators)

    def evaluate(self, ast: "Node", depth: int = 0, env: Environment = None):

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
                if value is None:
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
        elif isinstance(ast, PlusNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.addition(*values)
            return result
        elif isinstance(ast, MinusNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.subtraction(*values)
            return result
        elif isinstance(ast, MultiplyNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.multiply(*values)
            return result
        elif isinstance(ast, DivideNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.divide(*values)
            return result
        elif isinstance(ast, LessThanNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.less_than(*values)
            return result
        elif isinstance(ast, LessThanEqualNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.less_than_equal(*values)
            return result
        elif isinstance(ast, GreaterThanNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.greater_than(*values)
            return result
        elif isinstance(ast, GreaterThanEqualNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.greater_than_equal(*values)
            return result
        elif isinstance(ast, IfNode):
            condition = self.evaluate(ast.condition, env=env)
            if condition:
                result = self.evaluate(ast.true_case_expression, env=env)
            else:
                result = self.evaluate(ast.false_case_expression, env=env)
            return result
        elif isinstance(ast, BooleanNode):
            return ast.value
        elif isinstance(ast, EqualNode):
            values = [self.evaluate(op, env=env) for op in ast.operands]
            result = self.equality(*values)
            return result
        elif isinstance(ast, NotNode):
            result = self.__not(self.evaluate(ast.operand, env=env))
            return result
        elif isinstance(ast, AndNode):
            for operand in ast.operands:
                result = self.evaluate(operand, env=env)
                if not result:
                    return BooleanNode(False)
            return BooleanNode(True)
        elif isinstance(ast, OrNode):
            for operand in ast.operands:
                result = self.evaluate(operand, env=env)
                if result:
                    return BooleanNode(True)
            return BooleanNode(False)
        elif isinstance(ast, ConsCell):
            return ast
        elif isinstance(ast, CarNode):
            r: ConsCell = self.evaluate(ast.list_expr, env=env)
            if not isinstance(r, ConsCell):
                raise SyntaxError(f"car expects a type of cons cell or non-empty list")
            return self.evaluate(r.car, env=env)
        elif isinstance(ast, CdrNode):
            r: ConsCell = self.evaluate(ast.list_expr, env=env)
            if not isinstance(r, ConsCell):
                raise SyntaxError(f"cdr expects a type of cons cell or non-empty list")
            return self.evaluate(r.cdr, env=env)
        elif isinstance(ast, EmptyListNode):
            return ast
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
        a = int(a)
        if args:
            return a / self.multiply(*args)
        else:
            return a

    def less_than(self, *args):
        # If there is only one argument, return True
        if len(args) < 2:
            return True
        for i in range(len(args) - 1):
            if not args[i] < args[i+1]:
                return False
        return True

    def less_than_equal(self, *args):
        # If there is only one argument, return True
        if len(args) < 2:
            return True
        for i in range(len(args) - 1):
            if not args[i] <= args[i+1]:
                return False
        return True

    def greater_than(self, *args):
        if len(args) < 2:
            return True
        for i in range(len(args) - 1):
            if not args[i] > args[i+1]:
                return False
        return True

    def greater_than_equal(self, *args):
        if len(args) < 2:
            return True
        for i in range(len(args) - 1):
            if not args[i] >= args[i+1]:
                return False
        return True

    def display(self, a) -> None:
        print(a, flush=True)

    def equality(self, *args) -> bool:
        if len(args) < 2: return True
        for i in range(len(args) - 1):
            if not (args[i] == args[i+1]):
                return False
        return True

    def __not(self, arg) -> BooleanNode:
        if arg is None:
            raise SyntaxError("Expected 1 argument, got 0")
        value = arg.value if isinstance(arg, BooleanNode) else arg
        return BooleanNode(not value)
   