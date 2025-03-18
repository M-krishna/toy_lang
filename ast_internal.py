from token_type import TokenType
from environment import Environment
from typing import List


class Node:
    pass


class AtomNode(Node):
    def __init__(self, token_type: TokenType, value: str):
        self.type = token_type
        self.value = value
    
    def __str__(self):
        return str(self.value)

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
        from evaluator import Evaluator
        return Evaluator().evaluate(self.body, env=new_env)

class BeginNode(Node):
    def __init__(self, expressions: List):
        self.expressions: List = expressions

    def __repr__(self):
        return f"BeginNode({self.expressions})"

class PlusNode(Node):
    def __init__(self, operands):
        self.operator = "+"
        self.operands = operands
    
    def __str__(self):
        return f"(+ {' '.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"PlusNode({self.operands})"

class MinusNode(Node):
    def __init__(self, operands):
        self.operator = "-"
        self.operands = operands
    
    def __str__(self):
        return f"(- {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"MinusNode({self.operands})"

class MultiplyNode(Node):
    def __init__(self, operands):
        self.operator = "*"
        self.operands = operands

    def __str__(self):
        return f"(* {' '.join(str(op) for op in self.operands)})"

    def __reduce__(self):
        return f"MultiplyNode({self.operands})"

class DivideNode(Node):
    def __init__(self, operands):
        self.operator = "/"
        self.operands = operands
    
    def __str__(self):
        return f"(/ {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"DivideNode({self.operands})"

class LessThanNode(Node):
    def __init__(self, operands):
        self.operator = "<"
        self.operands = operands

    def __str__(self):
        return f"(< {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"LessThanNode({self.operands})"

class LessThanEqualNode(Node):
    def __init__(self, operands):
        self.operator = "<="
        self.operands = operands

    def __str__(self):
        return f"(<= {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"LessThanEqualNode({self.operands})"

class GreaterThanNode(Node):
    def __init__(self, operands):
        self.operator = ">"
        self.operands = operands

    def __str__(self):
        return f"(> {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"GreaterThanNode({self.operands})"

class GreaterThanEqualNode(Node):
    def __init__(self, operands):
        self.operator = ">="
        self.operands = operands

    def __str__(self):
        return f"(>= {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"GreaterThanEqualNode({self.operands})"

class IfNode(Node):
    def __init__(self, condition, true_case_expression, false_case_expression = None):
        self.condition = condition
        self.true_case_expression = true_case_expression
        self.false_case_expression = false_case_expression

class BooleanNode(Node):
    def __init__(self, value: bool):
        self.value = value

    def __str__(self):
        return f"#t" if self.value else "#f"
    
    def __repr__(self):
        return f"BooleanNode({self.value})"

class EqualNode(Node):
    def __init__(self, operands):
        self.operands = operands

    def __str__(self):
        return f"(= {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"EqualNode({self.operands})"

class NotNode(Node):
    def __init__(self, operand):
        self.operand = operand

    def __str__(self):
        return f"(not {self.operand})"
    
    def __repr__(self):
        return f"NotNode({self.operand})"

class AndNode(Node):
    def __init__(self, operands):
        self.operands = operands
    
    def __str__(self):
        return f"(and {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"AndNode({self.operands})"

class OrNode(Node):
    def __init__(self, operands):
        self.operands = operands
    
    def __str__(self):
        return f"(or {''.join(str(op) for op in self.operands)})"

    def __repr__(self):
        return f"OrNode({self.operands})"

class CarNode(Node):
    def __init__(self, list_expr):
        self.list_expr = list_expr

    def __str__(self):
        return f"(car {self.list_expr})"
    
    def __repr__(self):
        return f"(CarNode({self.list_expr}))"

class CdrNode(Node):
    def __init__(self, list_expr):
        self.list_expr = list_expr

    def __str__(self):
        return f"(cdr {self.list_expr})"
    
    def __repr__(self):
        return f"(CdrNode({self.list_expr}))"

class ConsCell(Node):
    def __init__(self, car: Node, cdr: Node):
        self.car = car # the first element in the list
        self.cdr = cdr # the rest of the list (another ConsCell or nil)
    
    def __str__(self):
        elements = []
        current = self

        while isinstance(current, ConsCell):
            elements.append(str(current.car))
            current = current.cdr
        if not isinstance(current, EmptyListNode):
            elements.append(". " + str(current))
        return "(" + " ".join(elements) + ")"
    
    def __repr__(self):
        return f"ConsCell({self.car}, {self.cdr})"

class EmptyListNode(Node):
    def __str__(self):
        return "()"
    
    def __repr__(self):
        return "NIL"


class LetNode(Node):
    def __init__(self, bindings, body_expressions):
        self.bindings = bindings
        self.body_expressions = body_expressions
    
    def __repr__(self):
        return f"LetNode({self.bindings}, {self.body_expressions})"