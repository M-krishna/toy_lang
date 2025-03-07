import unittest
from main import Lexer, Parser, Evaluator, TokenType, AtomNode, ListNode

class TestString(unittest.TestCase):
    def test_string(self):
        source = "this is a string"

        lexer = Lexer(source)
        lexer.scan_tokens()
        tokens = lexer.get_tokens()

        parser = Parser(tokens)
        parser.parse_tokens()
        ast = parser.get_ast()

        evaluator = Evaluator()

        self.assertEqual(tokens[0].tt, TokenType.IDENTIFIER.name)
        self.assertEqual(tokens[0].lexeme, "this")
        self.assertEqual(tokens[1].tt, TokenType.IDENTIFIER.name)
        self.assertEqual(tokens[1].lexeme, "is")
        self.assertEqual(tokens[2].tt, TokenType.IDENTIFIER.name)
        self.assertEqual(tokens[2].lexeme, "a")
        self.assertEqual(tokens[3].tt, TokenType.IDENTIFIER.name)
        self.assertEqual(tokens[3].lexeme, "string")

        self.assertIsInstance(ast[0], AtomNode)
        self.assertIsInstance(ast[1], AtomNode)
        self.assertIsInstance(ast[2], AtomNode)
        self.assertIsInstance(ast[3], AtomNode)

        self.assertEqual(ast[0].type, TokenType.IDENTIFIER.name)
        self.assertEqual(ast[0].value, "this")
        self.assertEqual(ast[1].type, TokenType.IDENTIFIER.name)
        self.assertEqual(ast[1].value, "is")
        self.assertEqual(ast[2].type, TokenType.IDENTIFIER.name)
        self.assertEqual(ast[2].value, "a")
        self.assertEqual(ast[3].type, TokenType.IDENTIFIER.name)
        self.assertEqual(ast[3].value, "string")

        for node in ast:
            result = evaluator.evaluate(node)
            print(result)


if __name__ == "__main__":
    unittest.main()