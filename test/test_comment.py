import unittest
from main import Lexer, Parser, Evaluator, TokenType

class TestComment(unittest.TestCase):
    def test_comment(self):
        source = "; this is a comment"

        lexer = Lexer(source)
        lexer.scan_tokens()
        tokens = lexer.get_tokens()

        parser = Parser(tokens)
        parser.parse_tokens()
        ast = parser.get_ast()

        evaluator = Evaluator()

        self.assertEqual(tokens[0].tt, TokenType.EOF.name)
        self.assertEqual(tokens[0].lexeme, "")

        self.assertEqual(ast, [])

        with self.assertRaises(Exception):
            evaluator.evaluate(ast)

if __name__ == "__main__":
    unittest.main()