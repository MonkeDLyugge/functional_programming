import re
from typing import Any, Dict, List, Tuple


TOKEN_REGEX = [
    (r'\d+', 'NUMBER'),
    (r'if\b', 'IF'),
    (r'then\b', 'THEN'),
    (r'else\b', 'ELSE'),
    (r'==', 'EQ'),
    (r'=', 'EQUALS'),
    (r'\\', 'LAMBDA'),
    (r'->', 'ARROW'),
    (r'\(', 'LPAREN'),
    (r'\)', 'RPAREN'),
    (r'[><=\*/+-]', 'OPERATOR'),
    (r'\btrue\b', 'TRUE'),
    (r'\bfalse\b', 'FALSE'),
    (r'\w+', 'IDENTIFIER'),
    (r'\s+', None),
]

def tokenize(code: str) -> List[Tuple[str, str]]:
    tokens = []
    while code:
        match = None
        for regex, token_type in TOKEN_REGEX:
            match = re.match(regex, code)
            if match:
                if token_type:
                    tokens.append((token_type, match.group(0)))
                code = code[match.end():]
                break
        if not match:
            raise SyntaxError(f"Непонятный символ в коде: {code[0]}")
    return tokens


class ASTNode:
    pass

class Boolean(ASTNode):
    def __init__(self, value: bool):
        self.value = value

class Number(ASTNode):
    def __init__(self, value: int):
        self.value = value

class Identifier(ASTNode):
    def __init__(self, name: str):
        self.name = name

class Lambda(ASTNode):
    def __init__(self, param: Identifier, body: ASTNode):
        self.param = param
        self.body = body

class Application(ASTNode):
    def __init__(self, func: ASTNode, arg: ASTNode):
        self.func = func
        self.arg = arg

class FunctionDef(ASTNode):
    def __init__(self, name: str, body: ASTNode):
        self.name = name
        self.body = body

class If(ASTNode):
    def __init__(self, condition: ASTNode, then_branch: ASTNode, else_branch: ASTNode):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

class BinaryOperation(ASTNode):
    def __init__(self, left: ASTNode, operator: str, right: ASTNode):
        self.left = left
        self.operator = operator
        self.right = right

class Parser:
    def __init__(self, tokens: List[Tuple[str, str]]):
        self.tokens = tokens
        self.current = 0

    def parse(self) -> List[ASTNode]:
        nodes = []
        while self.current < len(self.tokens):
            nodes.append(self.parse_definition())
        return nodes

    def parse_definition(self) -> FunctionDef:
        name = self.consume('IDENTIFIER')[1]
        self.consume('EQUALS')
        body = self.parse_expression()
        return FunctionDef(name, body)

    def parse_primary(self) -> ASTNode:
        if self.match('NUMBER'):
            return Number(int(self.consume('NUMBER')[1]))
        elif self.match('TRUE'):
            self.consume('TRUE')
            return Boolean(True)
        elif self.match('FALSE'):
            self.consume('FALSE')
            return Boolean(False)
        elif self.match('IDENTIFIER'):
            return Identifier(self.consume('IDENTIFIER')[1])
        elif self.match('LPAREN'):
            self.consume('LPAREN')
            expr = self.parse_expression()
            self.consume('RPAREN')
            return expr
        else:
            raise SyntaxError("Ошибка в синтаксисе")

    def parse_expression(self) -> ASTNode:
        if self.match('IF'):
            return self.parse_if()
        elif self.match('LAMBDA'):
            return self.parse_lambda()
        else:
            return self.parse_binary_operation()

    def parse_if(self) -> If:
        self.consume('IF')
        condition = self.parse_expression()
        self.consume('THEN')
        then_branch = self.parse_expression()
        self.consume('ELSE')
        else_branch = self.parse_expression()
        return If(condition, then_branch, else_branch)

    def parse_lambda(self) -> Lambda:
        self.consume('LAMBDA')
        param = Identifier(self.consume('IDENTIFIER')[1])
        self.consume('ARROW')
        body = self.parse_expression()
        return Lambda(param, body)

    def parse_binary_operation(self) -> ASTNode:
        left = self.parse_application()
        while self.match('EQ', 'OPERATOR'):
            operator = self.consume(self.tokens[self.current][0])[1]
            right = self.parse_application()
            left = BinaryOperation(left, operator, right)
        return left

    def parse_application(self) -> ASTNode:
        left = self.parse_primary()
        while self.match('LPAREN'):
            self.consume('LPAREN')
            arg = self.parse_expression()
            self.consume('RPAREN')
            left = Application(left, arg)
        return left

    def match(self, *types: str) -> bool:
        return self.current < len(self.tokens) and self.tokens[self.current][0] in types

    def consume(self, type_: str) -> Tuple[str, str]:
        if not self.match(type_):
            raise SyntaxError(f"Ожидался {type_}, но найдено {self.tokens[self.current][0]}")
        token = self.tokens[self.current]
        self.current += 1
        return token

    def ast_debug(self, node: ASTNode) -> str:
        if isinstance(node, Number):
            return f"Number({node.value})"
        if isinstance(node, Identifier):
            return f"Identifier({node.name})"
        if isinstance(node, Lambda):
            return f"Lambda({self.ast_debug(node.param)}, {self.ast_debug(node.body)})"
        if isinstance(node, Application):
            return f"Application({self.ast_debug(node.func)}, {self.ast_debug(node.arg)})"
        if isinstance(node, FunctionDef):
            return f"FunctionDef({node.name}, {self.ast_debug(node.body)})"
        if isinstance(node, If):
            return f"If({self.ast_debug(node.condition)}, {self.ast_debug(node.then_branch)}, {self.ast_debug(node.else_branch)})"
        if isinstance(node, BinaryOperation):
            return f"BinaryOperation({self.ast_debug(node.left)}, {node.operator}, {self.ast_debug(node.right)})"
        return "Unknown"


class Interpreter:
    def __init__(self):
        self.env: Dict[str, Any] = {
            'true': True,
            'false': False
        }

    def eval(self, node: ASTNode, env: Dict[str, Any] = None) -> Any:
        if env is None:
            env = self.env

        if isinstance(node, Boolean):
            return node.value

        if isinstance(node, Number):
            return node.value

        if isinstance(node, Identifier):
            if node.name not in env:
                raise NameError(f"Неопределенная переменная: {node.name}")
            return env[node.name]

        if isinstance(node, Lambda):
            return lambda arg: self.eval(node.body, {**env, node.param.name: arg})

        if isinstance(node, Application):
            func = self.eval(node.func, env)
            arg = self.eval(node.arg, env)
            return func(arg)

        if isinstance(node, FunctionDef):
            value = self.eval(node.body, env)
            env[node.name] = value
            print(f"{node.name} = {value}")
            return None

        if isinstance(node, If):
            condition = self.eval(node.condition, env)
            if condition:
                return self.eval(node.then_branch, env)
            else:
                return self.eval(node.else_branch, env)

        if isinstance(node, BinaryOperation):
            left = self.eval(node.left, env)
            right = self.eval(node.right, env)
            if node.operator == '==':
                return left == right
            if node.operator == '+':
                return left + right
            if node.operator == '-':
                return left - right
            if node.operator == '*':
                return left * right
            if node.operator == '/':
                return left / right
            if node.operator == '>':
                return left > right
            if node.operator == '<':
                return left < right

        raise TypeError(f"Неизвестный тип узла: {type(node)}")

    def ast_debug(self, node: ASTNode) -> str:
        if isinstance(node, Number):
            return f"Number({node.value})"
        if isinstance(node, Identifier):
            return f"Identifier({node.name})"
        if isinstance(node, Lambda):
            return f"Lambda({self.ast_debug(node.param)}, {self.ast_debug(node.body)})"
        if isinstance(node, Application):
            return f"Application({self.ast_debug(node.func)}, {self.ast_debug(node.arg)})"
        if isinstance(node, FunctionDef):
            return f"FunctionDef({node.name}, {self.ast_debug(node.body)})"
        if isinstance(node, If):
            return f"If({self.ast_debug(node.condition)}, {self.ast_debug(node.then_branch)}, {self.ast_debug(node.else_branch)})"
        if isinstance(node, BinaryOperation):
            return f"BinaryOperation({self.ast_debug(node.left)}, {node.operator}, {self.ast_debug(node.right)})"
        return "Unknown"


def repl():
    interpreter = Interpreter()
    while True:
        try:
            code = input(">>> ")
            tokens = tokenize(code)
            parser = Parser(tokens)
            ast = parser.parse()
            for node in ast:
                result = interpreter.eval(node)
                if result is not None:
                    print(result)
        except Exception as e:
            print(f"Ошибка: {e}")

if __name__ == "__main__":
    repl()
