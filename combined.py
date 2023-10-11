from string_with_arrows import *
import string
import os
import math

##########################
# CONSTANTS
##########################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS

##########################
# ERRORS
##########################

class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}\n' + f'Filename: {self.pos_start.file_name}, line {self.pos_start.line_num +1}, col {self.pos_end.col_num+1}' + '\n\n' + string_with_arrows(self.pos_start.file_text, self.pos_start, self.pos_end)
        return result
    
class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end,'Illegal Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end,'Invalid Syntax', details)        

class RTError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end,'Runtime Error', details)     

##########################
# POSITION
##########################

class Position:
    def __init__(self, index, line_num, col_num, file_name, file_text):
        self.index = index
        self.line_num = line_num
        self.col_num = col_num
        self.file_name = file_name
        self.file_text = file_text

    def advance(self, current_char = None):
        self.index += 1
        self.col_num += 1

        if current_char == '\n':
            self.line_num += 1
            self.col_num = 0

        return self
    
    def copy(self):
        return Position(self.index, self.line_num, self.col_num, self.file_name, self.file_text)

##########################
# TOKENS
##########################

TT_INT = 'INT' 
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_SEMICOLON = 'SEMICOLON'
TT_COLON = 'COLON'
TT_EOF = 'EOF'

class Token:
    def __init__(self, _type, value=None, pos_start = None, pos_end = None):
        self.type = _type
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end

    def __repr__(self): # representation method
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'
    
##########################
# LEXER
##########################

class Lexer:
    def __init__(self, file_name, text):
        self.file_name = file_name
        self.text = text
        self.pos = Position(-1, 0, -1, file_name, text)
        self.current_char = None
        self.advance()
    
    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in '\n':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()                                
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens , None                          
    
    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in DIGITS + ".":
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)


##########################
# NODES
##########################            

class NumberNode:
    def __init__(self, token):
        self.token = token

        self.pos_start = self.token.pos_start
        self.pos_end = self.token.pos_end

    def __repr__(self):
        return f'{self.token}'
    
class BinaryOperationNode:
    def __init__(self, left_node, operator_token, right_node):
        self.left_node = left_node
        self.operator_token = operator_token
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.operator_token}, {self.right_node})'
    
class UnaryOperationNode:
    def __init__(self, operator_token, node):
        self.operator_token = operator_token
        self.node = node
        
        self.pos_start = self.operator_token.pos_start
        self.pos_end = node.pos_end
    
    def __repr__(self):
        return f'({self.operator_token}, {self.node})'

    
##########################
# PARSER RESULT
##########################   

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, result):
        if isinstance(result, ParseResult):
            if result.error: self.error = result.error
            return result.node
        
        return result

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self   

##########################
# PARSER
##########################    

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.advance()

    def advance(self):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]
        return self.current_token
    
    def parse(self):
        result = self.expr()
        if not result.error and self.current_token.type != TT_EOF:
            return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '+', '-', '*', or '/'"))
        return result

    ##########################

    def factor(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (TT_PLUS, TT_MINUS):
            result.register(self.advance())
            factor = result.register(self.factor())
            if result.error: return result
            return result.success(UnaryOperationNode(token, factor))

        elif token.type in (TT_INT, TT_FLOAT):
            result.register(self.advance())
            return result.success(NumberNode(token))
        
        elif token.type == TT_LPAREN:
            result.register(self.advance())
            expression = result.register(self.expr())
            if result.error: return result
            if self.current_token.type == TT_RPAREN:
                result.register(self.advance())
                return result.success(expression)
            else:
                return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ')'"))

        return result.failure(InvalidSyntaxError(token.pos_start, token.pos_end, "Expected int or float"))

    def term(self):
        return self.binary_operation(self.factor, (TT_MUL, TT_DIV))

    def expr(self):
        return self.binary_operation(self.term, (TT_PLUS, TT_MINUS))

    ##########################

    def binary_operation (self, function, operation):
        result = ParseResult()
        left = result.register(function())
        if result.error: return result

        while self.current_token.type in operation:
            operator_token = self.current_token
            result.register(self.advance())
            right = result.register(function())
            if result.error: return result
            left = BinaryOperationNode(left, operator_token, right)
        
        return result.success(left)       
   
##########################
# RUNTIME RESULT
##########################

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None
        
    def register(self, result):
        if result.error: self.error - result.error
        return result.value
    
    def success(self, value):
        self.value = value
        return self
    
    def failure(self, error):
        self.error = error
        return self

##########################
# VALUES
##########################

class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()

    def set_pos (self, pos_start = None, pos_end = None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value), None
        
    def subtracted_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value), None        
        
    def multiplied_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value), None

    def divided_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(other.pos_start, other.pos_end, 'Division by zero')
            
            return Number(self.value / other.value), None      
        
    def __repr__(self):
        return str(self.value)             

##########################
# INTERPRETER
##########################

class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)
    
    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')

##########################

    def visit_NumberNode(self, node):
        return RTResult().success(Number(node.token.value).set_pos(node.pos_start, node.pos_end))

    def visit_BinaryOperationNode(self, node):
        res = RTResult()
        left = res.register(self.visit(node.left_node))
        if res.error: return res
        right = res.register(self.visit(node.right_node))
        if res.error: return res

        if node.operator_token.type == TT_PLUS:
            result, error= left.added_to(right)
        elif node.operator_token.type == TT_MINUS:
            result, error = left.subtracted_by(right)
        elif node.operator_token.type == TT_MUL:
            result, error = left.multiplied_by(right)
        elif node.operator_token.type == TT_DIV:
            result, error = left.divided_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOperationNode(self, node):
        res = RTResult()
        number = res.register(self.visit(node.node))
        if res.error: return res

        error = None

        if node.operator_token.type == TT_MINUS:
            number, error = number.multiplied_by(Number(-1))
        
        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

##########################
# TEST
##########################

def run(file_name, text):
    #Generate tokens
    input = Lexer(file_name, text)
    tokens, error = Lexer.make_tokens(input)
    if error: return None, error

    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    interpreter = Interpreter()
    result = interpreter.visit(ast.node)

    return result.value, result.error  

""" f = open("test.txt", "r")
result, error = run('test.txt', f.read())
if error: print(error.as_string())
else: print(result) """