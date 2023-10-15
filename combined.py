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
        result = f'{self.error_name}: {self.details}\n' + f'Filename: {self.pos_start.file_name}, line {self.pos_start.line_num +1}, col {self.pos_end.col_num}' + \
            '\n\n' + \
            string_with_arrows(self.pos_start.file_text,
                               self.pos_start, self.pos_end)
        return result

class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Expected Character', details)

class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)


class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}: {self.details}\n' + '\n\n' + \
            string_with_arrows(self.pos_start.file_text,
                               self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        context = self.context

        while context:
            result = f'Filename: {pos.file_name}, line {pos.line_num +1}, in {context.display_name}\n' + result
            pos = context.parent_entry_pos
            context = context.parent

        return 'Traceback (most recent call last):\n' + result

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

    def advance(self, current_char=None):
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
TT_STRING = 'STRING'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_SEMICOLON = 'SEMICOLON'
TT_COLON = 'COLON'
TT_LSQUARE = 'LSQUARE'
TT_RSQUARE = 'RSQUARE'
TT_IDENTIFIER = 'IDENTIFIER'
TT_NEWLINE = 'NEWLINE'
TT_KEYWORD = 'KEYWORD'
TT_EQ = 'EQ'
TT_EE = 'EE'
TT_EOF = 'EOF'
TT_POWER = 'POWER'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_COMMA = 'COMMA'
TT_ARROW = 'ARROW'

KEYWORDS = ['VAR','AND', 'OR', 'NOT', 'IF', 'THEN', 'ELIF', 'ELSE', 'FUN', 'END']


class Token:
    def __init__(self, _type, value=None, pos_start=None, pos_end=None):
        self.type = _type
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):  # representation method
        if self.value:
            return f'{self.type}:{self.value}'
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
        self.current_char = self.text[self.pos.index] if self.pos.index < len(
            self.text) else None

    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in ';\n':
                tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char in '"':
                tokens.append(self.make_string())                
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(self.make_arrow())
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(Token(TT_POWER, pos_start=self.pos))
                self.advance()
            elif self.current_char == ':':
                tokens.append(Token(TT_COLON, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
                self.advance()                
            elif self.current_char == '!':
                token, error = self.make_not_equals()
                if error:
                    return [], error
                tokens.append(token)
            elif self.current_char == '=':
                tokens.append(self.make_equals())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()                                                                        
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None

    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in DIGITS + ".":
            if self.current_char == '.':
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

    def make_identifier(self):
        id_string = ''
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_string += self.current_char
            self.advance()

        token_type = TT_KEYWORD if id_string in KEYWORDS else TT_IDENTIFIER
        return Token(token_type, id_string, pos_start, self.pos)
    
    def make_string(self):
        string = ''
        pos_start = self.pos.copy()
        escape_char = False
        
        self.advance()

        escape_characters = {
            'n': '\n',
            't': '\t'
        }
        
        while self.current_char != None and (self.current_char != '"' or escape_char):
            if escape_char:
                string += escape_characters.get(self.current_char, self.current_char)
            else:
                if self.current_char == '\\':
                    escape_char = True
                else:
                    string += self.current_char
            self.advance()
            escape_char = False
            
        self.advance()
        return Token(TT_STRING, string, pos_start, self.pos)

    def make_arrow(self):
        token_type = TT_MINUS
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '>':
            self.advance()
            token_type = TT_ARROW

        return Token(token_type, pos_start=pos_start, pos_end=self.pos)

    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
        
        self.advance()
        return None, ExpectedCharError(pos_start, self.pos, "'=' (after '!')")

    def make_equals(self):
        token_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            token_type = TT_EE
        
        return Token(token_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_less_than(self):
        token_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            token_type = TT_LTE
        
        return Token(token_type, pos_start=pos_start, pos_end=self.pos)   
    
    def make_greater_than(self):
        token_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            token_type = TT_GTE
        
        return Token(token_type, pos_start=pos_start, pos_end=self.pos)   

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


class ListNode:
    def __init__(self, element_nodes, pos_start, pos_end):
        self.element_nodes = element_nodes

        self.pos_start = pos_start
        self.pos_end = pos_end


class StringNode:
    def __init__(self, token):
        self.token = token

        self.pos_start = self.token.pos_start
        self.pos_end = self.token.pos_end

    def __repr__(self):
        return f'{self.token}'


class VarAccessNode:
    def __init__(self, var_name_token):
        self.var_name_token = var_name_token

        self.pos_start = self.var_name_token.pos_start
        self.pos_end = self.var_name_token.pos_end


class VarAssignNode:
    def __init__(self, var_name_token, value_node):
        self.var_name_token = var_name_token
        self.value_node = value_node

        self.pos_start = self.var_name_token.pos_start
        self.pos_end = self.value_node.pos_end


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


class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[len(self.cases)-1])[0].pos_end


class FuncDefNode:
    def __init__(self, var_name_token, arg_name_tokens, body_node, should_return_null):
        self.var_name_token = var_name_token
        self.arg_name_tokens = arg_name_tokens
        self.body_node = body_node
        self.should_return_null = should_return_null

        if self.var_name_token:
            self.pos_start = self.var_name_token.pos_start
        elif len(self.arg_name_toks) > 0:
            self.pos_start = self.arg_name_tokens[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start

        self.pos_end = self.body_node.pos_end


class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.pos_start = self.node_to_call.pos_start

        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[len(self.arg_nodes)-1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end

##########################
# PARSER RESULT
##########################


class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
        self.to_reverse_count = 0

    def register_advancement(self):
        self.advance_count += 1
        pass

    def register_reversal(self):
        self.advance_count -= 1
        pass

    def register(self, result):
        self.advance_count += result.advance_count
        if result.error:
            self.error = result.error
        return result.node

    def try_register(self, result):
        if result.error:
            self.to_reverse_count = result.advance_count
            return None
        return self.register(result)

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
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
        self.update_current_token()
        return self.current_token

    def reverse(self, amount=1):
        self.token_index -= amount
        self.update_current_token()
        return self.current_token

    def update_current_token(self):
        if self.token_index >= 0 and self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]

    def parse(self):
        result = self.statements()
        if not result.error and self.current_token.type != TT_EOF:
            return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"))
        return result

    ##########################

    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_token.pos_start.copy()

        while self.current_token.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

        statement = res.register(self.expr())
        if res.error:
            return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_token.type == TT_NEWLINE:
                res.register_advancement()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if not more_statements:
                break

            statement = res.try_register(self.expr())
            if not statement:
                self.reverse(res.to_reverse_count)
                more_statements = False
                continue
            statements.append(statement)

        return res.success(ListNode(statements, pos_start, self.current_token.pos_end.copy()))

    def list_expr(self):
        result = ParseResult()
        element_nodes = []
        pos_start = self.current_token.pos_start.copy()

        if self.current_token.type != TT_LSQUARE:
            return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '['"))

        result.register_advancement()
        self.advance()

        if self.current_token.type == TT_RSQUARE:
            result.register_advancement()
            self.advance()
        else:
            element_nodes.append(result.register(self.expr()))
            if result.error:
                return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ']', 'IF', 'FUNC', int, float, identifier, '+', '-', '(', '[' or 'NOT'",))

            while self.current_token.type == TT_COMMA:
                result.register_advancement()
                self.advance()

                element_nodes.append(result.register(self.expr()))
                if result.error:
                    return result

            if self.current_token.type != TT_RSQUARE:
                return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ',' or ']'",))
            result.register_advancement()

        return result.success(ListNode(element_nodes, pos_start, self.current_token.pos_end.copy()))

    def if_expr(self):
        result = ParseResult()
        all_cases = result.register(self.if_expr_cases('IF'))
        if result.error: return result
        cases, else_case = all_cases
        return result.success(IfNode(cases, else_case))

    def if_expr_b(self):
        return self.if_expr_cases('ELIF')
    
    def if_expr_c(self):
        result = ParseResult()
        else_case = None

        if self.current_token.matches(TT_KEYWORD, 'ELSE'):
            result.register_advancement()
            self.advance()

        if self.current_token.type == TT_NEWLINE:
            result.register_advancement()
            self.advance()

            statements = result.register(self.statements())
            if result.error: return result
            else_case = (statements, True)

            if self.current_token.matches(TT_KEYWORD, 'END'):
                result.register_advancement()
                self.advance()
            else:
                return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,"Expected 'END'"))
        else:
            expr = result.register(self.expr())
            if result.error: 
                return result
            else_case = (expr, False)

        return result.success(else_case)

    def if_expr_b_or_c(self):
        result = ParseResult()
        cases, else_case = [], None

        if self.current_token.matches(TT_KEYWORD, 'ELIF'):
            all_cases = result.register(self.if_expr_b())
            if result.error: return result
            cases, else_case = all_cases
        else:
            else_case = result.register(self.if_expr_c())
        if result.error: 
            return result
        
        return result.success((cases, else_case))

    def if_expr_cases(self, case_keyword):
        result = ParseResult()
        cases = []
        else_case = None

        if not self.current_token.matches(TT_KEYWORD, case_keyword):
            return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected '{case_keyword}'"
        ))

        result.register_advancement()
        self.advance()

        condition = result.register(self.expr())
        if result.error: return result

        if not self.current_token.matches(TT_KEYWORD, 'THEN'):
            return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected 'THEN'"))

        result.register_advancement()
        self.advance()

        if self.current_token.type == TT_NEWLINE:
            result.register_advancement()
            self.advance()

            statements = result.register(self.statements())
            if result.error: return result
            cases.append((condition, statements, True))

            if self.current_token.matches(TT_KEYWORD, 'END'):
                result.register_advancement()
                self.advance()
            else:
                all_cases = result.register(self.if_expr_b_or_c())
                if result.error: return result
                new_cases, else_case = all_cases
                cases.extend(new_cases)
        else:
            expr = result.register(self.expr())
            if result.error: return result
            cases.append((condition, expr, False))

            all_cases = result.register(self.if_expr_b_or_c())
            if result.error: return result
            new_cases, else_case = all_cases
            cases.extend(new_cases)

        return result.success((cases, else_case))

    def base(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (TT_INT, TT_FLOAT):
            result.register_advancement()
            self.advance()
            return result.success(NumberNode(token))

        if token.type == TT_STRING:
            result.register_advancement()
            self.advance()
            return result.success(StringNode(token))

        elif token.type == TT_IDENTIFIER:
            result.register_advancement()
            self.advance()
            return result.success(VarAccessNode(token))

        elif token.type == TT_LPAREN:
            result.register_advancement()
            self.advance()
            expression = result.register(self.expr())
            if result.error:
                return result
            if self.current_token.type == TT_RPAREN:
                result.register_advancement()
                self.advance()
                return result.success(expression)
            else:
                return result.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ')'"))

        elif token.type == TT_LSQUARE:
            list_expr = result.register(self.list_expr())
            if result.error:
                return result
            return result.success(list_expr)

        elif token.matches(TT_KEYWORD, 'IF'):
            if_expr = result.register(self.if_expr())
            if result.error:
                return result
            return result.success(if_expr)

        elif token.matches(TT_KEYWORD, 'FUN'):
            func_def = result.register(self.func_def())
            if result.error:
                return result
            return result.success(func_def)

        return result.failure(InvalidSyntaxError(token.pos_start, token.pos_end, "Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FUNC' or 'NOT'"))

    def power(self):
        return self.binary_operation(self.base, (TT_POWER,), self.factor)

    def call(self):
        res = ParseResult()
        base = res.register(self.base())
        if res.error:
            return res
        if self.current_token.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            args_nodes = []

            if self.current_token.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                args_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ')', 'IF', 'FUNC', int, float, identifier, '+', '-', '(', '[' or 'NOT'",))

                while self.current_token.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()

                    args_nodes.append(res.register(self.expr()))
                    if res.error:
                        return res

                if self.current_token.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ',' or ')'",))
                res.register_advancement()
                self.advance()
            return res.success(CallNode(base, args_nodes))
        return res.success(base)

    def factor(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (TT_PLUS, TT_MINUS):
            result.register_advancement()
            self.advance()
            factor = result.register(self.factor())
            if result.error:
                return result
            return result.success(UnaryOperationNode(token, factor))

        return self.power()

    def term(self):
        return self.binary_operation(self.factor, (TT_MUL, TT_DIV))

    def comp_expr(self):
        res = ParseResult()
        if self.current_token.matches(TT_KEYWORD, 'NOT'):
            operator_token = self.current_token
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error:
                return res
            return res.success(UnaryOperationNode(operator_token, node))

        node = res.register(self.binary_operation(self.arith_expr, (TT_EE,
                            TT_GT, TT_GTE, TT_LT, TT_LTE, TT_NE)))

        if res.error:
            return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, identifier, '+', '-', '(', '[', 'NOT'"))

        return res.success(node)

    def arith_expr(self):
        return self.binary_operation(self.term, (TT_PLUS, TT_MINUS))

    def expr_test(self):  # NANDITO YUNG ISSUE REGARDING SA FUNCTIONS
        res = ParseResult()
        flag_ident = False

        if self.current_token.type == TT_IDENTIFIER:
            if self.current_token.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ':'"))

            var_name = self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != TT_COLON:
                res.register_reversal()
                self.reverse()

            res.register_advancement()
            self.advance()

            if self.current_token.type != TT_EQ:
                res.register_reversal()
                self.reverse()
                if self.current_token.type == TT_INT or self.current_token.type == TT_FLOAT:
                    pass
                elif self.current_token.type == TT_IDENTIFIER:
                    flag_ident = True
                    pass
                elif self.current_token.type != TT_EQ:
                    return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '='"))

                if flag_ident == True:
                    node = res.register(self.binary_operation(
                        self.comp_expr, ((TT_KEYWORD, "AND"), (TT_KEYWORD, "OR"))))

                    if res.error:
                        return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, '+', '-', or '('"))

                    return res.success(node)

            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.binary_operation(self.comp_expr, ((TT_KEYWORD, "AND"), (TT_KEYWORD, "OR"))))

        if res.error:
            return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FUNC' or 'NOT'"))

        return res.success(node)

    def expr(self):  # NANDITO YUNG ISSUE REGARDING SA FUNCTIONS
        res = ParseResult()

        if self.current_token.matches(TT_KEYWORD, 'VAR'):
            res.register_advancement()
            self.advance()

            if self.current_token.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected identifier"
                ))

            var_name = self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != TT_EQ:
                return res.failure(InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected '='"
                ))

            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.binary_operation(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
            ))

        return res.success(node)


    def func_def(self):
        res = ParseResult()

        if not self.current_token.matches(TT_KEYWORD, 'FUN'):
            return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected 'FUNC'"))

        res.register_advancement()
        self.advance()

        if self.current_token.type == TT_IDENTIFIER:
            var_name_token = self.current_token
            res.register_advancement()
            self.advance()
            if self.current_token.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected '('"))
        else:
            var_name_token = None
            if self.current_token.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected identifier or '('"))

        res.register_advancement()
        self.advance()
        args = []

        if self.current_token.type == TT_IDENTIFIER:
            args.append(self.current_token)
            res.register_advancement()
            self.advance()

            while self.current_token.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                if self.current_token.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected identifier"))

                args.append(self.current_token)
                res.register_advancement()
                self.advance()

            if self.current_token.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected ',' or ')'"))
        else:
            if self.current_token.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected identifier or ')'"))

        res.register_advancement()
        self.advance()
        
        if self.current_token.type == TT_ARROW:
            res.register_advancement()
            self.advance()

            body = res.register(self.expr())
            if res.error: return res

            return res.success(FuncDefNode(var_name_token, args, body, False))
        
        if self.current_token.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,f"Expected '->' or NEWLINE"))

        res.register_advancement()
        self.advance()

        body = res.register(self.statements())
        if res.error: return res

        if not self.current_token.matches(TT_KEYWORD, 'END'):
            return res.failure(InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,f"Expected 'END'"))

        res.register_advancement()
        self.advance()
        
        return res.success(FuncDefNode(var_name_token, args, body, True))

    ##########################

    def binary_operation(self, function, operation, function_b=None):
        if function_b == None:
            function_b = function

        result = ParseResult()
        left = result.register(function())
        if result.error:
            return result

        while self.current_token.type in operation or (self.current_token.type, self.current_token.value) in operation:
            operator_token = self.current_token
            result.register_advancement()
            self.advance()
            right = result.register(function())
            if result.error:
                return result
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
		if result.error: self.error = result.error
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
class Value:
	def __init__(self):
		self.set_pos()
		self.set_context()

	def set_pos (self, pos_start = None, pos_end = None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self
	
	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		return None, self.illegal_operation(other)
		
	def subtracted_by(self, other):
		return None, self.illegal_operation(other)
		
	def multiplied_by(self, other):
		return None, self.illegal_operation(other)

	def divided_by(self, other):
		return None, self.illegal_operation(other)
	def powered_by(self, other):
		return None, self.illegal_operation(other)
		
	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)

	def anded_by(self, other):
		return None, self.illegal_operation(other)

	def ored_by(self, other):
		return None, self.illegal_operation(other)

	def notted(self):
		return None, self.illegal_operation(None)

	def is_true(self):
		return False

	def copy(self):
		raise Exception('No copy method defined')
	
	def execute(self, args):
		return RTResult().failure(self.illegal_operation())
			
	def illegal_operation(self, other):
		if not other:
			other = self
		return RTError(
			self.pos_start, self.pos_end,
			'Illegal operation',
			self.context
		)

class Number(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)
		
	def subtracted_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None       
		else:
			return None, Value.illegal_operation(other)         
		
	def multiplied_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)        

	def divided_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RTError(other.pos_start, other.pos_end, 'Division by zero', self.context)
			
			return Number(self.value / other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)           

	def powered_by(self, other):
		if isinstance(other, Number):
			return Number(self.value ** other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)           
		
	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(-int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)        

	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(-int(self.value != other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(-int(self.value < other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(-int(self.value > other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(-int(self.value <= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(-int(self.value >= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(-int(bool(self.value) and bool(other.value))).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(-int(bool(self.value) or bool(other.value))).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)

	def notted(self):
		return Number(-1 if self.value == 0 else 0).set_context(self.context), None

	def is_true(self):
		return self.value != 0

	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy        
		
	def __repr__(self):
		return str(self.value)      
	
Number.null = Number(-1)
Number.false = Number(0)
Number.true = Number(1)
Number.math_PI = Number(math.pi)	

class String(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)
		
	def subtracted_by(self, other):
		if isinstance(other, String):
			return String(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)
		
	def multiplied_by(self, other):
		if isinstance(other, Number):
			return String(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(other)
	
	def is_true(self):
		return len(self.value) > 0
	
	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	
	def __repr__(self):
		return f'"{self.value}"'

class List(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements

	def added_to(self, other):
		new_list = self.copy()
		new_list.elements.append(other)
		return new_list, None

	def subtracted_by(self, other):
		if isinstance(other, Number):
			new_list = self.copy()
			try:
				new_list.elements.remove(other)
				return new_list, None
			except:
				return None, RTError(other.pos_start, other.pos_end, 'Element at this index could not be removed from list because index is out of bounds', self.context)
		else:
			return None, Value.illegal_operation(self, other)

	def multiplied_by(self, other):
		if isinstance(other, List):
			new_list = self.copy()
			new_list.elements.extend(other.elements)
			return new_list, None
		else:
			return None, Value.illegal_operation(self, other)

	def divided_by(self, other):
		if isinstance(other, Number):
			try:
				return self.elements[other.value], None
			except:
				return None, RTError(other.pos_start, other.pos_end, 'Element at this index could not be retrieved from list because index is out of bounds', self.context)
		else:
			return None, Value.illegal_operation(self, other)		

	def copy(self):
		copy = List(self.elements)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def __repr__(self):
		return f'[{", ".join([str(x) for x in self.elements])}]'

class BaseFunction(Value):
	def __init__(self, name):
		super().__init__()
		self.name = name or "<anonymous>"

	def generate_new_context(self):
		new_context = Context(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
		return new_context

	def check_args(self, args, arg_names):
		result = RTResult()
		if len(args) > len(arg_names):
			return result.failure(RTError(self.pos_start, self.pos_end, f"{len(args) - len(arg_names)} too many args passed into {self.name}", self.context))
		if len(args) < len(arg_names):
			return result.failure(RTError(self.pos_start, self.pos_end, f"{len(arg_names) - len(args)} too few args passed into {self.name}", self.context))
		
		return result.success(None)

	def populate_args(self, arg_names, args, exec_ctx):
		for i in range(len(args)):
			arg_name = arg_names[i]
			arg_value = args[i]
			arg_value.set_context(exec_ctx)
			exec_ctx.symbol_table.set(arg_name, arg_value)

	def check_and_populate_args(self, arg_names, args, exec_ctx):
		result = RTResult()
		result.register(self.check_args(args, arg_names))
		if result.error: return result
		self.populate_args(arg_names, args, exec_ctx)
		return result.success(None)

class Function(BaseFunction):
	def __init__(self, name, body_node, arg_names, should_return_null):
		super().__init__(name)
		self.body_node = body_node
		self.arg_names = arg_names
		self.should_return_null = should_return_null
	
	def execute(self, args):
		res = RTResult()
		interpreter = Interpreter()
		exec_ctx = self.generate_new_context()

		self.check_and_populate_args(self.arg_names, args, exec_ctx)
		
		value = res.register(interpreter.visit(self.body_node, exec_ctx))
		if res.error: return res
		return res.success(Number.null if self.should_return_null else value)
	
	def copy(self):
		copy = Function(self.name, self.body_node, self.arg_names, self.should_return_null)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __repr__(self):
		return f"<function {self.name}>"
	
class BuiltInFunction(BaseFunction):
	def __init__(self, name):
		super().__init__(name)	

	def execute(self, args):
		result = RTResult()
		exec_ctx = self.generate_new_context()

		method_name = f'execute_{self.name}'
		method = getattr(self, method_name, self.no_execute_method)

		result.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
		if result.error: return result

		return_value = result.register(method(exec_ctx))
		if result.error: return result
		return result.success(return_value)	
	
	def no_visit_method(self, node, context):
		raise Exception(f'No execute_{self.name} method defined')

	def copy(self):
		copy = BuiltInFunction(self.name)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy
	
	def __repr__(self):
		return f"<built-in function {self.name}>"
	
	##########################

	def execute_print (self, exec_ctx):
		print(str(exec_ctx.symbol_table.get('value')))
		return RTResult().success(Number.null)
	execute_print.arg_names = ['value']

	def execute_print_ret(self, exec_ctx):
		return RTResult().success(String(str(exec_ctx.symbol_table.get('value'))))
	execute_print_ret.arg_names = ['value']

	def execute_clear(self):
		os.system('cls' if os.name == 'nt' else 'clear')
		return RTResult().success(Number.null)
	execute_clear.arg_names = []

BuiltInFunction.print = BuiltInFunction("output<<")
BuiltInFunction.print_ret = BuiltInFunction("output_ret<<")
BuiltInFunction.clear = BuiltInFunction("clear")

##########################
# CONTEXT
##########################

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None

##########################
# SYMBOL TABLE
##########################

class SymbolTable:
	def __init__(self, parent=None):
		self.symbols = {}
		self.parent = parent

	def get(self, name):
		value = self.symbols.get(name, None)
		if value == None and self.parent:
			return self.parent.get(name)
		return value
	
	def set (self, name, value):
		self.symbols[name] = value
	
	def remove (self, name):
		del self.symbols[name]

##########################
# INTERPRETER
##########################

class Interpreter:
	def visit(self, node, context):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)
	
	def no_visit_method(self, node, context):
		raise Exception(f'No visit_{type(node).__name__} method defined')

##########################

	def visit_NumberNode(self, node, context):
		return RTResult().success(Number(node.token.value).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_StringNode(self, node, context):
		return RTResult().success(String(node.token.value).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []
		for element_node in node.element_nodes:
			elements.append(res.register(self.visit(element_node, context)))
			if res.error: 
				return res
		return res.success(List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_token.value
		value = context.symbol_table.get(var_name)

		if value is None:
			return res.failure(RTError(node.pos_start, node.pos_end, f"'{var_name}' is not defined", context))

		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(value)
	
	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_token.value
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res

		context.symbol_table.set(var_name, value)
		return res.success(value)

	def visit_BinaryOperationNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.error: return res
		right = res.register(self.visit(node.right_node, context))
		if res.error: return res

		if node.operator_token.type == TT_PLUS:
			result, error= left.added_to(right)
		elif node.operator_token.type == TT_MINUS:
			result, error = left.subtracted_by(right)
		elif node.operator_token.type == TT_MUL:
			result, error = left.multiplied_by(right)
		elif node.operator_token.type == TT_DIV:
			result, error = left.divided_by(right)
		elif node.operator_token.type == TT_POWER:
			result, error = left.powered_by(right)
		elif node.operator_token.type == TT_EE:
			result, error = left.get_comparison_eq(right)
		elif node.operator_token.type == TT_NE:
			result, error = left.get_comparison_ne(right)
		elif node.operator_token.type == TT_LT:
			result, error = left.get_comparison_lt(right)
		elif node.operator_token.type == TT_GT:
			result, error = left.get_comparison_gt(right)                       
		elif node.operator_token.type == TT_LTE:
			result, error = left.get_comparison_lte(right)
		elif node.operator_token.type == TT_GTE:
			result, error = left.get_comparison_gte(right)     
		elif node.operator_token.matches(TT_KEYWORD, 'AND'):
			result, error = left.anded_by(right)
		elif node.operator_token.matches(TT_KEYWORD, 'OR'):
			result, error = left.ored_by(right)

		if error: # type: ignore
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end)) # type: ignore

	def visit_UnaryOperationNode(self, node, context):
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.error: return res

		error = None

		if node.operator_token.type == TT_MINUS:
			number, error = number.multiplied_by(Number(-1))
		elif node.operator_token.matches(TT_KEYWORD, 'NOT'):
			number, error = number.notted()
		
		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))
		
	def visit_IfNode(self, node, context):
		res = RTResult()

		for condition, expr, should_return_null in node.cases:
			condition_value = res.register(self.visit(condition, context))
			if res.error: return res

			if condition_value.is_true():
				expr_value = res.register(self.visit(expr, context))
				if res.error: return res
				return res.success(Number.null if should_return_null else expr_value)
		if node.else_case:
			expr, should_return_null = node.else_case
			expr_value = res.register(self.visit(expr, context))
			if res.error: return res
			return res.success(Number.null if should_return_null else expr_value)
		
		return res.success(Number.null)

	def visit_FuncDefNode(self, node, context):
		res = RTResult()
		func_name = node.var_name_token.value if node.var_name_token else None
		body_node = node.body_node
		arg_names = [arg_name.value for arg_name in node.arg_name_tokens]
		func_value = Function(func_name, body_node, arg_names, node.should_return_null).set_context(context).set_pos(node.pos_start, node.pos_end)
		
		if node.var_name_token:
			context.symbol_table.set(func_name, func_value)

		return res.success(func_value)

	def visit_CallNode(self, node, context):
		res = RTResult()
		args = []

		value_to_call = res.register(self.visit(node.node_to_call, context))
		if res.error: return res
		value_to_call = value_to_call().copy().set_pos(node.pos_start, node.pos_end)

		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.error: return res

		return_value = res.register(value_to_call.execute(args))
		if res.error: return res
		return_value = return_value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(return_value)


##########################
# TEST
##########################

global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number.null)
global_symbol_table.set("TRUE", Number.true)
global_symbol_table.set("FALSE", Number.false)
global_symbol_table.set("MATH_PI", Number.math_PI)
global_symbol_table.set("output<<", BuiltInFunction.print)
global_symbol_table.set("output_ret<<", BuiltInFunction.print_ret)
global_symbol_table.set("clear", BuiltInFunction.clear)
global_symbol_table.set("cls", BuiltInFunction.clear)

def run(file_name, text):
	#Generate tokens
	input = Lexer(file_name, text)
	tokens, error = Lexer.make_tokens(input)
	if error: return None, error

	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	interpreter = Interpreter()
	context = Context('<program>')
	context.symbol_table = global_symbol_table 
	result = interpreter.visit(ast.node, context)

	return result.value, result.error


while True:
    text = input('basic > ')
    if text.strip() == "": continue
    result, error = run('<stdin>',text)

    if error: print(error.as_string())
    elif result: 
        if len(result.elements) == 1:
            print(repr(result.elements[0]))
        print(repr(result))