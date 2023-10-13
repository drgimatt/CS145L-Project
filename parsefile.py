import lexer

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
        self.pos_end = self.else_case or self.cases[len(self.cases)-1][0].pos_end

##########################
# PARSER RESULT
##########################

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count += 1
        pass

    def register(self, result):
        self.advance_count += result.advance_count
        if result.error:
            self.error = result.error
        return result.node

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
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]
        return self.current_token

    def reverse(self):
        self.token_index -= 1
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]   
        return self.current_token

    def parse(self):
        result = self.expr()
        if not result.error and self.current_token.type != lexer.TT_EOF:
            return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '+', '-', '*', or '/'"))
        return result

    ##########################

    def if_expr(self):
        result = ParseResult()
        cases = []
        else_case = None

        if not self.current_token.matches(lexer.TT_KEYWORD, 'IF'):
            return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected 'IF'"))
        
        result.register_advancement()
        self.advance()

        condition = result.register(self.expr())
        if result.error: return result

        if not self.current_token.matches(lexer.TT_KEYWORD, 'THEN'):
            return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected 'THEN'"))
        
        result.register_advancement()
        self.advance()

        expression = result.register(self.expr())
        if result.error: return result
        cases.append((condition, expression))

        while self.current_token.matches(lexer.TT_KEYWORD, 'ELIF'):
            result.register_advancement()
            self.advance()

            condition = result.register(self.expr())
            if result.error: return result

            if not self.current_token.matches(lexer.TT_KEYWORD, 'THEN'):
                return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,f"Expected 'THEN'"))

            result.register_advancement()
            self.advance()

            expr = result.register(self.expr())
            if result.error: return result
            cases.append((condition, expr))

        if self.current_token.matches(lexer.TT_KEYWORD, 'ELSE'):
            result.register_advancement()
            self.advance()

            else_case = result.register(self.expr())
            if result.error: return result

        return result.success(IfNode(cases, else_case))            

    def base(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (lexer.TT_INT, lexer.TT_FLOAT):
            result.register_advancement()
            self.advance()
            return result.success(NumberNode(token))

        elif token.type == lexer.TT_IDENTIFIER:
            result.register_advancement()
            self.advance()
            return result.success(VarAccessNode(token))

        elif token.type == lexer.TT_LPAREN:
            result.register_advancement()
            self.advance()
            expression = result.register(self.expr())
            if result.error:
                return result
            if self.current_token.type == lexer.TT_RPAREN:
                result.register_advancement()
                self.advance()
                return result.success(expression)
            else:
                return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ')'"))
            
        elif token.matches(lexer.TT_KEYWORD, 'IF'):
            if_expr = result.register(self.if_expr())
            if result.error:
                return result
            return result.success(if_expr)

        return result.failure(lexer.InvalidSyntaxError(token.pos_start, token.pos_end, "Expected int, float, identifier, '+', '-', or '('"))

    def power(self):
        return self.binary_operation(self.base, (lexer.TT_POWER,), self.factor)

    def factor(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (lexer.TT_PLUS, lexer.TT_MINUS):
            result.register_advancement()
            self.advance()
            factor = result.register(self.factor())
            if result.error:
                return result
            return result.success(UnaryOperationNode(token, factor))

        return self.power()

    def term(self):
        return self.binary_operation(self.factor, (lexer.TT_MUL, lexer.TT_DIV))
    
    def comp_expr(self):
        res = ParseResult()
        if self.current_token.matches(lexer.TT_KEYWORD, 'NOT'):
            operator_token = self.current_token
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error:
                return res
            return res.success(UnaryOperationNode(operator_token, node))
        
        node = res.register(self.binary_operation(self.arith_expr,(lexer.TT_EE, lexer.TT_GT, lexer.TT_GTE, lexer.TT_LT, lexer.TT_LTE, lexer.TT_NE)))

        if res.error:
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, identifier, '+', '-', '(', 'NOT'"))
        
        return res.success(node)

    def arith_expr(self):
        return self.binary_operation(self.term, (lexer.TT_PLUS, lexer.TT_MINUS))

    def expr(self):
        res = ParseResult()
        
        if self.current_token.type == lexer.TT_IDENTIFIER:
            var_name = self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != lexer.TT_COLON:
                    return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ':'"))      

            res.register_advancement()
            self.advance()

            if self.current_token.type != lexer.TT_EQ:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '='"))
            
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.binary_operation(self.comp_expr, ((lexer.TT_KEYWORD, "AND"), (lexer.TT_KEYWORD, "OR"))))

        if res.error:
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, identifier, '+', '-', or '('"))

        return res.success(node)

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

