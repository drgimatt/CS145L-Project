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


class OutputNode:
    def __init__(self, value_node) -> None:
        self.value_node = value_node

        self.pos_start = value_node.pos_start
        self.pos_end = value_node.pos_end


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
        if not result.error and self.current_token.type != lexer.TT_EOF:
            return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"))
        return result

    ##########################

    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_token.pos_start.copy()

        while self.current_token.type == lexer.TT_NEWLINE:
            res.register_advancement()
            self.advance()

        statement = res.register(self.expr())
        if res.error:
            return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_token.type == lexer.TT_NEWLINE:
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

        if self.current_token.type != lexer.TT_LSQUARE:
            return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '['"))

        result.register_advancement()
        self.advance()

        if self.current_token.type == lexer.TT_RSQUARE:
            result.register_advancement()
            self.advance()
        else:
            element_nodes.append(result.register(self.expr()))
            if result.error:
                return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ']', 'IF', 'FUNC', int, float, identifier, '+', '-', '(', '[' or 'NOT'",))

            while self.current_token.type == lexer.TT_COMMA:
                result.register_advancement()
                self.advance()

                element_nodes.append(result.register(self.expr()))
                if result.error:
                    return result

            if self.current_token.type != lexer.TT_RSQUARE:
                return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ',' or ']'",))
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

        if self.current_token.matches(lexer.TT_KEYWORD, 'ELSE'):
            result.register_advancement()
            self.advance()

        if self.current_token.type == lexer.TT_NEWLINE:
            result.register_advancement()
            self.advance()

            statements = result.register(self.statements())
            if result.error: return result
            else_case = (statements, True)

            if self.current_token.matches(lexer.TT_KEYWORD, 'END'):
                result.register_advancement()
                self.advance()
            else:
                return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,"Expected 'END'"))
        else:
            expr = result.register(self.expr())
            if result.error: 
                return result
            else_case = (expr, False)

        return result.success(else_case)

    def if_expr_b_or_c(self):
        result = ParseResult()
        cases, else_case = [], None

        if self.current_token.matches(lexer.TT_KEYWORD, 'ELIF'):
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

        if not self.current_token.matches(lexer.TT_KEYWORD, case_keyword):
            return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected '{case_keyword}'"
        ))

        result.register_advancement()
        self.advance()

        condition = result.register(self.expr())
        if result.error: return result

        #if not self.current_token.matches(lexer.TT_KEYWORD, 'THEN'):
        #    return result.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected 'THEN'"))

        result.register_advancement()
        self.advance()

        if self.current_token.type == lexer.TT_NEWLINE:
            result.register_advancement()
            self.advance()

            statements = result.register(self.statements())
            if result.error: return result
            cases.append((condition, statements, True))

            if self.current_token.matches(lexer.TT_KEYWORD, 'END'):
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

        if token.type in (lexer.TT_INT, lexer.TT_FLOAT):
            result.register_advancement()
            self.advance()
            return result.success(NumberNode(token))

        elif token.type == lexer.TT_STRING:
            result.register_advancement()
            self.advance()
            return result.success(StringNode(token))

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

        elif token.type == lexer.TT_LSQUARE:
            list_expr = result.register(self.list_expr())
            if result.error:
                return result
            return result.success(list_expr)

        elif token.matches(lexer.TT_KEYWORD, 'IF'):
            if_expr = result.register(self.if_expr())
            if result.error:
                return result
            return result.success(if_expr)

        elif token.matches(lexer.TT_KEYWORD, 'FUNC'):
            func_def = result.register(self.func_def())
            if result.error:
                return result

        return result.failure(lexer.InvalidSyntaxError(token.pos_start, token.pos_end, "Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FUNC' or 'NOT'"))

    def power(self):
        return self.binary_operation(self.base, (lexer.TT_POWER,), self.factor)

    def call(self):
        res = ParseResult()
        base = res.register(self.base())
        if res.error:
            return res
        
        if self.current_token.type == lexer.TT_LPAREN:
            res.register_advancement()
            self.advance()
            args_nodes = []

            if self.current_token.type == lexer.TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                args_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ')', 'IF', 'FUNC', int, float, identifier, '+', '-', '(', '[' or 'NOT'",))

                while self.current_token.type == lexer.TT_COMMA:
                    res.register_advancement()
                    self.advance()

                    args_nodes.append(res.register(self.expr()))
                    if res.error:
                        return res

                if self.current_token.type != lexer.TT_RPAREN:
                    return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ',' or ')'",))
                res.register_advancement()
                self.advance()
            return res.success(CallNode(base, args_nodes))
        return res.success(base)

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

        node = res.register(self.binary_operation(self.arith_expr, (lexer.TT_EE,
                            lexer.TT_GT, lexer.TT_GTE, lexer.TT_LT, lexer.TT_LTE, lexer.TT_NE)))

        if res.error:
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, identifier, '+', '-', '(', '[', 'NOT'"))

        return res.success(node)

    def arith_expr(self):
        return self.binary_operation(self.term, (lexer.TT_PLUS, lexer.TT_MINUS))

    def expr(self):  # NANDITO YUNG ISSUE REGARDING SA FUNCTIONS
        res = ParseResult()
        flag_ident = False

        if self.current_token.type == lexer.TT_KEYWORD:
            if self.current_token.value == "output":
                # If this is output
                # Go to next token and check for <<
                res.register_advancement()
                self.advance()

                if self.current_token.type != lexer.TT_DLT:
                    res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '<<'"))

                # Go to next token and check for expression
                res.register_advancement()
                self.advance()
                expr = res.register(self.expr())

                return res.success(OutputNode(expr))
        
        elif self.current_token.type == lexer.TT_IDENTIFIER:
            if self.current_token.type != lexer.TT_IDENTIFIER:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ':'"))
            
            # Get Variable Name
            var_name = self.current_token

            # Go to next token
            res.register_advancement()
            self.advance()

            if self.current_token.type == lexer.TT_COLON:
                res.register_advancement()
                self.advance()
                
                if self.current_token.type == lexer.TT_EQ:
                    # This is only for :=
                    res.register_advancement()
                    self.advance()

                    # Process variable assignment
                    expr = res.register(self.expr())
                    if res.error:
                        return res
                    return res.success(VarAssignNode(var_name, expr))
                else:
                    # This is only for :

                    # Process variable declaration
                    if self.current_token.type == lexer.TT_INT or self.current_token.type == lexer.TT_FLOAT:
                        pass
                    raise Exception("No Impl")




        # START NUNG KAY MIGUEL
        elif self.current_token.type == lexer.TT_IDENTIFIER:
            if self.current_token.type != lexer.TT_IDENTIFIER:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected ':'"))

            var_name = self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != lexer.TT_COLON:
                res.register_reversal()
                self.reverse()

            res.register_advancement()
            self.advance()

            if self.current_token.type != lexer.TT_EQ:
                res.register_reversal()
                self.reverse()
                if self.current_token.type == lexer.TT_INT or self.current_token.type == lexer.TT_FLOAT:
                    pass
                elif self.current_token.type == lexer.TT_IDENTIFIER:
                    flag_ident = True
                    pass
                elif self.current_token.type != lexer.TT_EQ:
                    return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected '='"))

                if flag_ident == True:
                    node = res.register(self.binary_operation(
                        self.comp_expr, ((lexer.TT_KEYWORD, "AND"), (lexer.TT_KEYWORD, "OR"))))

                    if res.error:
                        return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, '+', '-', or '('"))

                    return res.success(node)

            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.binary_operation(self.comp_expr, ((lexer.TT_KEYWORD, "AND"), (lexer.TT_KEYWORD, "OR"))))

        if res.error:
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, "Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FUNC' or 'NOT'"))

        return res.success(node)

    def expr_test(self):  # NANDITO YUNG ISSUE REGARDING SA FUNCTIONS
        res = ParseResult()

        if self.current_token.matches(lexer.TT_KEYWORD, 'VAR'):
            res.register_advancement()
            self.advance()

            if self.current_token.type != lexer.TT_IDENTIFIER:
                return res.failure(lexer.InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected identifier"
                ))

            var_name = self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != lexer.TT_EQ:
                return res.failure(lexer.InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected '='"
                ))

            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.binary_operation(self.comp_expr, ((lexer.TT_KEYWORD, 'AND'), (lexer.TT_KEYWORD, 'OR'))))
        if res.error:
            return res.failure(lexer.InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
            ))

        return res.success(node)


    def func_def(self):
        res = ParseResult()

        if not self.current_token.matches(lexer.TT_KEYWORD, 'FUNC'):
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected 'FUNC'"))

        res.register_advancement()
        self.advance()

        if self.current_token.type == lexer.TT_IDENTIFIER:
            var_name_token = self.current_token
            res.register_advancement()
            self.advance()
            if self.current_token.type != lexer.TT_LPAREN:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected '('"))
        else:
            var_name_token = None
            if self.current_token.type != lexer.TT_LPAREN:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected identifier or '('"))

        res.register_advancement()
        self.advance()
        args = []

        if self.current_token.type == lexer.TT_IDENTIFIER:
            args.append(self.current_token)
            res.register_advancement()
            self.advance()

            while self.current_token.type == lexer.TT_COMMA:
                res.register_advancement()
                self.advance()

                if self.current_token.type != lexer.TT_IDENTIFIER:
                    return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected identifier"))

                args.append(self.current_token)
                res.register_advancement()
                self.advance()

            if self.current_token.type != lexer.TT_RPAREN:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected ',' or ')'"))
        else:
            if self.current_token.type != lexer.TT_RPAREN:
                return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end, f"Expected identifier or ')'"))

        res.register_advancement()
        self.advance()
        
        if self.current_token.type == lexer.TT_ARROW:
            res.register_advancement()
            self.advance()

            body = res.register(self.expr())
            if res.error: return res

            return res.success(FuncDefNode(var_name_token, args, body, False))
        
        if self.current_token.type != lexer.TT_NEWLINE:
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,f"Expected '->' or NEWLINE"))

        res.register_advancement()
        self.advance()

        body = res.register(self.statements())
        if res.error: return res

        if not self.current_token.matches(lexer.TT_KEYWORD, 'END'):
            return res.failure(lexer.InvalidSyntaxError(self.current_token.pos_start, self.current_token.pos_end,f"Expected 'END'"))

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
