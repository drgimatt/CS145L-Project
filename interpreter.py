import lexer
import parsefile as parse

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

class Number:
    def __init__(self, value):
        self.value = value
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
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        
    def subtracted_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None        
        
    def multiplied_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None

    def divided_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, lexer.RTError(other.pos_start, other.pos_end, 'Division by zero', self.context)
            
            return Number(self.value / other.value).set_context(self.context), None   

    def powered_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None   
        
    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(-int(self.value == other.value)).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(-int(self.value != other.value)).set_context(self.context), None

    def get_comparison_lt(self, other):
	    if isinstance(other, Number):
              return Number(-int(self.value < other.value)).set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(-int(self.value > other.value)).set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(-int(self.value <= other.value)).set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(-int(self.value >= other.value)).set_context(self.context), None

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(-int(bool(self.value) and bool(other.value))).set_context(self.context), None

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(-int(bool(self.value) or bool(other.value))).set_context(self.context), None

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
    def __init__(self):
        self.symbols = {}
        self.parent = None

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

    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_token.value
        value = context.symbol_table.get(var_name)

        if value is None:
            return res.failure(lexer.RTError(node.pos_start, node.pos_end, f"'{var_name}' is not defined", context))

        value = value.copy().set_pos(node.pos_start, node.pos_end)
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

        if node.operator_token.type == lexer.TT_PLUS:
            result, error= left.added_to(right)
        elif node.operator_token.type == lexer.TT_MINUS:
            result, error = left.subtracted_by(right)
        elif node.operator_token.type == lexer.TT_MUL:
            result, error = left.multiplied_by(right)
        elif node.operator_token.type == lexer.TT_DIV:
            result, error = left.divided_by(right)
        elif node.operator_token.type == lexer.TT_POWER:
            result, error = left.powered_by(right)
        elif node.operator_token.type == lexer.TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.operator_token.type == lexer.TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.operator_token.type == lexer.TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.operator_token.type == lexer.TT_GT:
            result, error = left.get_comparison_gt(right)                       
        elif node.operator_token.type == lexer.TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.operator_token.type == lexer.TT_GTE:
            result, error = left.get_comparison_gte(right)     
        elif node.operator_token.matches(lexer.TT_KEYWORD, 'AND'):
            result, error = left.anded_by(right)
        elif node.operator_token.matches(lexer.TT_KEYWORD, 'OR'):
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

        if node.operator_token.type == lexer.TT_MINUS:
            number, error = number.multiplied_by(Number(-1))
        elif node.operator_token.matches(lexer.TT_KEYWORD, 'NOT'):
            number, error = number.notted()
        
        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))
        
    def visit_IfNode(self, node, context):
        res = RTResult()

        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error: return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)
        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)
        
        return res.success(None)


##########################
# TEST
##########################

global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number(0))
global_symbol_table.set("TRUE", Number(-1))
global_symbol_table.set("FALSE", Number(0))

def run(file_name, text):
    #Generate tokens
    input = lexer.Lexer(file_name, text)
    tokens, error = lexer.Lexer.make_tokens(input)
    if error: return None, error

    parser = parse.Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table # type: ignore
    result = interpreter.visit(ast.node, context)

    return result.value, result.error    