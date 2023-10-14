from typing import Any
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
		return lexer.RTError(
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
				return None, lexer.RTError(other.pos_start, other.pos_end, 'Division by zero', self.context)
			
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

class Function(Value):
	def __init__(self, name, body_node, arg_names):
		super().__init__()
		self.name = name or "<anonymous>"
		self.body_node = body_node
		self.arg_names = arg_names
	
	def execute(self, args):
		res = RTResult()
		interpreter = Interpreter()
		new_context = Context(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)

		if len(args) > len(self.arg_names): 
			return res.failure(lexer.RTError(self.pos_start, self.pos_end, f"{len(args) - len(self.arg_names)} too many args passed into {self.name}", self.context))

		if len(args) < len(self.arg_names):
			return res.failure(lexer.RTError(self.pos_start, self.pos_end, f"{len(args) - len(self.arg_names)} too few args passed into {self.name}", self.context))

		for i in range(len(args)):
			arg_name = self.arg_names[i]
			arg_value = args[i]
			arg_value.set_context(new_context)
			new_context.symbol_table.set(arg_name, arg_value)
		
		value = res.register(interpreter.visit(self.body_node, new_context))
		if res.error: return res
		return res.success(value)
	
	def copy(self):
		copy = Function(self.name, self.body_node, self.arg_names)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __repr__(self):
		return f"<function {self.name}>"

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

	def visit_FuncDefNode(self, node, context):
		res = RTResult()
		func_name = node.var_name_token.value if node.var_name_token else None
		body_node = node.body_node
		arg_names = [arg_name.value for arg_name in node.arg_name_tokens]
		func_value = Function(func_name, body_node, arg_names).set_context(context).set_pos(node.pos_start, node.pos_end)
		
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
		return res.success(return_value)


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
	context.symbol_table = global_symbol_table 
	result = interpreter.visit(ast.node, context)

	return result.value, result.error    