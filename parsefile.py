from lexer import *
import functools 

#combinator portion
class Result:
    def __init__(self, value, position):
        self.value = value
        self.position = position

    def __repr__(self):
        return 'Result(%s, %d)' % (self.value, self.position)
    
class Parser:
    def __call__(self, tokens, position):
        return None  # subclasses will override this

    def __add__(self, other):
        return Concat(self, other)

    def __mul__ (self, other):
        return Exp(self, other)

    def __or__ (self, other):
        return Alternate(self, other)

    def __xor__(self, function):
        return Process(self, function)  

class Reserved(Parser): #parse reserved words and operators
    def __init__(self, value, tag): #constructor
       self.value = value
       self.tag = tag

    def __call__(self, tokens, position): # actual parsing prcoess
        if position < len(tokens) and \
            tokens[position][0] == self.value and \
            tokens[position][1] is self.tag:
                #print("nandito kaya? sa reserved")
                #print(tokens[position][0])
                #print(tokens[position][1])
                return Result(tokens[position][0], position + 1) #returns result object and new position - reserved word has been parsed successfully
        else:
            return None #returns none - no value-tag pair or reserved word has been match in the token list 

class Tag(Parser): #matches token with particular tag
    def __init__(self, tag):#constructor
        self.tag = tag

    def __call__(self, tokens, position):#  check if the token at the current position has specified tag
        if position < len(tokens) and tokens[position][1] is self.tag:
            return Result(tokens[position][0], position + 1) #returns result object and new position
        else:
            return None 
        
class Concat(Parser): #concatenate the results of two parsers
    def __init__(self, left, right): #constructor
        self.left = left
        self.right = right

    def __call__(self, tokens, position): #actual concatination
        left_result = self.left(tokens, position) #parse the left
        if left_result:
            right_result = self.right(tokens, left_result.position) #parse the right
            if right_result:
                combined_value = (left_result.value, right_result.value) #combine the two results if successful
                return Result(combined_value, right_result.position) # returns result object - concatenated value and new position
            
class Alternate(Parser): #similar to concat function except only returns the successful result of left or right only
    def __init__(self, left, right):
        self.left = left
        self.right = right            

    def __call__(self, tokens, position):
        left_result = self.left(tokens, position) #parse the left
        if left_result:
            return left_result
        else: #if not successful, parse the right
            right_result = self.right(tokens, position)
            return right_result
        
class Opt(Parser):
    def __init__(self, parser):
        self.parser = parser

    def __call__(self, tokens, position):
        result = self.parser(tokens, position)
        if result:
            return result
        else:
            return Result(None, position)
                
class Rep(Parser): #repeatedly applies a specified parser to the input tokens and collects the results into a list
    def __init__(self, parser):
        self.parser = parser

    def __call__(self, tokens, position):
        results = []
        result = self.parser(tokens, position)
        while result: #repeat as long as there is still valid result
            results.append(result.value)
            position = result.position
            result = self.parser(tokens, position)
        return Result(results, position) # returns result object - list of results and new position
    
class Process(Parser):# allows manipulation of result values / process the output of a parser using a specified function
    def __init__(self, parser, function):
        self.parser = parser
        self.function = function

    def __call__(self, tokens, position):
        result = self.parser(tokens, position)
        if result:
            result.value = self.function(result.value)
            return result
        
class Lazy(Parser):#only constructs parser when it is needed
    #it takes a zero-argument function which returns a parser. It will not call the function to get the parser until it's applied
    def __init__(self, parser_func):
        self.parser = None
        self.parser_func = parser_func

    def __call__(self, tokens, position):
        if not self.parser:
            self.parser = self.parser_func()
        return self.parser(tokens, position)
    
class Phrase(Parser):# ensures that a specified parser successfully matches and consumes the entire list of input tokens
    def __int__(self, parser):
        self.parser = parser

    def __call__(self, tokens, position): #responsible for applying the specified parser to the input tokens and ensuring that it matches the entire phrase.
        result = self.parser(tokens, position)
        if result and result.position == len(tokens):
            return result
        else:
            return None
        
class Exp(Parser): # handles expressions
    def __init__(self, parser, separator):
        self.parser = parser
        self.separator = separator # binary operator / separator

    def __call__(self, tokens, position):
        result = self.parser(tokens, position) #apply parser to tokens and position

        def process_next(parsed): # used to process next part of expression
            (sepfunc, right) = parsed
            return sepfunc(result.value, right)
        next_parser = self.separator + self.parser ^ process_next # combines the separator (:=), parser (x), right result (5)

        next_result = result
        while next_result: # applies combination until nothing is left
            next_result = next_parser(tokens, result.position)
            if next_result:
                result = next_result
        return result # fully parsed expression with binary operators.

#abstract syntax tree construction
#arithmetic expressions

class Equality:
    def __eq__(self, other):
        return isinstance(other, self.__class__) and \
               self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not self.__eq__(other)

#arithmetic expression are in three forms
#literal integer constants -> 43
#variables -> x
#binary operations -> x + 43

class Aexp(Equality): #Arithmetic Expression
    pass

class IntAexp(Aexp):
    def __init__(self, int):
        self.int = int
    
    def __repr__(self):
        return 'IntAexp(%d)' % self.int
        
    def eval(self, env):
        return self.int

class DoubleAexp(Aexp):
    def __init__(self, double):
        self.double = double
    
    def __repr__(self):
        return 'DoubleAexp(%.2f)' % self.double
        
    def eval(self, env):
        return self.double

class VarAexp(Aexp):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'VarAexp(%s)' % self.name
    
    def eval(self, env):
        if self.name in env:
            return env[self.name]
        else:
            return 0

class BinopAexp(Aexp):
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

    def __repr__(self):
        return 'BinopAexp(%s, %s, %s)' % (self.operator, self.left, self.right)
    
    def eval(self, env):
        left_value = self.left.eval(env)
        right_value = self.right.eval(env)
        if self.operator == '+':
            value = left_value + right_value
        elif self.operator == '-':
            value = left_value - right_value
        elif self.operator == '*':
            value = left_value * right_value
        elif self.operator == '/':
            value = left_value / right_value                        
        else:
            raise RuntimeError('unknown operator: ' + self.op)
        return value            

class Bexp(Equality): #Boolean Expression
    pass

class RelopBexp(Bexp): #Relational Expressions
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

    def __repr__(self):
        return 'RelopBexp(%s, %s, %s)' % (self.operator, self.left, self.right)

    def eval(self, env):
        left_value = self.left.eval(env)
        right_value = self.right.eval(env)
        if self.operator == '<':
            value = left_value < right_value
        elif self.operator == '<=':
            value = left_value <= right_value
        elif self.operator == '>':
            value = left_value > right_value
        elif self.operator == '>=':
            value = left_value >= right_value
        elif self.operator == '==':
            value = left_value == right_value
        elif self.operator == '!=':
            value = left_value != right_value                                                                          
        else:
            raise RuntimeError('unknown operator: ' + self.op)
        return value

class AndBexp(Bexp): #And Expression
    def __init__ (self, left, right):
        self.left = left
        self.right = right 

    def __repr__(self):
        return 'AndBexp(%s, %s)' % ( self.left, self.right)
    
    def eval (self, env):
        left_value = self.left.eval(env)
        right_value = self.right.eval(env)
        return left_value and right_value

class OrBexp(Bexp): #Or Expression
    def __init__ (self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return 'OrBexp(%s, %s)' % ( self.left, self.right)
    
    def eval (self, env):
        left_value = self.left.eval(env)
        right_value = self.right.eval(env)
        return left_value or right_value
    
class NotBexp(Bexp):
    def __init__(self, expression):
        self.expression = expression

    def __repr__(self):
        return 'NotBexp(%s)' % self.expression
    
    def eval(self, env):
        value = self.expression.eval(env)
        return not value

class Statement(Equality):
    pass

class AssignStatement(Statement):
    def __init__ (self, name, aexp):
        self.name = name
        self.aexp = aexp

    def __repr__(self):
        return 'AssignStatement(%s, %s)' % (self.name, self.aexp)
    
    def eval(self, env):
        value = self.aexp.eval(env)
        env[self.name] = value

class VariableStatement(Statement):
    def __init__ (self, name, type):
        self.name = name
        self.type = type

    def __repr__(self):
        return 'VariableStatement(%s, %s)' % (self.name, self.type)
    
    def eval(self, env):
        if self.type == "integer":
            env[self.name] = 0  # Assign an integer value, e.g., 0
        elif self.type == "double":
            env[self.name] = 0.0  # Assign a double value, e.g., 0.0

class OutputStatement(Statement):
    def __init__ (self, content):
        self.content = content

    def __repr__(self):
        return 'OutputStatement(%s)' % self.content
    
    def eval(self, env):
        return self.content

class CompoundStatement(Statement):
    def __init__(self, first, second):
        self.first = first
        self.second = second

    def __repr__(self):
        return 'CompoundStatement(%s, %s)' % (self.first, self.second)
    
    def eval(self, env):
        self.first.eval(env)
        self.second.eval(env)

class IfStatement(Statement):
    def __init__(self, condition, true_statement, false_statement):
        self.condition = condition
        self.true_statement = true_statement
        self.false_statement = false_statement

    def __repr__(self):
        return 'IfStatement(%s, %s, %s)' % (self.condition, self.true_statement, self.false_statement)

    def eval(self, env):
        condition_value = self.condition.eval(env)
        if condition_value:
            self.true_statement.eval(env)
        else:
            if self.false_statement:
                self.false_statement.eval(env)

class WhileStatement(Statement):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def __repr__(self):
        return 'WhileStatement(%s, %s)' % (self.condition, self.body)
    
    def eval(self, env):
        condition_value = self.condition.eval(env)
        while condition_value:
            self.body.eval(env)
            condition_value = self.condition.eval(env)

#parser proper

id = Tag(ID)
str = Tag(STRING)
#str = Tag(STRING) ^ (lambda s: s[1:-1])
int_asn = Tag(INT_DECL)
dbl_asn = Tag(DOUBLE_DECL)
integer = Tag(INT) ^ (lambda i: int(i))
double = Tag(DOUBLE) ^ (lambda d: float(d))
syntax_error_flag = False
# Top level parser

class SyntaxError(Exception):
    def __init__(self, message):
        self.message = message
        super().__init__(self.message)

def imp_parse_test(tokens):
    syntax_error_flag = False
    try:
        ast = parser()(tokens, 0)
    except SyntaxError as e:
        syntax_error_flag = True
        sys.stderr.write(f"Syntax Error: {e}\n")

    if syntax_error_flag:
        sys.stderr.write("Syntax errors were found in the file.\n")
    else:
        sys.stdout.write("No syntax errors found in the file.\n")
    return ast

def parser():
    return Phrase(stmt_list()) 

def keyword(kw):
    if kw == "output<<" or kw == "output <<":
        return Reserved(kw, OUTPUT)
    elif kw == "integer":
        return Reserved(kw, INT_DECL)
    elif kw == "double":
        return Reserved(kw, DOUBLE_DECL)
    else:
        return Reserved(kw, RESERVED)

def stmt_list():
    separator = keyword(';') ^ (lambda x: lambda l, r: CompoundStatement(l, r))
    return Exp(stmt(), separator)

def stmt():
    return output_stmt() | \
           assign_stmt() | \
           type_stmt() | \
           if_stmt() | \
           while_stmt()
def assign_stmt():
    def process(parsed):
        ((name, _), exp) = parsed
        return AssignStatement(name, exp)
    return (id + keyword(':=') + (aexp() | bexp()) ^ process)

def output_stmt():
    def process(parsed):
        ((_, value)) = parsed
        return OutputStatement(value)
    return (keyword('output<<') | keyword('output <<')) + (aexp() | str) ^ process

def type_stmt():
    def process(parsed):
        ((name, _), exp) = parsed
        return VariableStatement(name, exp)
    return  id + keyword(':') + (int_asn | dbl_asn)  ^ process 
def if_stmt():
    #print("stsa")
    def process(parsed):
        (((((_, condition), _), true_statement), false_parsed), _) = parsed
        #print(parsed)
        if false_parsed:
            (_, false_statement) = false_parsed
        else:
            false_statement = None
            #print("gst")
        return IfStatement(condition, true_statement, false_statement)
    #print("asfaegtaf")
    return keyword('if') + keyword('(') + bexp() + keyword(')')  + \
           Lazy(stmt_list) + \
           Opt(keyword('else') + Lazy(stmt_list)) + \
           keyword(';') ^ process

def while_stmt():
    def process(parsed):
        ((((_, condition), _), body), _) = parsed
        return WhileStatement(condition, body)
    return keyword('while') + bexp() + \
           keyword('do') + Lazy(stmt_list) + \
           keyword(';') ^ process

def bexp():

    if not precedence(bexp_term(), bexp_precedence_levels, process_logic):
        raise SyntaxError("Syntax error: bexp")
        
    else:
        return precedence(bexp_term(),
                      bexp_precedence_levels,
                      process_logic)

def bexp_term():
    return bexp_not()   | \
           bexp_relop() | \
           bexp_group()

def bexp_not():
    return keyword('not') + Lazy(bexp_term) ^ (lambda parsed: NotBexp(parsed[1]))

def bexp_relop():
    relops = ['<', '<=', '>', '>=', '==', '!=']
    return (aexp() + any_operator_in_list(relops) + aexp() ^ process_relop)

def bexp_group():
    return keyword('(') + Lazy(bexp) + keyword(')') ^ process_group

def aexp():
    if not precedence(aexp_term(), aexp_precedence_levels, process_binop):

        raise SyntaxError("Syntax error: aexp")

    return precedence(aexp_term(),
                      aexp_precedence_levels,
                      process_binop)

def aexp_term():
    return aexp_value() | aexp_group()

def aexp_group():
    return keyword('(') + Lazy(aexp) + keyword(')') ^ process_group

def aexp_value():
    return (integer ^ (lambda i: IntAexp(i))) | \
           (double ^ (lambda d: DoubleAexp(d))) | \
           (id  ^ (lambda v: VarAexp(v)))

def precedence(value_parser, precedence_levels, combine):
    def op_parser(precedence_level):
        return any_operator_in_list(precedence_level) ^ combine
    parser = value_parser * op_parser(precedence_levels[0])
    for precedence_level in precedence_levels[1:]:
        parser = parser * op_parser(precedence_level)
    return parser

def process_relop(parsed):
    ((left, op), right) = parsed
    return RelopBexp(op, left, right)

def process_logic(op):
    if op == 'and':
        return lambda l, r: AndBexp(l, r)
    elif op == 'or':
        return lambda l, r: OrBexp(l, r)
    else:
        raise RuntimeError('unknown logic operator: ' + op)

def process_group(parsed):
    ((_, p), _) = parsed
    return p

def process_binop(op):
    return lambda l, r: BinopAexp(op, l, r) 

def any_operator_in_list(ops):
    op_parsers = [keyword(op) for op in ops]
    parser = functools.reduce(lambda l, r: l | r, op_parsers)
    return parser

# Operator keywords and precedence levels
aexp_precedence_levels = [
    ['*', '/'],
    ['+', '-'],
]

bexp_precedence_levels = [
    ['and'],
    ['or'],
]

