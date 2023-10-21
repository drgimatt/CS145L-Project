import sys
import re

# Reserved Words
RESERVED = 'RESERVED'
INT = 'INT'
DOUBLE = 'DOUBLE'
ID = 'ID'
OUTPUT = 'OUTPUT'
INT_DECL = 'INT_DECL'
DOUBLE_DECL = 'DOUBLE_DECL'
STRING = 'STRING'

token_expressions = [
    (r'[ \n\t]+',                  None),
    (r'#[^\n]*',                   None),
    (r'\:=',                   RESERVED),
    (r'\:',                    RESERVED),
    (r'\(',                    RESERVED),
    (r'\)',                    RESERVED),
    (r':',                     RESERVED),
    (r';',                     RESERVED),
    (r'\+',                    RESERVED),
    (r'-',                     RESERVED),
    (r'\*',                    RESERVED),
    (r'/',                     RESERVED),
    (r'<=',                    RESERVED),
    (r'<',                     RESERVED),
    (r'>=',                    RESERVED),
    (r'>',                     RESERVED),
    (r'==',                    RESERVED),   
    (r'!=',                    RESERVED),
    (r'integer',               INT_DECL),
    (r'double',             DOUBLE_DECL),
    (r'output<<',                OUTPUT),
    (r'output <<',               OUTPUT),    
    (r'and',                   RESERVED),
    (r'or',                    RESERVED),
    (r'not',                   RESERVED),
    (r'if',                    RESERVED),
    (r'then',                  RESERVED),
    (r'else',                  RESERVED),
    (r'while',                 RESERVED),
    (r'do',                    RESERVED),
    (r'end',                   RESERVED),
    (r'[0-9]+\.[0-9]+',          DOUBLE),
    (r'[0-9]+',                     INT),
    (r'[A-Za-z][A-Za-z0-9_]*',       ID),
    (r'"([^"\\]*(\\.[^"\\]*)*)"',STRING)
]


def lex(characters, token_expressions):
    position = 0
    tokens = []
    while position < len(characters):
        match = None
        for token_expression in token_expressions:
            pattern, tag = token_expression
            regex = re.compile(pattern)
            match = regex.match(characters, position)
            if match:
                text = match.group(0)
                if tag:
                    token = (text, tag)
                    tokens.append(token)
                break
        if not match:
            sys.stderr.write('Illegal character: %s\\n' % characters[position])
            sys.stderr.write('\nERROR')
            sys.exit(1)
        else:
            position = match.end(0)
    return tokens

def imp_lex(characters):
        return lex(characters, token_expressions)