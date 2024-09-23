import ply.lex as lex
import sys

# List of token names
tokens = [
    'T_AND', 'T_ASSIGN', 'T_BOOLTYPE', 'T_BREAK', 'T_CHARCONSTANT', 'T_COMMA', 
    'T_COMMENT', 'T_CONTINUE', 'T_DIV', 'T_DOT', 'T_ELSE', 'T_EQ', 'T_EXTERN', 
    'T_FALSE', 'T_FOR', 'T_FUNC', 'T_GEQ', 'T_GT', 'T_IDENTIFIER', 'T_IF', 'T_IntConstant', 
    'T_Int', 'T_LCB', 'T_LEFTSHIFT', 'T_LessEqual', 'T_LPAREN', 'T_LSB', 'T_LT', 
    'T_MINUS', 'T_MOD', 'T_MULT', 'T_NEQ', 'T_NOT', 'T_NULL', 'T_OR', 'T_PACKAGE', 
    'T_PLUS', 'T_RCB', 'T_RETURN', 'T_RIGHTSHIFT', 'T_RPAREN', 'T_RSB', 'T_SEMICOLON', 
    'T_STRINGCONSTANT', 'T_STRINGTYPE', 'T_TRUE', 'T_VAR', 'T_VOID', 'T_WHILE', 'T_WHITESPACE', 
    'T_Print'
]

# Reserved keywords in Decaf
# to make sure that reserved words in the decaf language not calssified as identifiers
reserved = {
    'int': 'T_Int',
    'bool': 'T_BOOLTYPE',
    'void': 'T_VOID',
    'string': 'T_STRINGTYPE',
    'if': 'T_IF',
    'else': 'T_ELSE',
    'for': 'T_FOR',
    'return': 'T_RETURN',
    'true': 'T_TRUE',
    'false': 'T_FALSE',
    'null': 'T_NULL',
    'while': 'T_WHILE',
    'break': 'T_BREAK',
    'continue': 'T_CONTINUE',
    'extern': 'T_EXTERN',
    'package': 'T_PACKAGE',
    'var': 'T_VAR',
    'Print': 'T_Print'
}

# Regular expressions for tokens
t_T_AND = r'&&'
t_T_ASSIGN = r'='
t_T_COMMA = r','
t_T_CONTINUE = r'continue'
t_T_DIV = r'/'
t_T_DOT = r'\.'
t_T_EQ = r'=='
t_T_GEQ = r'>='
t_T_GT = r'>'
t_T_LCB = r'\{'
t_T_LEFTSHIFT = r'<<'
t_T_LessEqual = r'<='
t_T_LPAREN = r'\('
t_T_LSB = r'\['
t_T_LT = r'<'
t_T_MINUS = r'-'
t_T_MOD = r'%'
t_T_MULT = r'\*'
t_T_NEQ = r'!='
t_T_NOT = r'!'
t_T_OR = r'\|\|'
t_T_PLUS = r'\+'
t_T_RCB = r'\}'
t_T_RIGHTSHIFT = r'>>'
t_T_RPAREN = r'\)'
t_T_RSB = r'\]'
t_T_SEMICOLON = r';'

def t_T_CHARCONSTANT(t):
    r"'(\\.|[^\\'])'"
    t.value = f"(char_lit={t.value})"
    return t

def t_T_STRINGCONSTANT(t):
    r'"([^\\"]|\\.)*"'  # Handles string literals with escape sequences
    # t.value = t.value
    return t

def t_T_IntConstant(t):
    r'\d+'
    t.value = int(t.value)
    return t

# identifiers and reserved tokesn
# def t_T_IDENTIFIER(t):
#     r'[a-zA-Z_][a-zA-Z_0-9]*'
#     t.type = reserved.get(t.value, 'T_IDENTIFIER')  # Check if identifier is a keyword
#     return t
def t_T_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'T_IDENTIFIER')  # Check if identifier is a keyword
    if len(t.value) == 1:  # Check for single-character identifiers
        t.type = 'T_IDENTIFIER'  # Ensure single characters are treated as T_IDENTIFIER
    return t


# whitespace
# but do not print out any white spaces in scanner
def t_T_WHITESPACE(t):
    r'[ \t]+'
    pass  # Ignore whitespace

# Newline token to handle line numbers correctly
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# comments
# but to not print to scanner
def t_T_COMMENT(t):
    r'//.*|/\*[\s\S]*?\*/'
    pass  # Ignore comments

# error handling
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# function to get the column of thee current token
def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return (token.lexpos - line_start) + 1

# decaf scanner
def scan_decaf_code(code):
    lexer.input(code)
    while True:
        tok = lexer.token()
        if not tok: # end of file
            break
        col_start = find_column(code, tok)

        if isinstance(tok.value, str):  # For string and character values
            col_end = col_start + len(tok.value) - 1
        else:  # For integer constants, just set col_end to col_start
            col_end = col_start

        # Format output
        if tok.type == 'T_STRINGCONSTANT':
            print(f'{tok.value}     line {tok.lineno} Cols {col_start} - {col_end}  is  {tok.type} (value= {tok.value})')
        elif tok.type == 'T_IntConstant':
            print(f"{tok.value}     line {tok.lineno} Cols {col_start} - {col_end}  is  {tok.type} (value= {tok.value})")
        elif len(tok.value) == 1:  # For single-character tokens
            if (tok.type == 'T_IDENTIFIER'):
                print(f"{tok.value}     line {tok.lineno} Cols {col_start} - {col_end}  is  '{tok.type}'")
            else:
              print(f"{tok.value}     line {tok.lineno} Cols {col_start} - {col_end}  is  '{tok.value}'")
        else:
            print(f"{tok.value}     line {tok.lineno} Cols {col_start} - {col_end}  is  {tok.type}")



# read file decaf file then send to scanner
def read_decaf_file(file_path):
    with open(file_path, 'r') as f:
        code = f.read()
    scan_decaf_code(code)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python decaf_scanner.py <filename.decaf>")
        sys.exit(1)

    decaf_file = sys.argv[1]
    try:
        read_decaf_file(decaf_file)
    except FileNotFoundError:
        print(f"File '{decaf_file}' not found.")
