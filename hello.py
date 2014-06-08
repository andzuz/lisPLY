__author__ = 'Praca'

import ply.lex as lex
import ply.yacc as yacc

tokens = [
    'QUOTE', 'LPAREN', 'RPAREN', 'NIL', 'TRUE', 'FALSE', 'ID', 'TEXT',
    'NUMBER', 'ADD', 'SUB', 'MULT', 'POW', 'DIV', 'GTHAN', 'LTHAN',
    'GEQUAL', 'LEQUAL', 'EQUAL'
]

reserved = {
    'not': 'NOT', 'eq': 'EQ', 'length': 'LENGTH', 'cons': 'CONS',
    'car': 'CAR', 'cdr': 'CDR', 'list': 'LIST', 'null?': 'NULLS',
    'if': 'IF', 'define': 'DEFINE', 'print': 'PRINT'
}

tokens.extend(reserved.values())

t_QUOTE = r'\''
t_TRUE = r'\#t'
t_FALSE = r'\#f'
t_TEXT = r'"[^\"\n]*"'

def t_NUMBER(t):
    r'\d+'
    t.value = float(t.value)
    return t

t_ignore = ' \t'
t_ADD = r'\+'
t_SUB = r'-'
t_MULT = r'\*'
t_POW = r'\^'
t_DIV = r'/'
t_GTHAN = r'>'
t_LTHAN = r'<'
t_GEQUAL = r'>='
t_LEQUAL = r'<='
t_EQUAL = r'='


def t_ID(t):
    r'[A-Za-z][A-Za-z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_NIL = r'nil'

lex.lex()

lex.input("(+ (- 2 4) x 2)")
while True:
    tok = lex.token()
    if not tok: break
    print(tok)

vars = {}

def p_assignment(p):
    '''program : ID EQUAL list
        | list'''
    if len(p) == 4:
        vars[p[1]] = p[3]
    else:
        pass

# operacje arytmetyczne

def p_list_arith_op(p):
    '''list : arith_operation
        | print_operation
        | list_operation
        | car_operation
        | cdr_operation
        | define_variable_operation
        | comparison_operation
        | cons_operation
        | conditional_operation'''
    p[0] = p[1]

def p_arith_operation(p):
    'arith_operation : LPAREN arith_operator arith_items RPAREN'
    p[0] = eval_list([p[2]] + p[3])

def p_arith_operator(p):
    '''arith_operator : ADD
        | SUB
        | MULT
        | DIV
        | POW'''
    p[0] = p[1]

def p_arith_items(p):
    'arith_items : arith_item arith_items'
    p[0] = [p[1]] + p[2]

def p_arith_items_empty(p):
    'arith_items : empty'
    p[0] = []

def p_empty(p):
    'empty :'
    pass

def p_arith_item(p):
    '''arith_item : NUMBER
        | arith_operation'''
    p[0] = p[1]

def eval_list(parsed_list):
    operation = parsed_list[0]

    if len(parsed_list) > 1:
        result = parsed_list[1]
    else:
        return operation

    for elem in parsed_list[2:]:
        if type(elem) is list:
            toEval = eval_list(elem)
        else:
            toEval = elem

        if operation == '+':
            result += toEval
        elif operation == '-':
            result -= toEval
        elif operation == '/':
            result /= toEval
        elif operation == '*':
            result *= toEval

    return result

# operacja list
def p_list_op(p):
    'list_operation : LPAREN LIST list_items RPAREN'
    p[0] = p[3]

def p_list_items(p):
    '''list_items : list_item list_items'''
    p[0] = [p[1]] + p[2]

def p_list_items_empty(p):
    'list_items : empty'
    p[0] = []

def p_list_item(p):
    '''list_item : TEXT
        | NUMBER
        | quoted_list
        | TRUE
        | FALSE'''
    p[0] = p[1]

# quoted list
def p_quoted_list(p):
    'quoted_list : QUOTE LPAREN list_items RPAREN'
    p[0] = p[3]

# car
def p_car_op(p):
    'car_operation : LPAREN CAR quoted_list RPAREN'
    if len(p[3]) > 0:
        p[0] = p[3][0]
    else:
        p[0] = []

# cdr
def p_cdr_op(p):
    'cdr_operation : LPAREN CDR quoted_list RPAREN'
    if len(p[3]) > 1:
        p[0] = p[3][1:len(p[3])]
    else:
        p[0] = []

# define variable
def p_define_var_op(p):
    'define_variable_operation : LPAREN DEFINE ID variable_value RPAREN'
    vars[p[3]] = p[4]

def p_variable_value(p):
    '''variable_value : TEXT
        | NUMBER
        | TRUE
        | FALSE'''
    p[0] = p[1]

def p_comp_operation(p):
    'comparison_operation : LPAREN comp_operator comp_item comp_item RPAREN'
    comp_op = p[2]

    if comp_op == '=':
        p[0] = p[3] == p[4]
    elif comp_op == '>':
        p[0] = p[3] > p[4]
    elif comp_op == '<':
        p[0] = p[3] < p[4]
    elif comp_op == '>=':
        p[0] = p[3] >= p[4]
    elif comp_op == '<=':
        p[0] = p[3] <= p[4]
    elif comp_op == 'eq':
        p[0] = id(p[3]) == id(p[4])

def p_comp_item(p):
    '''comp_item : TEXT
        | NUMBER
        | TRUE
        | FALSE
        | length_operation'''
    p[0] = p[1]

def p_comp_item_id(p):
    '''comp_item : ID'''
    p[0] = vars[p[1]]

def p_comp_operator(p):
    '''comp_operator : EQUAL
        | EQ
        | GTHAN
        | LTHAN
        | GEQUAL
        | LEQUAL'''
    p[0] = p[1]

# if
def p_cond_operation(p):
    'conditional_operation : LPAREN IF comparison_operation conditional_item conditional_item RPAREN'
    if p[3] == True:
        p[0] = p[4]
    else:
        p[0] = p[5]

def p_cond_item(p):
    '''conditional_item : arith_operation
    | conditional_operation
	| NUMBER
	| TEXT
	| TRUE
	| FALSE
	| print_operation
	| empty
	| ID'''
    p[0] = p[1]

# length
def p_length_operation(p):
    '''length_operation : LPAREN LENGTH TEXT RPAREN
        | LPAREN LENGTH quoted_list RPAREN'''
    if isinstance(p[3], str):
        p[0] = len(p[3]) - 2
    else:
        p[0] = len(p[3])

# print
def p_print_operation(p):
    'print_operation : LPAREN PRINT TEXT RPAREN'
    print(p[3])

# cons
def p_cons_operation(p):
    'cons_operation : LPAREN CONS cons_items RPAREN'
    p[0] = p[3]

def p_cons_items(p):
    '''cons_items : cons_item cons_items
	    | empty'''
    if len(p) > 2:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_cons_item(p):
    '''cons_item : TEXT
	| NUMBER
	| TRUE
	| FALSE
	| cons_operation'''
    p[0] = p[1]

yacc.yacc()
# yacc.parse('(print "xdd")')
# yacc.parse("(define y 5)")
# yacc.parse('x = (if (= y (length "krowa")) (if (= 2 2) (+ 1 1) (+ 3 3)) (+ 2 2))')
yacc.parse('x = (cons 1 2 (cons 2 3))')
print(vars['x'])