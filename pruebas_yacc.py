import sys 
import ply.lex as lex
import ply.yacc as yacc

sys.path.insert(0, "../..")

if sys.version_info[0] >= 3:
    raw_input = input

tokens = [
    'CADENA',
    'ENTERO',
    'ID',
    'ASIG',
    'SUM',
    'MENORQUE',
    'MMENOS',
    'COMA',
    'NEG',
    'LPAREN',
    'RPAREN',
    'PYC',
    'PUNTO',
    'LLLAVE',
    'RLLAVE',
    'LCORCH',
    'RCORCH'
]

#Diccionario de PRs
reserved = {
    'do' : 'DO',
    'while' : 'WHILE',
    'var' : 'VAR',
    'print' : 'PRINT',
    'prompt' : 'PROMPT',
    'function' : 'FUNCTION',
    'return' : 'RETURN',
    'int' : 'INT',
    'string' : 'STRING',
    'bool' : 'BOOLEAN',
    'if' : 'IF'
    
}

#Anadimos palabras reservadas a la lista de tokens
tokens = tokens + list(reserved.values())

#Regular expression rules(SIMPLE)

t_ASIG = r'\='
t_SUM = r'\+'
t_MENORQUE = r'\<'
t_MMENOS = r'\--'
t_NEG = r'\!'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PYC = r'\;'
t_COMA = r'\,'
t_PUNTO = r'\.'
t_LLLAVE = r'\{'
t_RLLAVE = r'\}'
t_LCORCH = r'\['
t_RCORCH = r'\]'

#Regular expression rules(NOT SIMPLE)

def t_CADENA(t):
    r'\'[a-zA-Z]*\''
    t.value = str(t.value)
    return t

def t_ENTERO(t):    
    r'[0-9][0-9]*'
    t.value = int(t.value)
    if t.value <= 32767 and t.value >= -32767:
        return t
    print("ERROR 40: Entero mayor que 32767 o menor que -32767 no son contemplados")
    t.lexer.skip(1)

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.value = str(t.value)               
    t.type = reserved.get(t.value,'ID') #Busca si es una palabra reservada antes de generar token tipo ID, si no lo es genera token tipo ID
    return t
    
def parser(t):
    s = "<{},{}>".format(t.type, t.value)
    return s 

#T_IGNORE

t_ignore_TAB = r'\t' #TABULADOR
t_ignore_RT = r'\r'  #RETORNO DE CARRO
t_ignore_COMENTARIO = r'/\*.*?\*/'    #COMENTARIOS(/*comentario*/)

#T_ERROR
def t_error(t):
    t.lexer.skip(1)
    
###############
# Build Lexer #
###############
lexer = lex.lex()

#############################
# Diccionarios de variables #
#############################
enteros = {}
cadenas = {}
booleanos = {}
funciones = {}

########################
# Funciones auxiliares #
########################

def var_already_exist(x):
    if x in booleanos.keys() or x in enteros.keys() or x in cadenas.keys():
        return True
    else:
        return False

#####################################
# Creación y definición de Variable #
#####################################

def p_define_var_bool(p):
    'B : VAR BOOLEAN ID PYC'
    if not var_already_exist(p[3]):
        booleanos.setdefault(str(p[3]))
    else:
        print('The variable already exist')

def p_define_var_int(p):
    'B : VAR INT ID PYC'
    if not var_already_exist(p[3]):
        enteros.setdefault(str(p[3]))
    else:
        print('The variable already exist')

def p_define_var_cads(p):
    'B : VAR STRING ID PYC'
    if not var_already_exist(p[3]):
        cadenas.setdefault(str(p[3]))
    else:
        print('The variable already exist')
def p_b_s(p):
    'B : S'
    p[0]=p[1]

def p_asig(p):
    'S : ID ASIG E PYC'
    if type(p[3]) is int: 
        if p[1] in enteros.keys():
            enteros[str(p[1])] = p[3]
        else:
            print('Integer variable {} not define'.format(p[1]))
    elif type(p[3]) is bool:
        if p[1] in booleanos.keys():
            booleanos[str(p[1])] = p[3]
        else:
            print('Boolean variable {} not define'.format(p[1]))

    elif type(p[3]) is str:
        if p[1] in cadenas.keys():
            cadenas[str(p[1])] = p[3]
        else:
            print('String variable {} not define'.format(p[1]))
    else:
        print('Grammar only accepts integers, booleans and strings')

##############
# Operadores #
##############

def p_e_notr(p):
    'E : NEG R'
    p[0] = not p[2]

def p_e_r(p):
    'E : R'
    p[0]=p[1]

def p_erre_expression_minusthan(p):
    'R : U MENORQUE U'
    p[0] = p[1] < p[3]

def p_erre_expression(p):
    'R : U'
    p[0] = p[1]

def p_expression_plus(p):
    'U : V SUM U'

    #Si los dos son enteros
    if isinstance(p[1],int) and isinstance(p[3],int):
        p[0] = p[1] + p[3]

    #Si el primero es entero y el segundo id
    elif isinstance(p[1],int) and isinstance(p[3],str):
        if var_already_exist(p[3]):
            if p[3] in enteros.keys():
                p[0] = p[1] + enteros.get(p[3])
            else:
                print('Variable {} is not an integer'.format(p[3]))
        else:
            print('Variable {} not define'.format(p[3]))

    #Si el primero es variable y el segundo id
    elif isinstance(p[1],str) and isinstance(p[3],int):
        if var_already_exist(p[1]):
            if p[1] in enteros.keys():
                p[0] = enteros.get(p[1]) + p[3]
            else:
                print('Variable {} is not an integer'.format(p[1]))
        else:
            print('Variable {} not define'.format(p[1]))

    #Si los dos son ids
    elif isinstance(p[1],str) and isinstance(p[3],str):
        if var_already_exist(p[1]):
            if var_already_exist(p[3]):
                if p[1] in enteros.keys():
                    if p[3] in enteros.keys():
                        p[0] = enteros.get(p[1]) + enteros.get(p[3])
                    else:    
                        print('Variable {} is not an integer'.format(p[3]))
                else:
                    print('Variable {} is not an integer'.format(p[1]))
            else:
                print('Variable {} not define'.format(p[3]))
        else:
            print('Variable {} not define'.format(p[1]))
    else:
        print('Variables {} and {} are not integers'.format(p[1],p[3]))              

def p_expression_term(p):
    'U : V'
    p[0] = p[1]

def p_term_number(p):
    'V : ENTERO'
    p[0] = p[1]
def p_term_id(p):
    'V : ID'
    p[0] = p[1]

def p_paren(p):
    'V : LPAREN E RPAREN'
    p[0]= (p[2])


###############
#### Error ####
###############

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


yacc.yacc()

while 1:
    try:
        s = raw_input('Input > ')
    except EOFError:
        break
    if not s:
        continue
    yacc.parse(s)
