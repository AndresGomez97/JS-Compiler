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
    r'\'[a-zA-Z0-9_ ]*\''
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

def var_is_cadena(var):
    num = 0
    for n in var:
        if n == '\'':
            num = num +1
    if num==2:
        return True
    else:
        return False   

#########
# START #
#########



############
# Do While #
############

#def p_do_while(p):
#    'B : DO LCORCH C RCORCH WHILE LPAREN E RPAREN PYC'

#def p_c_b_c(p):
#    'C : B C'




######################################
# Creación y asisgnación de Variable #
######################################

def p_define_var(p):
    'B : VAR T ID PYC'
    if not var_already_exist(p[3]):
        if p[2] == 'bool':
            booleanos.setdefault(str(p[3]),False)
            print(booleanos)
        elif p[2] == 'int':
            enteros.setdefault(str(p[3]),0)
            print(enteros)
        elif p[2] == 'string':
            cadenas.setdefault(str(p[3]),'')
            print(cadenas)
    else:
        print('The variable already exist')

def p_b_s(p):
    'B : S'
    p[0]=p[1]


def p_asig(p):
    'S : ID ASIG E PYC'
    if p[1] in enteros.keys(): 
        if type(p[3]) is int:
            enteros[str(p[1])] = p[3]
        elif isinstance(p[3],str) and p[3] in enteros.keys():
            enteros[str(p[1])] = enteros[str(p[3])]
            print(enteros)
        else:
            print('Syntax error ASIG')

    elif p[1] in booleanos.keys():
        if isinstance(p[3],bool):
            booleanos[str(p[1])] = p[3]
            print(booleanos)
        elif isinstance(p[3],str) and p[3] in booleanos.keys():
            booleanos[str(p[1])] = booleanos[str(p[3])]
        else:
            print('Syntax error ASIG')

    elif p[1] in cadenas.keys():
        if isinstance(p[3],str) and var_is_cadena(p[3]) :
            cadenas[str(p[1])] = p[3]
        elif isinstance(p[3],str) and not var_is_cadena(p[3]):
            if p[3] in cadenas.keys():
                cadenas[str(p[1])] = cadenas[str(p[3])]
            else:
                print('Syntax error ASIG')
        else:
            print('Syntax error ASIG')
    else:
        print('Variable {} not define'.format(p[1]))

##########
# Return #
##########

def p_return(p):
    'S : RETURN E PYC'
    if type(p[2]) is str:
        if var_is_cadena(p[2]): 
            return p[2]
        elif not var_is_cadena(p[2]):    
            if var_already_exist(p[2]):
                if p[2] in enteros.keys():
                    return enteros[p[2]]
                elif p[2] in booleanos.keys():
                    return booleanos[p[2]]
                elif p[2] in cadenas.keys():
                    return cadenas[p[2]]
            else:
                print('Syntax error RETURN. Variable {} is not define'.format(p[2]))
    
#########
# Tipos #
#########

def p_tipo_str(p):
    'T : STRING'
    p[0] = p[1]

def p_tipo_bool(p):
    'T : BOOLEAN'
    p[0] = p[1]

def p_tipo_int(p):
    'T : INT'
    p[0] = p[1]
#def p_tipo_empty(p):
#    'T : empty'

#########
# Print #
#########
#### FALTA SEMANTICO #####
def p_print(p):
    'S : PRINT LPAREN E RPAREN PYC'
    if type(p[3]) is str:
        if var_is_cadena(p[3]): 
            print(p[3])
        elif not var_is_cadena(p[3]):    
            if var_already_exist(p[3]):
                if p[3] in enteros.keys():
                    print(enteros[p[3]])
                elif p[3] in booleanos.keys():
                    print(booleanos[p[3]])
                elif p[3] in cadenas.keys():
                    print(cadenas[p[3]])
            else:
                print('Syntax error PRINT. Variable {} is not define'.format(p[3]))   

    else:
        print(p[3]) 

#########
# Prompt #
#########
##### FALTA SEMANTICO ######
def p_prompt(p):
    'S : PROMPT LPAREN E RPAREN PYC'
    if type(p[3]) is str:
        if not var_is_cadena(p[3]):    
            if var_already_exist(p[3]):
                if p[3] in enteros.keys():
                    try:
                        enteros[p[3]] = int(input())
                    except ValueError:
                        print('Not acceptable value. {} is an integer variable'.format(p[3]))
                elif p[3] in booleanos.keys():
                    try:
                        booleanos[p[3]] = bool(input())
                    except ValueError:
                        print('Not acceptable value. {} is boolean variable'.format(p[3]))
                elif p[3] in cadenas.keys():
                    try:
                        cadenas[p[3]] = '\''+input()+'\''
                    except ValueError:
                        print('Not acceptable value. {} is a string variable'.format(p[3]))
            else:
                print('Syntax error PROMPT. Variable {} is not define'.format(p[3]))
        else:
            print('Syntax error PROMPT. {} is not a variable'.format(p[3])) 
    else:
        print('Syntax error PROMPT. {} is not a variable'.format(p[3]))   

##############
# Operadores #
##############

def p_id_mm(p):
    'S : MMENOS ID PYC'
    if p[2] in enteros.keys():
        enteros[p[2]] = enteros[p[2]] - 1
    else:
        print('Syntax error MMINUS')

def p_e_notr(p):
    'E : NEG R'
    if type(p[2]) is bool:
        p[0] = not p[2]
    elif type(p[2]) is str and not var_is_cadena(p[2]):
        if p[2] in booleanos.keys():
            p[0] = not booleanos[p[2]]
        else:
            print('Syntax error NOT')
    else:
        print('Syntax error NOT')
def p_e_r(p):
    'E : R'
    p[0]=p[1]

def p_erre_expression_minusthan(p):
    'R : U MENORQUE U'
    if type(p[1]) is int and type(p[3]) is int:
        p[0] = p[1] < p[3]
    elif type([1]) is int and type(p[3]) is str:
        if p[3] in enteros:
            p[0] = p[1] < enteros[p[3]]
        else:
            print('Syntax error LESSTHAN')
    elif type(p[1]) is str and type(p[3]) is int:
        if p[1] in enteros:
            p[0] = enteros[p[1]] < p[3]
        else:
            print('Syntax error LESSTHAN')
    elif type(p[1]) is str and type(p[3]) is str:
        if p[1] in enteros and p[3] in enteros:
            p[0] = enteros[p[1]] < enteros[p[3]]
        else:
            print('Syntax error LESSTHAN')
    else:
        print('Syntax error LESSTHAN')

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

def p_term_string(p):
    'V : CADENA'
    p[0] = p[1]

def p_paren(p):
    'V : LPAREN E RPAREN'
    p[0]= (p[2])


###############
#### Empty ####
###############

def p_empty(p):
     'empty :'
     pass

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
