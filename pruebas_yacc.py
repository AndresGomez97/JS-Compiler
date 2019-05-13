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




#################################################################################################################################
#################################### Sintactico y Semantico #####################################################################
#################################################################################################################################




#############################
# Diccionarios de variables #
#############################
enteros = {}
cadenas = {}
booleanos = {}

### ESQUEMA --> {'id': nombre,'tipo': tipo,'params':{},'vars':{}}
funciones = []

### BUFFERS ###
buffer_params = []
buffer_returns = []
buffer_vars_locales = []

nec_returns = 0

########################
# Funciones auxiliares #
########################

def var_already_exist(x):
    if x in booleanos.keys() or x in enteros.keys() or x in cadenas.keys():
        return True
    else:
        return False

def var_is_local_int(x):
    for n in buffer_vars_locales:
        if n[0] == x and n[1] == 'int':
            return True
        else:
            return False

def var_is_local_bool(x):
    for n in buffer_vars_locales:
        if n[0] == x and n[1] == 'bool':
            return True
        else:
            return False

def var_is_local_str(x):
    for n in buffer_vars_locales:
        if n[0] == x and n[1] == 'string':
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

def delete_buffer():
    global nec_returns
    nec_returns = 0
    l = len(buffer_params)
    while l>0:
        del buffer_params[l-1]
        l = l-1
    l = len(buffer_returns)
    while l>0:
        del buffer_returns[l-1]
        l=l-1

#########
# START #
#########

def p_b_p(p):
    'P : B'
    
def p_f_p(p):
    'P : F'

#def p_eof(p):
#    'P : empty'

############################################################################################################
######################################### Function #########################################################
############################################################################################################

def p_f_function(p):
    'F : FUNCTION H ID LPAREN A RPAREN LLLAVE W RLLAVE'

    global nec_returns
    
    nec_returns = nec_returns + 1

    encontrado = 0
    check = {}
    duplicados = 0
    returns_ok = 0
    duplicado = ''
    #Comprobamos si el id de la funcion ya existe
    for fun in funciones:
        if fun['id'] == p[3]:
            encontrado = 1
    
    #Si no existe
    if encontrado == 0:
        #Check de que no haya params duplicados
        for n in buffer_params:
            if n[0] not in check.keys():
                check.setdefault(n[0],n[1])
            else:
                duplicado = n[0]
                duplicados = 1
        
        #Si no hay duplicados
        #Comprobamos returns
        if p[2] != None:
            if len(buffer_returns) == 0:
                returns_ok = 1
            if len(buffer_returns) != nec_returns:
                returns_ok = 1
            for n in buffer_returns:
                if n != p[2]:
                    returns_ok = 1
        else:
            for n in buffer_returns:
                if n != 'null':
                    returns_ok = 1

        #Si las dos cosas están bien
        if duplicados == 0:
            if returns_ok == 0:
                funciones.append({'id':p[3],'tipo': p[2],'params':check,'vars':{}})
            else:
                print('Syntax error FUNCTION. Returns dont match with the specified type')
        #Si hay duplicados
        else:
            print('Syntax error FUNCTION. ID {} is already used as param'.format(duplicado))

    #Si existe
    else:
        print('Syntax error FUNCTION. ID {} already exists'.format(p[3]))
    
    #Limpiamos buffer
    delete_buffer()
    
    print(nec_returns)
    print(buffer_returns)
    print(buffer_params)
    print(funciones)          

##### TIPO A DEVOLVER #####
def p_h_tipo(p):
    'H : T'
    p[0] = p[1]

def p_h_empty(p):
    'H : empty'
    p[0] = p[1]
##########################

##### PARAMETROS #####
def p_a_params(p):
    'A : T ID K'
    buffer_params.append([p[2],p[1]])

def p_a_empty(p):
    'A : empty'

def p_k_params(p):
    'K : COMA T ID K'
    buffer_params.append([p[3],p[2]])


def p_k_empty(p):
    'K : empty'
    
#####################

def p_w_d(p):
    'W : D W'
    

def p_w_empty(p):
    'W : empty'

def p_define_var_func(p):
    'D : VAR T ID PYC'
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

def p_d_do_while(p):
    'D : DO LLLAVE W RLLAVE WHILE LPAREN E RPAREN PYC'
    if type(p[7]) is not bool:
        print('Syntax error DO/WHILE. {} is not a bool expression'.format(p[7]))

def p_d_if(p):
    'D : IF LPAREN E RPAREN LLLAVE W RLLAVE'
    if type(p[3]) is not bool:
        print('Syntax error DO/WHILE. {} is not a bool expression'.format(p[3]))

def p_d_s(p):
    'D : G'

##########
# Return #
##########

def p_return(p):
    'W : RETURN X PYC'
    if type(p[2]) is str:
        if var_is_cadena(p[2]): 
            buffer_returns.append('string')
        elif not var_is_cadena(p[2]):    
            if var_already_exist(p[2]):
                if p[2] in enteros.keys():
                    buffer_returns.append('int')
                elif p[2] in booleanos.keys():
                    buffer_returns.append('bool')
                elif p[2] in cadenas.keys():
                    buffer_returns.append('string')
            elif var_is_local_int(p[2]):
                buffer_returns.append('int')
            elif var_is_local_bool:
                buffer_returns.append('bool')
            elif var_is_local_str:
                buffer_returns.append('string')
            else:
                print('Syntax error RETURN. Variable {} is not define'.format(p[2]))
    elif type(p[2]) is int:
        buffer_returns.append('int')
    elif type(p[2]) is bool:
        buffer_returns.append('bool')    

def p_return_empty(p):
    'X : empty'
    buffer_returns.append('null')
def p_return_e(p):
    'X : E'
    p[0] = p[1]

################# RETOCAR #################
def p_asig_func(p):
    'G : ID ASIG E PYC'
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

def p_s_function_func(p):
    'G : ID LPAREN L RPAREN PYC'

def p_print_func(p):
    'G : PRINT LPAREN E RPAREN PYC'
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

def p_prompt_func(p):
    'G : PROMPT LPAREN E RPAREN PYC'
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

def p_id_mm_func(p):
    'G : MMENOS ID PYC'
    if p[2] in enteros.keys():
        enteros[p[2]] = enteros[p[2]] - 1
    else:
        print('Syntax error MMINUS')

############################################################################################################
############################################################################################################
############################################################################################################


######
# IF #
######
def p_if(p):
    'B : IF LPAREN E RPAREN LLLAVE C RLLAVE'
    global nec_returns
    nec_returns = nec_returns+1
    if type(p[3]) is not bool:
        print('Syntax error DO/WHILE. {} is not a bool expression'.format(p[3]))


#####################
# Llamada a funcion #
#####################

def p_s_function(p):
    'S : ID LPAREN L RPAREN PYC'

def p_l_eq(p):
    'L : E Q'

def p_l_empty(p):
    'L : empty'

def p_q_eq(p):
    'Q : COMA E Q'

def p_q_empty(p):
    'Q : empty'


############
# Do While #
############

def p_do_while(p):
    'B : DO LLLAVE C RLLAVE WHILE LPAREN E RPAREN PYC'
    global nec_returns
    nec_returns = nec_returns+1
    if type(p[7]) is not bool:
        print('Syntax error DO/WHILE. {} is not a bool expression'.format(p[7]))

def p_c_b_c(p):
    'C : B C'

def p_c_empty(p):
    'C : empty'

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


####### FALTA SEMANTICO ######   
def p_v_func(p):
    'V : ID LPAREN L RPAREN'


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
