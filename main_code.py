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
    print("Lex error: Entero mayor que 32767 o menor que -32767 no son contemplados")
    t.lexer.skip(1)

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.value = str(t.value)               
    t.type = reserved.get(t.value,'ID') #Busca si es una palabra reservada antes de generar token tipo ID, si no lo es genera token tipo ID
    return t
    
def parser(t):
    s = "<{},{}>".format(t.type, t.value)
    return s 

#T_ERROR
def t_error(t):
    t.lexer.skip(1)

#T_IGNORE

t_ignore_TAB = r'\t' #TABULADOR
t_ignore_Line = r'\n'    
t_ignore_RT = r'\r'  #RETORNO DE CARRO
t_ignore_COMENTARIO = r'/\*.*?\*/'    #COMENTARIOS(/*comentario*/)



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
vars_globales = []

### ESQUEMA --> {'id': nombre,'tipo': tipo,'params':[],'vars':[]}
funciones = []

### BUFFERS ###
buffer_params = []
buffer_returns = []
buffer_vars_locales = []

buffer_params_llamada = []

########################
# Funciones auxiliares #
########################

def get_id_function(string):
    flag = 0
    i = 0
    res = ''
    while flag==0:
        if string[i] == '(':
            flag = 1
        else:
            res = res+string[i]
        i = i+1
    return res

def get_global_or_local_var(x):
    res = None
    for n in vars_globales:
        if x == n[0]:
            res = n
    for m in buffer_vars_locales:
        if x == m[0]:
            res = m
    for k in buffer_params:
        if x == k[0]:
            res = k
    return res

def var_is_func(cadena):
    cont = 0
    for n in cadena:
        if n == ')' or n == '(':
            cont = cont + 1
    if cont == 2:
        return True
    else:
        return False

def func_exist(x):
    for f in funciones:
        if f['id'] == x:
            return f
    return None

def duplicated_param(x,params):
    for n in params:
        if n[0] == x:
            return True
    return False

def var_already_exist(x):
    for n in buffer_vars_locales:
        if n[0] == x:
            return True
    for n in vars_globales:
        if n[0] == x:
            return True
    for n in buffer_params:
        if n[0] == x:
            return True
    return False

def var_is_global_int(x):
    for n in vars_globales:
        if n[0] == x and n[1] == 'int':
            return True
        
    return False

def var_is_global_bool(x):
    for n in vars_globales:
        if n[0] == x and n[1] == 'bool':
            return True    
    return False

def var_is_global_string(x):
    for n in vars_globales:
        if n[0] == x and n[1] == 'string':
            return True

    return False

def var_is_local_int(x):
    for n in buffer_vars_locales:
        if n[0] == x and n[1] == 'int':
            return True
    for n in buffer_params:
        if n[0] == x and n[1] == 'int':
            return True
    return False

def var_is_local_bool(x):
    for n in buffer_vars_locales:
        if n[0] == x and n[1] == 'bool':
            return True
    for n in buffer_params:
        if n[0] == x and n[1] == 'bool':
            return True    
    return False

def var_is_local_str(x):
    for n in buffer_vars_locales:
        if n[0] == x and n[1] == 'string':
            return True
    for n in buffer_params:
        if n[0] == x and n[1] == 'string':
            return True    
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
    l = len(buffer_params)
    while l>0:
        del buffer_params[l-1]
        l = l-1
    l = len(buffer_returns)
    while l>0:
        del buffer_returns[l-1]
        l=l-1
    l = len(buffer_vars_locales)
    while l>0:
        del buffer_vars_locales[l-1]
        l=l-1

def delete_buffer_llamada():
    l = len(buffer_params_llamada)
    while l> 0:
        del buffer_params_llamada[l-1]
        l=l-1

def get_tipos_params(params):
    res = []
    for p in params:
        res.append(p[1])
    return res
#########
# START #
#########
parse_text = ""
linea = 0

def p_b_p(p):
    'P : B P'
    global parse_text 
    parse_text = parse_text + "2 "
    
def p_f_p(p):
    'P : F P'
    global parse_text 
    parse_text = parse_text + "3 "

def p_eof(p):
    'P : empty'
    global parse_text 
    parse_text = parse_text + "4 "

############################################################################################################
######################################### Function #########################################################
############################################################################################################

def p_f_function(p):
    'F : FUNCTION H ID LPAREN A RPAREN LLLAVE W RLLAVE'
    global parse_text 
    parse_text = parse_text + "5 "

    loc_vars = []
    check = []

    encontrado = 0
    duplicados = 0
    returns_ok = 0

    duplicado = ''
    
    #Comprobamos si el id de la funcion ya existe
    for fun in funciones:
        if fun['id'] == p[3]:
            encontrado = 1
    
    #Si no existe
    if encontrado == 0:
        #Volcamos datos de variables locales
        for n in buffer_vars_locales:
            loc_vars.append([n[0],n[1]])

        #Check de que no haya params duplicados
        for n in buffer_params:
            if duplicated_param(n[0],check):
                duplicado = n[0]
                duplicados = 1
            else:
                check.append([n[0],n[1]])

        #Si no hay duplicados
        #Comprobamos returns
        if p[2] != None:
            if len(buffer_returns) == 0:
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
                funciones.append({'id':p[3],'tipo': p[2],'params':check,'vars':loc_vars})
            else:
                print('Semantic error FUNCTION at line {}. Returns dont match with the specified type'.format(lineas()))
        #Si hay duplicados
        else:
            print('Semantic error FUNCTION at line {}. ID {} is already used as param'.format(lineas(),duplicado))

    #Si existe
    else:
        print('Semantic error FUNCTION at line {}. ID {} already exists'.format(lineas(),p[3]))
    
    #print(buffer_params)
    #print(buffer_vars_locales)
    #Limpiamos buffer
    delete_buffer()

    #print(funciones)   

##### TIPO A DEVOLVER #####
def p_h_tipo(p):
    'H : T'
    p[0] = p[1]
    global parse_text 
    parse_text = parse_text + "6 "

def p_h_empty(p):
    'H : empty'
    global parse_text 
    parse_text = parse_text + "7 "
    p[0] = p[1]
##########################

##### PARAMETROS #####
def p_a_params(p):
    'A : T ID K'
    global parse_text 
    parse_text = parse_text + "8 "
    buffer_params.append([p[2],p[1]])

def p_a_empty(p):
    'A : empty'
    global parse_text 
    parse_text = parse_text + "9 "

def p_k_params(p):
    'K : COMA T ID K'
    global parse_text 
    parse_text = parse_text + "10 "
    buffer_params.append([p[3],p[2]])
    

def p_k_empty(p):
    'K : empty'
    global parse_text 
    parse_text = parse_text + "11 "
    
#####################

def p_w_d(p):
    'W : D W'
    global parse_text 
    parse_text = parse_text + "12 "

def p_w_empty(p):
    'W : empty'
    global parse_text 
    parse_text = parse_text + "13 "

def p_define_var_func(p):
    'D : VAR T ID PYC'
    global parse_text 
    parse_text = parse_text + "14 "
    if not var_already_exist(p[3]):
        buffer_vars_locales.append([p[3],p[2]])
    else:
        print('Semantic error at line {}. The variable already exist'.format(lineas()))

def p_d_do_while(p):
    'D : DO LLLAVE W RLLAVE WHILE LPAREN E RPAREN PYC'
    global parse_text 
    parse_text = parse_text + "15 "
    if type(p[7]) is int:
        print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))
    elif type(p[7]) is str:
        if var_is_cadena(p[7]):
            print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))
        elif var_is_func(p[7]):
            id_func = get_id_function(p[7])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'bool':
                    print('Semantic error IF at line {}. Function doestn return a bool'.format(lineas()))
            else:
                print('Semantic error DO/WHILE at line {}. Function doesnt exist'.format(lineas()))
        elif not (var_is_global_bool(p[7]) or var_is_local_bool(p[7])):
            print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))
    elif p[3] is None:
        print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))

def p_d_if(p):
    'D : IF LPAREN E RPAREN LLLAVE W RLLAVE'
    global parse_text 
    parse_text = parse_text + "16 "
    if type(p[3]) is int:
        print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
    elif type(p[3]) is str:
        if var_is_cadena(p[3]):
            print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
        elif var_is_func(p[3]):
            id_func = get_id_function(p[3])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'bool':
                    print('Semantic error IF at line {}. Function doestn return a bool'.format(lineas()))
            else:
                print('Semantic error IF. Function doesnt exist')
        elif not(var_is_global_bool(p[3]) or var_is_local_bool(p[3])):
            print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
    elif p[3] is None:
        print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))

def p_d_s(p):
    'D : S'
    global parse_text 
    parse_text = parse_text + "17 "

##########
# Return #
##########

def p_return(p):
    'W : RETURN X PYC'
    global parse_text 
    parse_text = parse_text + "18 "
    if type(p[2]) is str:
        if var_is_cadena(p[2]): 
            buffer_returns.append('string')
        elif not var_is_cadena(p[2]):    
            if var_already_exist(p[2]):
                if var_is_global_int(p[2]):
                    buffer_returns.append('int')
                elif var_is_global_bool(p[2]):
                    buffer_returns.append('bool')
                elif var_is_global_string(p[2]):
                    buffer_returns.append('string')
                elif var_is_local_int(p[2]):
                    buffer_returns.append('int')
                elif var_is_local_bool(p[2]):
                    buffer_returns.append('bool')
                elif var_is_local_str(p[2]):
                    buffer_returns.append('string')
            else:
                print('Semantic error RETURN at line {}. Variable {} is not define'.format(lineas(),p[2]))
    elif type(p[2]) is int:
        buffer_returns.append('int')
    elif type(p[2]) is bool:
        buffer_returns.append('bool')    
        

def p_return_empty(p):
    'X : empty'
    global parse_text 
    parse_text = parse_text + "19 "
    buffer_returns.append('null')
def p_return_e(p):
    'X : E'
    global parse_text 
    parse_text = parse_text + "20 "
    p[0] = p[1]


############################################################################################################
############################################################################################################
############################################################################################################


######
# IF #
######
def p_if(p):
    'B : IF LPAREN E RPAREN LLLAVE C RLLAVE'
    global parse_text 
    parse_text = parse_text + "21 "
    if type(p[3]) is int:
        print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
    elif type(p[3]) is str:
        if var_is_cadena(p[3]):
            print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
        elif var_is_func(p[3]):
            id_func = get_id_function(p[3])
            fun = func_exist(id_func)
            if fun!=None:
                if fun['tipo'] != 'bool':
                    print('Semantic error IF at line {}. Function doesnt return a bool'.format(lineas()))
            else:
                print('Semantic error IF at line {}. Function doesnt exist'.format(lineas()))
        elif not var_is_global_bool(p[3]):
            print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
    elif p[3] is None:
        print('Semantic error IF at line {}. {} is not a bool expression'.format(lineas(),p[3]))
#####################
# Llamada a funcion #
#####################

def p_s_function(p):
    'S : ID LPAREN L RPAREN PYC'
    global parse_text 
    parse_text = parse_text + "22 "
    print(buffer_params_llamada)
    fun = func_exist(p[1])
    if fun == None:
        print('Semantic error ID() at line {}. Function {} does not exist'.format(lineas(),p[1]))
    else:
        if buffer_params_llamada != get_tipos_params(fun['params']):
            print('Semantic error ID() at line {}. Params are not right'.format(lineas()))
    delete_buffer_llamada()

def p_l_eq(p):
    'L : E Q'
    global parse_text 
    parse_text = parse_text + "23 "
    if type(p[1]) is int:
        buffer_params_llamada.append('int')
    elif type(p[1]) is bool:
        buffer_params_llamada.append('bool')
    elif type(p[1]) is str:
        if var_is_cadena(p[1]):
            buffer_params_llamada.append('string')
        elif var_already_exist(p[1]):
            var = get_global_or_local_var(p[1])
            buffer_params_llamada.append(var[1])
        elif var_is_func(p[1]):
            id_func = get_id_function(p[1])
            fun = func_exist(id_func)
            if fun != None:
                tipo = fun['tipo']
                buffer_params_llamada.append(tipo)
            else:
                print('Semantic error CALLING FUNCTION at line {}. The function {} you are trying to call is not define'.format(lineas,p[1]))
        else:
            print('Semantic error CALLING FUNCTION at line {}.'.format(lineas()))  
    else:
        print('Semantic error CALLING FUNCTION at line {}.'.format(lineas()))
def p_l_empty(p):
    'L : empty'
    global parse_text 
    parse_text = parse_text + "24 "

def p_q_eq(p):
    'Q : COMA E Q'
    global parse_text 
    parse_text = parse_text + "25 "
    if type(p[2]) is int:
        buffer_params_llamada.append('int')
    elif type(p[2]) is bool:
        buffer_params_llamada.append('bool')
    elif type(p[2]) is str:
        if var_is_cadena(p[2]):
            buffer_params_llamada.append('string')
        elif var_already_exist(p[2]):
            var = get_global_or_local_var(p[2])
            buffer_params_llamada.append(var[1])
        elif var_is_func(p[2]):
            id_func = get_id_function(p[2])
            fun = func_exist(id_func)
            if fun != None:
                tipo = fun['tipo']
                buffer_params_llamada.append(tipo)
            else:
                print('Semantic error CALLING FUNCTION at line {}.'.format(lineas()))  
        
        else:
            print('Semantic error CALLING FUNCTION at line {}.'.format(lineas()))  
            
def p_q_empty(p):
    'Q : empty'
    global parse_text 
    parse_text = parse_text + "26 "


############
# Do While #
############

def p_do_while(p):
    'B : DO LLLAVE C RLLAVE WHILE LPAREN E RPAREN PYC'
    global parse_text 
    parse_text = parse_text + "27 "
    if type(p[7]) is int:
        print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))
    elif type(p[7]) is str:
        if var_is_cadena(p[7]):
            print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))
        elif var_is_func(p[7]):
            id_func = get_id_function(p[7])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'bool':
                    print('Semantic error DO/WHILE at line {}. Function doesnt return a bool'.format(lineas()))
            else:
                print('Semantic error DO/WHILE at line {}. Function doesnt exist'.format(lineas()))
        elif not var_is_global_bool(p[7]):
            print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))
    elif p[7] is None:
        print('Semantic error DO/WHILE at line {}. {} is not a bool expression'.format(lineas(),p[7]))

def p_c_b_c(p):
    'C : B C'
    global parse_text 
    parse_text = parse_text + "28 "

def p_c_empty(p):
    'C : empty'
    global parse_text 
    parse_text = parse_text + "29 "

######################################
# Creación y asisgnación de Variable #
######################################

def p_define_var(p):
    'B : VAR T ID PYC'
    global parse_text 
    parse_text = parse_text + "30 "
    if not var_already_exist(p[3]):
        vars_globales.append([p[3],p[2]])
    else:
        print('Semnatic error defining new variable at line {}. The variable already exist'.format(lineas()))
    

def p_b_s(p):
    'B : S'
    global parse_text 
    parse_text = parse_text + "31 "
    p[0]=p[1]


def p_asig(p):
    'S : ID ASIG E PYC'
    global parse_text 
    parse_text = parse_text + "32 "
    if var_is_global_int(p[1]) or var_is_local_int(p[1]): 
        if type(p[3]) is bool:
            print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
        elif type(p[3]) is str:
            if var_is_cadena(p[3]):
               print('Semantic error ASIG at line {}. Types dont match'.format(lineas())) 
            elif var_is_func(p[3]):
                id_func = get_id_function(p[3])
                fun = func_exist(id_func)
                if fun != None:
                    if fun['tipo'] != 'int':
                        print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
                else:
                    print('Semantic error ASIG at line {}. The function doesnt exist'.format(lineas()))
            elif not(var_is_global_int(p[3]) or var_is_local_int(p[3])):
                print('Semantic error ASIG at line {}. The var doesnt exist'.format(lineas()))

    elif var_is_global_bool(p[1]) or var_is_local_bool(p[1]): 
        if type(p[3]) is int:
            print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
        elif type(p[3]) is str:
            if var_is_cadena(p[3]):
                print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
            elif var_is_func(p[3]):
                id_func = get_id_function(p[3])
                fun = func_exist(id_func)
                if fun != None:
                    if fun['tipo'] != 'bool':
                        print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
                else:
                    print('Semantic error ASIG at line {}. The function doesnt exist'.format(lineas()))
            elif not(var_is_global_bool(p[3]) or var_is_local_bool(p[3])):
                print('Semantic error ASIG at line {}. The var doesnt exist'.format(lineas()))
    
    elif var_is_global_string(p[1]) or var_is_local_str(p[1]):
        if type(p[3]) is str:
            if var_is_func(p[3]):
                id_func = get_id_function(p[3])
                fun = func_exist(id_func)
                if fun != None:
                    if fun['tipo'] != 'string':
                        print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
                else:
                    print('Semantic error ASIG at line {}. The function doesnt exist'.format(lineas()))
            elif not var_is_cadena(p[3]):
                if not (var_is_global_string(p[3]) or var_is_local_str(p[3])):
                    print('Semantic error ASIG at line {}. The var doesnt exist'.format(lineas()))

        elif type(p[3]) is bool:
            print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
        elif type(p[3]) is int:
            print('Semantic error ASIG at line {}. Types dont match'.format(lineas()))
     
    else:
        print('Semantic error ASIG at line {}. Variable {} not define'.format(lineas(),p[1]))


#########
# Tipos #
#########

def p_tipo_str(p):
    'T : STRING'
    global parse_text 
    parse_text = parse_text + "33 "
    p[0] = p[1]

def p_tipo_bool(p):
    'T : BOOLEAN'
    global parse_text 
    parse_text = parse_text + "34 "
    p[0] = p[1]

def p_tipo_int(p):
    'T : INT'
    global parse_text 
    parse_text = parse_text + "35 "
    p[0] = p[1]
#def p_tipo_empty(p):
#    'T : empty'

#########
# Print #
#########

def p_print(p):
    'S : PRINT LPAREN E RPAREN PYC'
    global parse_text 
    parse_text = parse_text + "36 "
    if type(p[3]) is str:
        if var_is_func(p[3]):
            id_func = get_id_function(p[3])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] == None:
                    print('Semantic error PRINT at line {}. Function has no return value'.format(lineas()))
            else:
                print('Semantic error PRINT at line {}. Function doesnt exist'.format(lineas())) 
        elif not var_is_cadena(p[3]):    
            if not var_already_exist(p[3]):
                print('Semantic error PRINT at line {}. Variable {} is not define'.format(lineas(),p[3]))  
    elif p[3] is None:
        print('Semantic error PRINT at line {}. Nothing was found to print'.format(lineas()))

#########
# Prompt #
#########

def p_prompt(p):
    'S : PROMPT LPAREN ID RPAREN PYC'  
    global parse_text 
    parse_text = parse_text + "37 "
    if not var_already_exist(p[3]):
            print('Semantic error PRINT at line {}. Variable {} is not define'.format(lineas(),p[3]))   

##############
# Operadores #
##############

def p_id_mm(p):
    'S : MMENOS ID PYC'
    global parse_text 
    parse_text = parse_text + "38 "
    if not var_already_exist(p[2]):
        print('Semantic error MMINUS at line {}. Variable {} is not define'.format(lineas(),p[2]))
    elif not (var_is_global_int(p[2]) or var_is_local_int(p[2])):
        print('Semantic error MMINUS at line {}. Variable {} is not an Integer'.format(lineas(),p[2]))

def p_e_notr(p):
    'E : NEG R'
    global parse_text 
    parse_text = parse_text + "39 "
    if type(p[2]) is bool:
        p[0] = not p[2]
    elif type(p[2]) is str and not var_is_cadena(p[2]):
        if not var_is_global_bool(p[2]):
            print('Semantic error NOT at line {}. Variable {} is not a boolean.'.format(lineas(),p[2]))   
    else:
        print('Semantic error NOT at line {}. Variable {} is not a boolean'.format(lineas(),p[2]))

def p_e_r(p):
    'E : R'
    global parse_text 
    parse_text = parse_text + "40 "
    p[0]=p[1]

######## RETOCAR #########
def p_erre_expression_minusthan(p):
    'R : U MENORQUE U'
    global parse_text 
    parse_text = parse_text + "41 "
    if type(p[1]) is int and type(p[3]) is int:
        p[0] = p[1] < p[3]
    elif type([1]) is int and type(p[3]) is str:
        if var_is_func(p[3]):
            id_func = get_id_function(p[3])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'int':
                    print('Semantic error LESSTHAN at line {}. Function {} does not return an Integer'.format(lineas(),p[3]))
            else:
                print('Semantic error LESSTHAN at line {}. Function {} is not define.'.format(lineas(),p[3]))
        elif var_is_global_int(p[3]) or var_is_local_int(p[3]):
            p[0] = False
        else:
            print('Semantic error LESSTHAN at line {}. {} is not an Integer'.format(lineas(),p[3]))
    elif type(p[1]) is str and type(p[3]) is int:
        if var_is_func(p[1]):
            id_func = get_id_function(p[1])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'int':
                    print('Semantic error LESSTHAN at line {}. Function {} does not return an Integer'.format(lineas(),p[1]))
            else:
                print('Semantic error LESSTHAN at line {}. Function {} is not define'.format(lineas(),p[1]))
        elif var_is_global_int(p[1]) or var_is_local_int(p[1]):
            p[0] = False
        else:
            print('Semantic error LESSTHAN at line {}. {} is not an Integer'.format(lineas(), p[1]))
    elif type(p[1]) is str and type(p[3]) is str:
        if (var_is_global_int(p[1]) or var_is_local_int(p[1])) and (var_is_global_int(p[3]) or var_is_local_int(p[3])):
            p[0] = False
        else:
            print('Semantic error LESSTHAN at line {}.'.format(lineas()))
    else:
        print('Semantic error LESSTHAN at line {}.'.format(lineas()))

def p_erre_expression(p):
    'R : U'
    global parse_text 
    parse_text = parse_text + "42 "
    p[0] = p[1]

def p_expression_plus(p):
    'U : V SUM U'
    global parse_text 
    parse_text = parse_text + "43 "

    #Si los dos son enteros
    if type(p[1]) is int and type(p[3]) is int:
        p[0] = p[1] + p[3]

    #Si el primero es entero y el segundo id
    elif type(p[1]) is int and type(p[3]) is str:
        if var_is_func(p[3]):
            id_func = get_id_function(p[3])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'int':
                    print('Semantic error SUM at line {}. Function {} does not return an Integer'.format(lineas(),p[3]))
            else:
                print('Semantic error SUM at line {}. Function {} is not define'.format(lineas(),p[3]))
        elif var_already_exist(p[3]):
            if var_is_global_int(p[3]) or var_is_local_int(p[3]):
                p[0] = 0
            else:
                print('Semantic error SUM at line {}. Variable {} is not an integer'.format(lineas(),p[3]))
        else:
            print('Semantic error SUM at line {}. Variable {} not define'.format(lineas(),p[3]))

    #Si el primero es variable y el segundo id
    elif type(p[1]) is str and type(p[3]) is int:
        if var_is_func(p[1]):
            id_func = get_id_function(p[1])
            fun = func_exist(id_func)
            if fun != None:
                if fun['tipo'] != 'int':
                    print('Semantic error SUM at line {}. Function {} does not return an Integer'.format(lineas(),p[1]))
                else:
                    p[0] = 0
            else:
                print('Semantic error SUM at line {}. Function {} is not define.'.format(lineas(),p[1]))
        elif var_already_exist(p[1]):
            if var_is_global_int(p[1]) or var_is_local_int(p[1]):
                p[0] = 0
            else:
                print('Semantic error SUM at line {}. Variable {} is not an integer'.format(lineas(),p[1]))
        else:
            print('Semantic error SUM at line {}. Variable {} is not an integer'.format(lineas(),p[1]))
    #Si los dos son ids
    elif type(p[1]) is str and type(p[3]) is str:
        if var_already_exist(p[1]):
            if var_already_exist(p[3]):
                if var_is_global_int(p[1]) or var_is_local_int(p[1]):
                    if var_is_global_int(p[3]) or var_is_local_int(p[3]):
                        p[0] = 0
                    else:    
                        print('Semantic error SUM at line {}. Variable {} is not an integer'.format(lineas(),p[3]))
                else:
                    print('Semantic error SUM at line {}. Variable {} is not an integer'.format(lineas(),p[1]))
            else:
                print('Semantic error SUM at line {}. Variable {} not define'.format(lineas(),p[3]))
        else:
            print('Sematnic error SUM at line {}. Variable {} not define'.format(lineas(),p[1]))
    else:
        print('Semantic error SUM at line {}. Variables {} and {} are not integers'.format(lineas(),p[1],p[3]))              

def p_expression_term(p):
    'U : V'
    global parse_text 
    parse_text = parse_text + "44 "
    p[0] = p[1]

def p_term_number(p):
    'V : ENTERO'
    global parse_text 
    parse_text = parse_text + "45 "
    p[0] = p[1]
def p_term_id(p):
    'V : ID'
    global parse_text 
    parse_text = parse_text + "46 "
    p[0] = p[1]

def p_term_string(p):
    'V : CADENA'
    global parse_text 
    parse_text = parse_text + "47 "
    p[0] = p[1]

def p_paren(p):
    'V : LPAREN E RPAREN'
    global parse_text 
    parse_text = parse_text + "48 "
    p[0]= (p[2])


   
def p_v_func(p):
    'V : ID LPAREN L RPAREN'
    global parse_text 
    parse_text = parse_text + "49 "
    fun = func_exist(p[1])
    if fun == None:
        print('Semantic error ID() at line {}. Function {} does not exist'.format(lineas(),p[1]))
    else:
        if buffer_params_llamada != get_tipos_params(fun['params']):
            print('Semantic error ID() at line {}. Params are not right'.format(lineas()))
    delete_buffer_llamada()

    if fun !=None:
        if fun['tipo'] == 'int':
            p[0] = 0
        elif fun['tipo'] == 'bool':
            p[0] = False
        elif fun['tipo'] == 'string':
            p[0] = '\'\''
        else:
            p[0] = None

    #p[0] = p[1]+'('+ ')'  
        

###############
#### Empty ####
###############

def p_empty(p):
    'empty :'
    global parse_text 
    parse_text = parse_text + "50 "
    pass

###############
#### Error ####
###############

def p_error(p):
    if p:
        print("Syntax error in {} at line {}".format(p.value,lineas()))
    else:
        print("Syntax error at EOF")


###############
### Lectura ###
###############
def lineas():
    contador = 0
    f = open("code.txt","r")
    for line in f:
        contador = contador + 1 
    return contador

codeRead = open("code.txt","r")
fl =codeRead.read()

########################
### Generamos Tokens ###
########################

f = open("tokens.txt","w+")
lexer.input(fl)

# Tokenize
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    f.write(parser(tok)+"\n")



############
### Yacc ###
############

yacc.yacc()

yacc.parse(fl)



"""
while True:
    try:
        s = raw_input('input>')
    except EOFError:
        break
    if not s:
        continue
    yacc.parse(s)
 """   


#################
# Fichero parse #
#################

f = open("./VASt/Parse.txt","w+")
f.write('A '+parse_text+'1')