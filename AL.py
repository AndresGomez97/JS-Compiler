import ply.lex as lex


#Lista de tokens

tokens = [
    'CADENA',
    'ENTERO',
    'ID',
    'ASIG',
    'SUM',
    'RES',
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
    'do' : 'DO'
    'while' : 'WHILE'
}

#Añadimos palabras reservadas a la lista de tokens

tokens = tokens + list(reserved.values())



#Regular expression rules(SIMPLE)

t_ASIG = r'\='
t_SUM = r'\+'
#t_RES = r'\-'
t_NEG = r'\!'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PYC = r'\;'
t_PUNTO = r'\.'
t_LLLAVE = r'\}'
t_RLLAVE = r'\{'
t_LCORCH = r'\]'
t_RCORCH = r'\['




#Regular expression rules(NOT SIMPLE)

def t_CADENA(t):
    r'[\'][a-zA-Z]*[\']'
    t.value = str(t.value)
    return t



def t_ENTERO(t):    
    r'[1-9][0-9]*'
    t.value = int(t.value)
    if t.value <= 32767 and t.value >= -32767:
        return t
    print("Entero mayor que 32767 o menor que -32767 no pueden ser contemplados")
    t.lexer.skip(1)



def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.value = str(t.value)
    for res in reserved:
        if res.keyword() != t.value:
            return t

    print(t.value+"es una palabra reservada")
    t.lexer.skip(1)    
        

#T_IGNORE

t_ignore = ' \t' #TABULADOR
t_ignore = '\r'  #RETORNO DE CARRO
t_ignore = ''    #COMENTARIOS

#T_ERROR

def t_error(t):
    t.lexer.skip(1)


    
    ################
    #  EJECUCIÓN   #
    ################
lexer = lex.lex()
