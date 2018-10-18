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

    return t



def t_ENTERO(t):    

    return t



def t_ID(t):

    return t

#T_IGNORE

t_ignore = ' \t'

#T_ERROR

def t_error(t):
    t.lexer.skip(1)


    
    ################
    #  EJECUCIÓN   #
    ################
lexer = lex.lex()
