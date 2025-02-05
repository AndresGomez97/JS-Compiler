import ply.lex as lex


#Lista de tokens



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

##### ESTA MAL ##########
def t_CADENA(t):
    r'\'[a-zA-Z0-9_ ]*\''
    t.value = str(t.value)
    return t
########################


def t_ENTERO(t):    
    r'[0-9][0-9]*'
    t.value = int(t.value)
    if t.value <= 32767 and t.value >= -32767:
        return t
    print("ERROR 40: Entero mayor que 32767 o menor que -32767 no son contemplados")
    exit(1)



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
#t_ignore_Line = r'\n'#SALTO DE LINEA     
t_ignore_COMENTARIO = r'/\*.*?\*/'    #COMENTARIOS(/*comentario*/)

#T_ERROR
def t_error(t):
    print("Error en el lexico.")
    print("Caracter no reconocido ({}) en la linea: {}".format(t.value[0], t.lexer.lineno))
    t.lexer.skip(1)
    #exit(1)

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
    ################
    #  EJECUCION   #
    ################
lexer = lex.lex()
lexer.input('var int a; a = 23; %')
f = open("tokens.txt","w+")

# Tokenize
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    f.write(parser(tok)+"\n")
