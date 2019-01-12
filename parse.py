import ply.yacc as yacc

 # Get the token map from the lexer.  This is required.
from AL import tokens

#COMIENZO
start = 'p1'


def p_prima_p(p):
    'p1 : p'
    p[0] = p[1]

#######NO ESTOY SEGURO SI SERÍA ASÍ#######
def p_bp(p):
    'p : b'
    p[0] = p[1] 

def p_fp(p):
    'p : f'
    p[0] = p[1]
#########################################







#############
#DECLARACIÓN#
#############
def p_var_t_id(p):
    'b : VAR t  ID PYC'
    if p[2] == 'INT':
        p[0] = int(p[3]) 
    elif p[2] == 'BOOLEAN':
        p[0] = bool(p[3])
    elif p[2] == 'STRING':
        p[0] = p[3]


        
##########
#DO_WHILE#
##########
def p_do_while(p):
    'b : DO LLLAVE c RLLAVE WHILE LPAREN e RPAREN PYC'
    while p[7]:
        p[3]

def p_b_s(p):
    'b : s'
    p[0] = p[1]

    
##############
# ASIGNACIÓN #
##############

def p_s_asig(p):
    's : ID ASIG e PYC'
    p[1] = p[3]
    p[0] = p[1]

def p_s_return(p):
    's : RETURN x PYC'
    
def p_s_callF(p):
    's : ID LPAREN l RPAREN PYC'
    p[0] = p[1](p[3])    

#########
# TIPOS #
#########

def p_t_INT(p):
    't : INT'   
    p[0] = p[1]
        
def p_t_BOOL(p):
    't : BOOLEAN'   
    p[0] = p[1]

def p_t_STR(p):
    't : STRING'   
    p[0] = p[1]


#############
# FUNCIONES #
#############

def p_function(p):
    'functiona : t ID LPAREN ArgListOpt RPAREN CompoundStmt'

def p_ArgListOpt_ArgList(p):
    'ArgListOpt: ArgList'

def p_ArgList_Arg(p):
    '''ArgList :  ArgList COMA Arg
               | Arg'''
                
def p_Arg_id(p):
    'Arg : t ID'

def p_CompoundStmt_StmtList(p):
    'CompoundStmt : LLLAVE StmtList RLLAVE'

def p_StmtList_Stmt(p):
    'StmtList : StmtList  Stmt'
    
def p_Stmt(p):
    'Stmt : s'
            
    

##############
# Operadores #
##############

def p_not_erre(p):
    'e : NEG r'
    p[0] = not p[2]

def p_eee_erre(p):
    'e : r'
    p[0] = p[1]

def p_erre_expression_minusthan(p):
    'r : r MENORQUE u'
    p[0] = p[1] < p[3]
    
        
def p_erre_expression(p):
    'r : u'
    p[0] = p[1]

def p_expression_plus(p):
    'u : v SUM u'
    p[0] = p[1] + p[3]

def p_expression_term(p):
    'u : v'
    p[0] = p[1]
    
def p_term_number(p):
    'v : ENTERO'
    p[0] = p[1]

#LAMBDA
def p_empty(p):
    'empty :'
    pass

def p_error(p):
     print("Syntax error in input!")


 # Build the parser
parser = yacc.yacc()
 
while True:
   try:
    s = input('parser>xsxs')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print(result)
