import ply.yacc as yacc

 # Get the token map from the lexer.  This is required.
from AL import tokens

def p_not_erre(p):
    'eee : NEG erre'
    p[0] = not p[2]

def p_eee_erre(p):
    'eee : erre'
    p[0] = p[1]

def p_erre_expression_minusthan(p):
    'erre : erre MENORQUE expression'
    p[0] = p[1] < p[3]
    
        
def p_erre_expression(p):
    'erre : expression'
    p[0] = p[1]

def p_expression_plus(p):
    'expression : term SUM expression'
    p[0] = p[1] + p[3]

def p_expression_term(p):
    'expression : term'
    p[0] = p[1]
    
def p_term_number(p):
    'term : ENTERO'
    p[0] = p[1]

def p_error(p):
     print("Syntax error in input!")


 # Build the parser
parser = yacc.yacc()
 
while True:
   try:
    s = input('parser')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print(result)
