import ply.yacc as yacc

 # Get the token map from the lexer.  This is required.
from AL import tokens

def p_expression_plus(p):
    'expression : expression SUM term'
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
