#!/usr/bin/python
#
# interpreterext.py -
#		A python implementation of the mini language, with user-defined
#		functions
#
# Kurt Schmidt
# 7/07
#
# EDITOR:  cols=80, tabstop=2
#
# NOTES:
#		the display() method everybody has is just to graphically spit the
#		actual parse tree to the screen
#
#		The grammar can be found in programext.py  (probably should be here)
#
#

import sys
from programext import *

######   LEXER   ###############################
# Note:  This is precisely the same lexer that exp1 uses.  Could've pulled
# it out to a different file.

from ply import lex

tokens = (
	'PLUS',
	'MINUS',
	'TIMES',
	'LPAREN',
	'RPAREN',
	'SEMICOLON',
	'COMMA',
	'NUMBER',
	'ASSIGNOP',
	'WHILE',
	'DO',
	'OD',
	'IF',
	'THEN',
	'ELSE',
	'FI',
	'DEFINE',
	'PROC',
	'END',
	'IDENT',

    # Stuff we added
    "EQ",
    "NE",
    "LT",
    "GT",
    "LTE",
    "GTE",
    "LBRACKET",
    "RBRACKET",
    "CAR",
    "CDR",
    "CONS",
    # End of stuff we added
)

	# These are all caught in the IDENT rule, typed there.
reserved = {
		'while' : 'WHILE',
		'do'		: 'DO',
		'od'		: 'OD',
		'if'		: 'IF',
		'then'	: 'THEN',
		'else'	: 'ELSE',
		'fi'		: 'FI',
		'define': 'DEFINE',
		'proc'	: 'PROC',
		'end'		: 'END',

        # Stuff we added
        "car": "CAR",
        "cdr": "CDR",
        "cons": "CONS",
		}

# Now, this section.  We have a mapping, REs to token types (please note
# the t_ prefix).  They simply return the type.

	# t_ignore is special, and does just what it says.  Spaces and tabs
t_ignore = ' \t'

	# These are the simple maps
t_PLUS		= r'\+'
t_MINUS   = r'-'
t_TIMES		= r'\*'
t_LPAREN	= r'\('
t_RPAREN	= r'\)'
t_ASSIGNOP = r':='
t_SEMICOLON = r';'
t_COMMA		= r','

# Stuff we added
t_EQ = r"=="
t_NE = r"!="
t_LT = r"<"
t_GT = r">"
t_LTE = r"<="
t_GTE = r">="
t_LBRACKET = r"\["
t_RBRACKET = r"\]"
# End of stuf we added

def t_IDENT( t ):
	#r'[a-zA-Z_][a-zA-Z_0-9]*'
	r'[a-z]+'
	t.type = reserved.get( t.value, 'IDENT' )    # Check for reserved words
	return t

def t_NUMBER( t ) :
	r'[0-9]+'

		# t.value holds the string that matched.  Dynamic typing - no unions
	t.value = int( t.value )
	return t

	# These are standard little ditties:
def t_newline( t ):
  r'\n+'
  t.lexer.lineno += len( t.value )

  # Error handling rule
def t_error( t ):
  print "Illegal character '%s' on line %d" % ( t.value[0], t.lexer.lineno )
  return t
  #t.lexer.skip( 1 )

lex.lex()

#-----   LEXER (end)   -------------------------------


######   YACC   #####################################

import ply.yacc as yacc

	# create a function for each production (note the prefix)
	# The rule is given in the doc string

# Be able to access the name table in this later
P = None

def p_program( p ) :
  'program : stmt_list'
  global P
  P = Program( p[1] )
  #P.display()
  print 'Running Program'
  P.eval()
  P.dump()

def p_stmt_list( p ) :
 '''stmt_list : stmt SEMICOLON stmt_list
       | stmt'''
 if len( p ) == 2 :  # single stmt => new list
   p[0] = StmtList()
   p[0].insert( p[1] )
 else :  # we have a stmtList, keep adding to front
   p[3].insert( p[1] )
   p[0] = p[3]

def p_stmt( p ) :
	'''stmt : assign_stmt
				| while_stmt
				| if_stmt
				| define_stmt'''
	p[0] = p[1]

def p_add( p ) :
	'expr : expr PLUS term'
	p[0] = Plus( p[1], p[3] )

def p_sub( p ) :
	'expr : expr MINUS term'
	p[0] = Minus( p[1], p[3] )

def p_expr_list( p ) :
  '''expr_list : expr COMMA expr_list
              | expr'''
  if len( p ) == 2 :  # single expr => new list
    p[0] = [ p[1] ]
  else :  # we have a expr_list, keep adding to front
    p[3].insert( 0, p[1] )
    p[0] = p[3]

def p_expr_term( p ) :
	'expr : term'
	p[0] = p[1]

def p_mult( p ) :
	'''term : term TIMES fact'''
	p[0] = Times( p[1], p[3] )

def p_term_fact( p ) :
	'term : fact'
	p[0] = p[1]

def p_fact_expr( p ) :
	'fact : LPAREN expr RPAREN'
	p[0] = p[2]

def p_fact_NUM( p ) :
	'fact : NUMBER'
	p[0] = Number( p[1] )

def p_fact_IDENT( p ) :
	'fact : IDENT'
	p[0] = Ident( p[1] )

def p_fact_funcall( p ) :
	'fact : func_call'
	p[0] = p[1]

def p_assn( p ) :
	'assign_stmt : IDENT ASSIGNOP expr'
	p[0] = AssignStmt( p[1], p[3] )

def p_while( p ) :
	'while_stmt : WHILE expr DO stmt_list OD'
	p[0] = WhileStmt( p[2], p[4] )

def p_if( p ) :
	'if_stmt : IF expr THEN stmt_list ELSE stmt_list FI'
	p[0] = IfStmt( p[2], p[4], p[6] )

def p_def( p ) :
  'define_stmt : DEFINE IDENT PROC LPAREN param_list RPAREN stmt_list END'
  p[0] = DefineStmt( p[2], Proc( p[5], p[7] ))

def p_param_list( p ) :
  '''param_list : IDENT COMMA param_list
              | IDENT'''
  if len( p ) == 2 :  # single param => new list
    p[0] = [ p[1] ]
  else :  # we have a param_list, keep adding to front
    p[3].insert( 0, p[1] )
    p[0] = p[3]

def p_func_call( p ) :
  'func_call : IDENT LPAREN expr_list RPAREN'
  p[0] = FunCall( p[1], p[3] )


# Stuff we added
def p_eq(p):
    "expr : expr EQ expr"
    p[0] = Eq(p[1], p[3])
def p_ne(p):
    "expr : expr NE expr"
    p[0] = Ne(p[1], p[3])
def p_lt(p):
    "expr : expr LT expr"
    p[0] = LT(p[1], p[3])
def p_gt(p):
    "expr : expr GT expr"
    p[0] = GT(p[1], p[3])
def p_lte(p):
    "expr : expr LTE expr"
    p[0] = LTE(p[1], p[3])
def p_gte(p):
    "expr : expr GTE expr"
    p[0] = GTE(p[1], p[3])
def p_list(p):
    """expr : LBRACKET RBRACKET
        | LBRACKET list_content RBRACKET"""
    if len(p) == 3:
        p[0] = ListImpl()
    else:
        p[0] = p[2]
def p_list_content(p):
    """list_content : expr COMMA list_content
        | expr"""
    if len( p ) == 2 :  # single element => new list
        p[0] = ListImpl()
        p[0].insert( p[1] )
    else :  # we have a list, keep adding to front
        p[3].insert( p[1] )
        p[0] = p[3]
def p_car(p):
    "expr : CAR LPAREN expr RPAREN"
    p[0] = ListCar(p[3])
def p_cdr(p):
    "expr : CDR LPAREN expr RPAREN"
    p[0] = ListCdr(p[3])
def p_cons(p):
    "expr : CONS LPAREN expr COMMA expr RPAREN"
    p[0] = ListCons(p[3], p[5])
# End of stuff we added


# Error rule for syntax errors
def p_error( p ):
	print "Syntax error in input!", str( p )
	sys.exit( 2 )

	# now, build the parser
yacc.yacc()


######   MAIN   #################################

def test_scanner( arg=sys.argv ) :

	data = ' 1+2 1-2 3*4 x blah y := 5 '

	lex.input( data )

	# attempt to get that first token
	tok = lex.token()
	while tok :
		print tok
		tok = lex.token()


def test_parser( arg=sys.argv ) :
    """
    Tests for Prob1 of PA4
    """
    # ==
    data = """
    x := 5;
    if x == 5 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1

    data = """
    x := 5;
    if x == 6 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 2

    # !=
    data = """
    x := 5;
    if x != 5 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 2

    data = """
    x := 5;
    if x != 6 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1


    # <
    data = """
    x := 5;
    if x < 6 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1

    data = """
    x := 5;
    if 6 < x then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 2


    # >
    data = """
    x := 5;
    if x > 6 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 2

    data = """
    x := 5;
    if 6 > x then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1


    # <=
    data = """
    x := 5;
    if x <= 6 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1

    data = """
    x := 5;
    if 6 <= x then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 2


    data = """
    x := 5;
    if x <= 5 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1

    data = """
    x := 5;
    if 5 <= x then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1


    # >=
    data = """
    x := 5;
    if x >= 6 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 2

    data = """
    x := 5;
    if 6 >= x then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1


    data = """
    x := 5;
    if x >= 5 then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1

    data = """
    x := 5;
    if 5 >= x then
        s := 1
    else
        s := 2
    fi
    """
    yacc.parse( data )
    assert P.nameTable["s"] == 1


    # List
    data = """
    x := [8, 2, 3]
    """
    yacc.parse( data )
    assert P.nameTable["x"] == [8, 2, 3]

    data = """
    x := [8, 2, 3];
    x := car(x)
    """
    yacc.parse( data )
    assert P.nameTable["x"] == 8

    data = """
    x := car([3])
    """
    yacc.parse( data )
    assert P.nameTable["x"] == 3

    data = """
    x := cdr([8, 2, 3])
    """
    yacc.parse( data )
    assert P.nameTable["x"] == [2, 3]

    data = """
    x := [8, 2, 3];
    x := cdr(x)
    """
    yacc.parse( data )
    assert P.nameTable["x"] == [2, 3]

    data = """
    x := [8, 2, 3];
    x := cons(1, x)
    """
    yacc.parse( data )
    assert P.nameTable["x"] == [1, 8, 2, 3]

    data = """
    x := cons(10, []);
    x := cons(2, x);
    y := cdr(x);
    z := car(y)
    """
    yacc.parse( data )
    assert P.nameTable["x"] == [2, 10]
    assert P.nameTable["y"] == [10]
    assert P.nameTable["z"] == 10


    # Regular parse form stdin
    print "\nAll tests passed.\n"
    data = sys.stdin.read()
    yacc.parse( data )


if __name__ == '__main__' :
	test_parser()
