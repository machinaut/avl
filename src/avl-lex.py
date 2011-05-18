#!/usr/bin/env python
# avl-lex.py  - lexer (tokenizer) for avl input file
# Alex Ray <ajray@ncsu.edu> 2011
import ply.lex as lex

# reserved words
reserved = {
    'surf' : 'SURFACE',
    'comp' : 'COMPONENT',
    'inde' : 'INDEX',
    'ydup' : 'YDUPLICATE',
    'scal' : 'SCALE',
    'tran' : 'TRANSLATE',
    'angl' : 'ANGLE',
    'nowa' : 'NOWAKE',
    'noal' : 'NOALBE',
    'nolo' : 'NOLOAD',
    'sect' : 'SECTION',
    'naca' : 'NACA',
    'airf' : 'AIRFOIL',
    'claf' : 'CLAF',
    'cdcl' : 'CDCL',
    'afil' : 'AFILE',
    'cont' : 'CONTROL',
    'body' : 'BODY',
    'bfil' : 'BFILE'
    }

# list of token names.   This is always required
tokens = [
    'SPACE',
    'COMMENT',
    'INT',
    'FLOAT',
    'ID',
    'LINE',
    'NEWLINE'
    ] + list(reserved.values())

# Regular expression rules for simple tokens
t_SPACE   = r'[ \t]+'

def t_COMMENT(t):
  r'((\#|\!).*\n)*'
  pass
  # No return value. Token discarded

def t_INT(t):
  r'\d+'
  t.value = int(t.value)
  return t

def t_FLOAT(t):
  r'-?((\d*\.\d+((e|E)-?\d+)?)|(\d+\.?((e|E)-?\d+)?))'
  t.value = float(t.value)
  return t

def t_ID(t):
  r'[a-zA-Z_][a-zA-Z_0-9]*'
  if t.value.lower()[0:4] in reserved:
    t.type = reserved[t.value.lower()[0:4]]
  else:
    t.type = 'ID'
  return t

def t_LINE(t):
  r'.+'
  return t

# Define a rule so we can track line numbers
def t_NEWLINE(t):
  r'\n+'
  t.lexer.lineno += len(t.value)

# Compute column. 
#     input is the input text string
#     token is a token instance
def find_column(input,token):
  last_cr = input.rfind('\n',0,token.lexpos)
  if last_cr < 0:
    last_cr = 0
  column = (token.lexpos - last_cr) + 1
  return column

# A string containing ignored characters (wierd whitespace)
t_ignore = '\r\f\v'

# Error handling rule
def t_error(t):
  print "Illegal character '%s'" % t.value[0]
  t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Test it out
data = open('../runs/sample.avl').read()

# Give the lexer some input
lexer.input(data)

# Tokenize
while True:
  tok = lexer.token()
  if not tok: break     # no more input
  print tok.type, tok,value, tok.line, tok.lexpos

# print tokens for fun
for tok in lexer: print tok
