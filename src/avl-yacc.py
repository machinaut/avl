#!/usr/bin/env python
# avl-yacc.py - parser for avl input files
# Alex Ray <ajray@ncsu.edu> 2011
import ply.yacc as yacc

# Get the token map from the lexer. This is required.
from avl-lex import tokens

# XXX bullshit
grammar = """
Grammar                           Action
------------------------------    -----------------------------------------
plane  : header body
header : title headerparams
title  : subtitle* ID
"""

def p_plane(p):
  'plane : header definition'

def p_header(p):
  'header : name headerparams'


