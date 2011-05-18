#!/usr/bin/env python
# avl-yacc.py - parser for avl input files
# Alex Ray <ajray@ncsu.edu> 2011
import ply.yacc as yacc

# Get the token map from the lexer. This is required.
from avl-lex import tokens
from avl-class import *

def p_configuration(p): # TODO last spacing (end of file) necessary?
  '''configuration : spacing LINE header spacing'''
  p[0]        = Configuration(p[2])
  p[0].mach   = p[3][0]
  p[0].iysym  = p[3][1][0]
  p[0].izsym  = p[3][1][1]
  p[0].zsym   = p[3][1][2]
  p[0].sref   = p[3][2][0] 
  p[0].cref   = p[3][2][0]
  p[0].bref   = p[3][2][0]
  p[0].xref   = p[3][3]
  p[0].cdp    = p[3][4] 
  p[0].body   = p[4]

def p_header(p):
  '''header : spacing FLOAT spacing trio spacing trio spacing trio spacing FLOAT craft
            | spacing FLOAT spacing trio spacing trio spacing trio craft'''
  if len(p) < 11: # with CDp
    p[0] = (p[2],p[4],p[6],p[8],0.0,  p[9])
  else: # without CDp
    p[0] = (p[2],p[4],p[6],p[8],p[10],p[11])

def p_trio(p):
  '''trio : FLOAT SPACE FLOAT SPACE FLOAT'''
  p[0] = (p[1],p[3],p[5])

def p_craft(p):
  '''craft  : spacing craft surface
            | spacing craft body
            | spacing surface
            | spacing body'''
  if len(p) < 4:
    p[0] = [p[2]]
  else:
    p[0] = p[2] + [p[3]]

def p_surface_component(p):
  '''surface  : surface spacing COMPONENT spacing INT
              | surface spacing INDEX     spacing INT'''
  p[0] = p[1]
  p[0].component = p[5]

def p_surface_yduplicate(p):
  '''surface  : surface spacing YDUPLICATE spacing FLOAT'''
  p[0] = p[1]
  p[0].yduplicate = p[5]

def p_surface_scale(p):
  '''surface  : surface spacing SCALE spacing trio'''
  p[0] = p[1]
  p[0].scale = p[5]

def p_surface_translate(p):
  '''surface  : surface spacing TRANSLATE spacing trio'''
  p[0] = p[1]
  p[0].translate = p[5]

def p_surface_angle(p):
  '''surface  : surface spacing ANGLE spacing FLOAT'''
  p[0] = p[1]
  p[0].angle = p[5]

def p_surface_nowake(p):
  '''surface  : surface spacing NOWAKE'''
  p[0] = p[1]
  p[0].nowake = True

def p_surface_noalbe(p):
  '''surface  : surface spacing NOALBE'''
  p[0] = p[1]
  p[0].noalbe = True

def p_surface_noload(p):
  '''surface  : surface spacing NOLOAD'''
  p[0] = p[1]
  p[0].noload = True

def p_surface_section(p):
  '''surface  : surface section'''

def p_surface(p):
  '''surface  : spacing SURFACE spacing LINE surfacedata'''
  p[0] = Surface(p[2])
  p[0].name = p[4]
  p[0].nchord = p[5][0]
  p[0].cspace = p[5][1]
  p[0].nspan  = p[5][2]
  p[0].sspace = p[5][3]

def p_surfacedata(p):
  '''surfacedata  : spacing INT SPACE FLOAT SPACE INT SPACE FLOAT
                  | spacing INT SPACE FLOAT'''
  if len(p) < 9:
    p[0] = (p[2],p[4],None,None)
  else:
    p[0] = (p[2],p[4],p[6],p[8])

def p_section_naca(p):
  '''section  : section naca'''

def p_section_airfoil(p):
  '''section  : section airfoil'''

def p_section_afile(p):
  '''section  : section afile'''

def p_section_claf(p):
  '''section  : section claf'''

def p_section_cdcl(p):
  '''section  : section cdcl'''

def p_section_control(p):
  '''section  : section control'''

def p_section_design(p):
  '''section  : section design'''

def p_section(p):
  '''section  : spacing SECTION sectiondata'''

def p_sectiondata(p):
  '''sectiondata  : spacing trio SPACE FLOAT SPACE FLOAT SPACE INT SPACE FLOAT
                  | spacing trio SPACE FLOAT SPACE FLOAT'''

def p_body_yduplicate(p):
  '''body : body yduplicate'''

def p_body_scale(p):
  '''body : body scale'''

def p_body_translate(p):
  '''body : body translate'''

def p_body_bfile(p):
  '''body : body bfile'''

def p_body(p):
  '''body : spacing BODY spacing LINE bodydata'''

def p_bodydata(p):
  '''bodydata : spacing INT SPACE FLOAT'''

def p_body_yduplicate(p):
  '''body : body yduplicate'''

def p_spacing(p):
  '''spacing  : spacing NEWLINE
              | spacing COMMENT
              | spacing SPACE
              | '''
  pass

def p_
