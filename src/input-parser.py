#!/usr/bin/env python
# input-parser.py - parser for AVL input files using regular expressions
# Alex Ray <ajray@ncsu.edu> 2011
#      Notes: (and differences)
# Sref,Cref,Bref won't default to 1.0 if given a negative input
import re
header = re.compile(r"""(?sx)         # re.VERBOSE and re.DOTALL
  (?P<title>[^\n]*)               \s+ # Title
  (?P<mach>\S+)                   \s+ # Mach
  (?P<isym>\S+[ \t]+\S+)       [ \t]+ # iYsym iZsym
      (?P<zsym>\S+)               \s+ #             Zsym
  (?P<sref>\S+[ \t]+\S+[ \t]+\S+) \s+ # Sref  Cref  Bref
  (?P<xref>\S+[ \t]+\S+[ \t]+\S+) \s* # Xref  Yref  Zref
  (?P<cdp>\S*)                    \s* # CDp (optional)
  (?P<remainder>.*)                   # Remainder
  """)
surface = re.compile(r"""(?sx)        # re.VERBOSE and re.DOTALL
  (?P<surf>SURF\S*)               \s+ # keyword SURFACE
  """

class plane:
  """ plane is a flight configuration (airplane) in AVL """
  def __init__(self):
    title = ''
#    title, mach,  cdp   = None, None, None
#    iysym, izsym, zsym  = None, None, None
#    sref,  cref,  bref  = None, None, None
#    xref,  yref,  zref  = None, None, None # TODO combine to one reference point
    ysym = 0.0
  def __str__(self):
    return self.title

if __name__ == "__main__":
  import sys
  if len(sys.argv) < 2:
    print "Usage: ./input-parser.py <filename>"
    exit(1)
  p = plane()
  s = open(sys.argv[1]).read() # read whole input file
  s = re.sub(r'(\#|!)[^\n]*\n',r'',s) # parse out comments
  m = header.match(s)
  if m is None: sys.exit("Error: Failed to match header")
  d = m.groupdict() # match header
  p.title = d['title']
  p.mach  = float(d['mach'])
  p.iysym, p.izsym       = [cmp(float(i),0) for i in d['isym'].split()]
  p.zsym                 = [float(i) for i in d['zsym'].split()]
  p.sref, p.cref, p.bref = [float(i) for i in d['sref'].split()]
  p.xref, p.yref, p.zref = [float(i) for i in d['sref'].split()]
  try: p.cdp = float(d['cdp']) # take care of optional CDp
  except ValueError: d['remainder'] = d['cdp']+d['remainder'] ; p.cdp = 0.0
  s = d['remainder']
  while s != '':
    m = surface.match(s)
    if m is not None:
      if isurf != 0:
        #makesurf()
        if ldupl: pass #sdupl()
        isurf = 0
      if ibody != 0:
        #makebody()
        if ldupl: pass #bdupl()
        ibody = 0

      # XXX - ended in line 262 of original ainput.f
  print p

