#!/usr/bin/env python
# input-parser.py - parser for AVL input files using regular expressions
# Alex Ray <ajray@ncsu.edu> 2011
#      Notes: (and differences)
# Only end of line character is newline (\n), the rest are just whitespace
# Sref,Cref,Bref won't default to 1.0 if given a negative input
# No max number of surfaces
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
  (?P<name>[^\n]*)                \s+ # surface name
  (?P<params>\S+[ \t]+\S+([ \t]+\S+[ \t]+\S+)?) \s*  # surface parameters
              # Nchord  Cspace [ Nspan Sspace ]
  (?P<remainder>.*)                   # Remainder
  """)
body = re.compile(r"""(?sx)           # re.VERBOSE and re.DOTALL
  (?P<body>BODY\S*)               \s+ # keyword BODY
  (?P<name>[^\n]*)                \s+ # body name
  (?P
  """ ) # XXX not done yet

class Plane:
  def __init__(self):
    title = ''
#    title, mach,  cdp   = None, None, None
#    iysym, izsym, zsym  = None, None, None
#    sref,  cref,  bref  = None, None, None
#    xref,  yref,  zref  = None, None, None # TODO combine to one reference point
    self.ysym = 0.0
    self.surface = [] # surfaces
  def __str__(self):
    return self.title

class Surface:
  def __init__(self):
    components = 0
  def __str__(self):
    return self.name

if __name__ == "__main__":
  import sys
  if len(sys.argv) < 2:
    print "Usage: ./input-parser.py <filename>"
    exit(1)
  p = Plane()
  s = open(sys.argv[1]).read() # read whole input file
  s = re.sub(r'(\#|!)[^\n]*\n',r'',s) # parse out comments
  m = header.match(s)
  if m is None: sys.exit("Error: Failed to match header")
  d = m.groupdict() # match header
  print 'Configuration:', d['title']
  p.title = d['title']
  p.mach  = float(d['mach'])
  p.iysym, p.izsym       = [cmp(float(i),0) for i in d['isym'].split()]
  p.zsym                 = [float(i) for i in d['zsym'].split()]
  p.sref, p.cref, p.bref = [float(i) for i in d['sref'].split()]
  p.xref, p.yref, p.zref = [float(i) for i in d['sref'].split()]
  try: p.cdp = float(d['cdp']) # take care of optional CDp
  except ValueError: d['remainder'] = d['cdp']+d['remainder'] ; p.cdp = 0.0
  s = d['remainder'].upper() # convert remainder to all upper case
  # XXX - stupid random initialization shit to make this work
  isurf,nsurf,ibody = 0,0,0
  lscomp = {}
  lfwake,lfalbe,lfload = {},{},{}
  while s != '':
    m = surface.match(s)
    if m is not None: # matched 'SURF'
      surf = Surface()
      d = m.groupdict()
      # new surface is about to start
      if isurf != 0:
        # old surface is still active
        #makesurf() # TODO
        if ldupl: pass #sdupl()
        isurf = 0
      if ibody != 0:
        # old body is still active, so build it before finishing
        #makebody() # TODO
        if ldupl: pass #bdupl()
        ibody = 0
      # new surface; isurf==0 denotes surface accumulation is active
      nsurf = nsurf + 1
      isurf = nsurf
      # default surface component index is just the surface number
      lscomp[isurf] = isurf # XXX wtf is lscomp?
      # clear indices for accumulation TODO probably not necessary
      nsec, isec = 0, 0
      # set up surface defaults
      ldupl, lhinge = False, False
      # assume this will be a conventional loaded surface
      lfwake[isurf],lfalbe[isurf],lfload[isurf] = True, True, True
      xyzcal, xyztran = [1.0,1.0,1.0], [0.0,0.0,0.0]
      addinc, ncvar = 0.0, 0
      # XXX holy shit it takes this long to get to actual surface parsing
      l = d['params'].split()
      if len(d['params'].split()) == 4:
        nchord,cspace,nspan,sspace = int(l[0]),float(l[1]),int(l[2]),float(l[3])
      else:
        nchord,cspace,nspan,sspace = int(l[0]),float(l[1]),0,0.0
      # XXX - what the fuck are these, but they're in there
      # nchord = int(float(l[0]) + 0.001)
      # nspan = int(float(l[2]) + 0.001) if *&*& else nspan,sspace = 0,0.0
      # TODO should move this print and parsing before the heavier computations
      print "Building surface:", d['name']
      surf.name = d['name']
      surf.nchord,surf.cspace,surf.nspan,surf.sspace = nchord,cspace,nspan,sspace
      p.surface.append(surf)
      s = d['remainder']
      continue
      

      # XXX - ended in line 262 of original ainput.f
  print p

