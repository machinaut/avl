#!/usr/bin/env python
# ainput.py - rewrite of ainput.f from AVL
# Alex Ray <ajray@ncsu.edu> 2011
#***********************************************************************
#    Module:  ainput.f
# 
#    Copyright (C) 2002 Mark Drela, Harold Youngren
# 
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#***********************************************************************
import re

class Configuration:
  def __init__(self,name,mach,iysym,izsym,zsym,sref,cref,bref,xref,cdp):
    self.name       = name
    self.mach       = mach
    self.iysym      = iysym
    self.izsym      = izsym
    self.ysym       = 0.0
    self.zsym       = zsym
    self.sref       = sref
    self.cref       = cref
    self.bref       = bref
    self.xref       = xref
    self.cdp        = cdp
    self.comp       = {None:[]} # None is a list of individual surfaces
    self.body       = []
    #XXX - need to finish print strings so i can start testing

class Surface:
  def __init__(self,name,nchord,cspace,nspan=None,sspace=None):
    self.name       = name
    self.nchord     = nchord
    self.cspace     = cspace
    self.nspan      = nspan
    self.sspace     = sspace
    self.comp       = None
    self.ydup       = None
    self.scale      = [1.0, 1.0, 1.0]
    self.trans      = [0.0, 0.0, 0.0]
    self.angle      = 0.0
    self.wake       = True
    self.albe       = True
    self.load       = True
    self.sect       = []

class Section:
  def __init__(self,xle,chord,ainc,nspan=None,sspace=None):
    self.xle        = xle
    self.chord      = chord
    self.ainc       = ainc
    self.nspan      = nspan
    self.sspace     = sspace
    self.airfoil    = None
    self.claf       = 1.0
    self.cdcl       = None
    self.design     = []
    self.cont       = []

# Airfoil types
class Naca:
  def __init__(self,naca):
    self.naca       = naca
  def __str__(self):
    return 'Naca(naca='+str(self.naca)+')'
class Airfoil:
  def __init__(self,start,end,xy):
    self.start      = start
    self.end        = end
    self.xy         = xy
  def __str__(self):
    return 'Airfoil(start='+str(self.start)+',end='+str(self.end)+\
        ',xy='+str(self.xy)+')'
class Afile:
  def __init__(self,start,end,filename):
    self.start      = start
    self.end        = end
    self.filename   = filename
  def __str__(self):
    return 'Afile(start='+str(self.start)+',end='+str(self.end)+\
        ',filename="'+str(self.filename)+'")'

class Design:
  def __init__(self,name,weight):
    self.name       = name
    self.weight     = weight
  def __str__(self):
    return 'Design(name="'+str(self.name)+',weight='+str(self.weight)+')'

class Control:
  def __init__(self,name,gain,xhinge,hvec,sgndup):
    self.name       = name
    self.gain       = gain
    self.xhinge     = xhinge
    self.hvec       = hvec
    self.sgndup     = sgndup
  def __str__(self):
    return 'Control(name="'+str(self.name)+'",gain='+str(self.gain)+\
        ',xhinge='+str(self.xhinge)+',hvec='+str(self.hvec)+\
        ',sgndup='+str(self.sgndup)+')'

class Body:
  def __init__(self,name,nbody,bspace):
    self.name       = name
    self.nbody      = nbody
    self.bspace     = bspace
    self.ydup       = None
    self.scale      = [1.0, 1.0, 1.0]
    self.trans      = [0.0, 0.0, 0.0]
    self.bfile      = None
  def __str__(self):
    return 'Body(name="'+str(self.name)+'",nbody='+str(self.nbody)+\
        ',bspace='+str(self.bspace)+',ydup='+str(self.ydup)+\
        ',scale='+str(self.scale)+',trans='+str(self.trans)+\
        ',bfile="'+str(self.bfile)+'")'

def read_infile(lun,infilename,ferr): # TODO the fuck is lun or ferr?
  ''' Parse an AVL configuration input file '''
  print 'Reading file:', infilename
  infile = open(infilename).read()
  # Read the file into a list of lines, parse by manipulating an index
  infile = re.sub(r'(\#|!)[^\n]*\n',r'',infile) # remove comments
  line = filter (lambda i: i != '', infile.split('\n')) # remove blanks
  line = [i.strip() for i in line] # remove extra whitespace
  index = 0
  def getline(): # Get next line and increment index
    index += 1
    return line[index-1]
  def putline(): # opposite of getline (just decrements index)
    index -= 1
  name = getline()
  mach = float(getline())
  iYsym, iZsym, Zsym = [float(i) for i in getline().split()]
  iYsym = float(cmp(iYsym,0))
  iZsym = float(cmp(iZsym,0))
  Sref, Cref, Bref = [float(i) for i in getline().split()]
  if Sref < 0 : Sref = 1.0
  if Cref < 0 : Cref = 1.0
  if Bref < 0 : Bref = 1.0
  Xref = [float(i) for i in getline().split()]
  # optional CDp
  try: CDp = float(getline())
  except ValueError: CDp = 0.0; putline()
  config = Configuration(name,mach,iYsym,iZsym,Zsym,Sref,Cref,Bref,Xref,CDp)
  # loop, parsing 'surfaces' and 'bodies'
  while(True): # you spin me right round baby right round
    # start of keyword-interpretation loop
    keyword = getline().upper().split()[0][0:4]
    if   keyword == 'EOF': #XXX TODO FIXME clean this up
      pass #cleanup()
    elif keyword == 'SURF':
      name = getline()
      print 'Surface:', name
      l = getline().split()
      Nchord, Cspace = int(l[0]),float(l[1])
      if len(l) < 4: Nspan, Sspace = None,      None
      else:          Nspan, Sspace = int(l[2]), float(l[3])
      surf = Surface(name,Nchord,Cspace,Nspan,Sspace)
      while(True): # loop over surface parameters
        keyword = getline().upper().split()[0][0:4]
        if   keyword == 'YDUP': surf.ydup  = float(getline()) 
        elif keyword == 'COMP': surf.comp  = int(getline()) 
        elif keyword == 'INDE': surf.comp  = int(getline()) 
        elif keyword == 'SCAL': surf.scale = [float(i) for i in getline()] 
        elif keyword == 'TRAN': surf.trans = [float(i) for i in getline()] 
        elif keyword == 'ANGL': surf.angle = float(getline()) 
        elif keyword == 'NOWA': surf.wake  = False 
        elif keyword == 'NOAL': surf.albe  = False 
        elif keyword == 'NOLO': surf.load  = False 
        elif keyword == 'SECT':
          l = getline().split()
          Xle = [float(i) for i in l[0:3]]
          Chord, Ainc = float(l[3]),float(l[4])
          if len(l) < 7: Nspan, Sspace = 0,           0.0
          else:          Nspan, Sspace = float(l[5]), float(l[6])
          sect = Section(Xle,Chord,Ainc,Nspan,Sspace)
          while(True): # parse out 'section' subcommands
            keyword = getline().upper().split()[0][0:4]
            if   keyword=='DESI':
              DName, Wdes = getline().split()
              Wdes = float(Wdes)
              sect.design.append(Design(DName,Wdes))
            elif keyword=='CONT':
              l = getline().split()
              name    = l[0]
              gain    = float(l[1])
              Xhinge  = float(l[2])
              XYZhvec = [float(i) for i in l[3:6]]
              SgnDup  = float(l[6])
              sect.cont.append(Control(name,gain,Xhinge,XYZhvec,SgnDup))
            elif keyword=='CLAF': self.claf = float(getline())
            elif keyword=='CDCL':self.cdcl=[float(i) for i in getline().split()]
            # Airfoil definition methods
            elif keyword=='NACA': sect.airfoil = Naca(int(getline()))
            elif keyword=='AFIL':
              putline()
              l = getline().split()
              keyword = l[0]
              if len(l) < 3: X1, X2 = None,        None
              else:          X1, X2 = float(l[1]), float(l[2])
              filename = getline()
              sect.airfoil = Afile(X1,X2,filename)
            elif keyword=='AIRF':
              putline()
              l = getline().split()
              keyword = l[0]
              if len(l) < 3: X1, X2 = None,        None
              else:          X1, X2 = float(l[1]), float(l[2])
              # loop over airfoil coordinates
              xy = []
              while (True):
                try: xy.append([float(i) for i in getline().split()])
                except ValueError: putline() ; break
              sect.airfoil = Airfoil(X1,X2,xy)
          surf.sect.append(sect)
      config.comp[surf.comp].append(surf)
    elif keyword == 'BODY':
      name = getline()
      print 'Body:', name
      l = getline().split()
      Nbody,Bspace = int(l[0]),float(l[1])
      body = Body(name,Nbody,Bspace)
      while(True): # loop over body parameters
        keyword = getline().upper().split()[0][0:4]
        if   keyword == 'YDUP': body.ydup  = float(getline()) 
        elif keyword == 'SCAL': body.scale = [float(i) for i in getline()] 
        elif keyword == 'TRAN': body.trans = [float(i) for i in getline()] 
        elif keyword == 'BFIL': body.bfile = getline()
      config.body.append(body)
  return config # output flight configuration
