#!/usr/bin/env python
# visualize.py - try to visualize AVL files with processing (somehow)
# Alex Ray <ajray@ncsu.edu> 2011
import math

class Section:
  def __init__(self,position=None,chord=None,angle=None,shape=None,
      spans=None,interpol=None):
    self.position = position  # airfoil leading edge location [x,y,z]
    self.chord    = chord     # chord length
    self.angle    = angle     # angle of inclination (of chord rel to XY)
    self.shape    = shape     # airfoil shape, for now just a NACA number
    self.spans    = spans     # number of spanwise vorticies until next section
    self.interpol = interpol  # interpolation parameter for spanwise vorticies

class Naca:
  """ Class for NACA airfoils """
  # Reference: http://en.wikipedia.org/wiki/NACA_airfoil
  # Reference: http://people.clarkson.edu/~pmarzocc/AE429/The%20NACA%20airfoil%20series.pdf
  def __init__(self,naca=2415):     # intelligent
    self.naca = '%04d' % int(naca)  # convert to a string
    naca   = self.naca
    self.t = float(naca[3:])/100.0  # maximum thickness as ratio of chord
    self.m = float(naca[0])/100.0   # maximum camber as a ratio of chord
    self.p = float(naca[1])/10.0    # location of maximum camber along chord
  def mean(self,x):
    """ calculate the mean line value for a given x coordinate along chord """
    t,m,p = self.t,self.m,self.p
    if x < 0.0 or x > 1.0:  return 0.0 # outside of airfoil
    elif x < p:             return m *   x   *     (2*p - x) /   p   ** 2
    else:                   return m * (1-x) * (1 + x - 2*p) / (1-p) ** 2
  def dmean(self,x):
    """ calculate the slope of the mean line value for a given x coordinate """
    t,m,p = self.t,self.m,self.p
    if x < 0.0 or x > 1.0:  return 0.0 # outside of airfoil
    elif x < p:             return 2*m/p          - 2*m*x/p**2
    else:                   return 2*m*p/(1-p)**2 - 2*m*x/(1-p)**2 
  def thick(self,x):
    """ calculate half thickness for a given x coordinate along chord """
    t = self.t
    return t/0.2*(0.2969*x**.5-0.1260*x-0.3516*x**2+0.2843*x**3-0.1015*x**4)
  def theta(self,x):
    """ calculate theta angle for a given x coordinate along chord """
    return math.atan(self.dmean(x))
  def upper(self,x):
    """ calculate the upper surface x- and y-coordinates given x along chord """
    t     = self.thick(x)
    theta = self.theta(x)
    y     = self.mean(x)
    xu    = x - t * math.sin(theta)
    yu    = y + t * math.cos(theta)
    return (xu,yu)
  def upperx(self,x): return self.upper(x)[0]
  def uppery(self,x): return self.upper(x)[1]
  def lower(self,x):
    """ calculate the lower surface x- and y-coordinates given x along chord """
    t     = self.thick(x)
    theta = self.theta(x)
    y     = self.mean(x)
    xl    = x + t * math.sin(theta)
    yl    = y - t * math.cos(theta)
    return (xl,yl)
  def lowerx(self,x): return self.lower(x)[0]
  def lowery(self,x): return self.lower(x)[1]
  def plot(self):
    """ plot airfoil with matplotlib """
    import numpy as np
    import matplotlib.pyplot as plt
    # points along the chord
    x   = np.arange(0.,1.001,0.001) # 1000 points along chord
    y   = [self.mean(i)   for i in x]
    xu  = [self.upperx(i) for i in x]
    yu  = [self.uppery(i) for i in x]
    xl  = [self.lowerx(i) for i in x]
    yl  = [self.lowery(i) for i in x]
    plt.plot(x,y)
    plt.plot(xu,yu)
    plt.plot(xl,yl)
    plt.xlim((-0.1,1.1))
    plt.ylim((-0.1,0.1))
    plt.show()
  def draw(self):
    """ draw airfoil with cairo """
    import numpy as np
    import cairo
    size=500
    surface = cairo.ImageSurface (cairo.FORMAT_ARGB32, size,size)
    ctx = cairo.Context (surface) # context
    #ctx.scale (size, size) # Normalizing the canvas
    ctx.set_source_rgb (0.7, 0.9, 0.8) # Solid color
    ctx.fill() # background
    x = np.arange(0.,1.001,0.001) # 1000 points along chord
    for i in [self.upper(i) for i in x]: ctx.line_to(i[0]*.8+.1,i[1]*.8+.1)
    ctx.set_source_rgb (0.3, 0.2, 0.5) # Solid color
    ctx.set_line_width(0.01)
    ctx.stroke() # draw line
    surface.write_to_png ("example.png") # Output to PNG

if __name__ == "__main__":
  naca = Naca()
  #naca.plot()
  naca.draw()
