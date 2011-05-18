#!/usr/bin/env python
# avl-class.py - avl class definitions

class Configuration:
  """
  Header data
  - - - - - - 
  The input file begins with the following information in the first 5 non-blank,
  non-comment lines:

  Abc...              | case title

  #                   | comment line begins with "#" or "!"

  0.0                 | Mach
  1     0     0.0     | iYsym  iZsym  Zsym
  4.0   0.4   0.1     | Sref   Cref   Bref
  0.1   0.0   0.0     | Xref   Yref   Zref
  0.020               | CDp  (optional)



    Mach  = default freestream Mach number for Prandtl-Glauert correction

    iYsym =  1  case is symmetric about Y=0    , (X-Z plane is a solid wall)
          = -1  case is antisymmetric about Y=0, (X-Z plane is at const. Cp)
          =  0  no Y-symmetry is assumed

    iZsym =  1  case is symmetric about Z=Zsym    , (X-Y plane is a solid wall)
          = -1  case is antisymmetric about Z=Zsym, (X-Y plane is at const. Cp)
          =  0  no Z-symmetry is assumed (Zsym ignored)

    Sref  = reference area  used to define all coefficients (CL, CD, Cm, etc)
    Cref  = reference chord used to define pitching moment (Cm)
    Bref  = reference span  used to define roll,yaw moments (Cl,Cn)

    X,Y,Zref = default location about which moments and rotation rates are defined
               (if doing trim calculations, XYZref must be the CG location,
                which can be imposed with the MSET command described later)

    CDp = default profile drag coefficient added to geometry, applied at XYZref
           (assumed zero if this line is absent, for previous-version compatibility)
  """
  def __init__(self,name):
    self.name     = name
    self.mach     = 0.0
    self.iysym    = 0
    self.izsym    = 0
    self.zsym     = 0.0
    self.sref     = 0.0
    self.cref     = 0.0
    self.bref     = 0.0
    self.xref     = [0.0, 0.0, 0.0]
    self.cdp      = 0.0
    self.body     = [] # list of surfaces and bodies
    self.comp     = {} # list of components # TODO is this redundant with surf?
    self.control  = {} # list of control surfaces
  def __str__(self):
    return 'Configuration: ' + self.name

class Surface:
  """
  SURFACE              | (keyword)
  Main Wing            | surface name string
  12   1.0  20  -1.5   | Nchord  Cspace   [ Nspan Sspace ]

  The SURFACE keyword declares that a surface is being defined until 
  the next SURFACE or BODY keyword, or the end of file is reached.  
  A surface does not really have any significance to the underlying 
  AVL vortex lattice solver, which only recognizes the overall 
  collection of all the individual horseshoe vortices.  SURFACE 
  is provided only as a configuration-defining device, and also 
  as a means of defining individual surface forces.  This is 
  necessary for structural load calculations, for example.

    Nchord =  number of chordwise horseshoe vortices placed on the surface
    Cspace =  chordwise vortex spacing parameter (described later)

    Nspan  =  number of spanwise horseshoe vortices placed on the surface [optional]
    Sspace =  spanwise vortex spacing parameter (described later)         [optional]

  If Nspan and Sspace are omitted (i.e. only Nchord and Cspace are present on line),
  then the Nspan and Sspace parameters will be expected for each section interval,
  as described later.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) SURFACE
    self.name     = ''      # surface name string
    self.nchord   = 0       # Nchord
    self.cspace   = 0.0     # Cspace
    self.nspan    = None    # Nspan  [optional]
    self.sspace   = None    # Sspace [optional]
    self.component = None
    self.yduplicate = None
    self.scale    = None
    self.translate = None
    self.angle = None
    self.nowake = False
    self.noalbe = False
    self.noload = False
    self.section = []
  def __str__(self):
    return 'Surface: ' + self.name

class Component:
  """
  COMPONENT       | (keyword) or INDEX 
  3               | Lcomp

  This optional keywords COMPONENT (or INDEX for backward compatibility)
  allows multiple input SURFACEs to be grouped together into a composite 
  virtual surface, by assigning each of the constituent surfaces the same 
  Lcomp value.  Application examples are:
  - A wing component made up of a wing and a winglet.
  - A T-tail component made up of horizontal and vertical tail surfaces.

  A common Lcomp value instructs AVL to _not_ use a finite-core model
  for the influence of a horseshoe vortex and a control point which lies
  on the same component, as this would seriously corrupt the calculation.

  If each COMPONENT is specified via only a single SURFACE block,
  then the COMPONENT (or INDEX) declaration is unnecessary.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) COMPONENT or INDEX
    self.lcomp    = None    # component index # TODO is this necessary
    self.surface  = []      # list of surfaces
  def __str__(self):
    return 'Component: ' + str(self.lcomp)

class Yduplicate:
  """
  YDUPLICATE      | (keyword)
  0.0             | Ydupl

  The YDUPLICATE keyword is a convenient shorthand device for creating 
  another surface which is a geometric mirror image of the one 
  being defined.  The duplicated surface is _not_ assumed to be 
  an aerodynamic image or anti-image, but is truly independent.  
  A typical application would be for cases which have geometric 
  symmetry, but not aerodynamic symmetry, such as a wing in yaw.  
  Defining the right wing together with YDUPLICATE will conveniently 
  create the entire wing.

  The YDUPLICATE keyword can _only_ be used if iYsym = 0 is specified.
  Otherwise, the duplicated real surface will be identical to the
  implied aerodynamic image surface, and velocities will be computed
  directly on the line-vortex segments of the images.  This will 
  almost certainly produce an arithmetic fault.

  The duplicated surface gets the same Lsurf value as the parent surface,
  so they are considered to be the same physical surface.  There is
  no significant effect on the results if they are in reality 
  two physical surfaces.


    Ydupl =  Y position of X-Z plane about which the current surface is 
             reflected to make the duplicate geometric-image surface.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) YDUPLICATE
    self.ydupl    = 0.0     # Ydupl
  def __str__(self):
    return 'Yduplicate: ' + str(self.ydupl)

class Scale:
  """
  SCALE            |  (keyword)
  1.0  1.0  0.8    | Xscale  Yscale  Zscale

  The SCALE allows convenient rescaling for the entire surface.
  The scaling is applied before the TRANSLATE operation described below. 

    Xscale,Yscale,Zscale  =  scaling factors applied to all x,y,z coordinates
                             (chords are also scaled by Xscale)
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) SCALE
    self.scale    = [0.0, 0.0, 0.0] # Xscale Yscale Zscale
  def __str__(self):
    return 'Scale: ' + str(self.scale)

class Translate:
  """
  TRANSLATE         |  (keyword)
  10.0  0.0  0.5    | dX  dY  dZ

  The TRANSLATE keyword allows convenient relocation of the entire 
  surface without the need to change the Xle,Yle,Zle locations 
  for all the defining sections.  A body can be translated without
  the need to modify the body shape coordinates.

    dX,dY,dZ =  offset added on to all X,Y,Z values in this surface.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) TRANSLATE
    self.trans    = [0.0, 0.0, 0.0] # dX dY dZ
  def __str__(self):
    return 'Scale: ' + str(self.trans)

class Angle:
  """
  ANGLE       |  (keyword)
  2.0         | dAinc

  The ANGLE keyword allows convenient changing of the incidence angle 
  of the entire surface without the need to change the Ainc values 
  for all the defining sections.  The rotation is performed about
  the spanwise axis projected onto the y-z plane.

    dAinc =  offset added on to the Ainc values for all the defining sections
             in this surface
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) ANGLE
    self.dainc    = 0.0     # dAinc
  def __str__(self):
    return 'Angle: ' + str(self.dainc)

# The following keyword declarations would be used in envisioned applications.
# 
# 1) Non-lifting fuselage modeled by its side-view and top-view profiles.
# This will capture the moment of the fuselage reasonably well.
# NOWAKE
# 
# 2) Another nearby aircraft, with both aircraft maneuvering together.
# This would be for trim calculation in formation flight.
# NOALBE
# NOLOAD
# 
# 3) Another nearby aircraft, with only the primary aircraft maneuvering.
# This would be for a flight-dynamics analysis in formation flight.
# NOLOAD
# 
# 4) Nearby wind tunnel walls or ground plane.
# NOALBE
# NOLOAD

class Nowake:
  """
  NOWAKE     |  (keyword)

  The NOWAKE keyword specifies that this surface is to NOT shed a wake,
  so that its strips will not have their Kutta conditions imposed.
  Such a surface will have a near-zero net lift, but it will still 
  generate a nonzero moment.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) NOWAKE
  def __str__(self):
    return 'Nowake'

class Noalbe:
  """
  NOALBE    |  (keyword)

  The NOALBE keyword specifies that this surface is unaffected by
  freestream direction changes specified by the alpha,beta angles
  and p,q,r rotation rates.  This surface then reacts to only to
  the perturbation velocities of all the horseshoe vortices and 
  sources and doublets in the flow.
  This allows the SURFACE/NOALBE object to model fixed surfaces such 
  as a ground plane, wind tunnel walls, or a nearby other aircraft 
  which is at a fixed flight condition.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) NOALBE
  def __str__(self):
    return 'Noalbe'

class Noload:
  """
  NOLOAD    |  (keyword)

  The NOLOAD keyword specifies that the force and moment on this surface
  is to NOT be included in the overall forces and moments of the configuration.
  This is typically used together with NOALBE, since the force on a ground
  plane or wind tunnel walls certainly is not to be considered as part
  of the aircraft force of interest.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) NOLOAD
  def __str__(self):
    return 'Noload'

class Section:
  """
  SECTION                             |  (keyword)
  0.0 5.0 0.2   0.50  1.50   5 -2.0   | Xle Yle Zle   Chord Ainc   [ Nspan Sspace ]

  The SECTION keyword defines an airfoil-section camber line at some 
  spanwise location on the surface.

    Xle,Yle,Zle =  airfoil's leading edge location
    Chord       =  the airfoil's chord  (trailing edge is at Xle+Chord,Yle,Zle)
    Ainc        =  incidence angle, taken as a rotation (+ by RH rule) about 
                   the surface's spanwise axis projected onto the Y-Z plane.  
    Nspan       =  number of spanwise vortices until the next section [ optional ]
    Sspace      =  controls the spanwise spacing of the vortices      [ optional ]


  Nspan and Sspace are used here only if the overall Nspan and Sspace 
  for the whole surface is not specified after the SURFACE keyword.
  The Nspan and Sspace for the last section in the surface are always ignored.

  Note that Ainc is used only to modify the flow tangency boundary 
  condition on the airfoil camber line, and does not rotate the geometry 
  of the airfoil section itself.  This approximation is consistent with 
  linearized airfoil theory.

  The local chord and incidence angle are linearly interpolated between
  defining sections.  Obviously, at least two sections (root and tip)
  must be specified for each surface.

  The default airfoil camber line shape is a flat plate.  The NACA, AIRFOIL,
  and AFIL keywords, described below, are available to define non-flat
  camber lines.  If one of these is used, it must immediately follow 
  the data line of the SECTION keyword.

  All the sections in the surface must be defined in order across the span.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) SECTION
    self.le       = [0.0, 0.0, 0.0] # Xle Yle Zle
    self.chord    = 0.0     # Chord
    self.ainc     = 0.0     # Ainc
    self.nspan    = None    # Nspan  [optional]
    self.sspace   = None    # Sspace [optional]
  def __str__(self):
    return 'Section'

class Naca:
  """
  NACA                      |    (keyword)
  4300                      | section NACA camberline

  The NACA keyword sets the camber line to the NACA 4-digit shape specified
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) NACA
    self.naca     = 0       # section NACA camberline
  def __str__(self):
    return 'Naca: %04d' % self.le

class Airfoil:
  """
  AIRFOIL   X1  X2          |(keyword)   [ optional x/c range ]
  1.0   0.0                 | x/c(1)  y/c(1)
  0.98  0.002               | x/c(2)  y/c(2)
   .     .                  |  .       .
   .     .                  |  .       .
   .     .                  |  .       .
  1.0  -0.01                | x/c(N)  y/c(N)


  The AIRFOIL keyword declares that the airfoil definition is input
  as a set of x/c, y/c pairs.

    x/c,y/c =  airfoil coordinates 

  The x/c, y/c coordinates run from TE, to LE, back to the TE again 
  in either direction.  These corrdinates are splined, and the slope 
  of the camber y(x) function is obtained from the middle y/c values 
  between top and bottom.  The number of points N is deterimined 
  when a line without two readable numbers is encountered.

  If present, the optional X1 X2 parameters indicate that only the 
  x/c range X1..X2 from the coordinates is to be assigned to the surface.
  If the surface is a 20%-chord flap, for example, then X1 X2
  would be 0.80 1.00.  This allows the camber shape to be easily 
  assigned to any number of surfaces in piecewise manner.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) AIRFOIL
    self.range    = [None, None] # [X1, X2] [optional]
    self.xy       = []      # [(x/c,y/c),...]
  def __str__(self):
    return 'Airfoil'

class Afile:
  """
  AFILE      X1  X2         | (keyword)   [ optional x/c range ]
  filename                  | filename string

  The AFILE keyword is essentially the same as AIRFOIL, except
  that the x/c,y/c pairs are generated from a standard (XFOIL-type)
  set of airfoil coordinates contained in the file "filename".  
  The first line of this file is assumed to contain a string
  with the name of the airfoil (as written out with XFOIL's SAVE
  command).   If the path/filename has embedded blanks
  double quotes should be used to delimit the string.

  The optional X1 X2 parameters are used as in AIRFOIL.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) AFILE
    self.range    = [None, None] # [X1, X2] [optional]
    self.filename = ''      # filename string
  def __str__(self):
    return 'Afile: ' + self.filename

class Design:
  """
  DESIGN                  | (keyword)
  DName  Wdes             | design parameter name,  local weight

  This declares that the section angle Ainc is to be virtually 
  perturbed by a design parameter, with name DName and local
  Wdes.  

  For example, declarations for design variables "twist1" and "bias1"

  DESIGN
  twist1  -0.5

  DESIGN 
  bias1   1.0

  Give an effective (virtual) section incidence that is set using the "twist1" 
  and "bias1" design variables as:

    Ainc_total = Ainc  - 0.5*twist1_value + 1.0*bias_value

  where twist1_value and bias1_value are design parameters specified at runtime.

  The sensitivities of the flow solution to design variable changes
  can be displayed at any time during program execution.  Hence,
  design variables can be used to quickly investigate the effects
  of twist changes on lift, moments, induced drag, etc.

  Declaring the same design parameter with varying weights for multiple 
  sections in a surface allows the design parameter to represent a convenient 
  "design mode", such as linear washout, which influences all sections.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) DESIGN
    self.name     = ''      # DName
    self.wdes     = 0.0     # Wdes
  def __str__(self):
    return 'Design: ' + self.name

class Control:
  """
  CONTROL                              | (keyword)
  elevator  1.0  0.6   0. 1. 0.   1.0  | name, gain,  Xhinge,  XYZhvec,  SgnDup



  The CONTROL keyword declares that a hinge deflection at this section
  is to be governed by one or more control variables.  An arbitrary
  number of control variables can be used, limited only by the array
  limit NDMAX.

  The data line quantities are...

   name     name of control variable
   gain     control deflection gain, units:  degrees deflection / control variable
   Xhinge   x/c location of hinge.  
             If positive, control surface extent is Xhinge..1  (TE surface)
             If negative, control surface extent is 0..-Xhinge (LE surface)
   XYZhvec  vector giving hinge axis about which surface rotates 
             + deflection is + rotation about hinge by righthand rule
             Specifying XYZhvec = 0. 0. 0. puts the hinge vector along the hinge
   SgnDup   sign of deflection for duplicated surface
             An elevator would have SgnDup = +1
             An aileron  would have SgnDup = -1
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) CONTROL
    self.name     = ''      # name
    self.gain     = 0.0     # gain
    self.xhinge   = 0.0     # Xhinge
    self.hvec     = [0.0, 0.0, 0.0] # XYZhvec
    self.sgndup   = 0       # SgnDup
  def __str__(self):
    return 'Control: ' + self.name

class Claf:
  """
  CLAF        |  (keyword)
  CLaf        |  dCL/da scaling factor

  This scales the effective dcl/da of the section airfoil as follows:
   dcl/da  =  2 pi CLaf
  The implementation is simply a chordwise shift of the control point
  relative to the bound vortex on each vortex element.

  The intent is to better represent the lift characteristics 
  of thick airfoils, which typically have greater dcl/da values
  than thin airfoils.  A good estimate for CLaf from 2D potential
  flow theory is

    CLaf  =  1 + 0.77 t/c

  where t/c is the airfoil's thickness/chord ratio.  In practice,
  viscous effects will reduce the 0.77 factor to something less.
  Wind tunnel airfoil data or viscous airfoil calculations should
  be consulted before choosing a suitable CLaf value.

  If the CLAF keyword is absent for a section, CLaf defaults to 1.0, 
  giving the usual thin-airfoil lift slope  dcl/da = 2 pi.  
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) CLAF
    self.claf     = 0.0     # CLaf
  def __str__(self):
    return 'Claf: ' + str(self.claf)

class Cdcl:
  """
  CDCL                         |  (keyword)
  CL1 CD1  CL2 CD2  CL3 CD3    |  CD(CL) function parameters


  The CDCL keyword specifies a simple profile-drag CD(CL) function 
  for this section.  The function is parabolic between CL1..CL2 and 
  CL2..CL3, with rapid increases in CD below CL1 and above CL3.
  See the SUBROUTINE CDCL header (in cdcl.f) for more details.

  The CD(CL) function is interpolated for stations in between
  defining sections.  Hence, the CDCL declaration on any surface 
  must be used either for all sections or for none.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) CDCL
    self.cdcl     = [(0.,0.),(0.,0.),(0.,0.)] # CL1 CD1  CL2 CD2  CL3 CD3
  def __str__(self):
    return 'Cdcl: ' + str(self.cdcl)

class Body:
  """
  BODY                 | (keyword)
  Fuselage             | body name string
  15   1.0             | Nbody  Bspace

  The BODY keyword declares that a body is being defined until
  the next SURFACE or BODY keyword, or the end of file is reached.  
  A body is modeled with a source+doublet line along its axis,
  in accordance with slender-body theory.

    Nbody  =  number of source-line nodes
    Bspace =  lengthwise node spacing parameter (described later)
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) BODY
    self.name     = ''      # body name string
    self.nbody    = 0       # Nbody
    self.bspace   = 0.0     # Bspace
  def __str__(self):
    return 'Body: ' + self.name

class Bfile:
  """
  BFILE            |  (keyword)
  filename         | filename string

  This specifies the shape of the body as an "airfoil" file
  which gives the top or side view of the body, which is
  assumed to have a round cross-section.  Hence, the diameter
  of the body is the difference between the top and bottom 
  Y values.  Bodies which are not round must be approximated
  with an equivalent round body which has roughly the same
  cross-sectional areas. If the path/filename has embedded blanks
  double quotes should be used to delimit the string.
  """
  def __init__(self,keyword):
    self.keyword  = keyword # (keyword) BFILE
    self.filename = ''      # filename string
  def __str__(self):
    return 'Bfile: ' + self.filename
