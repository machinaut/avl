#!/usr/bin/env python
# avl-class.py - avl class definitions

class Configuration:
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
    self.surface  = [] # list of surfaces
    self.body     = [] # list of bodies
    self.comp     = [] # list of components # TODO is this redundant with surf?
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
  def __str__(self):
    return 'Section: '
