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

config = None #global flight configuration

class Configuration:
  def __init__(self,name,mach,iysym,izsym,zsym,sref,cref,bref,xref,cdp):
    self.name   = name
    self.mach   = mach
    self.iysym  = iysym
    self.izsym  = izsym
    self.ysym   = 0.0
    self.zsym   = zsym
    self.sref   = sref
    self.cref   = cref
    self.bref   = bref
    self.xref   = xref
    self.cdp    = cdp
    self.comp   = {None:[]} # None is a list of individual surfaces
    self.body   = []

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
    self.xle      = xle
    self.chord    = chord
    self.ainc     = ainc
    self.nspan    = nspan
    self.sspace   = sspace
    self.airfoil  = None

class Body:
  def __init__(self,name,nbody,bspace):
    self.name       = name
    self.nbody      = nbody
    self.bspace     = bspace
    self.ydup       = None
    self.scale      = [1.0, 1.0, 1.0]
    self.trans      = [0.0, 0.0, 0.0]
    self.bfile      = None

def read_infile(lun,infilename,ferr):
  """
  Reads an processes an AVL configuration input file
  """
  infile = open(infilename)
  print "Reading file:", infilename
  # AJR - quick and dirty file parser
  def getline(ainfile):
    aline = ''
    while aline == '':
      aline = ainfile.readline()
      if aline.find("#") >= 0 : aline = aline[:aline.find("#")]
      if aline.find("!") >= 0 : aline = aline[:aline.find("!")]
      aline = aline.strip()
    return aline
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
  line = getline()
  try: CDp = float(line)
  except ValueError: CDp = 0.0; line = getline()
  global config # flight configuration
  config = Configuration(name,mach,iYsym,iZsym,Zsym,Sref,Cref,Bref,Xref,CDp)
  # loop, parsing 'surfaces' and 'bodies'
  while(True): # you spin me right round baby right round
    # TODO this goes at the end now line = infile.readline()
    # start of keyword-interpretation loop
    keyword = line.upper().split()[0][0:4]
    if   keyword == 'EOF': #XXX TODO FIXME clean this up
      cleanup()
    elif keyword == 'SURF':
      name = getline()
      l = getline().split()
      Nchord, Cspace = int(l[0]),float(l[1])
      if len(l) < 4: Nspan, Sspace = None, None
      else: Nspan, Sspace = int(l[2]),float(l[3])
      surf = Surface(name,Nchord,Cspace,Nspan,Sspace)
      while(True): # loop over surface parameters
        keyword = getline().upper().split()[0][0:4]
        if   keyword == 'YDUP': surf.ydup = float(getline()) ; continue
        elif keyword == 'COMP': surf.comp = int(getline()) ; continue
        elif keyword == 'INDE': surf.comp = int(getline()) ; continue
        elif keyword == 'SCAL':
          surf.scale = [float(i) for i in getline()] ; continue
        elif keyword == 'TRAN':
          surf.trans = [float(i) for i in getline()] ; continue
        elif keyword == 'ANGL': surf.angle = float(getline()) ; continue
        elif keyword == 'NOWA': surf.wake = False ; continue
        elif keyword == 'NOAL': surf.albe = False ; continue
        elif keyword == 'NOLO': surf.load = False ; continue
        elif keyword == 'SECT':
          l = getline().split()
          Xle = [float(i) for i in l[0:3]]
          Chord, Ainc = float(l[3]),float(l[4])
          if len(l) < 7: Nspan, Sspace = 0, 0.0
          else: Nspan, Sspace = float(l[5]), float(l[6])
          sect = Section(Xle,Chord,Ainc,Nspan,Sspace)
          while(True):
            keyword = getline().upper().split()[0][0:4]
            if   keyword == 'NACA': sect
# XXX done for the night here, need to add airfoil parsing
          surf.sect.append(sect)
      config.comp[surf.comp].append(surf)
    elif keyword == 'BODY':
      name = getline()
      l = getline().split()
      Nbody,Bspace = int(l[0]),float(l[1])
      body = Body(name,Nbody,Bspace)
      config.body.append(body)
    line = infile.readline()
  # end while(True)
#
#===========================================================================
      ELSEIF(KEYWD.EQ.'BODY') THEN
#------ new body is about to start
#
        IF(ISURF.NE.0) THEN
#------- "old" surface is still active, so build it before starting new one
         CALL MAKESURF(ISURF, IBX,NSEC, 
     &       NVC, CSPACE, NVS, SSPACE,
     &       XYZSCAL,XYZTRAN,ADDINC,
     &       XYZLES,CHORDS,AINCS,SSPACES,NSPANS,
     &       XASEC,SASEC,TASEC,NASEC,
     &       CLCDSEC,CLAF,
     &       ICONX, 
     &       ICONTD,NSCON,GAIND,XHINGED,VHINGED,REFLD,
     &       IDESTD,NSDES,GAING )
#
         IF(LDUPL) THEN
          CALL SDUPL(ISURF,YDUPL,'YDUP')
         ENDIF
#
         ISURF = 0
        ENDIF
#
        IF(IBODY.NE.0) THEN
#------- "old" body is still active, so build it before finishing
         CALL MAKEBODY(IBODY, IBX,
     &       NVB, BSPACE,
     &       XYZSCAL,XYZTRAN,
     &       XBOD,YBOD,TBOD,NBOD)
#
         IF(LDUPL) THEN
          CALL BDUPL(IBODY,YDUPL,'YDUP')
         ENDIF
#
         IBODY = 0
        ENDIF
#
#------ new body  (IBODY.ne.0 denotes body accumulation is active)
        NBODY = NBODY + 1
        IBODY = MIN( NBODY , NBMAX )
#
        NSEC = 0
        ISEC = 0
        NBOD = 0
#
        NIN = 0
#
        LDUPL  = .FALSE.
#
        XYZSCAL(1) = 1.0
        XYZSCAL(2) = 1.0
        XYZSCAL(3) = 1.0
        XYZTRAN(1) = 0.
        XYZTRAN(2) = 0.
        XYZTRAN(3) = 0.
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        BTITLE(IBODY) = LINE(1:NLINE)
        WRITE(*,*)
        WRITE(*,*) '  Building body: ', BTITLE(IBODY)
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ(LINE,*,ERR=990) NVB, BSPACE
#
#===========================================================================
      ELSEIF(KEYWD.EQ.'YDUP') THEN
#------ this surface is to be duplicated with an image surface
        IF    (ISURF.NE.0) THEN
#C         WRITE(*,*) '  + duplicate surface ',STITLE(ISURF)
        ELSEIF(IBODY.NE.0) THEN
#C         WRITE(*,*) '  + duplicate body ',BTITLE(IBODY)
        ELSE
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active surface or body to duplicate'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ (LINE,*,ERR=990) YDUPL
#
        LDUPL = .TRUE.
#
        IF(IYSYM.NE.0) THEN
         WRITE(*,*)'** Warning: Y-duplicate AND Y-sym specified'
        ENDIF
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'INDE' .OR. KEYWD.EQ.'COMP') THEN
#------ set component index for surface (may be lumped together)
        IF(ISURF.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active surface for component index'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ (LINE,*,ERR=990) LSCOMP(ISURF)
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'SCAL') THEN
#------ read scaling factors
        IF(ISURF.EQ.0 .AND. IBODY.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active surface or body for scaling'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ (LINE,*,ERR=990) XYZSCAL(1), XYZSCAL(2), XYZSCAL(3)
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'TRAN') THEN
#------ read translation vector
        IF(ISURF.EQ.0 .AND. IBODY.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active surface or body for translation'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ (LINE,*,ERR=990) XYZTRAN(1), XYZTRAN(2), XYZTRAN(3)
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'ANGL') THEN
#------ read surface angle change
        IF(ISURF.EQ.0 .AND. IBODY.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active surface or body for rotation'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ (LINE,*,ERR=990) ADDINC
        ADDINC = ADDINC*DTR
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'NOWA') THEN
#------ disable wake shedding for this surface
        IF(ISURF.EQ.0) THEN
         WRITE(*,9000)'** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)   '** No active surface for wake-shedding flag'
         GO TO 10
        ENDIF
#
        LFWAKE(ISURF) = .FALSE.

#===========================================================================
      ELSEIF (KEYWD.EQ.'NOAL') THEN
#------ disable freestream angles for this surface
        IF(ISURF.EQ.0) THEN
         WRITE(*,9000)'** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)   '** No active surface for freestream-angles flag'
         GO TO 10
        ENDIF
#
        LFALBE(ISURF) = .FALSE.

#===========================================================================
      ELSEIF (KEYWD.EQ.'NOLO') THEN
#------ disable total-load contributions for this surface
        IF(ISURF.EQ.0) THEN
         WRITE(*,9000)'** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)   '** No active surface for load-disable flag'
         GO TO 10
        ENDIF
#
        LFLOAD(ISURF) = .FALSE.

#===========================================================================
      ELSEIF (KEYWD.EQ.'BSEC') THEN
#------ read body section
#
        IF(IBODY.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active body for this section'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#
#------ store section data for current body
        NSEC = NSEC + 1
        ISEC = MIN( NSEC , NWRK )
#
        NINPUT = 5
        CALL GETFLT(LINE,RINPUT,NINPUT,ERROR)
        IF(ERROR .OR. NINPUT.LT.4) GO TO 990
#
        XYZLES(1,ISEC) = RINPUT(1)
        XYZLES(2,ISEC) = RINPUT(2)
        XYZLES(3,ISEC) = RINPUT(3)
        BRADY(ISEC) = RINPUT(4)
#
        IF(NINPUT.GE.5) THEN
         BRADZ(ISEC) = RINPUT(5)
        ELSE
         BRADZ(ISEC) = BRADY(ISEC)
        ENDIF
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'SECT') THEN
#------ read surface section
#
        IF(ISURF.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active surface for this section'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#
#------ store section data for current surface
        NSEC = NSEC + 1
        ISEC = MIN( NSEC , NWRK )
#
        NINPUT = 7
        CALL GETFLT(LINE,RINPUT,NINPUT,ERROR)
        IF(ERROR .OR. NINPUT.LT.5) GO TO 990
#
        XYZLES(1,ISEC) = RINPUT(1)
        XYZLES(2,ISEC) = RINPUT(2)
        XYZLES(3,ISEC) = RINPUT(3)
        CHORDS(ISEC) = RINPUT(4)
        AINCS(ISEC)  = RINPUT(5)*DTR
#
        IF(NINPUT.GE.7) THEN
         NSPANS(ISEC) = INT( RINPUT(6) + 0.001 )
         SSPACES(ISEC) = RINPUT(7)
        ELSE
         NSPANS(ISEC) = 0
         SSPACES(ISEC) = 0.
        ENDIF
#
#------ default section...
#    ...flat camberline
        NASEC(ISEC)   = 2
        XASEC(1,ISEC) = 0.0
        XASEC(2,ISEC) = 1.0
        SASEC(1,ISEC)  = 0.0
        SASEC(2,ISEC)  = 0.0
        TASEC(1,ISEC)  = 0.0
        TASEC(2,ISEC)  = 0.0
#    ...no polar data
        DO L=1, 6
          CLCDSEC(L,ISEC) = 0.0
        END DO
#    ...no control
        NSCON(ISEC) = 0
#    ...no design
        NSDES(ISEC) = 0
#
#    ...unity dCL/da factor
        CLAF(ISEC) = 1.0
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'NACA') THEN 
#------ input NACA camberline
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for this airfoil'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#
        IB = INDEX(LINE,' ')
        READ(LINE(1:IB-1),*,ERR=990) IDES
        IF(LINE(IB:NLINE).NE.' ') THEN
         READ(LINE(IB:NLINE),*,ERR=990) XFMIN, XFMAX
ccc           WRITE(*,*) '   Using data in normalized range ',XFMIN,XFMAX
        ELSE
         XFMIN = 0.
         XFMAX = 1.
        ENDIF
#
        ICAM = IDES/1000
        IPOS = (IDES-1000*ICAM)/100
        ITHK =  IDES-1000*ICAM-100*IPOS
        C = FLOAT(ICAM) / 100.0
        P = FLOAT(IPOS) / 10.0
        T = FLOAT(ITHK) / 100.0
#
        NASEC(ISEC) = MIN( 50 , IBX )
        DO I = 1, NASEC(ISEC)
          XF = XFMIN + (XFMAX-XFMIN)*FLOAT(I-1)/FLOAT(NASEC(ISEC)-1)
          IF(XF.LT.P) THEN
           SLP = C/P**2 * 2.0*(P - XF)
          ELSE
           SLP = C/(1.0-P)**2 * 2.0*(P - XF)
          ENDIF
          THK = (0.29690*SQRT(XF)
     &          - 0.12600*XF
     &          - 0.35160*XF**2
     &          + 0.28430*XF**3
     &          - 0.10150*XF**4) * T / 0.10
#
          XASEC(I,ISEC) = XF
          SASEC(I,ISEC) = SLP
          TASEC(I,ISEC) = THK
        ENDDO
        CALL NRMLIZ(NASEC(ISEC),XASEC(1,ISEC))
#
#===========================================================================
      ELSE IF (KEYWD.EQ.'AIRF') THEN 
#------ input y(x) for an airfoil, get camber then slopes via spline
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for this airfoil'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#
        IB = INDEX(LINE,' ')
        READ(LINE(IB:NLINE),*,ERR=990) XFMIN, XFMAX
#
        DO I = 1, 123456
          IB = MIN(I,IBX)
#
          CALL RDLINE(LUN,LINE,NLINE,ILINE)
          NINPUT = 2
          CALL GETFLT(LINE,RINPUT,NINPUT,ERROR)
          IF(ERROR .OR. NINPUT.LT.2) THEN
           NB = IB-1
           GO TO 40
          ELSE
           XB(IB) = RINPUT(1)
           YB(IB) = RINPUT(2)
          ENDIF
        ENDDO
#
 40     CONTINUE
        IF(I.GT.IBX) THEN
         WRITE(*,*) 
     &    '*** AINPUT: Airfoil array overflow.  Increase IBX to', I
         STOP
        ENDIF
#
#------ set camber and thickness, normalized to unit chord
        NIN = MIN( 50 , IBX )
        CALL GETCAM(XB,YB,NB,XIN,YIN,TIN,NIN,.TRUE.)
#
#------ store airfoil only if surface and section are active
        NASEC(ISEC) = NIN
        DO I = 1, NIN
          XF = XFMIN + (XFMAX-XFMIN)*FLOAT(I-1)/FLOAT(NASEC(ISEC)-1)
          XASEC(I,ISEC) = XIN(1) + XF*(XIN(NIN)-XIN(1))
          CALL AKIMA(XIN,YIN,NIN,XASEC(I,ISEC),DUMMY,SASEC(I,ISEC))
          CALL AKIMA(XIN,TIN,NIN,XASEC(I,ISEC),TASEC(I,ISEC),DUMMY)
        END DO
        CALL NRMLIZ(NASEC(ISEC),XASEC(1,ISEC))
#
#------ go to top of keyword-reading loop, with last-read line
        GO TO 11
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'AFIL') THEN 
#------ input y(x) from an airfoil coordinate file
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for this airfoil'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#---- parse file name and optional parameters
#     double quotes checked to delimit file name to allow blanks in name 
        IDQ1 = INDEX(LINE,'"')
        IF(IDQ1.NE.0) THEN
         IDQ2 = INDEX(LINE(IDQ1+1:),'"')
         IF(IDQ2.GT.1) THEN 
           CNAME = LINE(IDQ1+1:IDQ2+IDQ1-1)
           IB = IDQ2 + IDQ1 + 1
         ELSE
           WRITE(*,9000) '** Bad quotes in file name ',ILINE,
     &                   LINE(1:NLINE)
           GO TO 10
         ENDIF
        ELSE
#---- Find blank after filename as delimiter for optional parameters
         IB = INDEX(LINE,' ')
         CNAME = LINE(1:IB)
        ENDIF
#
        IF(LINE(IB:NLINE).NE.' ') THEN
         READ(LINE(IB:NLINE),*,ERR=990) XFMIN, XFMAX
#CC         WRITE(*,*) '     Using data in normalized range ',XFMIN,XFMAX
        ELSE
         XFMIN = 0.
         XFMAX = 1.
        ENDIF
#
        CALL STRIP(CNAME,NCN)
        WRITE(*,*) '    Reading airfoil from file: ',CNAME(1:NCN)
        NBLDS = 1
        CALL READBL(CNAME,IBX,NBLDS,XB,YB,NB,NBL,
     &               ANAME,XINL,XOUT,YBOT,YTOP)
#
        IF(NBL.EQ.0) THEN
         WRITE(*,*) '**   Airfoil file not found  : ',CNAME(1:NCN)
         WRITE(*,*) '**   Using default zero-camber airfoil'
#
         NASEC(ISEC) = MIN( 50 , IBX )
         DO I = 1, NASEC(ISEC)
          XASEC(I,ISEC) = FLOAT(I-1)/FLOAT(NASEC(ISEC)-1)
          SASEC(I,ISEC) = 0.0
          TASEC(I,ISEC) = 0.0
         ENDDO
#
        ELSE
#------- camber and thickness
         NIN = MIN( 50 , IBX )
         CALL GETCAM(XB,YB,NB,XIN,YIN,TIN,NIN,.TRUE.)
#
#------- camberline slopes at specified locations from spline
         NASEC(ISEC) = NIN
         DO I = 1, NIN
           XF = XFMIN + (XFMAX-XFMIN)*FLOAT(I-1)/FLOAT(NASEC(ISEC)-1)
           XASEC(I,ISEC) = XIN(1) + XF*(XIN(NIN)-XIN(1))
           CALL AKIMA(XIN,YIN,NIN,XASEC(I,ISEC),DUMMY,SASEC(I,ISEC))
           CALL AKIMA(XIN,TIN,NIN,XASEC(I,ISEC),TASEC(I,ISEC),DUMMY)
         END DO
         CALL NRMLIZ (NASEC(ISEC),XASEC(1,ISEC))
        ENDIF
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'BFIL') THEN 
#------ input r(x) from an airfoil coordinate file
#
        IF(IBODY.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active body for this shape'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#---- parse file name and optional parameters
#     double quotes checked to delimit file name to allow blanks in name 
        IDQ1 = INDEX(LINE,'"')
        IF(IDQ1.NE.0) THEN
         IDQ2 = INDEX(LINE(IDQ1+1:),'"')
         IF(IDQ2.GT.1) THEN 
           CNAME = LINE(IDQ1+1:IDQ2+IDQ1-1)
           IB = IDQ2 + IDQ1 + 1
         ELSE
           WRITE(*,9000) '** Bad quotes in file name ',ILINE,
     &                   LINE(1:NLINE)
           GO TO 10
         ENDIF
        ELSE
#---- Find blank after filename as delimiter for optional parameters
         IB = INDEX(LINE,' ')
         CNAME = LINE(1:IB)
        ENDIF
#
        IF(LINE(IB:NLINE).NE.' ') THEN
         READ(LINE(IB:NLINE),*,ERR=990) XFMIN, XFMAX
#CC         WRITE(*,*) '     Using data in normalized range ',XFMIN,XFMAX
        ELSE
         XFMIN = 0.
         XFMAX = 1.
        ENDIF
#
        CALL STRIP(CNAME,NCN)
        WRITE(*,*) '    Reading body shape from file: ',CNAME(1:NCN)
        NBLDS = 1
        CALL READBL(CNAME,IBX,NBLDS,XB,YB,NB,NBL,
     &               ANAME,XINL,XOUT,YBOT,YTOP)
#
#------ set thread line y, and thickness t ( = 2r)
        NBOD = MIN( 50 , IBX )
        CALL GETCAM(XB,YB,NB,XBOD,YBOD,TBOD,NBOD,.FALSE.)
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'CDCL') THEN 
#------ input approximate CD(CL) polar defining data
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for this polar'
         GO TO 10
        ENDIF

        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ(LINE,*,ERR=990) CLX(1),CDX(1),CLX(2),CDX(2),CLX(3),CDX(3)
#
        LMAX = 1
        LMIN = 1
        DO L = 2, 3 
          IF(CLX(L).GT.CLX(LMAX)) LMAX = L
          IF(CLX(L).LT.CLX(LMIN)) LMIN = L
        END DO
#
        IF(ISEC.GT.1) THEN
         IF(CLCDSEC(4,ISEC-1).LE.0.0) THEN
          WRITE(*,*) '* AINPUT: previous section defined with no polar' 
         ENDIF
        ENDIF
#
#------ Trick: sum must be 6 so we can get the "other" index
        LMID = 6 - (LMIN+LMAX)
        CLCDSEC(1,ISEC) = CLX(LMIN)
        CLCDSEC(2,ISEC) = CDX(LMIN)
        CLCDSEC(3,ISEC) = CLX(LMID)
        CLCDSEC(4,ISEC) = CDX(LMID)
        CLCDSEC(5,ISEC) = CLX(LMAX)
        CLCDSEC(6,ISEC) = CDX(LMAX)
        WRITE(*,1700) CLX(LMIN),CDX(LMIN),
     &                CLX(LMID),CDX(LMID),
     &                CLX(LMAX),CDX(LMAX)
 1700   FORMAT('    Reading CD(CL) data for section',
     &         /'     CLneg    = ',F8.3,'  CD@CLneg = ',F10.5,
     &         /'     CL@CDmin = ',F8.3,'  CDmin    = ',F10.5,
     &         /'     CLpos    = ',F8.3,'  CD@CLpos = ',F10.5)
        LVISC = .TRUE.
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'CLAF') THEN 
#------ input dCL/da scaling factor
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for dCL/da factor'
         GO TO 10
        ENDIF

        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        READ(LINE,*,ERR=990) CLAF(ISEC)
#
        IF(CLAF(ISEC) .LE. 0.0 .OR. 
     &     CLAF(ISEC) .GE. 2.0      ) THEN
         WRITE(*,*) '** dCL/da factor must be in the range 0..2 --',
     &              ' Setting factor to 1.0'
         CLAF(ISEC) = 1.0
        ENDIF
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'CONT') THEN
#------ link section to control variables
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for this control'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#
#
#------ increment control-declaration counter for this section
        NSCON(ISEC) = NSCON(ISEC) + 1
        ISCON = MIN( NSCON(ISEC) , ICONX )
#
#------ extract control name
        NNAME = INDEX(LINE,' ') - 1
        IF(NNAME.LE.0) THEN
         WRITE(*,*) '** Bad control declaration line:  ', LINE
         STOP
        ENDIF
#
#------ see if this control variable has already been declared
        DO N = 1, NCONTROL
          IF(LINE(1:NNAME) .EQ. DNAME(N)(1:NNAME)) THEN
           ICONTROL = N
           GO TO 62
          ENDIF
        ENDDO
#
#------ new control variable... assign slot for it
        NCONTROL = NCONTROL + 1
        ICONTROL = MIN( NCONTROL , NDMAX )
        DNAME(ICONTROL) = LINE(1:NNAME)
#
 62     CONTINUE
        ICONTD(ISCON,ISEC) = ICONTROL
#
#------ read numbers after control variable name
        NINPUT = 6
        CALL GETFLT(LINE(NNAME+1:120),RINPUT,NINPUT,ERROR)
        IF(ERROR) THEN
         WRITE(*,*) '*** Bad control data line:  ', LINE
         STOP
        ENDIF
#
        IF(NINPUT.LT.1) THEN
         GAIND(ISCON,ISEC) = 1.0
        ELSE
         GAIND(ISCON,ISEC) = RINPUT(1)
        ENDIF
#
        IF(NINPUT.LT.2) THEN
         XHINGED(ISCON,ISEC) = 0.0
        ELSE
         XHINGED(ISCON,ISEC) = RINPUT(2)
        ENDIF
#
        IF(NINPUT.LT.5) THEN
         VHINGED(1,ISCON,ISEC) = 0.0
         VHINGED(2,ISCON,ISEC) = 0.0
         VHINGED(3,ISCON,ISEC) = 0.0
        ELSE
         VHINGED(1,ISCON,ISEC) = RINPUT(3)
         VHINGED(2,ISCON,ISEC) = RINPUT(4)
         VHINGED(3,ISCON,ISEC) = RINPUT(5)
        ENDIF
#
        IF(NINPUT.LT.6) THEN
         REFLD(ISCON,ISEC) = 1.0
        ELSE
         REFLD(ISCON,ISEC) = RINPUT(6)
        ENDIF
#
#===========================================================================
      ELSEIF (KEYWD.EQ.'DESI') THEN 
#------ link section to design variable and weight
#
        IF(ISURF.EQ.0 .OR. ISEC.EQ.0) THEN
         WRITE(*,9000) '** Misplaced line', ILINE, LINE(1:NLINE)
         WRITE(*,*)    '** No active section for this design var.'
         GO TO 10
        ENDIF
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
#
#------ increment design-declaration counter for this section
        NSDES(ISEC) = NSDES(ISEC) + 1
        ISDES = MIN( NSDES(ISEC) , ICONX )
#
#------ extract design name
        NNAME = INDEX(LINE,' ') - 1
        IF(NNAME.LE.0) THEN
         WRITE(*,9000) '   *** Bad design declaration line', 
     &                  ILINE, LINE(1:NLINE)
         STOP
        ENDIF
#
#------ see if this control variable has already been declared
        DO K = 1, NDESIGN
          IF(LINE(1:NNAME) .EQ. GNAME(K)(1:NNAME)) THEN
           IDESIGN = K
           GO TO 72
          ENDIF
        ENDDO
#
        NDESIGN = NDESIGN + 1
        IDESIGN = MIN( NDESIGN , NGMAX )
        GNAME(IDESIGN) = LINE(1:NNAME)
#
 72     CONTINUE
        IDESTD(ISDES,ISEC) = IDESIGN
#
#------ read numbers after control variable name
        NINPUT = 1
        CALL GETFLT(LINE(NNAME+1:120),RINPUT,NINPUT,ERROR)
        IF(ERROR) GO TO 990
#
        IF(NINPUT.LT.1) THEN
         GAING(ISDES,ISEC) = 1.0
        ELSE
         GAING(ISDES,ISEC) = RINPUT(1)
        ENDIF
#
#===========================================================================
      ELSE
#------ line not recognized or unassignable ... keep reading file
        WRITE(*,8000) ILINE, LINE(1:NLINE)
 8000   FORMAT('  * Line',I5,' ignored: ', A)
#
      ENDIF
      GO TO 10
#
#===========================================================================
#---- normal end-of-file exit point
 900  CONTINUE
      CLOSE(UNIT=LUN)
#
#*********************************************************************
#
      WRITE (*,2018) MACH0,NBODY,NSURF,NSTRIP,NVOR
#
      IF(IYSYM.GT.0) WRITE (*,2024) YSYM
      IF(IYSYM.LT.0) WRITE (*,2025) YSYM
      IF(IZSYM.GT.0) WRITE (*,2026) ZSYM
      IF(IZSYM.LT.0) WRITE (*,2027) ZSYM
#
 2018 FORMAT (/' Mach =',F10.4,'  (default)'
     &        /' Nbody =',I4,5X,
     &         ' Nsurf =',I4,5X,
     &         ' Nstrp =',I4,5X,
     &         ' Nvor  =',I4)
 2024 FORMAT (/' Y Symmetry: Wall plane   at Ysym =',F10.4)
 2025 FORMAT (/' Y Symmetry: Free surface at Ysym =',F10.4)
 2026 FORMAT (/' Z Symmetry: Ground plane at Zsym =',F10.4)
 2027 FORMAT (/' Z Symmetry: Free surface at Zsym =',F10.4)
#
      LGEO = .TRUE.
      RETURN
#
#*********************************************************************
 990  CONTINUE
      WRITE(*,9000) '** Read error on line', ILINE, LINE(1:NLINE)
      FERR = .TRUE.
      RETURN
#
 9000 FORMAT(/ 1X,A,I5,' ...' / 1X,A)
      END ! INPUT

#XXX TODO FIXME rest of that cleanup routine
def cleanup():
      IF    (KEYWD.EQ.'EOF ') THEN
#------ end of file... clean up loose ends
#
        IF(ISURF.NE.0) THEN
#------- "old" surface is still active, so build it before finishing
         CALL MAKESURF(ISURF, IBX,NSEC, 
     &       NVC, CSPACE, NVS, SSPACE,
     &       XYZSCAL,XYZTRAN,ADDINC,
     &       XYZLES,CHORDS,AINCS,SSPACES,NSPANS,
     &       XASEC,SASEC,TASEC,NASEC,
     &       CLCDSEC,CLAF,
     &       ICONX, 
     &       ICONTD,NSCON,GAIND,XHINGED,VHINGED,REFLD,
     &       IDESTD,NSDES,GAING )
#
         IF(LDUPL) THEN
          CALL SDUPL(ISURF,YDUPL,'YDUP')
         ENDIF
#
         ISURF = 0
        ENDIF
#
        IF(IBODY.NE.0) THEN
#------- "old" body is still active, so build it before finishing
         CALL MAKEBODY(IBODY, IBX,
     &       NVB, BSPACE,
     &       XYZSCAL,XYZTRAN,
     &       XBOD,YBOD,TBOD,NBOD)
#
         IF(LDUPL) THEN
          CALL BDUPL(IBODY,YDUPL,'YDUP')
         ENDIF
#
         IBODY = 0
        ENDIF
#
#------ go finish up
        GO TO 900
#



      SUBROUTINE RDLINE(LUN,LINE,NLINE,ILINE)
#-----------------------------------------------------------------------
#     Reads next non-comment line from logical unit LU
#     Strips off leading blanks
#     Ignores everything after and including "!"
#
#     LINE returns the line
#     NLINE returns the number of characters in non-blank portion
#
#     If e.o.f. is reached, LINE returns 'EOF'
#     If read error occurs, LINE returns 'ERR'
#-----------------------------------------------------------------------
      CHARACTER*(*) LINE
#
 1000 FORMAT(A)
   20 READ (LUN,1000,END=80,ERR=90) LINE
      ILINE = ILINE + 1
#
#---- skip comment line
      IF(INDEX('!#',LINE(1:1)) .NE. 0) GO TO 20
#
#---- skip blank line
      IF(LINE.EQ.' ') GO TO 20
#
#---- strip off leading blanks and do normal return after significant line
      CALL STRIP(LINE,NLINE)
      KEXL = INDEX(LINE(1:NLINE),'!')
      IF(KEXL.GT.1) NLINE = KEXL-1
      RETURN
#
   80 LINE = 'EOF '
      RETURN
#
   90 LINE = 'ERR '
      RETURN
      END


      SUBROUTINE TOUPER(INPUT)
      CHARACTER*(*) INPUT
#
      CHARACTER*26 LCASE, UCASE
      DATA LCASE / 'abcdefghijklmnopqrstuvwxyz' /
      DATA UCASE / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
#
      N = LEN(INPUT)
#
      DO 10 I=1, N
        K = INDEX( LCASE , INPUT(I:I) )
        IF(K.GT.0) INPUT(I:I) = UCASE(K:K)
 10   CONTINUE
#
      RETURN
      END 


