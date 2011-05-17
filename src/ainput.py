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

def read_infile(lun,infilename,ferr): #      SUBROUTINE INPUT(LUN,FNAME,FERR)
  """
  Reads an processes an AVL configuration input file
  """
# XXX no import #      INCLUDE 'AVL.INC'
#
#      CHARACTER*(*) FNAME
#      LOGICAL FERR
#
#      CHARACTER*4  KEYWD
#      CHARACTER*80 CNAME, ANAME
#      CHARACTER*128 LINE
#      LOGICAL LDUPL, LHINGE
#
#      REAL CLX(3), CDX(3)
#      REAL XYZSCAL(3), XYZTRAN(3)
#
#      PARAMETER (NWRK=NSMAX, IBX=300)
#      REAL XYZLES(3,NWRK),CHORDS(NWRK),AINCS(NWRK),SSPACES(NWRK),
#     &     BRADY(NWRK), BRADZ(NWRK), CLAF(NWRK)
#      INTEGER NSPANS(NWRK), NASEC(NWRK)
#      REAL XASEC(IBX,NWRK), SASEC(IBX,NWRK), TASEC(IBX,NWRK)
#      REAL CLCDSEC(6,NWRK)
#
#      REAL XB(IBX), YB(IBX)
#      REAL XIN(IBX), YIN(IBX), TIN(IBX)
#      REAL XBOD(IBX), YBOD(IBX), TBOD(IBX)
#
#---- max number of control or design variable declaration lines per section
#      PARAMETER (ICONX = 20)
#
#      INTEGER ICONTD(ICONX,NWRK),
#     &        NSCON(NWRK),
#     &        IDESTD(ICONX,NWRK),
#     &        NSDES(NWRK)
#
#      REAL XHINGED(ICONX,NWRK), 
#     &     VHINGED(3,ICONX,NWRK), 
#     &     GAIND(ICONX,NWRK),
#     &     REFLD(ICONX,NWRK),
#     &     GAING(ICONX,NWRK)
#
#
#      REAL    RINPUT(10)
#      INTEGER IINPUT(10)
#      LOGICAL ERROR
#
#----------------------------------------------------
#     FERR = .FALSE.
#
  # start reading file
  infile = open(infilename) #      OPEN(UNIT=LUN,FILE=FNAME,STATUS='OLD',ERR=3)
  print "Reading file:", infilename #      GO TO 6
#
 3    CONTINUE
      CALL STRIP(FNAME,NFN)
      WRITE(*,*) 
      WRITE(*,*) '** Open error on file: ', FNAME(1:NFN)
      FNAME = FNAME(1:NFN) // '.avl'
      WRITE(*,*) '   Trying alternative: ', FNAME(1:NFN+4)
      OPEN(UNIT=LUN,FILE=FNAME,STATUS='OLD',ERR=4)
      GO TO 6
#
 4    CONTINUE
      CALL STRIP(FNAME,NFN)
      WRITE(*,*) 
      WRITE(*,*) '** Open error on file: ', FNAME(1:NFN)
      FERR = .TRUE.
      RETURN
#
#----------------------------------------------------
#6    CONTINUE
#     CALL STRIP(FNAME,NFN)
#     WRITE(*,*) 
#     WRITE(*,*) 'Reading file: ', FNAME(1:NFN), '  ...'
#
#     DCL_A0 = 0.
#     DCM_A0 = 0.
#     DCL_U0 = 0.
#     DCM_U0 = 0.
#
#---- initialize all entity counters
#     NSEC = 0
#
#     NSURF = 0
#     NVOR = 0
#     NSTRIP = 0
#
#     NBODY = 0
#     NLNODE = 0
#
#     NCONTROL = 0
#     NDESIGN = 0
#
#     LVISC  = .FALSE.
#
#---- initialize counters and active-entity indicators
#     ISURF = 0
#     IBODY = 0
#
#---- initialize input-file line counter
#     ILINE = 0
#
#------------------------------------------------------------------------------
#---- start reading file
#
#---------------------------------------------------
  # AJR - quick and dirty file parser
  title = None
  mach0 = None
  iYsym, iZsym, Zsym = None, None, None
  Sref, Cref, Bref = None, None, None
  Xref, Yref, Zref = None, None, None
  CDp = None
  while(True): # you spin me right round baby right round
    line = infile.readline()
    if line.find("#") >= 0 : line = line[:line.find("#")]
    if line.find("!") >= 0 : line = line[:line.find("!")]
    line = line.strip()
    if line == "" : continue
    if title is None:
      title = line
      print "Configuration:", title
      continue
    if mach0 is None:
      mach0 = float(line)
      continue
    if iYsym is None:
      iYsym, iZsym, Zsym = [float(i) for i in line.split()]
      # y-symmetry plane hard-wired at y=0
      Ysym = 0.0
      iYsym = float(cmp(iYsym,0))
      iZsym = float(cmp(iZsym,0))
      continue
    if Sref is None:
      Sref, Cref, Bref = [float(i) for i in line.split()]
      if Sref < 0 : Sref = 1.0
      if Cref < 0 : Cref = 1.0
      if Bref < 0 : Bref = 1.0
      continue
    if Xref is None: #TODO this is all one point
      Xref, Yref, Zref = [float(i) for i in line.split()]
      continue
    if CDp is None:
      # try to read CD data which may or may not be present
      try: CDp = float(line)
      except ValueError: CDp = 0.0
      continue
    # start of keyword-interpretation loop
    keyword = line.upper()[0:4]
    if keyword == "EOF": #XXX TODO FIXME clean this up
      cleanup()
    elif keyword == "SURF":
      if iSurf != 0:
        makesurf(ISURF, IBX,NSEC, NVC, CSPACE, NVS, SSPACE,
         XYZSCAL,XYZTRAN,ADDINC, XYZLES,CHORDS,AINCS,SSPACES,NSPANS,
         XASEC,SASEC,TASEC,NASEC, CLCDSEC,CLAF,
         ICONX, ICONTD,NSCON,GAIND,XHINGED,VHINGED,REFLD,
         IDESTD,NSDES,GAING )
        if ldupl: sdupl(ISURF, YDUPL, 'YDUP') # XXX The fuck is this
        iSurf = 0
      if iBody != 0:
        makebody(IBODY, IBX, NVB, BSPACE, XYZSCAL,XYZTRAN, XBOD,YBOD,TBOD,NBOD)
        if ldupl: bdupl(IBODY,YDUPL,'YDUP') #TODO The fuck is this
        iBody = 0
#------ new surface  (ISURF.ne.0 denotes surface accumulation is active)
      nSurf = nSurf + 1 # TODO: why does this have a max?
      iSurf = nSurf if nSurf < nSmax else nSmax
      lscomp[iSurf] = iSurf
#------ default surface component index is just the surface number
      lscomp[iSurf] = iSurf # LSCOMP(ISURF) = ISURF # XXX WTF
#------ clear indices for accumulation
      nsec = 0 # NSEC = 0
      isec = 0 # ISEC = 0
#------ set surface defaults
      ldupl = False  #  LDUPL  = .FALSE.   
      lhinge = False #  LHINGE = .FALSE.
#------ assume this will be a conventional loaded surface
      lfwake[iSurf] = True #  LFWAKE(ISURF) = .TRUE.
      lfalbe[iSurf] = True #  LFALBE(ISURF) = .TRUE.
      lfload[iSurf] = True #  LFLOAD(ISURF) = .TRUE.

      xyzscal = [1.,1.,1.] #XYZSCAL(1)=1.0 XYZSCAL(2)=1.0 XYZSCAL(3)=1.0
      xyztran = [0.,0.,0.] #XYZTRAN(1) = 0. XYZTRAN(2) = 0. XYZTRAN(3) = 0.
      addinc = 0. # ADDINC = 0.
      ncvar = 0 # NCVAR = 0
      # XXX TODO move this to relevant section
      # parse surface
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        STITLE(ISURF) = LINE(1:NLINE)
        WRITE(*,*)
        WRITE(*,*) '  Building surface: ', STITLE(ISURF)
#
        CALL RDLINE(LUN,LINE,NLINE,ILINE)
        NINPUT = 4
        CALL GETFLT(LINE,RINPUT,NINPUT,ERROR)
        IF(ERROR .OR. NINPUT.LT.2) GO TO 990
#
        NVC = INT( RINPUT(1) + 0.001 )
        CSPACE = RINPUT(2)
#
        IF(NINPUT.GE.4) THEN
         NVS = INT( RINPUT(3) + 0.001 )
         SSPACE = RINPUT(4)
        ELSE
         NVS = 0
         SSPACE = 0.0
        ENDIF
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


