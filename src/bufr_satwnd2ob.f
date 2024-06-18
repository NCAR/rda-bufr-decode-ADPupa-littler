      PARAMETER (MXMN = 10)
      PARAMETER (MXLV = 255)

      REAL*8  idarr(MXMN, MXLV), instarr(MXMN, MXLV),
     +        locarr(MXMN, MXLV), locarr2(MXMN, MXLV), 
     +        obsarr(MXMN, MXLV) 

      CHARACTER*40 idstr,instr,lstr,lstr2,obstr

c BUFR mnemonics
      DATA idstr/'SAID RPID                               '/ 
      DATA instr/'SIID SCLF SIDP SWCM                     '/
      DATA lstr /'YEAR MNTH DAYS HOUR MINU                '/
      DATA lstr2/'CLAT CLON CLATH CLONH                   '/
      DATA obstr/'TMDBST PRLC WDIR WSPD                   '/

      PARAMETER (iu=9, iou=10, lunit=11)

      real*8 dumm
      PARAMETER (dumm=9999999.99999)

      INTEGER year,month,days,hour
      real said,lat,lon,selv
      real pr,tt,td,wdir,wspd
      character*40 rpid
      INTEGER nlevi, nlevl, nlevo, nlev
      INTEGER irec, isub

      integer xht, i, iargc, n, k
      character*30 fin, fout
      character*10  date_tag, date
      character*6 dname
      character argv*300, minute*2, mins*2
      character*12 ilev
      real wlon, elon, slat, nlat

      CHARACTER  csubset*8, inf*200, outstg*200
      INTEGER    y, z, idate, iflag

      INTEGER lun, il, im
      INTEGER saidval
      CHARACTER*40 csad
      CHARACTER*80 csadstr
      CHARACTER*40 satname, satid, satwndsource

      integer iogce, mtyp, msbt, lcmmsbt, iermsbt
      character*80 cmmsbt

C*-----------------------------------------------------------------------
c*    Read the command-line arguments
c*      
      n = iargc()
      IF (n .GE. 2) THEN
        call getarg( 1, argv )
        inf=argv
        call getarg(2,argv)
        date_tag=argv
        IF (n .eq. 6) THEN  ! User-specified lat/lon boundaries
          call getarg(3,argv)
          read(argv,*) wlon
          call getarg(4,argv)
          read(argv,*) elon
          call getarg(5,argv)
          read(argv,*) slat
          call getarg(6,argv)
          read(argv,*) nlat
          write(*,*) 'Lon/lat boundaries: ',wlon,elon,slat,nlat
        ELSE  ! Default lon/lat boundaries
          slat = -90.
          nlat = 90.
          wlon = -180.
          elon = 180.
        END IF
      ELSE
        write(*,*) 'Usage: bufr_satwnd2ob.x gdas.satwnd.t<HH>z.
     +<YYYYMMDD>.bufr <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
        STOP
      END IF

C*-----------------------------------------------------------------------

C*    Open BUFR input file
      OPEN (UNIT=lunit, FILE=inf, form='unformatted')

      write(dname, '(A6)') 'SATOB'

C*    Open output file
      fout= "satwnd"//date_tag//'.obs'
      open(iou,file=fout,status='unknown',form='formatted')
      write(iou,fmt='(a10)') date_tag

      write(satwndsource, '(A40)') 'NCEP GDAS BUFR SATWND observations'

      iflag=0
      nlev=1

      isurf = 0
      ibogus = 0
      selv = dumm
      dslp = dumm
      date='MMMMMMMMMM'
      mins='MM'
      lat=dumm
      lon=dumm
      pr=dumm
      tt=dumm
      td=dumm
      wdir=dumm
      wspd=dumm

C* Connect BUFR file to the BUFRLIB software for input operations.
      CALL OPENBF(lunit, 'IN', lunit)

c     Include code and flag table information from master BUFR tables
      CALL codflg('Y')

C*    Specify format of IDATE values returned by BUFRLIB
C*    (YYYYMMDDHHMM)
      CALL DATELEN(10)
     
C*-----------------------------------------------------------------------
C*    Loop through BUFR subsets

      DO WHILE(.true.)

c       Get file ID (lun) associated with the BUFR file
        CALL status(lunit, lun, il, im)

        CALL READNS(lunit, csubset, idate, ierr)

        IF (ierr .eq.  -1 ) THEN
          write(*,*) '[bufr_satwnd2ob]....all records read, Exit'
          CALL CLOSBF(lunit)
          goto 2000 
        END IF

c Get current message and data subset number
        CALL UFBCNT(lunit, irec, isub)

c        print'(''MESSAGE: '',A8,2(2X,I6),i12 )',
c     +           csubset,irec,isub,idate

        write(date, '(I10)') idate

c Get data local subtype
        write(cmmsbt, '(A40)') repeat(' ', 40)
        write(satname, '(A40)') repeat(' ', 40)
        iogce = iupvs01(lunit, 'OGCE')
        mtyp = iupvs01(lunit, 'MTYP')
        msbt = iupvs01(lunit, 'MSBT')
        call getcfmng(lunit, 'TABLASL', msbt, 'TABLAT', mtyp, 
     +                cmmsbt, lcmmsbt, iermsbt)

        if (iermsbt .eq. 0) then
           write(satname, '(A40)') cmmsbt(1:lcmmsbt)
        else
           write (satname, '(A40)') 'BUFR MESSAGE TYPE '//csubset
        end if

C*      Read data values into arrays
        CALL UFBINT(lunit, idarr, MXMN, MXLV, nlevi, idstr)
        CALL UFBINT(lunit, locarr, MXMN, MXLV, nlevl, lstr)
        CALL UFBINT(lunit, locarr2, MXMN, MXLV, nlevl, lstr2)
        CALL UFBINT(lunit, obsarr, MXMN, MXLV, nlevo, obstr)

        if(nlevi .ne. nlevl .or. nlevi .ne. nlevo) then
           stop 'nlevi <> nlevl/o'
        else
           nlev=nlevi
        endif

        if (ibfms(locarr(5,1)) .eq. 1) then
           write(mins, '(I2.2)') '00'
        else
           write (mins, '(I2.2)') int(locarr(5,1))
        endif

C*-----------------------------------------------------------------------
c       Prepare output

        DO z = 1, nlev
          CALL get_val(idarr(1,z), said)
          CALL get_charval(idarr(2,z), rpid)
          CALL get_val(obsarr(1,z), tt)
          CALL get_val(obsarr(2,z), pr)
          CALL get_val(obsarr(3,z), wdir)
          CALL get_val(obsarr(4,z), wspd)
          CALL get_lat_lon(locarr2(1,z), locarr2(3,z), lat)
          CALL get_lat_lon(locarr2(2,z), locarr2(4,z), lon)

          if(pr.ne.0 .and. pr<99999 ) then
             pr=pr/100
          end if

          write(satid, '(A40)') repeat(' ', 40)
          write(csadstr, '(A80)') repeat(' ',80)
          CALL getcfmng(lunit, 'SAID', nint(said), '  ', -1,  
     +                  csadstr, len, iret)
          write(satid, '(A40)') csadstr(1:len)

c------------------------------------------------------------------------
c       Write output
          if(slat<=lat .and. nlat>=lat .and. 
     +       wlon<=lon .and. elon>=lon) then
                write(iou,111) isurf,
     +                         dname,
     +                         satid,
     +                         satname,
     +                         satwndsource,
     +                         date,
     +                         mins,
     +                         lat,
     +                         lon,
     +                         selv,
     +                         dslp,
     +                         nlev,
     +                         ibogus
               write(iou,112) pr,selv,tt,td,wdir,wspd
          endif
111       format(i1,1x,a6,1x,3(a40,1x),a10,a2,1x,
     +           3(f20.5,1x),f13.5,1x,i10,1x,i1)
112       format(6(f13.5,1x))

        END DO
      END DO

C*-----------------------------------------------------------------------

2000  continue 

      END

C*-----------------------------------------------------------------------
       SUBROUTINE get_val(mval, retval)

C      Checks variable value returned by UFBINT and returns either the 
c      observation value or missing.
c
c      Input:
c         mval: BUFR parameter value returned by UFBINT
c      Output:
c         retval: observation value

       real*8 mval, missing
       real retval
       parameter(missing=9999999.99999)

       IF (ibfms(mval) .EQ. 0) THEN
          retval = mval
       ELSE
          retval = missing
       ENDIF
       
       RETURN
       END
C*-----------------------------------------------------------------------
       SUBROUTINE get_charval(mval, retval)

C      Checks character value returned by UFBINT and returns either the 
c      string value or missing.
c
c      Input:
c         mval: BUFR character value returned by UFBINT
c      Output:
c         retval: observation string

       real*8 mval
       character*40 retval

       IF (ibfms(mval) .EQ. 0) THEN
          write(retval, '(A)') mval
       ELSE
          write(retval, '(A)') 'MISSING'
       ENDIF
       
       RETURN
       END
C*-----------------------------------------------------------------------
       SUBROUTINE get_lat_lon(clatlon, clatlonh, retval)

C      Get latitude and longitude from either CLAT/CLON or CLATH/CLONH
c      Input:
c         clatlon: CLAT or CLON returned by UFBINT
c         clatlonh: CLATH or CLONH returned by UFBINT
c      Output:
c         retval: latitude or longitude value

       real*8 clatlon, clatlonh, missing
       real retval
       parameter(missing=9999999.99999)

       IF (ibfms(clatlon) .EQ. 0) THEN
          retval = clatlon
       ELSE IF (ibfms(clatlonh) .EQ. 0) THEN
          retval = clatlonh
       ELSE
          retval = missing
       ENDIF
       
       RETURN
       END
