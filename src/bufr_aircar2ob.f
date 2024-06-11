      use moda_tababd

      PARAMETER ( MXMN = 10 )
      PARAMETER ( MXLV = 255 )

      REAL*8 idarr(MXMN, MXLV), nlocarr(MXMN, MXLV),
     +       locarr(MXMN, MXLV), obsarr(MXMN, MXLV) 

c BUFR mnemonics
      CHARACTER*40 idstr, nlocstr, locstr, obstr
      DATA idstr  /'ACID ACRN ARST                          '/
      DATA nlocstr/'YEAR MNTH DAYS HOUR MINU                '/
      DATA locstr /'CLAT CLON PRLC IALT CLATH CLONH         '/
      DATA obstr  /'MIXR REHU TMDB WDIR WSPD                '/

      PARAMETER (iu=9,iou=10,lunit=11)
      PARAMETER (dumm=99999.9)

      INTEGER year,month,days,hour
      real lat,lon,pr,tt,td,wdir,wspd
      INTEGER nlevi, nlevn, nlevl, nlevo, nlev
      INTEGER irec, isub
      REAL ter

      CHARACTER*12 M1,M2,M3,M4,M5,M6,M7,M8,M9,M10
      CHARACTER*2 M11, minute, mins

      integer iargc, n, minu, k
      character*20 fin,fout
      character*10 date_tag,date
      character*6 dname
      character*300 argv
      real wlon,elon,slat,nlat

      CHARACTER csubset*8, inf*200
      INTEGER y, z, i, idate, iflag

      INTEGER lun, il, im
      CHARACTER*80 desc
      CHARACTER*40 aircarname, aircarid, aircarsource

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
        write(*,*) 'Usage: bufr_aircar2ob.x gdas.adpsfc.t<HH>z.
     +<YYYYMMDD>.bufr.be <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
        STOP
      END IF

C*-----------------------------------------------------------------------
C*    Open BUFR input file
      OPEN(UNIT=lunit, FILE=inf, form='unformatted')

      dname=' AIREP'
      aircarsource='NCEP GDAS BUFR AIRCAR observations      '
      fout= "aircar"//date_tag//'.obs'

c     Open output file
      open(iou, file=fout, status='unknown', form='formatted')
      write(iou,fmt='(a10)') date_tag

      iflag=0 
      nlev=1

      isurf=0
      ibogus=0
      ter=dumm
      dslp=dumm
      date='MMMMMMMMMM'
      mins='MM'
      lat=dumm
      lon=dumm
      pr=dumm
      zx=dumm
      tt=dumm
      td=dumm
      d=dumm
      v=dumm

C* Connect BUFR file to the BUFRLIB software for input operations.

      CALL OPENBF(lunit, 'IN', lunit)

C* Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHH ).

c  Include code and flag table information from master BUFR tables
      CALL codflg('Y')

C* Specify format of IDATE values returned by BUFRLIB (YYYYMMDDHHMM)
      CALL DATELEN(10)

C*-----------------------------------------------------------------------
c  Loop through BUFR subsets

      DO WHILE ( .true. )

c       Get file ID (lun) associated with the BUFR file
        CALL status(lunit, lun, il, im)

        call readns(lunit, csubset, idate, ierr)
        call ufbcnt(lunit, irec, isub)

c        print'(''MESSAGE: '',A8,2(2X,I6),i12 )',
c     +           csubset,irec,isub,idate

        IF (ierr .eq.  -1) THEN
          write(*,*) '[bufr_aircar2ob]....all records read.'
          CALL CLOSBF  (lunit)
          GOTO 2000  
        END IF

C* Read data values into arrays

        CALL UFBINT(lunit, idarr, MXMN, MXLV, nlevi, idstr)
        CALL UFBINT(lunit, nlocarr, MXMN, MXLV, nlevn, nlocstr)
        CALL UFBINT(lunit, locarr, MXMN, MXLV, nlevl, locstr)
        CALL UFBINT(lunit, obsarr, MXMN, MXLV, nlevo, obstr)

        if(nlevi .ne. nlevn .or. 
     +     nlevi .ne. nlevl .or. 
     +     nlevi .ne. nlevo) then
             stop 'nlevi <> nlevn/l/o'
        else
           nlev=nlevi
        endif

        if (ibfms(nlocarr(5,1)) .eq. 1) then
           minu=0
           minute='00'
        else
           minu=int(nlocarr(5,1))
           write (unit=minute, FMT='(I2)') minu
        endif

        DO k=1,2
           IF ( minute (k:k) .eq. ' ' .or. minute(k:k) .eq. '*') THEN
             minute (k:k) = '0'
           ENDIF
        ENDDO 

          write(M10, '(I10)') idate
          write(M11, '(A2)') minute
          date=M10
          mins=M11

c  Get Table D index for csubset mnemonic, and get the description
        CALL nemtab(lun, csubset, idn, tab, n)
        desc=tabd(n, lun)(16:70)
        write(aircarname, '(A40)') desc(15:)

C*-----------------------------------------------------------------------
c         Prepare output

        DO z = 1, nlev
          CALL get_val(locarr(3,z), pr)
          CALL get_val(locarr(4,z), ialt)
          CALL get_val(obsarr(1,z), mixr)
          CALL get_val(obsarr(2,z), rehu)
          CALL get_val(obsarr(3,z), tt)
          CALL get_val(obsarr(4,z), wdir)
          CALL get_val(obsarr(5,z), wspd)
          CALL get_lat_lon(locarr(1,z), locarr(5,z), lat)
          CALL get_lat_lon(locarr(2,z), locarr(6,z), lon)
                         
          if(pr.ne.0 .and. pr.ne.99999.9) then
            pr=pr/100
          end if

          if(ibfms(idarr(1,1)) .ne. 0) then
              write(aircarid, '(A40)') 'ACID: MISSING'
          else
              write(aircarid, '(A,1X,A20)') 'ACID:',idarr(1,1)
          endif
          
c------------------------------------------------------------------------
c         Write output 
          if (iflag.eq.0) then
            write(iou,fmt='(a10)') date_tag
            iflag=1
          endif
          if(slat<=lat .and. nlat>=lat .and.
     +       wlon<=lon .and. elon>=lon) then
            write(iou,111) isurf,
     +                     dname,
     +                     aircarid,
     +                     aircarname,
     +                     aircarsource,
     +                     date,
     +                     mins,
     +                     lat,
     +                     lon,
     +                     ter,
     +                     dslp,
     +                     nlev,
     +                     ibogus
            write(iou,112) pr,zx,tt,td,wdir,wspd
          endif

111       format(i1,1x,a6,1x,3(a40,1x),a10,a2,1x,
     +           3(f20.5,1x),f13.5,1x,i10,1x,i1)
112       format(6(f13.5,1x))
          
        ENDDO
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

       real*8 mval
       real retval
       parameter(missing=99999.9)

       IF (ibfms(mval) .EQ. 0) THEN
          retval = mval
       ELSE
          retval = missing
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

       real*8 clatlon, clatlonh
       real retval
       parameter(missing=99999.9)

       IF (ibfms(clatlon) .EQ. 0) THEN
          retval = clatlon
       ELSE IF (ibfms(clatlonh) .EQ. 0) THEN
          retval = clatlonh
       ELSE
          retval = missing
       ENDIF
       
       RETURN
       END
