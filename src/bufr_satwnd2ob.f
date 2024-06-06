      use moda_tababd
    
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
      PARAMETER (dumm=99999.9)

      INTEGER year,month,days,hour
      real lat,lon,pr,tt,td,wdir,wspd
      INTEGER nlevi, nlevl, nlevo, nlev
      INTEGER irec, isub
      REAL ter

      integer xht, i, iargc, n, minu, k
      real zx
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
      CHARACTER*16 csad
      CHARACTER*80 csid
      CHARACTER*255 csadstr
      CHARACTER*80 desc
      CHARACTER*40 satname, satid, satwndsource

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

      dname=' SATOB'
      satwndsource='NCEP GDAS BUFR SATWND observations      '
      fout= "satwnd"//date_tag//'.obs'

C*    Open output file
      open(iou,file=fout,status='unknown',form='formatted')
      write(iou,fmt='(a10)') date_tag

      iflag=0
      nlev=1

      isurf = 0
      ibogus = 0
      ter = dumm
      dslp = dumm
      date='MMMMMMMMMM'
      mins='MM' 
      pr=dumm
      zx=dumm
      tt=dumm
      td=dumm
      d=dumm
      v=dumm

C*    Connect BUFR file to the BUFRLIB software for input operations.
C*    DX BUFR tables are embedded within the first few messages of 
C*    the BUFR file itself, thus the logical unit for the BUFR tables 
C*    file is the same as the BUFR file itself.
      CALL OPENBF(lunit, 'IN', lunit)

c     Include code and flag table information from master BUFR tables
      CALL codflg('Y')

C*    Specify format of IDATE values returned by BUFRLIB
C*    (YYYYMMDDHHMM )
      CALL DATELEN(10)
     
C*-----------------------------------------------------------------------
C*    Loop through BUFR subsets

      DO WHILE(.true.)

c       Get file ID (lun) associated with the BUFR file
        CALL status(lunit, lun, il, im)

        CALL READNS(lunit, csubset, idate, ierr)
        CALL UFBCNT(lunit, irec, isub)

c        print'(''MESSAGE: '',A8,2(2X,I6),i12 )',
c     +           csubset,irec,isub,idate

        IF (ierr .eq.  -1 ) THEN
          write(*,*) '[bufr_satwnd2ob]....all records read, Exit'
          CALL CLOSBF(lunit)
          goto 2000 
        END IF

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

        write(date, '(I10)') idate

        if (ibfms(locarr(5,1)) .eq. 1) then
           minu=0
           mins='00'
        else
           minu=int(locarr(5,1))
           write (mins, FMT='(I2)') minu
        endif

c       Get Table D index for csubset mnemonic, and get the 
c       description
        CALL nemtab(lun, csubset, idn, tab, n)
        desc=tabd(n, lun)(16:70)
        write(satname, '(A40)') desc(17:)

C*-----------------------------------------------------------------------
c       Prepare output

        DO z = 1, nlev
          CALL get_val(obsarr(1,z), tt)
          CALL get_val(obsarr(2,z), pr)
          CALL get_val(obsarr(3,z), wdir)
          CALL get_val(obsarr(4,z), wspd)
          CALL get_lat_lon(locarr2(1,z), locarr2(3,z), lat)
          CALL get_lat_lon(locarr2(2,z), locarr2(4,z), lon)

          if(pr.ne.0 .and. pr<99999 ) then
             pr=pr/100
          end if

          saidval=nint(idarr(1,z))
          csad=repeat(' ', 40)
          csadstr=repeat(' ',255)
          CALL getcfmng(lunit, 'SAID', saidval, '  ', -1, csadstr, 
     +                  len, iret)
          csad=csadstr(1:40)

c       Write to output file
          if (iflag.eq.0) then
            write(iou,fmt='(a10)') date_tag
            iflag=1
          endif
          if(slat<=lat .and. nlat>=lat .and. 
     +       wlon<=lon .and. elon>=lon) then
                write(iou,111) isurf,
     +                         dname,
     +                         csad,
     +                         satname,
     +                         satwndsource,
     +                         date,
     +                         mins,
     +                         lat,
     +                         lon,
     +                         ter,
     +                         dslp,
     +                         nlev,
     +                         ibogus
               write(iou,112) pr,zx,tt,td,wdir,wspd
          endif
111       format(i1,1x,a6,3(1x,a40),1x,a10,a2,4(f7.1,1x),i5,1x,i1)
112       format(6(f7.1,1x))

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

       real*8 mval, retval, dumm
       dumm=99999.9

       IF (ibfms(mval) .EQ. 0) THEN
          retval = mval
       ELSE
          retval = dumm
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

       real*8 clatlon, clatlonh, retval, dumm
       dumm=99999.9

       IF (ibfms(clatlon) .EQ. 0) THEN
          retval = clatlon
       ELSE IF (ibfms(clatlonh) .EQ. 0) THEN
          retval = clatlonh
       ELSE
          retval = dumm
       ENDIF
       
       RETURN
       END
