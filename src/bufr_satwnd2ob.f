      use moda_tababd
    
      PARAMETER (MXMN = 10)
      PARAMETER (MXLV = 255)

      REAL*8  idarr(MXMN, MXLV), instarr(MXMN, MXLV),
     +        locarr(MXMN, MXLV), obsarr(MXMN, MXLV) 

      CHARACTER*40 idstr,instr,lstr,obstr

c BUFR mnemonics
      DATA idstr/'SAID                                    '/ 
      DATA instr/'SIID SCLF SIDP SWCM                     '/
      DATA lstr /'YEAR MNTH DAYS HOUR MINU CLAT CLON      '/
      DATA obstr/'TMDBST PRLC WDIR WSPD                   '/

      PARAMETER (iu=9, iou=10, lunit=11)

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
      character argv*300, minute*2, M11*2, mins*2
      character*12 ilev
      character*12 M1,M2,M3,M4,M5,M6,M10
      real wlon, elon, slat, nlat

      CHARACTER  csubset*8, inf*200, outstg*200
      INTEGER    y, z, idate, iflag

      INTEGER lun, il, im
      INTEGER saidval
      CHARACTER*16 csad
      CHARACTER*80 csid
      CHARACTER*255 csadstr
      CHARACTER*80 desc
      CHARACTER*40 satname, satid

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
      fout= "satwnd"//date_tag//'.obs'

C*    Open output file
      open(iou,file=fout,status='unknown',form='formatted')
      write(iou,fmt='(a10)') date_tag

      iflag=0
      nlev=1
      dumm=99999.9

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

c     Get file ID (lun) associated with the BUFR file
      CALL status(lunit, lun, il, im)

c     Include code and flag table information from master BUFR tables
      CALL codflg('Y')

C*    Specify format of IDATE values returned by BUFRLIB
C*    (YYYYMMDDHHMM )
      CALL DATELEN(10)
     
C*-----------------------------------------------------------------------
C*    Loop through BUFR subsets

      DO WHILE(.true.)

        CALL READNS(lunit, csubset, idate, ierr)
        CALL UFBCNT(lunit, irec, isub)

        print'(''MESSAGE: '',A8,2(2X,I4),i12 )',
     +           csubset,irec,isub,idate

        IF (ierr .eq.  -1 ) THEN
          write(*,*) '[bufr_satwnd2ob]....all records read, Exit'
          CALL CLOSBF(lunit)
          goto 2000 
        END IF

C*      Read data values into arrays
        CALL UFBINT(lunit, idarr, MXMN, MXLV, nlevi, idstr)
        CALL UFBINT(lunit, locarr, MXMN, MXLV, nlevl, lstr)
        CALL UFBINT(lunit, obsarr, MXMN, MXLV, nlevo, obstr)

        if((nlevi .ne. nlevl) .or. (nlevi .ne. nlevo)) 
     +      stop 'nlevi <> nlevl/o'
        else
           nlev=nlevi
        endif

        minu=int(locarr(5,1))
        write (unit=minute, FMT='(I2)') minu

        DO k=1,2
          IF (minute (k:k) .eq. ' ' .or. minute(k:k) .eq. '*') THEN
            minute (k:k) = '0'
          ENDIF
        ENDDO

c       Get Table D index for csubset mnemonic, and get the 
c       description
        CALL nemtab(lun, csubset, idn, tab, n)
        desc=tabd(n, lun)(16:70)

C*-----------------------------------------------------------------------
c       Prepare output

        DO z = 1, nlev
          write(M1, '(F7.2)') locarr(6,z)  ! lat
          write(M2, '(F7.2)') locarr(7,z)  ! lon
          write(M3, '(F6.2)') obsarr(1,z)  ! tt
          write(M4, '(F7.1)') obsarr(2,z)  ! pr
          write(M5, '(F5.1)') obsarr(3,z)  ! wdir
          write(M6, '(F6.2)') obsarr(4,z)  ! wspd
          write(M10, '(I10)') idate
          write(M11, '(A2)') minute

          CALL READMval(M1,lat)
          CALL READMval(M2,lon)
          CALL READMval(M3,tt)
          CALL READMval(M4,pr)
          CALL READMval(M5,wdir)
          CALL READMval(M6,wspd)

          date=M10
          mins=M11

          if(pr.ne.0 .and. pr<99999 ) then
             pr=pr/100
          end if

          saidval=nint(idarr(1,z))
          csad=repeat(' ', 40)
          csadstr=repeat(' ',255)
          CALL getcfmng(lunit, 'SAID', saidval, '  ', -1, csadstr, 
     +                  len, iret)
          csad=csadstr(1:40)
          staname=desc(1:40)

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
     +                         staname,
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
111       format(i1,1x,a6,2(1x,a40),1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112       format(6(f7.1,1x))

        END DO
    END DO

C*-----------------------------------------------------------------------

2000    stop 99999     

        END

C*-----------------------------------------------------------------------

      SUBROUTINE READMval(M1,fl)
           character*8 M1
           dumm=99999.9
           if(M1(1:1) ==  'm') then
               fl = dumm
           else
               read(M1,*)fl
           endif 

       RETURN
         END
