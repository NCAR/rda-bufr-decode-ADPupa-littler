        PARAMETER       ( MXMN = 8 )
        PARAMETER       ( MXLV = 220 )
        PARAMETER       ( NVAR = 17 )
        PARAMETER       ( NSTR = 4 )

        REAL*8          r8arr ( MXMN, MXLV ), r8arr2(MXMN, MXLV ),
     +                  r8arr3 ( MXMN, MXLV ), r8arr4(MXMN, MXLV ) 

        parameter (iu=9, iou=10)

        real pr,tt,td
        integer xht, nlev, i, iargc, n, minu, k
        real xpr, xu, xv
        real temp, v, zx, d
        real lat, lon
        real xt, xtd
        character*30 fin, fout
        character*10  date_tag, date
        character*6 dname
        character argv*300, minute*2, M11*2, mins*2
        character*12 ilev
        character*12 M10,M1,M2,M3,M4,M5,M6
        real wlon, elon, slat, nlat

        CHARACTER     csubset*8, inf*200, outstg*200
        CHARACTER*80  ostr(NSTR)
        INTEGER       y, z, idate, iflag

        ostr(1)='SAID RPID'
        ostr(2)='SIDP SWCM YEAR MNTH DAYS'
        ostr(3)='HOUR MINU CLAT CLON'
        ostr(4)='TMDBST PRLC WDIR WSPD'

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
          write(*,*) 'Usage: bufr_sat2ob.x gdas.satwnd.t<HH>z.
     +<YYYYMMDD>.bufr <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
          STOP
        END IF

C*-----------------------------------------------------------------------

C*      Open the BUFR messages file
        OPEN (UNIT=11, FILE=inf, form='unformatted')

c*        write(*,*) 'Date_tag (YYYYMMDDHH) : '
c*        read(*,fmt='(a10)') date_tag

        dname=' SATOB'
        fout= "Satob"//date_tag//'.obs'

C*      Open output file
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

C*      Identify BUFR file to the BUFRLIB software.  DX BUFR tables
C*      are embedded within the first few messages of the BUFR file
C*      itself, thus we logical unit for the BUFR tables file is the 
C*      same as the BUFR file itself.

        CALL OPENBF  (11, 'IN', 11)

C*      Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHHMM ).

        CALL DATELEN  ( 10 )
     
C*-----------------------------------------------------------------------
        DO WHILE  ( .true. )

C*          Read the next BUFR message.

           call readns(11,csubset,idate,ierr)
c            write(*,*)' idate: ',idate,'  ',csubset
            IF  ( ierr .eq.  -1 )  THEN
                write(*,*) '[bufr_sat2ob]....all records read, Exit'
                CALL CLOSBF  ( 11 )
                goto 2000 
            END IF

C*            At this point, we have a data subset within the
C*            internal arrays of BUFRLIB, and we can now begin
C*            reading actual data values:

              CALL UFBINT(11, r8arr, MXMN, MXLV, nlv, ostr(1))
              CALL UFBINT(11, r8arr2, MXMN, MXLV, nlv, ostr(2))
              CALL UFBINT(11, r8arr3, MXMN, MXLV, nlv, ostr(3))
              CALL UFBINT(11, r8arr4, MXMN, MXLV, nlv, ostr(4))

            minu=int(r8arr3(2,1))
            write (unit=minute, FMT='(I2)') minu

            DO k=1,2
               IF ( minute (k:k) .eq. ' ') THEN
                 minute (k:k) = '0'
               ENDIF
            ENDDO

            DO z = 1, nlv
              WRITE (UNIT=outstg, FMT='(I10,1x,A8, 
     +          3(1X,F5.1),1X,A8,1X,F5.1, 
     +          1X,F4.1,1X,F6.1,4(1x,F4.1),2(1X,F7.2),1X,F6.2,
     +          1X,F7.1,1X,F5.1,1x,F6.2)') idate,csubset,
     +          (r8arr(i,z), i = 1,2),(r8arr2(i,z), i = 1,5),
     +          (r8arr3(i,z), i = 1,4),(r8arr4(i,z), i = 1,4)
              DO y = 1,200
               IF ( outstg (y:y) .eq. '*') THEN
                 outstg (y:y) = 'm'
               ENDIF
              ENDDO

              read(outstg,21,end=2000) M10,M1,M2,M3,M4,M5,M6
              read(minute,22) M11

21            format(A10,64X,A6,1X,A7,1X,A6,1X,A7,1X,A5,2X,A5)            
22            format(A2)

C*-----------------------------------------------------------------------
c         Prepare output

              CALL READMval(M1,lat)
              CALL READMval(M2,lon)
              CALL READMval(M3,tt)
              CALL READMval(M4,pr)
              CALL READMval(M5,d)
              CALL READMval(M6,v)

              date=M10
              mins=M11
              if(pr.ne.0 .and. pr<99999 ) then
                pr=pr/100
              end if

c        write to output file
              if (iflag.eq.0) then
                write(iou,fmt='(a10)') date_tag
                iflag=1
              endif
              if(slat<=lat .and. nlat>=lat .and. wlon<=lon .and. 
     &        elon>=lon) then
                 write(iou,111)isurf,dname,dname,date,mins,
     &                         lat,lon,ter,dslp,nlev,ibogus
                 write(iou,112)pr,zx,tt,td,d,v
              endif
111      format(i1,2(1x,a6),1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112      format(6(f7.1,1x))

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
