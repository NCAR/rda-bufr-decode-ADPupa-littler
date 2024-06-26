      use moda_tababd

      PARAMETER (MXMN = 10)
      PARAMETER (MXLV = 255)

      REAL*8 idarr(MXMN, MXLV), idarr2(MXMN, MXLV),
     +       nlocarr(MXMN, MXLV), locarr(MXMN, MXLV), 
     +       obsarr(MXMN, MXLV) 

c  BUFR mnemonics
      CHARACTER*40 idstr, idstr2, nlocstr, locstr, obstr
      DATA idstr  /'WMOB WMOS WMOR STSN SSTN BPID ACID      '/
      DATA idstr2 /'RSERL RSML RPID                         '/
      DATA nlocstr/'YEAR MNTH DAYS HOUR MINU                '/
      DATA locstr /'CLAT CLON SELV CLATH CLONH              '/
      DATA obstr  /'TMDB TMDP PRLC WDIR WSPD                '/

      parameter(iu=9,iou=10,lunit=11,nz=9999999)
      parameter(dumm=99999.9)
      
      dimension pr(nz),tt(nz),td(nz)
      integer  xht,nlev,i, iargc, n,minu,k
      real  xu,xv,xy,xm,xh,xmm,xd
      real  temp,zx(nz),wdir(nz),wspd(nz),ter(nz)
      real  lat(nz), lon(nz)
      real xt,xtd,xtt
      character*80 fin,fout
      character*10  date_tag, date(nz), xpr
      character*300 argv
      character*2 minute, mins(nz) 
      character*6 dname
      character*80 staid(nz)
      character*3 ilev
      character*8 min,xlat,xlon

      real wlon,elon,slat,nlat

      CHARACTER*10 M10
      CHARACTER*2 M11
      CHARACTER*80 M20
      CHARACTER*8 csubset
      CHARACTER*200 inf

      INTEGER y,z,idate

      INTEGER lun, il, im
      CHARACTER*80 desc
      CHARACTER*40 adpupaname(nz), adpupasource

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
c  Open BUFR input file
      OPEN(UNIT=lunit, FILE=inf, form='unformatted')

      dname='  TEMP'
      adpupasource='NCEP GDAS BUFR ADPUPA observations      '
      fout="adpupa"//date_tag//'.obs'

c  Open output file
      open(iou, file=fout, status='unknown', form='formatted')

      iflag = 0

      iupper=0
      ibogus=0
      dslp=dumm
      do k=1,nz
        date(k)='MMMMMMMMMM'
        mins(k)='MM'
        staid(k)='MMMMMM'
        lat(k)=dumm
        lon(k)=dumm
        ter(k)=dumm
        pr(k)=dumm
        zx(k)=dumm
        tt(k)=dumm
        td(k)=dumm
        wdir(k)=dumm
        wspd(k)=dumm
      enddo

C* Connect BUFR file to the BUFRLIB software for input operations.

      CALL OPENBF(lunit, 'IN', lunit)

C* Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHH ).

c  Include code and flag table information from master BUFR tables
      CALL codflg('Y')

C* Specify format of IDATE values returned by BUFRLIB (YYYYMMDDHHMM)
      CALL DATELEN(10)

      ln=0 

C*-----------------------------------------------------------------------
c  Loop through BUFR subsets

      DO WHILE ( .true. )

c       Get file ID (lun) associated with the BUFR file
        CALL status(lunit, lun, il, im)

        CALL READNS(lunit, csubset, idate, ierr)
        call ufbcnt(lunit, irec, isub)

c        print'(''MESSAGE: '',A8,2(2X,I6),i12 )',
c     +           csubset,irec,isub,idate

        IF (ierr .eq.  -1) THEN
          WRITE (*,*) '[bufr_adpupa2ob]....all records read, Exit'
          CALL CLOSBF(lunit)
          GOTO 1000 
        END IF

c  Read data values into arrays

        CALL UFBINT(lunit, idarr, MXMN, MXLV, nlevi, idstr)
        CALL UFBINT(lunit, idarr2, MXMN, MXLV, nlevi, idstr2)
        CALL UFBINT(lunit, nlocarr, MXMN, MXLV, nlevn, nlocstr)
        CALL UFBINT(lunit, locarr, MXMN, MXLV, nlevl, locstr)
        CALL UFBINT(lunit, obsarr, MXMN, MXLV, nlevo, obstr)

        nlev = nlevo

        if (ibfms(nlocarr(5,1)) .eq. 1) then
           minu=0
           minute='00'
        else
           minu=int(nlocarr(5,1))
           write (unit=minute, FMT='(I2)') minu
        endif

        DO k=1,2
           IF (minute (k:k) .eq. ' ' .or. minute(k:k) .eq. '*') THEN
              minute (k:k) = '0'
           ENDIF
        ENDDO

        write(M10, '(I10)') idate
        write(M11, '(A2)') minute

c  Get Table D index for csubset mnemonic, and get the description
        CALL nemtab(lun, csubset, idn, tab, n)
        desc=tabd(n, lun)(16:70)

C*-----------------------------------------------------------------------
c  Prepare output

        DO z = 1,nlev
c          IF(ibfms(obsarr(3,z)) .eq. 0) THEN
             iflag=iflag+1
             j=iflag

             CALL get_val(obsarr(1,z), tt(j))
             CALL get_val(obsarr(2,z), td(j))
             CALL get_val(obsarr(3,z), pr(j))
             CALL get_val(obsarr(4,z), wdir(j))
             CALL get_val(obsarr(5,z), wspd(j))
             CALL get_val(locarr(3,z), ter(j))
             CALL get_lat_lon(locarr(1,z), locarr(4,z), lat(j))
             CALL get_lat_lon(locarr(2,z), locarr(5,z), lon(j))

             date(j)=M10
             mins(j)=M11
             if(ibfms(idarr2(3,1)) .ne. 0) then
                write(M20, '(A)') 'RPID: MISSING'
             else
                write(M20, '(A,1X,A)') 'RPID:',idarr2(3,1)
             endif
             staid(j)=M20
        
             if(pr(j).ne.0 .and. pr(j).ne.99999.9) then
                pr(j)= pr(j)/100
             end if

             write(adpupaname(j), '(A40)') desc(15:)

c         ENDIF 
        END DO
      END DO

C*-----------------------------------------------------------------------
c  write output

1000  if (iflag .ne. 0) then 
        iflag1=0
        iflag2=1
        iflag3=1
        write(iou,fmt='(a10)') date_tag
        alat1=9999
        alon1=9999
        l=1
        do k = 1, iflag 
          if(slat<=lat(k) .and. nlat>=lat(k) .and. 
     &       wlon<=lon(k) .and. elon>=lon(k)) then

            if(alat1.ne.lat(k) .and. alon1.ne.lon(k)) then
               alat1=lat(k)
               alon1=lon(k)
               if(iflag1.ne.0) then
                  CALL SORTWRITE(pr,zx,tt,td,wdir,wspd,l,l1,m)
                  write(iou,111) iupper,
     +                           dname,
     +                           staid(l),
     +                           adpupaname(l),
     +                           adpupasource,
     +                           date(l),
     +                           mins(l),
     +                           lat(l),
     +                           lon(l),
     +                           ter(l),
     +                           dslp,
     +                           m-l+1,
     +                           ibogus
                  do i=l,m
                    write(iou,112) pr(i),zx(i),tt(i),
     +                             td(i),wdir(i),wspd(i)
                  enddo
!                 write(*,*)l,' ',m,' ',l1
                  iflag2=1
               endif

               iflag1=1
               l=k

            endif
            l1=k
          endif
        enddo

111       format(i1,1x,a6,1x,3(a40,1x),a10,a2,1x,
     +           3(f20.5,1x),f13.5,1x,i10,1x,i1)
112       format(6(f13.5,1x))
      
        CALL SORTWRITE(pr,zx,tt,td,wdir,wspd,l,l1,m)
        write(iou,111) iupper,
     +                 dname,
     +                 staid(l),
     +                 adpupaname(l),
     +                 adpupasource,
     +                 date(l),
     +                 mins(l),
     +                 lat(l),
     +                 lon(l),
     +                 ter(l),
     +                 dslp,
     +                 m-l+1,
     +                 ibogus
        do i=l,m
          write(iou,112)pr(i),zx(i),tt(i),td(i),wdir(i),wspd(i)
        enddo

        close(iou)
      endif

C*-----------------------------------------------------------------------
!       write(*,*)'nlev ', nlev

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
       
C*-----------------------------------------------------------------------
      SUBROUTINE SORTWRITE(pr,zx,tt,td,d,v,l,k,m)
      parameter(nz=9999999)

      dimension pr(nz),tt(nz),td(nz)
      real temp,v(nz),zx(nz),d(nz)
      dimension prt(nz),ttt(nz),tdt(nz)
      real vt(nz),zxt(nz),dt(nz)
      
      do i=l,k-1
        do j= i+1,k
          if (pr(i).lt.pr(j)) then
             call SWAPTWO(pr(i),pr(j))
             call SWAPTWO(zx(i),zx(j))
             call SWAPTWO(tt(i),tt(j))
             call SWAPTWO(td(i),td(j))
             call SWAPTWO(v(i),v(j))
             call SWAPTWO(d(i),d(j))
          endif 
        enddo
      enddo
      m=l 
      do i=l+1,k
        if(pr(i).eq.pr(m))then
           pr(m)=pr(i)
           if(zx(i)<99999) then
              zx(m)=zx(i)
           endif
           if(tt(i)<99999) then
              tt(m)=tt(i)
           endif
           if(td(i)<99999) then
              td(m)=td(i)
           endif
           if(v(i)<99999) then
              v(m)=v(i)
           endif
           if(d(i)<99999) then
              d(m)=d(i)
           endif
        else
           m=m+1
           pr(m)=pr(i)
           zx(m)=zx(i)
           tt(m)=tt(i)
           td(m)=td(i)
           v(m)=v(i)
           d(m)=d(i)
        endif
      enddo

      RETURN
      END

C*-----------------------------------------------------------------------
      SUBROUTINE SWAPTWO(X1,X2)
      temp=X1
      X1=X2
      X2=temp
      RETURN
      END
