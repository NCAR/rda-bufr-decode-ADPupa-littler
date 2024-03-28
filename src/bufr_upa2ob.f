      PARAMETER       ( MXMN = 8 )
      PARAMETER       ( MXLV = 220 )
      PARAMETER       ( NVAR = 13 )
      PARAMETER       ( NSTR = 3 )

      REAL*8 r8arr(MXMN, MXLV), r8arr2(MXMN, MXLV),
     +       r8arr3(MXMN, MXLV)

      parameter(iu=9,iou=10,nz=9999999)
      dimension pr(nz),tt(nz),td(nz)
      integer  xht,nlev,i, iargc, n,minu,k
      real  xu,xv,xy,xm,xh,xmm,xd
      real  temp,v(nz),zx(nz),d(nz),ter(nz)
      real  lat(nz), lon(nz)
      real xt,xtd,xtt
      character*80 fin,fout
      character*10  date_tag, date(nz),M10,xpr
      character   argv*300,minute*2,M11*2,mins(nz)*2 
      character*6 dname,staid(nz),M20
      character*3 ilev
      character*8 M1,M2,min,M3,M4,M5,xlat,xlon
      real wlon,elon,slat,nlat

      CHARACTER csubset*8, inf*200,outstg*211
      INTEGER y,z,idate

      CHARACTER*80 ostr(NSTR)

      ostr(1)='RPID YEAR MNTH DAYS'
      ostr(2)='HOUR MINU CLAT CLON SELV'
      ostr(3)='TMDB TMDP PRLC WDIR WSPD'
    
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
C*    Open the BUFR messages file.
      OPEN(UNIT=11, FILE=inf, form='unformatted')

      dname='  TEMP'
      fout="Upper"//date_tag//'.obs'

C*    Open output file
      open(18, file=fout, status='unknown', form='formatted')

      iflag = 0
      dumm=99999.9

      iupper=0
      ibogus=0
      dslp=dumm
      do k=1,nz
        date(k)='MMMMMMMMMM'
        mins(k)='MM'
        staid(k)='MMMMMM'
        ter(k)=dumm
        pr(k)=dumm
        zx(k)=dumm
        tt(k)=dumm
        td(k)=dumm
        d(k)=dumm
        v(k)=dumm
      enddo

C*      Associate the tables file with the messages file, and identify
C*      the latter to the BUFRLIB software.

      CALL OPENBF(11, 'IN', 11)

C*      Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHH ).

      CALL DATELEN(10)
     
      ln=0 

C*-----------------------------------------------------------------------
      DO WHILE ( .true. )

C*      Read the next BUFR message.
        CALL READNS(11,csubset,idate,ierr)
c            write(*,*)' idate: ',idate,'  ',csubset

        IF (ierr .eq.  -1) THEN
          WRITE (*,*) '[bufr_upa2ob]....all records read, Exit'
          CALL CLOSBF  ( 11 )
          GOTO 1000 
        END IF

C*      At this point, we have a data subset within the
C*      internal arrays of BUFRLIB, and we can now begin
C*      reading actual data values:

        CALL UFBINT(11, r8arr,  MXMN, MXLV, nlv, ostr(1))
        CALL UFBINT(11, r8arr2, MXMN, MXLV, nlv, ostr(2))
        CALL UFBINT(11, r8arr3, MXMN, MXLV, nlv, ostr(3))

        minu=int(r8arr2(2,2))
        write (unit=minute, FMT='(I2)') minu
        DO k=1,2
           IF ( minute (k:k) .eq. ' ') THEN
              minute (k:k) = '0'
           ENDIF
        ENDDO

        DO z = 1,nlv
          IF(r8arr3(3,z) .lt. 10E9) THEN  
             WRITE (UNIT=outstg, FMT='(I10,1x A8,1X,A6,1X,F6.1,
     +          4(1x,F4.1),2(1X,F7.2),1X,F6.2,1X,F6.2,1X,F6.1,1X,
     +          F8.1,1X,F5.1,1x,F6.2,1X,I4,1X,I6)')
     +          idate,csubset,
     +          (r8arr(i,1), i = 1,4),
     +          (r8arr2(i,1), i = 1,5),(r8arr3(i,z), i = 1,5),
     +          nlv,z
             DO y = 1, 211
                IF ( outstg (y:y) .eq. '*' ) THEN
                  outstg (y:y) = 'm'
                ENDIF
             ENDDO
         
             read(outstg,21) M10,xn1,M20,xy,xm,xd,xh,min,
     &                       xlat,xlon,M5,M1,M2,xpr,M3,M4
             read(minute,22) M11
!             write(*,*)M10,xlat,xlon,M5,M1,M2,xpr,M3,M4
21           format(A10,1X,A8,1X,A6,1X,A6,4(1x,A4),2(1X,A7),
     &              1X,A6,1X,A6,1X,A6,1X,A8,1X,A5,1x,A6)       
22           format(A2)

             iflag=iflag+1
             j=iflag

C*-----------------------------------------------------------------------
c            Prepare output

             CALL READMval(M1,tt(j))
             CALL READMval(M2,td(j))
             CALL READMval(M3,d(j))
             CALL READMval(M4,v(j))
             CALL READMval(M5,ter(j))
             CALL READMval(xlat,lat(j))
             CALL READMval(xlon,lon(j))
             CALL READMval(xpr,pr(j))
        
             if(pr(j).ne.0 .and. pr(j).ne.99999.9) then
                pr(j)= pr(j)/100
             end if

             date(j)=M10
             mins(j)=M11
             staid(j)=M20
         ENDIF 
        END DO
      END DO

C*-----------------------------------------------------------------------
1000  if (iflag .ne. 0) then 
        iflag1=0
        iflag2=1
        iflag3=1
        write(18,fmt='(a10)') date_tag
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
                  CALL SORTWRITE(pr,zx,tt,td,d,v,l,l1,m)
                  write(18,111) iupper,dname,staid(l),
     &                          date(l),mins(l),
     &                          lat(l),lon(l),ter(l),dslp,
     &                          m-l+1,ibogus
                  do i=l,m
                    write(18,112)pr(i),zx(i),tt(i),td(i),d(i),v(i)
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
111     format(i1,1x,a6,1x,a6,1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112     format(6(f7.1,1x))
      
        CALL SORTWRITE(pr,zx,tt,td,d,v,l,l1,m)
        write(18,111) iupper,dname,staid(l),date(l),mins(l),
     &                lat(l),lon(l),ter(l),dslp,m-l+1,ibogus
        do i=l,m
          write(18,112)pr(i),zx(i),tt(i),td(i),d(i),v(i)
        enddo

        close(18)
      endif

C*-----------------------------------------------------------------------
!       write(*,*)'nlev ', nlev
2000  stop 99999

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
