!================================================================================
      program makeobs
!================================================================================
!     Read in prepared upper air observations and write output in little_r 
!     format

!     Usage:
!         upperair_obs2littler.x <input obs file> <yyyymmddhh>
!     Example:
!         upperair_obs2littler.x adpupa2024030100.obs 2024030100

!     ... pressure is in Pa, height in m, temperature and dew point are in
!         K, speed is in m/s, and direction is in degrees

!     ... sea level pressure is in Pa, terrain elevation is in m, latitude
!         is in degrees N, longitude is in degrees E

!     ... the first 40 character string may be used for the description of
!         the station (i.e. name city country, etc)

!     ... the second character string we use for our source

!     ... the third string should be left alone, it uses the phrase "FM-35 TEMP"
!         for an upper air station, and should use "FM-12 SYNOP" for surface data

!     ... the fourth string is unused, feel free to experiment with labels!

!     ... bogus data are not subject to quality control
!=================================================================================
      parameter (kx=500)
      integer i, iargc, n, mdatea
      real p(kx),z(kx),t(kx),td(kx),spd(kx),dir(kx)
      logical bogus
      character*30 fin(20),fout1,fout2
      character*6 dname
      character*40 staid, staname
      character   argv*200, mdate*12
      character*13 time_tag
      character*40 fn,codestr,source
      data iunit,iounit1,iounit2/114,150,151/
      parameter (maxob=9999999)

      n = iargc()
      call getarg(1, argv)
      fn=argv
      call getarg(2,argv)
      time_tag=argv

	    open(1,file=fn,form='formatted',status='old')

	    m = iunit
	    do i=1,10
        read(1,fmt='(a30)',end=222) fin(i)
	      open(m,file=fin(i),status='old',form='formatted')
	      m = m + 1
	    enddo

222   fout1 = 'OBS:'//time_tag
	    open(iounit1,file=fout1,status='unknown')

c-----7---------------------------------------------------------------72
c  loop through observations in input file
      i = 1

      do 333 iu=iunit,m-1
        write(*,*) "reading file: ", fin(i)
        read(iu,fmt='(i10)') mdatea

c-----7---------------------------------------------------------------72
      do 444 iter=1,maxob

      call miss(kx, p, z, t, td, spd, dir, slp, ter, 
     +          dname, staid, staname, source)
      call getdat(iu, isurf, nlev, p, z, t, td, spd, dir, slp, ter,
     +            xlat, xlon, dname, staid, staname, source, 
     +            bogus, iflag, mdate)
      call wmo_codename(isurf,dname,codestr) 
	    if (iflag .eq. 0)then
        write(*,111)fin(i)
111   format('finished file : ',a30)
        i=i+1
        goto 333
      endif

c Set desired area

      if(xlat .ge. -90.0 .and. xlat .le. 90.0 ) then
      if(xlon .ge. -180.0 .and. xlon .le. 180.0 ) then 
         call write_obs (p, z, t, td, spd, dir, 
     +         slp, ter, xlat, xlon, mdate, nlev, 
     +         staid, staname, codestr, source,
     +         bogus, iseq_num, iounit1)
      endif
      endif

444   continue
     
333   continue
  
      close(iounit1)
      close(iounit2)

20    stop 99999
      end

c-----7---------------------------------------------------------------72
c  Subroutine to fill -888888. in place of missing data

      subroutine miss(kx, p, z, t, td, spd, dir, slp, ter,
     +                dname, staid, staname, source)
      real p(kx), z(kx), t(kx), td(kx), spd(kx), dir(kx)
      character*6 dname
      character*40 staid, staname, source

      do k=1,kx
        p(k)=-888888.
        z(k)=-888888.
        t(k)=-888888.
        td(k)=-888888.
        spd(k)=-888888.
        dir(k)=-888888.
      enddo

      slp=-888888.
      ter=-888888.
      dname = '99001 '
      staid   = '99001                                   '
      staname = '99001                                   '
      source = '99001                                   '

      return
      end

c-----7---------------------------------------------------------------72
c subroutine to read data from file unit

      subroutine getdat(iunit, isurf, nlev, 
     +                  p, z, t, td, spd, dir, slp, ter,
     +                  xlat, xlon, dname, 
     +                  staid, staname, source, bogus, iflag, mdate)

      parameter (kx=500)
      real p(kx), z(kx), t(kx), td(kx), spd(kx), dir(kx)
      real px(kx), zx(kx), tx(kx), tdx(kx), spdx(kx), dirx(kx)
      character*6 dname
      character*40 staid, staname, source
      character*12 mdate
      logical bogus

      bogus=.TRUE.
      dmiss = 99999.99999

      read(iunit,113,end=1000) 
     +     isurf, dname, staid, staname, source,
     +     mdate, xlat, xlon, xter, xslp, nlev, ibogus
113   format(i1,1x,a6,1x,3(a40,1x),a12,1x,
     +       3(f20.5,1x),f13.5,1x,i10,1x,i1)

      if (xter .ne. dmiss) ter = xter
      if (xslp .ne. dmiss) slp = xslp*100.
      if(ibogus.eq.0) bogus=.FALSE.

	      do kk=1,nlev
	        read(iunit,114,end=1000) 
     +         px(kk), zx(kk), tx(kk), 
     +         tdx(kk), dirx(kk), spdx(kk)
          
          if (px(kk) .ne. dmiss)     p(kk) = px(kk)*100.
	        if (zx(kk) .ne. dmiss)     z(kk) = zx(kk)
	        if (tx(kk) .ne. dmiss)     t(kk) = tx(kk)
          if (tdx(kk) .ne. dmiss)   td(kk) = tdx(kk) 
	        if (dirx(kk) .ne. dmiss) dir(kk) = dirx(kk)
	        if (spdx(kk) .ne. dmiss) spd(kk) = spdx(kk)
	      enddo

	  iflag = 1
	  goto 2000

114   format(6(f13.5,1x))
1000  iflag = 0

2000  return

      end

c-----7---------------------------------------------------------------72
c   Subroutine to put WMO code for each type of data
c
c   Given the WMO code fm, return the observation platform type and increment
c   the corresponding counter if present.
c
c Returned platforms are reduced to 13 output classes:
c
c   Name    WMO Codes     WMO Code names
c   synop    12,14       'SYNOP','SYNOP MOBIL'
c   ship     13          'SHIP'
c   metar    15,16       'METAR','SPECI'
c   buoy     18          'BUOY'
c   pilot    32,33,34    'PILOT','PILOT SHIP','PILOT MOBIL'
c   sound    35,36,37,38 'TEMP','TEMP SHIP, 'TEMP DROP','TEMP MOBIL'
c   amdar    42          'AMDAR'
c   satem    86          'SATEM'
c   satob    88          'SATOB'
c   airep    96,97       'AIREP'
c   gpspw    111         'GPSPW'
c   ssmt1    121         'SSMT1'
c   ssmt2    122         'SSMT2'
c   ssmi     125,126     'SSMI'
c   tovs     131         'TOVS'
c   qscat    281         'Quikscat'
c   profl    132         'Profilers'
c   other Any other code 'UNKNOWN'
c-----7---------------------------------------------------------------72
      
      subroutine wmo_codename(isurf,dname,codestr) 
      
      character dname*6, codestr*40
      integer isurf

      if (isurf .eq. 1 ) then

      if (dname .eq. '  SHIP' ) then
        codestr = 'FM-13 SHIP'
      elseif(dname .eq. '  BUOY') then
        codestr = 'FM-18 BUOY' 
      elseif(dname .eq. ' QSCAT') then 
        codestr = 'FM-281 QSCAT'
      elseif(dname .eq. ' METAR' .or. dname .eq. 'SPECI ') then
        codestr = 'FM-16 METAR' 
      elseif(dname .eq. '  SSMI') then
        codestr = 'FM-125 SSMI'
      else
        codestr = 'FM-12 SYNOP'
      endif 

      else

      if(dname .eq. 'PILOT ' ) then
        codestr = 'FM-32 PILOT'
      elseif(dname .eq. ' AMDAR' ) then
        codestr = 'FM-42 AMDAR'
      elseif(dname .eq. ' AIREP' ) then
        codestr = 'FM-96 AIREP'
      elseif(dname .eq. ' SATOB' ) then
        codestr = 'FM-88 SATOB'
      elseif(dname .eq. ' SATEM' ) then
        codestr = 'FM-86 SATEM'
      elseif(dname .eq. '  TOVS' ) then
        codestr = 'FM-131 TOVS'
      else
        codestr ='FM-35 TEMP'
      endif

      endif

      return
      end

c-----7---------------------------------------------------------------72
c  Subroutine to write data in a specified format for little_r

      SUBROUTINE write_obs (p, z, t, td, spd, dir, 
     +                      slp, ter, xlat, xlon, mdate, kx, 
     +                      string1, string2, string3, string4, 
     +                      bogus, iseq_num, iounit1)

      dimension p(kx), z(kx),t(kx),td(kx),spd(kx),dir(kx)

      integer iounit1
      character *20 date_char
      character *12 mdate
      character *40 string1, string2, string3, string4
      CHARACTER *84  rpt_format 
      CHARACTER *22  meas_format 
      CHARACTER *14  end_format
      logical bogus
c changed Osuri
      iseq_num =0      
c changed Osuri end

      rpt_format = '(2f20.5, 2a40, '
     +          //  '2a40, 1f20.5, 5i10, 3L10, '
     +          //  '2i10, a20, 13(f13.5, i7))'
      meas_format = '(10(f13.5, i7))'
      end_format = '(3(i7))'
      write (date_char(7:18),fmt='(a12)') mdate
c     if (mdate/1000000 .GT. 70 ) then
c        date_char(7:8)='19'
c     else
c        date_char(7:8)='20'
c     endif
      date_char(19:20)='00'
      date_char(1:6)='      '

      WRITE (UNIT=iounit1, ERR=19, FMT=rpt_format) 
     +       xlat, xlon, string1, string2, string3, string4, 
     +       ter, kx, 0, 0, iseq_num, 0, 
     +       .true., bogus, .false., 
     +       -888888, -888888, date_char, slp, 0,
     +       -888888., 0, -888888., 0, -888888., 0, -888888., 0,
     +       -888888., 0, -888888., 0, -888888., 0, -888888., 0,
     +       -888888., 0, -888888., 0, -888888., 0, -888888., 0
   
      do 100 k = 1 , kx
         WRITE (UNIT=iounit1, ERR=19, FMT=meas_format) 
     +          p(k), 0, z(k), 0, t(k), 0, 
     +          td(k), 0, spd(k), 0, dir(k), 0, 
     +          -888888., 0, -888888., 0,-888888., 0, -888888., 0
100   continue

      WRITE (UNIT=iounit1, ERR=19, FMT=meas_format) 
     +       -777777., 0, -777777., 0, float(kx), 0,
     +       -888888., 0, -888888., 0, -888888., 0, 
     +       -888888., 0, -888888., 0, -888888., 0, 
     +       -888888., 0
      WRITE (UNIT=iounit1, ERR=19, FMT=end_format) kx, 0, 0

      return
19    continue

      print*, 'error writing upper air data'
      stop 19

      END
