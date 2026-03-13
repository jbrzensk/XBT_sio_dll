!****************************************************************
! 23jul2014 try to look at ierror44&45) if set, do not write to ifile!
! 22jul2014 seas10 15 lines only (rm "empty launcher reload time"
! 15jul2014 add 'ifile' in passing params - that's the *.log output file # - you
!  were using it in sio, but not wexbt/srpedit etc, so now that it's a single 
!  rountine, use it for all
! 14jul2014 LL - Ibis getting 'error opening control.dat' when calling prstat.
!    probably because neither of us is resetting ierror(14->16) Janet must've
!    been doing that in seas2k. So add it into rdcntrl.for at beginning.
! 02jul2014 LL - rm from every program that uses it, make it a separate subroutine.
! This is copy for all. kakapo:/home/llehmann/auto2/rdcntrl.for
!                       fulaga:\Users\llehmann\auto2\rdcntrl.for
! Should really clean up and make more robust, but at least this version works
! for k98 control.dat seas2k and possibly Seas10
! 
! rdcntrl:
!01jul2014 mods for Ibis version of control.dat...
! This should be able to switch between old and new control.dat formats!!!
! this used in siosub.for too!!!   make changes there too!!!
!
        SUBROUTINE rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $             deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $             dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $             ichkprofdepth)
!GCC$ ATTRIBUTES DLLEXPORT :: rdcntrl
! INPUT:
!	acontrol - character string of path to control.dat
!
! file numbers opened and closed in rdcntrl:
!       22=control.dat
! ierrors set in rdcntrl:
!(14) - check control.dat OBSOLETE, this was error reading launcher sequence
!(15) - error opening control.dat
!(16) - error reading control.dat
!(33) - if operator name = "debug" turn on ierrlev=6 (fill .log files)
!
! This should be able to switch between old and new control.dat formats!!!
! read Janet Brockett new format of control.dat
! 01jul2014        parameter (nlines=17)
        parameter (nlines=18)
        parameter (nlnchrs=12)
        parameter (nerr=50)

        integer*4 ierror(nerr), ichkprofdepth, file
        integer*4 la(nlines)
        integer*4 len_acruise, launcher(nlnchrs)

	real*4 xmaxspd, deadmin, dropmin, relodmin
	real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
	real*4 tm_pl_mn,tm_pl_mx
        character*(*) acruise
        character*(*) acontrol

        character a(nlines)*34
        character a72*72, acut*72

        data  a(1)/'Ship Name ='/
        data  a(2)/'Cruise Name ='/
        data  a(3)/'Operator Name ='/
        data  a(4)/'Max Ship Speed ='/
        data  a(5)/'Auto Launcher Sequence ='/
        data  a(6)/'Max Plot  Temp ='/
        data  a(7)/'Min Plot Temp ='/
! 14jul2014 Janet used "Reakon", Ibis correctly wants "Reckon": (23:23)
        data  a(8)/'Max Minutes to Dead Reakon ='/
        data  a(9)/'Max min duration between drops ='/
        data a(10)/'Empty Launcher Reload Alarm Time ='/
        data a(11)/'Max Displacement ='/
        data a(12)/'Max Rms Displacement ='/
        data a(13)/'Min dtdz ='/
        data a(14)/'Min dtdz Displacement Test ='/
        data a(15)/'Max Delta ='/
        data a(16)/'Max 700m Delta ='/
        data a(17)/'Check Profile Depth ='/
! 01jul2014 add this for Ibis changing format:
        data a(18)/'Min Plot  Temp ='/
! 14jul2014 oddly the next 15 lines used to be above the "data" above, why?
!   move below:
! may want to check these default values.   Only used if error reading.
        xmaxspd1 = 30.
        xminspd1 = 30.
        deadmin1 = 60.
        dropmin1 = -90.
        relodmin1 = 15.
!22sep2014 just set relodmin if seas not sending it in:
        relodmin = 15.
        tdzmx1 = 450.
        tdzrms1 = 110.
        dtdzmn1 = -0.01100
        dtdzth1 = 0.00050
        dtmx1 = 4.6
        dtmx7001 = 0.8
        tm_pl_mn1 = 0.0
        tm_pl_mx1 = 30.0
        ichkprofdepth1 = 700
! 22sep2014 just set launcher(1:12) to 1 to 12...
        do 7 i = 1, nlnchrs
7        launcher(i) = i 
! 14jul2014 reset the ierror(15->16,33) to zero at beginning. These are only
!  ones that rdcntrl sets:
        ierror(15) = 0
        ierror(16) = 0
        ierror(33) = 0      ! my 'debug' in operator flag
! check ierror(44) & (45) if either set, do not write to ifile (calling log)
        iw = 1              ! iw=1 write to log, iw=0 do not write to log
        if(ierror(44).eq.1.or.ierror(45).eq.1) iw = 0

! set lengths of "a" array:
        do 10 i = 1, nlines
! getslen won't work because string do not end with a ' ' !!
!??	   call getslen(a(i),la(i))
! len_trim is fortran function that returns an integer value = length
!  of string WITHOUT trailing blanks. So if string='abc  ', len_trim=3
           la(i) = len_trim(a(i))
!           if(iw.eq.1) write(ifile,*)i,'la=', la(i)
10      continue

        open(22,file=acontrol,status='old',form='formatted',
     $         err=500)

! check 1st line of file to see what version it is:
        read(22,'(a)',err=301) a72
        rewind(22) 

! new format:

        if(a72(1:la(1)).eq.a(1)(1:la(1))) then

        if(iw.eq.1) write(ifile,*)'In rdcntrl,rd cntrl.dat:'
! 200 loop is reading control.dat where nlines can vary:
        do 200 i = 1, nlines
           read(22,'(a)',end=301,err=301) a72
           if(iw.eq.1) write(ifile,*)i, a72
!
! j here is looping over your ascii definitions above
           do 150 j = 1, nlines
              ifound = 0
!             special case for Reakon vs Reckon:
              if(j.eq.8) then
                 if(a72(1:19).eq.a(8)(1:19)) then
                    lacut = 72 - la(j) + 1
                    laj1 = la(j) + 1
                    acut(1:lacut) = a72(laj1:72)
                    ifound = 1
                 endif
              else
                 if(a72(1:la(j)).eq.a(j)(1:la(j))) then
                    lacut = 72 - la(j) + 1
                    laj1 = la(j) + 1
! acut is the part of the string read in that is on the right side of the '='
!   the part we are decoding to get the value:
                    acut(1:lacut) = a72(laj1:72)
                    ifound = 1
                 endif
              endif
              if(ifound.eq.1) then
!                if(iw.eq.1) write(ifile,*) j, acut
        
              if(j.eq.1) then
! skip ship name
              elseif(j.eq.2) then
! cruise name
! this doesn't work - returns 72, so find first space
!		    len_acruise = len_trim(acut)
!	            if(len_acruise.gt.7) len_acruise = 7
!	            acruise(1:len_acruise) = acut(1:len_acruise)
! this doesn't work either: still reading in 7 chars
!	            read(acut,'(a)') acruise
!	            if(iw.eq.1) write(ifile,*) 'acruise=',acruise
!	            len_acruise = len_trim(acruise)
! find 1st blank space:
                 iblank = lacut
                 do 15 k = 1, lacut
                     if(acut(k:k).eq.' ') then
                      iblank = k
                      go to 16
                     endif
15               continue
16               continue
            len_acruise = iblank - 1
            if(len_acruise.gt.7) len_acruise = 7
            acruise(1:len_acruise) = acut(1:len_acruise)
            if(iw.eq.1) then
               write(ifile,*)'len_acruise=',len_acruise
               write(ifile,*)'acruise=',acruise(1:len_acruise)
            endif 
         elseif(j.eq.3) then
! skip operator name
! 21jul2014 try to add back 'debug' !
            if(acut(1:5).eq.'debug') then
               ierror(33)=6
            endif

         elseif(j.eq.4) then
! max ship speed = xmaxspd
            read(acut,*,err=20) xmaxspd
            if(iw.eq.1) write(ifile,*)'xmaxspd=',xmaxspd
            goto 150
20            xmaxspd = xmaxspd1

         elseif(j.eq.5) then
!01jul2014 skip launcher sequence
! launcher sequence
!            read(acut,*,err=22) (launcher(ii),ii=1, nlnchrs)
!            goto 150
!22          continue
!! if error reading launchers, set all to negative?
!            do 24 ii = 1, nlnchrs
!               launcher(i) = -1*i
!24          continue
         elseif(j.eq.6) then
! plot temperature max
            read(acut,*,err=25) tm_pl_mx
            if(iw.eq.1) write(ifile,*)'tm_pl_mx=',tm_pl_mx
            goto 150
25          tm_pl_mx = tm_pl_mx1
         elseif(j.eq.7) then
! plot temperature min
            read(acut,*,err=26) tm_pl_mn
            if(iw.eq.1) write(ifile,*)'tm_pl_mn=',tm_pl_mn
            goto 150
26          tm_pl_mn = tm_pl_mn1
         elseif(j.eq.8) then
! max min to deadreckon = deadmin
            read(acut,*,err=28) deadmin
            if(iw.eq.1) write(ifile,*)'deadmin=',deadmin
            goto 150
28          deadmin = deadmin1

         elseif(j.eq.9) then
! max min to between drops = dropmin
            read(acut,*,err=30) dropmin
            if(iw.eq.1) write(ifile,*)'dropmin=',dropmin
            goto 150
30          dropmin = dropmin1

         elseif(j.eq.10) then
! empty launcher reload time = relodmin
            read(acut,*,err=32) relodmin
            if(iw.eq.1) write(ifile,*)'relodmin=',relodmin
            goto 150
32          relodmin = relodmin1
         elseif(j.eq.11) then
! tdzmx
            read(acut,*,err=34) tdzmx
            if(iw.eq.1) write(ifile,*)'tdzmx=',tdzmx
            goto 150
34          tdzmx = tdzmx1
         elseif(j.eq.12) then
! tdzrms
            read(acut,*,err=36) tdzrms
            if(iw.eq.1) write(ifile,*)'tdzrms=',tdzrms
            goto 150
36          tdzrms = tdzrms1
         elseif(j.eq.13) then
! dtdzmn
            read(acut,*,err=38) dtdzmn
            if(iw.eq.1) write(ifile,*)'dtdzmn=',dtdzmn
            goto 150
38          dtdzmn = dtdzmn1
         elseif(j.eq.14) then
! dtdzth
            read(acut,*,err=40) dtdzth
            if(iw.eq.1) write(ifile,*)'dtdzth=',dtdzth
            goto 150
40          dtdzth = dtdzth1

         elseif(j.eq.15) then
! dtmx
            read(acut,*,err=42) dtmx
            if(iw.eq.1) write(ifile,*)'dtmx=',dtmx  
            goto 150
42          dtmx = dtmx1
         elseif(j.eq.16) then
! dtmx700
            read(acut,*,err=44) dtmx700
            if(iw.eq.1) write(ifile,*)'dtmx700=',dtmx700  
            goto 150
44          dtmx700 = dtmx7001
         elseif(j.eq.17) then
! ichkprofdepth
            read(acut,*,err=45) ichkprofdepth
            if(iw.eq.1) write(ifile,*)'ichkprofdepth=',ichkprofdepth  
            goto 150
45          ichkprofdepth = ichkprofdepth1
         elseif(j.eq.18) then
! plot temperature min
            read(acut,*,err=46) tm_pl_mn
            if(iw.eq.1) write(ifile,*)'tm_pl_mn=',tm_pl_mn  
            goto 150
46          tm_pl_mn = tm_pl_mn1
          endif
          endif

150       continue
200     continue
         if(tm_pl_mn.eq.tm_pl_mx) then
            tm_pl_mn = tm_pl_mn1
            tm_pl_mx = tm_pl_mx1
         endif

        go to 900

! old format:
       else

        read(22,'(a)',end=301,err=301) acruise
        len_acruise=len_trim(acruise)
! board addresses
         read(22,*,err=701,end=701)
! drop time in seconds, 0, screen type
701      read(22,*,err=702,end=702)
702      read(22,551,err=703,end=703) deadmin, dropmin, relodmin, runsec
551      format(4f5.0)
         go to 704
703      deadmin = deadmin1
         dropmin = dropmin1
         relodmin = relodmin1
         runsec = 0
704      read(22,520,err=705,end=705)tdzmx,tdzrms,dtdzmn,dtdzth,
     $                               dtmx,dtmx700
520      format(2f6.0,2f9.5,2f5.1)
         go to 706
705      tdzmx = tdzmx1
         tdzrms = tdzrms1
         dtdzmn = dtdzmn1
         dtdzth = dtdzth1
         dtmx = dtmx1
         dtmx700 = dtmx7001
! tem plot min, max
706      read(22,*,err=707,end=707) ii, tm_pl_mn,tm_pl_mx
         go to 708
707      tm_pl_mn = tm_pl_mn1
         tm_pl_mx = tm_pl_mx1
! shipname
708      continue
         if(tm_pl_mn.eq.tm_pl_mx) then
            tm_pl_mn = tm_pl_mn1
            tm_pl_mx = tm_pl_mx1
         endif
 
         read(22,*,err=709,end=709)
! operator name
709      read(22,*,err=710,end=710)
! gps type, com port
710      read(22,*,err=711,end=711)
! ship minimum speed (knots), ship maximum speed
711      read(22,522,err=712,end=712) xminspd, xmaxspd
522      format(2f5.1)
         go to 713
712      xminspd = xminspd1
         xmaxspd = xmaxspd1
! data path
713      read(22,*,err=714,end=714)
! read version from control.dat
714      read(22,*,err=715,end=715)
715      read(22,550,err=13,end=13) (launcher(i),i=1,6)
550      format(6i3)
         go to 746
13       continue
! set all launchers negative (empty) if error reading control.dat
        do 14 i = 1, 6
14        launcher(i) = -1*i
! 22sep2014 ierror(14 redefined, no longer using!!!!        ierror(14) = 1
746      continue
! fill in launcher(7 to nlnchrs)
        do 47 i = 7, nlnchrs
47        launcher(i) = -99

        endif

        go to 900

!01jul2014 old: 301        ierror(16) = 1
!01jul2014 change 301
!ok, this 301 bit here is just checking that we've read at least 15 lines
! in to call it good, bogus, but it works
! since control.dat for seas10 has only 15 lines, (vs 17 for seas2k)
! 31jul2014, not sure why, but this error getting set even though
!  control.dat has 15 lines, change to i.le.3 since the rest go to default?
!301     if(i.le.15) ierror(16) = 1
301     if(i.le.3) ierror(16) = 1
        go to 900
500        ierror(15) = 1
        go to 900

900        continue
        close(22,iostat=ios)
        if(iw.eq.1) then
           write(ifile,*)'close22ios=',ios
           write(ifile,*)'end rdcntrl:ierror(15)=',ierror(15)
           write(ifile,*)'end rdcntrl:ierror(16)=',ierror(16)
           write(ifile,*)'leaving rdcntrl'
        endif
! Lisa do not close ifile here as this routine appends!

        return
        END SUBROUTINE rdcntrl
!*********************************************
