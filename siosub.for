!05jun2015 LL sub ave add write ierr38=jptr,ierr39=icall at exit
!09sep2014 LL add iw/file for error log output
!21jul2014 LL clean up, figure out ierrors, etc
!02jul2014 remove subroutine rdcntrl. Now an external independent subroutine.
! possibly found in kakapo:/home/llehmann/auto2/rdcntrl.for 
!    and            fulaga c:\users\llehmann\autoxbt\rdcntrl.for
!02jul2014 change siodir.txt path to Ibis requested spot. 
!
! subroutine for sio.for.   These are NOT called directly from Seas2k.
! These are only called from old K98 routines - which is what seas2k calls.

!23456789012345678901234567890123456789012345678901234567890123456789012
!        1         2         3         4         5         6         7
!********************************************************************
! try computing spd from present and previous ave positions instead of
! from first derivative in fit
         SUBROUTINE ave(ibuf,xlat,xlon,timetag,avlath,avlonh,
     $                 s10,d10,timeave,vlat,vlon,ierror,
     $                 ierr,iSIOSpeedAveMin,iw,ifile)
! INPUT:
!        ibuf	- integer*4 - # values used to create average
!        xlat(200)- real*4 - latitude values, use 1 to ibuf
!        xlon(200)- real*4 - longitude values, use 1 to ibuf
!        timetag(200) - real*4 - timetag values, use 1 to ibuf
!        timeave - real*4 - previous averaged values timetag
!        vlat - real*4 - previous averaged values latitude
!        vlon - real*4 - previous averaged values longitude
!        iSIOSpeedAveMin - integer*4 - number of minutes of past data
!              to use to calculate speed and dir (originally=10, but I
!              think this causes problems when doing circles.
! new!   ierror(38) = jptr (set to 0 in siobegin) reset here 13jun2005
! new!   ierror(39) = icall (set to 0 in siobegin) reset here 13jun2005
!        iw = iw=0 no write to log (ifile), iw=1, write to log
!        ifile = file number to write too, usually 33
! OUTPUT:
!       avlath - character*1 - averaged latitude hemisphere N or S
!       avlonh - character*1 - averaged longitude hemisphere N or S
!       s10 - real*4 - average speed of up to last 10 calls to ave
!       d10 - real*4 - average direction of up to last 10 calls to ave
!       timeave - real*4 - outputs current averaged values timetag
!       vlat - real*4 - outputs current averaged values latitude
!       vlon - real*4 - outputs current averaged values longitude
!       ierror(nerr) - 12 is only possible one set
!       ierr - error rtn value from dpolft (1 is good)

        parameter(nerr=50)

        integer*4 ibuf, ierror(nerr), ierr
        character avlath*1, avlonh*1
       real*4 timetag(200),xlat(200),xlon(200)
       real*4 s10, d10, timeave, vlat, vlon
        dimension w(200),r(200),b(220)
        dimension tbuf(10),xltbuf(10),xlnbuf(10)
        integer*4 iw,ifile
! 06oct2006 iSIOsave new...
!11aug2006 the save:
       save tbuf,xltbuf,xlnbuf, iSIOsave, iSIOset

       deg2rad = 3.141592654/180.0

! new! 13jun2005
! the ifirst is VERY sketchy, test!
       jptr = ierror(38)
       icall = ierror(39)
       ifirst = icall + 1
! Save incoming time,lat, long averages:
       stime=timeave
       svlat = vlat
       svlon = vlon

! 11aug2006
       if(ifirst.eq.1) then
          do 5 i = 1, 10
             tbuf(i) = 0.0
             xltbuf(i) = 0.0
             xlnbuf(i) = 0.0
5          continue
       endif

! just for writing out errors:
       if(ierror(33).ne.0) ierrlev = 6

       if(iw.eq.1.and.ierrlev.eq.6) then
        write(ifile,*)'in ave, iSIOSpeedAveMin=',iSIOSpeedAveMin
        write(ifile,*)'jptr,icall,ifirst=',jptr,icall,ifirst
        write(ifile,*)'stime,svlat,svlon=',stime,svlat,svlon
        write(ifile,*)'i, tbuf,xltbuf,xlnbuf'
        do 6 i = 1, 10
           write(ifile,*)i, tbuf(i),xltbuf(i),xlnbuf(i)
6        continue
       endif

       w(1) = -1.0
       eps = 0.0

! check for Saturday night rollover: (604800 secs in 1 week)
       if(timetag(1).gt.600000.0.and.timetag(ibuf).lt.6000.0) then
        do 10 i = 1, ibuf
         if(timetag(i).lt.600.0) timetag(i) = timetag(i) + 604800.0
10        continue
       endif
       if(iw.eq.1.and.ierrlev.eq.6) then
        write(ifile,*)'begin ave, ibuf= ', ibuf, 'timeave=',timeave
        do 745 ik = 1, ibuf
745        write(ifile,755) ik, timetag(ik),xlat(ik),xlon(ik)
       endif
755       format(i3,1x,f7.0,2f9.4)

! use timeave1 so we don't change timeave if error returning from dpolft
       timeave1 = 0.0
       do 30 i = 1, ibuf
        timeave1 = timeave1 + timetag(i)
30       continue
       timeave1 = timeave1/float(ibuf)
! subtract timetag ave from each timetag
       do 40 i = 1, ibuf
        timetag(i) = timetag(i) - timeave1
40       continue

       if( (xlon(1).gt.359.0.and.xlon(ibuf).lt.1.0).or.
     $      (xlon(1).lt.1.0.and.xlon(ibuf).gt.359.0) ) then
         do 450 jj = 1, ibuf
           if(xlon(jj).lt.1.0) xlon(jj) = 360.0 + xlon(jj)
450         continue
       endif

! Bruce's (slatec) polynomial fit routine, for lat
       call dpolft(ibuf,timetag,xlat,w,1,ndeg,eps,r,ierr,b)
       if(ierr.ne.1) go to 102
! output of dp1vlu:  yfit = latitude or longitude, yp = speed in deg/sec
       x = 0.0
       call dp1vlu(1,1,x,yfit,yp,b)
       if(iw.eq.1.and.ierrlev.ge.6) then
          write(ifile,*)'timeave1=',timeave1
          write(ifile,*)'after dp1vlu, yfit, yp=',yfit,yp
       endif
! I haven't a clue why we're setting to -99:  Dean?
       yp=-99.0
       yplat=-99.0
! add sat night here too(>0 part)
       if(((timeave1-stime).lt.150.0).and.
     $     ((timeave1-stime).gt.0.0))yp=(yfit-vlat)/(timeave1-stime)

! for n.m./hr : (60 nm/deg * 3600 sec/hr * yp deg/sec)
       if(yp.ne.-99.0)yplat = 216000.0*yp
       yfitlat = yfit
       if(iw.eq.1.and.ierrlev.ge.6) then
          write(ifile,*)'next yp=',yp
          write(ifile,*)'yfitlat=',yfitlat
       endif
       if(ifirst.eq.1) ylatsav = yfitlat
! Putting current minutes average into vlat:
       vlat = yfitlat
       call dec2deg('lat',ideg,xlatm,avlath,vlat)
! Bruce's (slatec) polynomial fit routine, for lon
       w(1) = -1.0
       eps = 0.0
       call dpolft(ibuf,timetag,xlon,w,1,ndeg,eps,r,ierr,b)
       if(ierr.ne.1) go to 102
! ok, put timeave1 into timeave if return from dpolft successfully
       timeave = timeave1
       x = 0.0
       call dp1vlu(1,1,x,yfit,yp,b)
       if(iw.eq.1.and.ierrlev.ge.6) then
          write(ifile,*)'after dp1vlu, yfit, yp=',yfit,yp
       endif
! if some of the xlon's 359.9 and some 0.1, changed the 0.1 to 360.1....
       if(yfit.gt.360.0) yfit = 360.0 - yfit
! I haven't a clue why we're setting to -99:  Dean?
       yp=-99.0
       yplon=-99.0
! add sat night here too(>0 part)
       if(((timeave-stime).lt.150.0).and.
     $     ((timeave-stime).gt.0.0)) yp=(yfit-vlon)/(timeave1-stime)
       if(iw.eq.1.and.ierrlev.ge.6) then
          write(ifile,*)'next yp=',yp
       endif
! for nm/hr:
       if(yp.ne.-99.0)yplon = 216000.0*cos(yfitlat*deg2rad)*yp
       if(iw.eq.1.and.ierrlev.eq.6) then
        write(ifile,*)'yplon=',yplon,' yp=',yp, 'yfit=',yfit
       endif
! 13jun2005 I really think this needs to be ylonsav=yfit!
       if(ifirst.eq.1) then
! this doesn't work:          ylonsav = yplon
! so test this
          ylonsav = yfit
       endif
! Putting current minutes average into vlon:
       vlon = yfit
       call dec2deg('lon',ideg,xlonm,avlonh,vlon)
! calc ave speed:
       if(iw.eq.1.and.ierrlev.ge.6) then
          write(ifile,*)'bef calc speed/dir'
          write(ifile,*)'yplat,yplon=',yplat,yplon
       endif
! this is calculating speed and dir for THIS ONE AVERAGE value
       speed = -99.0
       dir=-99.0
!
       if(yplat.ne.-99.0)then
           speed = sqrt(yplat**2 + yplon**2)
! direction:
           if(speed.eq.0.0) then
             dir = acos(yplon)*(1.0/deg2rad)
           else
             dir = acos(yplon/speed)*(1.0/deg2rad)
           endif
! this gets us so that 90 is N and 0 is E, -90 is S
        if(yplat.lt.0.0) dir = -dir
! this gets us so that 90 is E, 0 is N
        dir = 90.0 - dir
        if(dir.lt.0.0) dir = dir + 360.0
!21aug2006 add to deal with slow speeds:
        if(speed.lt.8.0) then
! 05oct2006 duh, reset jptr and icall too...otherwise overflow array!
! 06oct2006 duh again = this is reset on every call to ave if speed<8
! so check what previous iSIOSpeedAveMin is and only reset if 
! iSIOSpeedAveMin > 5 say?
            if(iSIOSpeedAveMin.ge.5) then
! save incoming iSIOSpeedAveMin and set iSIOset=2
               iSIOsave = iSIOSpeedAveMin
               iSIOset = 2
               iSIOSpeedAveMin = 2
               jptr = 0
               icall = 0
                if(iw.eq.1.and.ierrlev.eq.6) then
                 write(ifile,*)'set iSIOSpeedAveMin=',iSIOSpeedAveMin,
     $          ' jptr=',jptr,' icall=',icall,' iSIOset=',iSIOset,
     $          ' iSIOsave=', iSIOsave,' speed=',speed
                endif
            endif      !iSIOSpeedAveMin.ge.5
! this checks is speed goes back up and whether I set above already
        elseif(speed.ge.8.0.and.iSIOset.eq.2) then
            iSIOSpeedAveMin = iSIOsave
            iSIOset = 0
            if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*)'set iSIOSpeedAveMin=',iSIOSpeedAveMin,
     $          ' jptr=',jptr,' icall=',icall,' iSIOset=',iSIOset,
     $          ' iSIOsave=', iSIOsave,' speed=',speed
            endif
        endif           ! speed.lt.8
       endif            !yplat.ne.-99.0

       if(iw.eq.1)write(ifile,*)'current speed&dir=',speed,dir
! this DOES NOT WORK.  WHY.  I mean if I set ylatsav&ylonsav
! at ifirst=1 only - well ok, we drop xbt's more than 1 per degree.
! never mind, fixed what ylondif is defined above, keep testing
       if(ifirst.ne.1) then
        ylatdif = ylatsav - vlat
        ylondif = ylonsav - vlon
             if(iw.eq.1) then
             write(ifile,*) ylatdif, ' = ',ylatsav,' -',vlat
             write(ifile,*) ylondif, ' = ',ylonsav,' -',vlon
             endif
!
! 09jun2006 LL this is a problem for crossing 1W to 1E !
! so add the ".and.abs(ylondif).lt.350.0"
        if(abs(ylatdif).gt.1.0.or.
     $     (abs(ylondif).gt.1.0.and.abs(ylondif).lt.350.0)) then
         if(iw.eq.1)write(ifile,*)'set ierror(12)=1'
         ierror(12) = 1         ! change in DR lat or lon too big
        endif
       endif

! reset if cross the 360 line
       if(jptr.gt.1.and.abs(xlnbuf(1)-vlon).gt.350.0) then
        ifirst = 1
        jptr = 0
        icall = 0
       endif
! reset at saturday night...  seems the easiest thing to do!
       if(jptr.gt.1.and.abs(tbuf(1)-timeave).gt.35000.0) then
        ifirst = 1
        jptr = 0
        icall = 0
        if(iw.eq.1)write(ifile,*)'reset jptr in ave',jptr, ifirst, icall
       endif
! 10aug2006-Note that ave MAY NOT BE CALLED EVERY MINUTE IF GPS
! FIXES ARE SPOTTY - so this is NOT NECESSARILY MINUTES!
! get avg spd from last (up to) ten calls to ave.
! 02jun2005 add variable iSIOSpeedAveMin
!   set it to 10 if it's a wierd number...
        if(iSIOSpeedAveMin.lt.1.or.iSIOSpeedAveMin.gt.10) then
           iSIOSpeedAveMin = 10
        endif
       
       d10=-99.0
       s10=-99.0
          jptr=jptr+1
       icall=icall+1
! 11aug2006 add s10=speed & d10=dir
       if(jptr.eq.1) then
! does not work too well:          s10 = speed
! does not work too well:          d10 = dir
          go to 101
       endif
!        		       !old	if(jptr.eq.11)jptr=1
       if(jptr.ge.iSIOSpeedAveMin+1)jptr=1
       iptr=1
!        		       !old	if(icall.ge.11)iptr=jptr
       if(icall.ge.iSIOSpeedAveMin+1)iptr=jptr
!
        if(iw.eq.1.and.ierrlev.eq.6) then
           write(ifile,*)'iSIOSpeedAveMin=',iSIOSpeedAveMin,
     $                   ' iptr=',iptr,' jptr=',jptr,' icall=',icall
           write(ifile,*)'tbuf(iptr)=',tbuf(iptr)
           write(ifile,*)'vlon=',vlon,' xlnbuf(iptr)=',xlnbuf(iptr)
        endif
!
       t10=timeave-tbuf(iptr)
! 10aug2006 check t10(in seconds) above to see how long it's been:
! 600 seconds in 10 minutes  
       if(t10.gt.650.0) then
! what...
          if(iw.eq.1)write(ifile,*)'TOO HIGH t10=',t10
! So put in current averages speed and dir
! hmm, may not work well either since at jptr=1 it's not great.
! not sure how this will work with so few fixes so far apart...
          s10 = speed
          d10 = dir
          go to 101
       endif

       x10=216000.0*cos(vlat*deg2rad)*(vlon-xlnbuf(iptr))/t10
       y10=216000.0*(vlat-xltbuf(iptr))/t10
       s10=sqrt(x10*x10+y10*y10)
       if(iw.eq.1.and.ierrlev.eq.6) then
        write(ifile,*)'t10=',t10,' x10=',x10,' y10=',y10,' s10=',s10
       endif
       if(s10.eq.0.0) then
        d10 = acos(x10)*(1.0/deg2rad)
       else
        d10 = acos(x10/s10)*(1.0/deg2rad)
       endif
       if(y10.lt.0.0) d10=-d10
       d10 =  90.0 - d10
       if(d10.lt.0.0) d10 = d10 + 360.0

101       tbuf(jptr)=timeave
       xltbuf(jptr)=vlat
       xlnbuf(jptr)=vlon

c new! 13jun2005
       ierror(38) = jptr
       ierror(39) = icall

       if(iw.eq.1.and.ierrlev.eq.6) then
        write(ifile,*)'s10,d10',s10,d10
        write(ifile,*)'tbuf(jptr)=',tbuf(jptr)
        write(ifile,*)'xltbuf(jptr)=',xltbuf(jptr)
        write(ifile,*)'xlnbuf(jptr)=',xlnbuf(jptr)
        write(ifile,*)'ierror(38)/jptr=',ierror(38)
        write(ifile,*)'ierror(39)/icall=',ierror(39)
        write(ifile,*)'end ave'
       endif

102       return
       end
!
!*****************************************************
!
       SUBROUTINE ch2real(acmsg,lpos,length,x)
! translate character to real, only works for xxxxxx.xxxxxxx at moment
! IN:  acmsg = character string to be translated
!      integer*4 lpos = position in acmsg of leftmost #
!      integer*4 length = length of string to be translated
! OUT: real*4 x = translated number
!
! nl = number of digits to the left of the decimal point
! nr =   "         "        right         "
       character acmsg*(*)
       integer*4 lpos,length,l,lp,neg,ik,k,i,nl,nr
       real*4 x

       l = length
       lp = lpos
       x = 0.0
       neg = 0
       ik = 0
       if(acmsg(lp:lp).eq.'-') then
        neg = 1
        lp = lp + 1
        l = l - 1
       endif
       k = lp
       do 1 i = k, k+l-1
       ik = ik + 1
        if(acmsg(i:i).eq.'.') then
         nl = ik - 1
         nr = l - ik
         go to 2
        endif
1       continue
       ik = 0
       do 11 i = k, k+l-1
       ik = ik + 1
        if(acmsg(i:i).eq.' ') then
         nl = ik - 1
         if(nl.eq.0) then
          ik = ik - 1
          go to 11
         endif
         nr = 0
         go to 2
        endif
11       continue
       nl = l
       nr = 0
2       continue
       do 3 i = lp, lp+l-1 
        if(acmsg(i:i).eq.' ') acmsg(i:i) = '0'
3       continue
       if(nl.eq.6) then
        x = real((ichar(acmsg(k:k))-48)*100000)
        k = k + 1
       endif
       if(nl.ge.5) then
        x = x + real((ichar(acmsg(k:k))-48)*10000)
        k = k + 1
       endif
       if(nl.ge.4) then
        x = x + real((ichar(acmsg(k:k))-48)*1000)
        k = k + 1
       endif
       if(nl.ge.3) then
        x = x + real((ichar(acmsg(k:k))-48)*100)
        k = k + 1
       endif
       if(nl.ge.2) then
        x = x + real((ichar(acmsg(k:k))-48)*10)
        k = k + 1
       endif
       if(nl.ge.1) then
        x = x + real(ichar(acmsg(k:k))-48)
        k = k + 1
       endif
       if(nr.eq.0) go to 10
       if(nr.ge.1) k = k + 1
       if(nr.ge.1) x = x + real((ichar(acmsg(k:k))-48))*0.1
       k = k + 1
       if(nr.ge.2) x = x + real((ichar(acmsg(k:k))-48))*0.01
       k = k + 1
       if(nr.ge.3) x = x + real((ichar(acmsg(k:k))-48))*0.001
       k = k + 1
       if(nr.ge.4) x = x + real((ichar(acmsg(k:k))-48))*0.0001
       k = k + 1
       if(nr.ge.5) x = x + real((ichar(acmsg(k:k))-48))*0.00001
       k = k + 1
       if(nr.ge.6) x = x + real((ichar(acmsg(k:k))-48))*0.000001
       k = k + 1
       if(nr.ge.7) x = x + real((ichar(acmsg(k:k))-48))*0.0000001
10       if(neg.eq.1) x = -x
         return
       end
!
!***********************************************
!
        SUBROUTINE chkall(xlat,xlon,speed,dir,ierr)
! check values on speed and direction before they get written to nav file
! so we don't screw it up
! IN:       real*4 speed, dir, xlat, xlon
! OUT:       integer*4 ierr  =0 ok,   =1 somebody bad

       real*4 xlat, xlon, speed, dir
       integer*4 ierr

       ierr = 0
       if(speed.lt.0.0.or.speed.gt.99.99) then
          ierr = 1
       endif
       if(dir.lt.0.0.or.dir.gt.360.0) then
          ierr = 1
       endif
       if(xlat.lt.-90.0.or.xlat.gt.90.0) then
          ierr = 1
       endif
       if(xlon.lt.0.0.or.xlon.gt.360.0) then
          ierr = 1
       endif
       return
       end
!
!***********************************************
!
        SUBROUTINE chkbuf(ibuf,clatbuf,clonbuf,ctagbuf,ierr,iw,ifile)
! INPUT:
!        ibuf	- integer*4 - # values used to create average
!        xlat(200)- real*4 - latitude values, use 1 to ibuf
!        xlon(200)- real*4 - longitude values, use 1 to ibuf
!        timetag(200) - real*4 - timetag values, use 1 to ibuf
!        iw = 0 no write to log, iw=1 write to log
!        ifile log file number, usually 33
! OUTPUT:
!       ierr = 0 - good, use them,   =1 bad, do not use

! check values in buffer that we're passing to ave - make sure
! they are actually near each other.  Maybe gps came back with
! a real bogus position or timetag...
        integer*4 ibuf, ierr
        real*4 clatbuf(200), clonbuf(200), ctagbuf(200)
        real*4 clatbuf1(200), clonbuf1(200), ctagbuf1(200)
        dimension imark(200)

        ierr = 0
        iwrite = 0
        do 5 i = 1, 70
5       imark(i) = 0
! first check timetags for saturday rollover time
        if(ctagbuf(1).gt.604500.0) then
! debug:
         if(iw.eq.1)write(ifile,*)'Sat night? DOS:, ibuf= ',ibuf
         do 10 i = 1, ibuf
          if(iw.eq.1)write(ifile,500)i,ctagbuf(i),clatbuf(i),clonbuf(i)
500       format('ctagbuf ',i2,'      is: ',f8.1,f10.5,f10.5)
          if(ctagbuf(i).lt.1000.0) ctagbuf(i) = ctagbuf(i) + 604800.0
          if(iw.eq.1)write(ifile,501) i,ctagbuf(i),clatbuf(i),clonbuf(i)
501       format('ctagbuf ',i2,' becomes: ',f8.1,f10.5,f10.5)
10       continue
        endif
! Let's assume the last value in the buffer is good and compare to that.
! Are timetags actually near each other and in increasing order?
!wcheck
! 200 tied to one minute NAV file updates, increase if go to 5 min updates!
! 18aug2006 - increase 200 to 400 to deal with ship drifting and seas
! not sending me iupdate=1 so takes longer to fill buffer
        ibad = 0
        do 20 i = 1, ibuf - 1
         if((ctagbuf(i).gt.ctagbuf(ibuf)).or.
     $      (ctagbuf(ibuf)-ctagbuf(i)).gt.400.0) then
          ibad = ibad + 1
          imark(i) = 1
          iwrite = 1
         endif
20      continue
! if have too many "bad" points don't do an ave here
        if(ibad.ge.(ibuf-1).or.ibad.gt.10) then
         ierr = 1
         go to 300
        endif
! check lats & lons
        ibad2 = 0
! NAV file update rate here too!
        do 50 i = 1, ibuf-1
         if( abs(clatbuf(ibuf)-clatbuf(i)).gt.0.5.or.
     $       abs(clonbuf(ibuf)-clonbuf(i)).gt.0.5) then
          ibad2 = ibad2 + 1
          imark(i) = 2
          iwrite = 1
         endif
50      continue
        if(ibad2.ge.(ibuf-1).or.ibad2.gt.10) then
         ierr = 1
         go to 300
        endif
        if(ibad.eq.0.and.ibad2.eq.0) go to 300
        ibufnew = 0
!09sep2014 comment this out:
!        if(iwrite.eq.1) then
!        if(iw.eq.1)write(ifile,503) ibuf,ctagbuf(ibuf),
!     $      clatbuf(ibuf),clonbuf(ibuf)
!503      format('ibuf=', i3,f8.1,f10.5,f10.5)
!         if(iw.eq.1)write(ifile,*)'rm from buf before ave:'
!        endif
        do 70 i = 1, ibuf
         if(imark(i).eq.0) then
          ibufnew = ibufnew + 1
          ctagbuf1(ibufnew) = ctagbuf(i)
          clatbuf1(ibufnew) = clatbuf(i)
          clonbuf1(ibufnew) = clonbuf(i)
         else
!         if(iw.eq.1)write(ifile,502) i,ctagbuf(i), clatbuf(i), clonbuf(i),imark(i)
!502      format(i3,1x, f8.1,f10.5,f10.5,i2)
         endif
70      continue
!
        ibuf = ibufnew
        do 80 i = 1, ibuf
         ctagbuf(i) = ctagbuf1(i)
         clatbuf(i) = clatbuf1(i)
         clonbuf(i) = clonbuf1(i)
80      continue
300     continue
        return
        end
!
!*******************************
!
        SUBROUTINE chknav(iday,imo,iyr,ierr,fnav,len_adir,adir,iw,ifile)
!
! 25jul2014 LL - DO NOT USE THIS ROUTINE, IT HAS NEVER WORKED PROPERLY!!!
!
! 25jul2014 LL add iw,ifile for writing to log file, plus clean up a bit
! INPUT:
!     iday - int, day of nav file
!     imo - int, month of nav file
!     iyr - int, 2 digit, I think, year of nav file
!     fnav - char, name of nav file we are checking
!     len_adir - int, length of path to nav file
!     adir - char*80, path to nav file
!     iw - init =0 no write to ifile (gpspos.log), =1 write to ifile
!     ifile - file number to write output log junk
! OUTPUT:
!     ?
!
! Check that date IN nav file matches date OF nav file.
! Check that times are monotonically increasing.
! Look for and remove blank lines.
! SKIP for now11-19-97LL-check that values read in from nav file are "usable"
        parameter(n=1500)
        character a64(n)*64, alth(n)*1, alnh(n)*1, chkdnav(60)*6
        character*10 ftemp, fnav*(*)
        character*80 adir
!
        integer*4 iday,imo,iyr,ierr,len_adir,iw,ifile
        dimension kdy(n),kmo(n),kyr(n),khr(n),kmn(n),ksc(n)
        dimension ibad(n), ktimesec(n)
        dimension klatd(n), xlatm(n)
        dimension klond(n), xlonm(n)
        dimension speed(n), dir(n)

        data chkdnav/60*'000000'/
        data ichkdnav/0/

! put actual nav file name (minus path) into ftemp
! 18sep2006 LL = change 10 to 15 here!   
! 25jul2014 whoa, one is 10 and one is 15 ! duh. figure out:
!       Keep at 10 since it looks like you only look at 1:6
        ftemp(1:10) = fnav(len_adir+1:len_adir+10)
        ierr = 0
        ibadcntr = 0
! don't recheck already checked nav files.  Time consuming.
        if(iw.eq.1)write(ifile,*)'  ichkdnav=',ichkdnav
        do 10 i = 1, ichkdnav
        if(iw.eq.1)write(ifile,*)'  chkdnav',i,'=',chkdnav(i)
           if(chkdnav(i)(1:6).eq.ftemp(1:6)) return
10      continue
        ichkdnav = ichkdnav + 1
        chkdnav(ichkdnav)(1:6) = ftemp(1:6)

        do 100 i = 1, n
         a64(i)(1:32) =  '                                '
         a64(i)(33:64) = '                                '
         read(8,'(a64)',end=101,err=601) a64(i)
! do we have actual data on the line?:
         icha642 = ichar(a64(i)(2:2))
! ichar 32 is a space...
         if(icha642.ne.32) then
          ibad(i) = 0
          read(a64(i),556,err=90) kdy(i), kmo(i), kyr(i), khr(i),
     $                          kmn(i), ksc(i), klatd(i), xlatm(i),
     $                          alth(i), klond(i), xlonm(i), alnh(i),
     $                          speed(i), dir(i)
556       format(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1)
! does the date on the line match the date of the file?:
          if((kdy(i).ne.iday).or.(kmo(i).ne.imo).or.(kyr(i).ne.iyr))then
             ibad(i) = 2
             if(iw.eq.1)write(ifile,*)'  bad date'
             ibadcntr = ibadcntr + 1
          else
! change time of day to seconds into day
             ktimesec(i) = ksc(i) + 60*kmn(i) + 3600*khr(i)
! if screwy time, mark bad
             if(ktimesec(i).lt.0.or.ktimesec(i).gt.86400) then
                ibad(i) = 4
                ibadcntr = ibadcntr + 1
             endif
          endif
         else
! blank line
            ibad(i) = 1
            if(iw.eq.1)write(ifile,*)'  blank line'
            ibadcntr = ibadcntr + 1
         endif
        go to 100
!
90      ibad(i) = 8
        ibadcntr = ibadcntr + 1
100     continue
101     continue
        nread = i - 1
        if(iw.eq.1)write(ifile,*)'  ibadcntr=',ibadcntr,' nread=',nread

! check that times are monotonically increasing on "good" lines:
        do 200 i = 1, nread-1
         if(ibad(i).eq.0) then
          do 150 j = i+1, nread-1
           if(ibad(j).eq.0) then
            if(ktimesec(i).lt.ktimesec(j)) then
! order ok
             go to 200
            else
! out of order
             ibad(j) = 3
             ibadcntr = ibadcntr + 1
            endif
           else
           endif
150       continue
151       continue
         endif
200     continue
! look at actual values - this takes some time!
        do 300 i = 1, nread
         if(ibad(i).eq.0) then
! add 1 to ibadcntr at beginning, if passes all tests, subtract one.
          ibadcntr = ibadcntr + 1
          ichlat = ichar(alth(i))
          ichlon = ichar(alnh(i))
          if(klatd(i).lt.-90.or.klatd(i).gt.90) then
           ibad(i) = 5
          elseif(xlatm(i).lt.0.0.or.xlatm(i).gt.60.0) then
           ibad(i) = 5
! check that lat hem is N or n or S or s
          elseif(ichlat.ne.78.and.ichlat.ne.110.and.
     $           ichlat.ne.83.and.ichlat.ne.115) then
          elseif(klond(i).lt.0.or.klond(i).gt.360) then
           ibad(i) = 6
          elseif(xlonm(i).lt.0.0.or.xlonm(i).gt.60.0) then
           ibad(i) = 6
! check that lon hem is W or w or W or e
          elseif(ichlon.ne.87.and.ichlon.ne.119.and.
     $           ichlon.ne.69.and.ichlon.ne.101) then
           ibad(i) = 6
          elseif(speed(i).lt.-0.1.or.speed(i).gt.45.0) then
           ibad(i) = 7
          elseif(dir(i).lt.0.0.or.dir(i).gt.360.0) then
           ibad(i) = 8
          else
           ibadcntr = ibadcntr - 1
          endif
         endif
300     continue

! if found no errors, continue merrily on...
        if(ibadcntr.eq.0) then
         rewind(8)
         return
! if found, say, less <= 4 errors, fix up file, make a note, and continue
        elseif(ibadcntr.le.4) then
         if(iw.eq.1)write(ifile,*) 'I found ',ibadcntr,
     $              ' errors in the nav file'
         close(8)
         if(iw.eq.1)write(ifile,*)' Closing nav file to fix it'
         call navopen(iday,imo,iyr,ierr,fnav,adir,len_adir)
         if(iw.eq.1)write(ifile,510) ibadcntr,ftemp(1:6)
510      format('Removed following ',i2,' lines from ',a6,'.nav file:')
         do 400 i = 1, nread
          if(ibad(i).eq.0) then
           write(8,'(a64)') a64(i)
          elseif(ibad(i).eq.1) then
           if(iw.eq.1)write(ifile,511) a64(i)
511        format(a64,' blank line')
          elseif(ibad(i).eq.2) then
           if(iw.eq.1)write(ifile,512) a64(i)
512        format(a64,' bad date')
          elseif(ibad(i).eq.3) then
           if(iw.eq.1)write(ifile,513) a64(i)
513        format(a64,' tim out of ordr')
          elseif(ibad(i).eq.4) then
           if(iw.eq.1)write(ifile,514) a64(i)
514        format(a64,' screwy time')
          elseif(ibad(i).eq.5) then
           if(iw.eq.1)write(ifile,515) a64(i)
515        format(a64,' bad latitude')
          elseif(ibad(i).eq.6) then
           if(iw.eq.1)write(ifile,516) a64(i)
516        format(a64,' bad longitude')
          elseif(ibad(i).eq.7) then
           if(iw.eq.1)write(ifile,517) a64(i)
517        format(a64,' bad speed')
          elseif(ibad(i).eq.8) then
           if(iw.eq.1)write(ifile,518) a64(i)
518        format(a64,' bad something!')
          endif
400      continue
         rewind(8)
         return
! 5 or more errors in nav file, this is bad, what to do....
        else
         ierr = 1
         if(iw.eq.1)write(ifile,*)'Help me!'
         if(iw.eq.1)write(ifile,*)
     $      ' Here is the nav file and some of the errors I found'
         do 450 i = 1, nread
          if(ibad(i).eq.0) then
           if(iw.eq.1)write(ifile,'(a64)') a64(i)
          elseif(ibad(i).eq.1) then
           if(iw.eq.1)write(ifile,511) a64(i)
          elseif(ibad(i).eq.2) then
           if(iw.eq.1)write(ifile,512) a64(i)
          elseif(ibad(i).eq.3) then
           if(iw.eq.1)write(ifile,513) a64(i)
          elseif(ibad(i).eq.4) then
           if(iw.eq.1)write(ifile,514) a64(i)
          elseif(ibad(i).eq.5) then
           if(iw.eq.1)write(ifile,515) a64(i)
          elseif(ibad(i).eq.6) then
           if(iw.eq.1)write(ifile,516) a64(i)
          elseif(ibad(i).eq.7) then
           if(iw.eq.1)write(ifile,517) a64(i)
          elseif(ibad(i).eq.8) then
           if(iw.eq.1)write(ifile,518) a64(i)
          endif
450      continue
        endif
! 601 is actually error reading nav file, deal with that in main
601     continue
        return
        end
!
!******************************************
!
        SUBROUTINE chkwrite(ylat,ylon,ierr)
        ierr = 0
        if(ylat.lt.-90.0.or.ylat.gt.90.0) ierr = 1
        if(ylon.lt.0.0.or.ylon.gt.360.0) ierr = 1
        return
        end
!
!******************************************
!
        SUBROUTINE compare(nday,nmon,nyear,nhr,nmin,nsec,
     $                     iday,imon,iyear,ihr,imin,isec,iflg)
! INPUT:
!       nday,nmon,nyear(4digit!) date.nave data
!        iday,imon,iyear: navtrk.dat file data
! OUTPUT:
!       iflg = 0 - do not use nav file,  =1 use nav file
! ok - what iflg really means: if the first set of date/time (n) is later
! in time than the second set of date/times (i) then set iflg=1

! compare nav file date/time to date/time in navtrk.dat, if nav file data
! is more recent, use that to DR from!  (just set iflg to 1 in here)
       integer*4 nhr,nmin,nsec,ihr,imin,isec,iflg
       integer*4 nday,nmon,nyear,iday,imon,iyear
! iflg=0 - don't use nav file (first set of positions)
        iflg = 0
! compare year... just forget about 99 to 00
! if entered year greater than year in navtrk, write new data
        if(nyear.gt.iyear) then
         iflg = 1
         return
! if entered year less than navtrk, don't write
        elseif(nyear.lt.iyear) then
         return
        endif
! same year, check month...
        if(nmon.gt.imon) then
         iflg = 1
         return
        elseif(nmon.lt.imon) then
         return
        endif
! same month, check day
        if(nday.gt.iday) then
         iflg = 1
         return
        elseif(nday.lt.iday) then
         return
        endif
! same day, check hr
        if(nhr.gt.ihr) then
         iflg = 1
         return
        elseif(nhr.lt.ihr) then
         return
        endif
! same hour, check min
        if(nmin.gt.imin) then
         iflg = 1
         return
        elseif(nmin.lt.imin) then
         return
        endif
! same min, check sec
        if(nsec.gt.isec) then
         iflg = 1
         return
        elseif(nsec.lt.isec) then
         return
        endif
        return
        end
!
!***********************************************
!
!        SUBROUTINE comparedir(ispec,iplandir,dir,idirck)
!       integer*4 ispec, iplandir, idirck
!       real*4 dir
! ispec = integer of aspec, lat=1, lon=0
! compare iplandir (direction of plan.dat positions) to
! dir - actual ship direction.   To help stop drops when ship
! going in circles...
! set idirck=1 if same direction - this is normal and good
! set idirck=0 if going different directions...
!    assume good:
!       idirck = 1
!c remember iplandir: N=1,E=2,S=3,W=4
!c our dirs are N=0deg, E=90deg, S=180deg, W=270deg
!
!c23456789012345678901234567890123456789012345678901234567890123456789012
!c    set only if bad:
!       if(ispec.eq.0) then
!          if(dir.ge.0.0.and.dir.le.180.0.and.iplandir.eq.4) idirck=0
!          if(dir.le.360.0.and.dir.ge.180.0.and.iplandir.eq.2) idirck=0
!       elseif(ispec.eq.1) then
!          if(dir.ge.270.0.and.dir.le.90.0.and.iplandir.eq.3) idirck=0
!          if(dir.le.270.0.and.dir.ge.90.0.and.iplandir.eq.1) idirck=0
!       endif
!       return
!       end
!
!******************************************
!
        SUBROUTINE dayofw(iweekday)

        integer*4 iweekday
        character*24 ch
        call fdate(ch)
        if(ch(1:3).eq.'Sun') iweekday = 0
        if(ch(1:3).eq.'Mon') iweekday = 1
        if(ch(1:3).eq.'Tue') iweekday = 2
        if(ch(1:3).eq.'Wed') iweekday = 3
        if(ch(1:3).eq.'Thu') iweekday = 4
        if(ch(1:3).eq.'Fri') iweekday = 5
        if(ch(1:3).eq.'Sat') iweekday = 6
        return
        end
!
!***********************************************
!
         SUBROUTINE dec2deg(typ,ideg,xmin,ahem,x)
! translate decimal position to degrees minutes hemi
! IN:         character*3 typ = lat or lon
!        real*4 x = decimal position
! OUT         integer*4 ideg = integer degrees
!         real*4 xmin = real minutes
!         ahem = 1 char hemisphere

       real*4 xmin, x
       integer*4 ideg
       character*1 ahem, typ*3

       if(typ.eq.'lat') then
          ideg = int(abs(x))
          xmin = ( abs(x) - real(ideg) ) * 60.0
          if(x.ge.0.0) then
             ahem = 'N'
          else
             ahem = 'S'
          endif
       else
!wcheck
! this assumes you're passing 0-360 E, if you pass 120.0 W you're screwed.
          if(x.le.180.0) then
             ahem = 'E'
             ideg = int(abs(x))
             xmin = ( abs(x) - real(ideg) ) * 60.0
          else
             ahem = 'W'
             xlon = 360.0 - x
             ideg = int(abs(xlon))
             xmin = ( abs(xlon) - real(ideg) ) * 60.0
          endif
       endif
       return
       end
!
!*****************************************************
!
       SUBROUTINE decodeplan(aplan,latd,xlatm,ahem,ierrplan,
     $             ispec1)
       character*(*) aplan
       character*1 ahem
       integer*4 latd, ispec1
       real*4 xlatm, xlat
       integer*4 ifounddeg, ifoundmin, ifoundhem, i

! ierrplan - you are not doing anything with ierrplan...
! ierrplan=1 = bad, ierrplan=0=good
       ierrplan = 1
! set to zero until found something, then =1
       ifounddeg = 0
       ifoundmin = 0
       ifoundhem = 0
! i is position in aplan string (from plan.dat)
       i = 1
5       continue
! look for space at beginning of line:
       if(aplan(i:i).eq.' ') then
          i = i + 1
          go to 5
       else
          call findspace(aplan,i,ic)
          call ch2real(aplan,i,ic,xlat)
          latd = int(xlat)
          ifounddeg = 1
       endif
! increment i by length of last variable plus 1 for the space:
       i = i + ic + 1
! look for extra spaces between deg and minutes:
10       continue
       if(aplan(i:i).eq.' ') then
          i = i + 1
          go to 10
       else
          call findspace(aplan,i,ic)
          call ch2real(aplan,i,ic,xlatm)
          ifoundmin = 1
       endif
! increment i by length of last variable plus 1 for the space:
       i = i + ic + 1
! look for extra spaces between minutes and hemi:
15       continue
       if(aplan(i:i).eq.' ') then
          i = i + 1
          go to 15
       else
          ahem(1:1) = aplan(i:i)
          ifoundhem = 1
       endif
! set ispec1 =0=longitude or ispec1=1=latitude
       if(ahem(1:1).eq.'N'.or.ahem(1:1).eq.'n'.or.
     $     ahem(1:1).eq.'S'.or.ahem(1:1).eq.'s') then
             ispec1 = 1
       elseif(ahem(1:1).eq.'E'.or.ahem(1:1).eq.'e'.or.
     $     ahem(1:1).eq.'W'.or.ahem(1:1).eq.'w') then
             ispec1 = 0
       endif

       return
       end
!
!*****************************************************
!
       SUBROUTINE deg2dec(ideg,xmin,ahem,x)
! translate degrees minutes hemi position to decimal
! IN:         ideg = integer degrees
!         xmin = real minutes
!         ahem = 1 char hemisphere
! OUT:        x = decimal position
       
       integer*4 ideg
       real*4 xmin, x
       character*1 ahem

       if(ahem.eq.'N'.or.ahem.eq.'E') then
          x = real(ideg) + (xmin/60.0)
       elseif(ahem.eq.'S') then
          x = -1.0* (real(abs(ideg)) + (abs(xmin)/60.0))
       elseif(ahem.eq.'W') then
          x = 360.0 - (real(ideg) + (xmin/60.0))
       endif
       return
       end
!
!*****************************************************
!
! changed to single precision!
*DECK DPOLFT
      SUBROUTINE DPOLFT(N,X,Y,W,MAXDEG,NDEG,EPS,R,IERR,A)
C***BEGIN PROLOGUE  DPOLFT
C***DATE WRITTEN   740601   (YYMMDD)
C***REVISION DATE  851111   (YYMMDD)
C***CATEGORY NO.  K1A1A2
C***KEYWORDS  CURVE FITTING,DATA FITTING,DOUBLE PRECISION,LEAST SQUARES,
C             POLYNOMIAL FIT
C***AUTHOR  SHAMPINE, L. F., (SNLA)
C           DAVENPORT, S. M., (SNLA)
C           HUDDLESTON, R. E., (SNLL)
C***PURPOSE  Fit discrete data in a least squares sense by polynomials
C            in one variable.
C***DESCRIPTION
C
C       *** Double Precision version of POLFIT ***
C
C     Written by L. F. Shampine and S. M.  Davenport.  The statistical
C     options provided were written by R. E. Huddleston.
C
C     Abstract
C
C     Given a collection of points X(I) and a set of values Y(I) which
C     correspond to some function or measurement at each of the X(I),
C     subroutine  DPOLFT  computes the weighted least-squares polynomial
C     fits of all degrees up to some degree either specified by the user
C     or determined by the routine.  The fits thus obtained are in
C     orthogonal polynomial form.  Subroutine  DP1VLU  may then be
C     called to evaluate the fitted polynomials and any of their
C     derivatives at any point.  The subroutine  DPCOEF  may be used to
C     express the polynomial fits as powers of (X-C) for any specified
C     point C.
C
C     The parameters for  DPOLFT  are
C
C     Input -- All TYPE REAL variables are DOUBLE PRECISION
C         N -      the number of data points.  The arrays X, Y, W, R
C                  must be dimensioned at least  N  (N .GE. 1).
C         X -      array of values of the independent variable.  These
C                  values may appear in any order and need not all be
C                  distinct.
C         Y -      array of corresponding function values.
C         W -      array of positive values to be used as weights.  If
C                  W(1) is negative,  DPOLFT  will set all the weights
C                  to 1.0, which means absolute error will be minimized.
C                  to minimize relative error, the user should set
C                  weights to:  W(I) = 1.0/Y(I)**2, I = 1,...,N .
C         MAXDEG - maximum degree to be allowed for polynomial fit.
C                  MAXDEG  may be any non-negative integer less than  N.
C                  Note -- MAXDEG  cannot be equal to  N-1  when a
C                  statistical test is to be used for degree selection,
C                  i.e., when input value of  EPS  is negative.
C         EPS -    specifies the criterion to be used in determining
C                  the degree of fit to be computed.
C                  (1)  If  EPS  is input negative,  DPOLFT  chooses the
C                       degree based on a statistical F test of
C                       significance.  One of three possible
C                       significance levels will be used:  .01, .05 or
C                       .10.  If  EPS=-1.0 , the routine will
C                       automatically select one of these levels based
C                       on the number of data points and the maximum
C                       degree to be considered.  If  EPS  is input as
C                       -.01, -.05, or -.10, a significance level of
C                       .01, .05, or .10, respectively, will be used.
C                  (2)  If  EPS  is set to 0.,  DPOLFT  computes the
C                       polynomials of degrees 0 through  MAXDEG .
C                  (3)  If  EPS  is input positive,  EPS  is the RMS
C                       error tolerance which must be satisfied by the
C                       fitted polynomial.  DPOLFT  will increase the
C                       degree of fit until this criterion is met or
C                       until the maximum degree is reached.
C
C     Output -- All TYPE REAL variables are DOUBLE PRECISION
C         NDEG -   degree of the highest degree fit computed.
C         EPS -    RMS error of the polynomial of degree  NDEG .
C         R -      vector containing values of the fit of degree  NDEG
C                  at each of the  X(I) .  Except when the statistical
C                  test is used, these values are more accurate than
C                  results from subroutine  DP1VLU  normally are.
C         IERR -   error flag with the following possible values.
C             1 -- indicates normal execution, i.e., either
C                  (1)  the input value of  EPS  was negative, and the
C                       computed polynomial fit of degree  NDEG
C                       satisfies the specified F test, or
C                  (2)  the input value of  EPS  was 0., and the fits of
C                       all degrees up to  MAXDEG  are complete, or
C                  (3)  the input value of  EPS  was positive, and the
C                       polynomial of degree  NDEG  satisfies the RMS
C                       error requirement.
C             2 -- invalid input parameter.  At least one of the input
C                  parameters has an illegal value and must be corrected
C                  before  DPOLFT  can proceed.  Valid input results
C                  when the following restrictions are observed
C                       N .GE. 1
C                       0 .LE. MAXDEG .LE. N-1  for  EPS .GE. 0.
C                       0 .LE. MAXDEG .LE. N-2  for  EPS .LT. 0.
C                       W(1)=-1.0  or  W(I) .GT. 0., I=1,...,N .
C             3 -- cannot satisfy the RMS error requirement with a
C                  polynomial of degree no greater than  MAXDEG .  Best
C                  fit found is of degree  MAXDEG .
C             4 -- cannot satisfy the test for significance using
C                  current value of  MAXDEG .  Statistically, the
C                  best fit found is of order  NORD .  (In this case,
C                  NDEG will have one of the values:  MAXDEG-2,
C                  MAXDEG-1, or MAXDEG).  Using a higher value of
C                  MAXDEG  may result in passing the test.
C         A -      work and output array having at least 3N+3MAXDEG+3
C                  locations
C
C     Note - DPOLFT  calculates all fits of degrees up to and including
C            NDEG .  Any or all of these fits can be evaluated or
C            expressed as powers of (X-C) using  DP1VLU  and  DPCOEF
C            after just one call to  DPOLFT .
C***REFERENCES  SHAMPINE L.F., DAVENPORT S.M., HUDDLESTON R.E., *CURVE
C                 FITTING BY POLYNOMIALS IN ONE VARIABLE*, SLA-74-
C                 0270, SANDIA LABORATORIES, JUNE 1974.
C***ROUTINES CALLED  DP1VLU,XERROR
C***END PROLOGUE  DPOLFT
      INTEGER I,IDEGF,IERR,J,JP1,JPAS,K,K1,K1PJ,K2,K2PJ,K3,K3PI,K4,
     * K4PI,K5,K5PI,KSIG,M,MAXDEG,MOP1,NDEG,NDER,NFAIL
      REAL*4 TEMD1,TEMD2
      REAL*4 A(*),DEGF,DEN,EPS,ETST,F,FCRIT,R(*),SIGJ,
     * SIGJM1,SIGPAS,TEMP,X(*),XM,Y(*),W(*),W1,W11
      REAL*4 CO(4,3)
      DATA  CO(1,1), CO(2,1), CO(3,1), CO(4,1), CO(1,2), CO(2,2),
     1      CO(3,2), CO(4,2), CO(1,3), CO(2,3), CO(3,3),
     2  CO(4,3)/-13.086850,-2.4648165,-3.3846535,-1.2973162,
     3          -3.3381146,-1.7812271,-3.2578406,-1.6589279,
     4          -1.6282703,-1.3152745,-3.2640179,-1.9829776/
C***FIRST EXECUTABLE STATEMENT  DPOLFT
      M = IABS(N)
      IF (M .EQ. 0) GO TO 30
      IF (MAXDEG .LT. 0) GO TO 30
      A(1) = MAXDEG
      MOP1 = MAXDEG + 1
      IF (M .LT. MOP1) GO TO 30
      IF (EPS .LT. 0.0 .AND.  M .EQ. MOP1) GO TO 30
      XM = M
      ETST = EPS*EPS*XM
      IF (W(1) .LT. 0.0) GO TO 2
      DO 1 I = 1,M
        IF (W(I) .LE. 0.0) GO TO 30
 1      CONTINUE
      GO TO 4
 2    DO 3 I = 1,M
 3      W(I) = 1.0
 4    IF (EPS .GE. 0.0) GO TO 8
C
C DETERMINE SIGNIFICANCE LEVEL INDEX TO BE USED IN STATISTICAL TEST FOR
C CHOOSING DEGREE OF POLYNOMIAL FIT
C
      IF (EPS .GT. (-.55)) GO TO 5
      IDEGF = M - MAXDEG - 1
      KSIG = 1
      IF (IDEGF .LT. 10) KSIG = 2
      IF (IDEGF .LT. 5) KSIG = 3
      GO TO 8
 5    KSIG = 1
      IF (EPS .LT. (-.03)) KSIG = 2
      IF (EPS .LT. (-.07)) KSIG = 3
C
C INITIALIZE INDEXES AND COEFFICIENTS FOR FITTING
C
 8    K1 = MAXDEG + 1
      K2 = K1 + MAXDEG
      K3 = K2 + MAXDEG + 2
      K4 = K3 + M
      K5 = K4 + M
      DO 9 I = 2,K4
 9      A(I) = 0.0
      W11 = 0.0
      IF (N .LT. 0) GO TO 11
C
C UNCONSTRAINED CASE
C
      DO 10 I = 1,M
        K4PI = K4 + I
        A(K4PI) = 1.0
 10     W11 = W11 + W(I)
      GO TO 13
C
C CONSTRAINED CASE
C
 11   DO 12 I = 1,M
        K4PI = K4 + I
 12     W11 = W11 + W(I)*A(K4PI)**2
C
C COMPUTE FIT OF DEGREE ZERO
C
 13   TEMD1 = 0.0
      DO 14 I = 1,M
        K4PI = K4 + I
        TEMD1 = TEMD1 + W(I)*Y(I)*A(K4PI)
 14     CONTINUE
      TEMD1 = TEMD1/W11
      A(K2+1) = TEMD1
      SIGJ = 0.0
      DO 15 I = 1,M
        K4PI = K4 + I
        K5PI = K5 + I
        TEMD2 = TEMD1*A(K4PI)
        R(I) = TEMD2
        A(K5PI) = TEMD2 - R(I)
 15     SIGJ = SIGJ + W(I)*((Y(I)-R(I)) - A(K5PI))**2
      J = 0
C
C SEE IF POLYNOMIAL OF DEGREE 0 SATISFIES THE DEGREE SELECTION CRITERION
C
      IF (EPS) 24,26,27
C
C INCREMENT DEGREE
C
 16   J = J + 1
      JP1 = J + 1
      K1PJ = K1 + J
      K2PJ = K2 + J
      SIGJM1 = SIGJ
C
C COMPUTE NEW B COEFFICIENT EXCEPT WHEN J = 1
C
      IF (J .GT. 1) A(K1PJ) = W11/W1
C
C COMPUTE NEW A COEFFICIENT
C
      TEMD1 = 0.0
      DO 18 I = 1,M
        K4PI = K4 + I
        TEMD2 = A(K4PI)
        TEMD1 = TEMD1 + X(I)*W(I)*TEMD2*TEMD2
 18     CONTINUE
      A(JP1) = TEMD1/W11
C
C EVALUATE ORTHOGONAL POLYNOMIAL AT DATA POINTS
C
      W1 = W11
      W11 = 0.0
      DO 19 I = 1,M
        K3PI = K3 + I
        K4PI = K4 + I
        TEMP = A(K3PI)
        A(K3PI) = A(K4PI)
        A(K4PI) = (X(I)-A(JP1))*A(K3PI) - A(K1PJ)*TEMP
 19     W11 = W11 + W(I)*A(K4PI)**2
C
C GET NEW ORTHOGONAL POLYNOMIAL COEFFICIENT USING PARTIAL DOUBLE
C PRECISION
C
      TEMD1 = 0.0
      DO 20 I = 1,M
        K4PI = K4 + I
        K5PI = K5 + I
        TEMD2 = W(I)*((Y(I)-R(I))-A(K5PI))*A(K4PI)
 20     TEMD1 = TEMD1 + TEMD2
      TEMD1 = TEMD1/W11
      A(K2PJ+1) = TEMD1
C
C UPDATE POLYNOMIAL EVALUATIONS AT EACH OF THE DATA POINTS, AND
C ACCUMULATE SUM OF SQUARES OF ERRORS.  THE POLYNOMIAL EVALUATIONS ARE
C COMPUTED AND STORED IN EXTENDED PRECISION.  FOR THE I-TH DATA POINT,
C THE MOST SIGNIFICANT BITS ARE STORED IN  R(I) , AND THE LEAST
C SIGNIFICANT BITS ARE IN  A(K5PI) .
C
      SIGJ = 0.0
      DO 21 I = 1,M
        K4PI = K4 + I
        K5PI = K5 + I
        TEMD2 = R(I) + A(K5PI) + TEMD1*A(K4PI)
        R(I) = TEMD2
        A(K5PI) = TEMD2 - R(I)
 21     SIGJ = SIGJ + W(I)*((Y(I)-R(I)) - A(K5PI))**2
C
C SEE IF DEGREE SELECTION CRITERION HAS BEEN SATISFIED OR IF DEGREE
C MAXDEG  HAS BEEN REACHED
C
      IF (EPS) 23,26,27
C
C COMPUTE F STATISTICS  (INPUT EPS .LT. 0.)
C
 23   IF (SIGJ .EQ. 0.0) GO TO 29
      DEGF = M - J - 1
      DEN = (CO(4,KSIG)*DEGF + 1.0)*DEGF
      FCRIT = (((CO(3,KSIG)*DEGF) + CO(2,KSIG))*DEGF + CO(1,KSIG))/DEN
      FCRIT = FCRIT*FCRIT
      F = (SIGJM1 - SIGJ)*DEGF/SIGJ
      IF (F .LT. FCRIT) GO TO 25
C
C POLYNOMIAL OF DEGREE J SATISFIES F TEST
C
 24   SIGPAS = SIGJ
      JPAS = J
      NFAIL = 0
      IF (MAXDEG .EQ. J) GO TO 32
      GO TO 16
C
C POLYNOMIAL OF DEGREE J FAILS F TEST.  IF THERE HAVE BEEN THREE
C SUCCESSIVE FAILURES, A STATISTICALLY BEST DEGREE HAS BEEN FOUND.
C
 25   NFAIL = NFAIL + 1
      IF (NFAIL .GE. 3) GO TO 29
      IF (MAXDEG .EQ. J) GO TO 32
      GO TO 16
C
C RAISE THE DEGREE IF DEGREE  MAXDEG  HAS NOT YET BEEN REACHED  (INPUT
C EPS = 0.)
C
 26   IF (MAXDEG .EQ. J) GO TO 28
      GO TO 16
C
C SEE IF RMS ERROR CRITERION IS SATISFIED  (INPUT EPS .GT. 0.)
C
 27   IF (SIGJ .LE. ETST) GO TO 28
      IF (MAXDEG .EQ. J) GO TO 31
      GO TO 16
C
C RETURNS
C
 28   IERR = 1
      NDEG = J
      SIG = SIGJ
      GO TO 33
 29   IERR = 1
      NDEG = JPAS
      SIG = SIGPAS
      GO TO 33
 30   IERR = 2
c      CALL XERROR ( 'DPOLFT-INVALID INPUT PARAMETER.',31,2,1)
! you cannot write to error output in here! write(33,*) 'DPOLFT-INVALID INPUT PARAMETER.', ierr
      GO TO 37
 31   IERR = 3
      NDEG = MAXDEG
      SIG = SIGJ
      GO TO 33
 32   IERR = 4
      NDEG = JPAS
      SIG = SIGPAS
C
 33   A(K3) = NDEG
C
C WHEN STATISTICAL TEST HAS BEEN USED, EVALUATE THE BEST POLYNOMIAL AT
C ALL THE DATA POINTS IF  R  DOES NOT ALREADY CONTAIN THESE VALUES
C
      IF(EPS .GE. 0.0  .OR.  NDEG .EQ. MAXDEG) GO TO 36
      NDER = 0
      DO 35 I = 1,M
        CALL DP1VLU (NDEG,NDER,X(I),R(I),YP,A)
 35     CONTINUE
 36   EPS = SQRT(SIG/XM)
 37   RETURN
      END
!
!*****************************************************
!
!21jul2014 the next line was just "*DECK DP1VLU" (no quotes) is
!   * a comment?
!*DECK DP1VLU
      SUBROUTINE DP1VLU(L,NDER,X,YFIT,YP,A)
!***BEGIN PROLOGUE  DP1VLU
!***DATE WRITTEN   740601   (YYMMDD)
!***REVISION DATE  851223   (YYMMDD)
!***CATEGORY NO.  K6
!***KEYWORDS  CURVE FITTING,DOUBLE PRECISION,LEAST SQUARES,
!             POLYNOMIAL APPROXIMATION
!***AUTHOR  SHAMPINE, L. F., (SNLA)
!           DAVENPORT, S. M., (SNLA)
!***PURPOSE  Use the coefficients generated by DPOLFT to evaluate the
!            polynomial fit of degree L, along with the first NDER of
!            its derivatives, at a specified point.
!***DESCRIPTION
!
!       *** Double Precision Version of PVALUE ***
!
!     Written by L. F. Shampine and S. M. Davenport.
!
!     Abstract
!
!     The subroutine  DP1VLU  uses the coefficients generated by  DPOLFT
!     to evaluate the polynomial fit of degree  L , along with the first
!     NDER  of its derivatives, at a specified point.  Computationally
!     stable recurrence relations are used to perform this task.
!
!     The parameters for  DP1VLU  are
!
!     Input -- ALL TYPE REAL variables are DOUBLE PRECISION
!         L -      the degree of polynomial to be evaluated.  L  may be
!                  any non-negative integer which is less than or equal
!                  to  NDEG , the highest degree polynomial provided
!                  by  DPOLFT .
!         NDER -   the number of derivatives to be evaluated.  NDER
!                  may be 0 or any positive value.  If NDER is less
!                  than 0, it will be treated as 0.
!         X -      the argument at which the polynomial and its
!                  derivatives are to be evaluated.
!         A -      work and output array containing values from last
!                  call to  DPOLFT .
!
!     Output -- ALL TYPE REAL variables are DOUBLE PRECISION
!         YFIT -   value of the fitting polynomial of degree  L  at  X
!         YP -     array containing the first through  NDER  derivatives
!                  of the polynomial of degree  L .  YP  must be
!                  dimensioned at least  NDER  in the calling program.
!***REFERENCES  SHAMPINE L.F., DAVENPORT S.M., HUDDLESTON R.E., *CURVE
!                 FITTING BY POLYNOMIALS IN ONE VARIABLE*, SLA-74-0270,
!                 SANDIA LABORATORIES, JUNE 1974.
!***ROUTINES CALLED  XERROR,XERRWV
!***END PROLOGUE  DP1VLU
      INTEGER I,IC,ILO,IN,INP1,IUP,K1,K1I,K2,K3,K3P1,K3PN,K4,K4P1,K4PN,
     * KC,L,LM1,LP1,MAXORD,N,NDER,NDO,NDP1,NORD
      REAL*4 A(*),CC,DIF,VAL,X,YFIT,YP(*)
C***FIRST EXECUTABLE STATEMENT  DP1VLU
      IF (L .LT. 0) GO TO 12
      NDO = MAX0(NDER,0)
      NDO = MIN0(NDO,L)
      MAXORD = A(1) + 0.5
      K1 = MAXORD + 1
      K2 = K1 + MAXORD
      K3 = K2 + MAXORD + 2
      NORD = A(K3) + 0.5
      IF (L .GT. NORD) GO TO 11
      K4 = K3 + L + 1
      IF (NDER .LT. 1) GO TO 2
      DO 1 I = 1,NDER
 1      YP(I) = 0.0
 2    IF (L .GE. 2) GO TO 4
      IF (L .EQ. 1) GO TO 3
C
C L IS 0
C
      VAL = A(K2+1)
      GO TO 10
C
C L IS 1
C
 3    CC = A(K2+2)
      VAL = A(K2+1) + (X-A(2))*CC
      IF (NDER .GE. 1) YP(1) = CC
      GO TO 10
C
C L IS GREATER THAN 1
C
 4    NDP1 = NDO + 1
      K3P1 = K3 + 1
      K4P1 = K4 + 1
      LP1 = L + 1
      LM1 = L - 1
      ILO = K3 + 3
      IUP = K4 + NDP1
      DO 5 I = ILO,IUP
 5      A(I) = 0.0
      DIF = X - A(LP1)
      KC = K2 + LP1
      A(K4P1) = A(KC)
      A(K3P1) = A(KC-1) + DIF*A(K4P1)
      A(K3+2) = A(K4P1)
C
C EVALUATE RECURRENCE RELATIONS FOR FUNCTION VALUE AND DERIVATIVES
C
      DO 9 I = 1,LM1
        IN = L - I
        INP1 = IN + 1
        K1I = K1 + INP1
        IC = K2 + IN
        DIF = X - A(INP1)
        VAL = A(IC) + DIF*A(K3P1) - A(K1I)*A(K4P1)
        IF (NDO .LE. 0) GO TO 8
        DO 6 N = 1,NDO
          K3PN = K3P1 + N
          K4PN = K4P1 + N
 6        YP(N) = DIF*A(K3PN) + real(N)*A(K3PN-1) - A(K1I)*A(K4PN)
C
C SAVE VALUES NEEDED FOR NEXT EVALUATION OF RECURRENCE RELATIONS
C
        DO 7 N = 1,NDO
          K3PN = K3P1 + N
          K4PN = K4P1 + N
          A(K4PN) = A(K3PN)
 7        A(K3PN) = YP(N)
 8      A(K4P1) = A(K3P1)
 9      A(K3P1) = VAL
C
C NORMAL RETURN OR ABORT DUE TO ERROR
C
 10   YFIT = VAL
      RETURN
11    continue
!   no write to error in herewrite(33,*) 'DP1VLU-THE ORDER OF POLYNOMIAL EVALUATION, L(I1), R
!     1EQUESTED EXCEEDS THE HIGHEST ORDER FIT, NORD(I2), COMPUTED BY POLF
!     2IT -- EXECUTION TERMINATED.'
      RETURN
12    continue
! no write toerror in here:write(33,*) 'DP1VLU-INVALID INPUT PARAMETER.  ORDER OF POLYNOMIA
!     1L EVALUATION REQUESTED IS NEGATIVE -- EXECUTION TERMINATED.'
      RETURN
      END
!
!----------------------------------------
!
        SUBROUTINE findspace(aplan,i,ic)
        character*(*) aplan
! now count chars to next space
! set ii = current position in aplan
        ii = i
! ic is counter of chars to next space
        ic = 0
        do 320 j = 1, 15
           if(aplan(ii:ii).ne.' ') then
              ic = ic + 1
              ii = ii + 1
           else
              go to 321
           endif
320     continue
321     continue
        return
        end
!
!***********************************************
!
        SUBROUTINE findtime(nhr,nmin,nsec,ihr,imin,isec,iflg)

        integer*4 nhr,nmin,nsec,ihr,imin,isec,iflg
! n's - time in from nav file
! i's - time just inputted
! iflg = 1 time entered is greater than time just read in from nav file.
! iflg = 0 time entered is less than time just read in from nav file.
        iflg = 0
! check hr
        if(ihr.gt.nhr) then
         iflg = 1
         return
        elseif(ihr.lt.nhr) then
         return
        endif
c same hour, check min
        if(imin.gt.nmin) then
         iflg = 1
         return
        elseif(imin.lt.nmin) then
         return
        endif
c same min, check sec
        if(isec.gt.nsec) then
         iflg = 1
         return
        elseif(isec.lt.nsec) then
         return
        endif
        return
        end
!
!*******************************
!
        SUBROUTINE getdir(adir,len_adir,ierror,igderr)
!
! read seas2k file "xbtdirectory" to determine where we are
! to read and write out files
!
! Check to see if last char is a '\'.   If not, add it!
! open and read: siodir.txt
! INPUT:
!    none
! OUTPUT:
!    adir:     character string of path that is inside siodir.txt
!              (this is where the SIO data resides for cruise)
!    len_adir: integer length of adir
!    ierror(7) = 1 if error opening siodir.txt
!    ierror(17) = 1 if error reading siodir.txt
!    igderr(1) = 1 if iostat = 1 on open 31
!    igderr(2) = 1 if iostat = 1 on read 31
!    igderr(3) = 1 if iostat = 1 on close 31
! 
! file numbers opened and closed in here:
! 31=siodir.txt
        parameter (nerr=50)

        integer*4 ierror(nerr)
        integer*4 igderr(3)
        integer*4 len_adir, i
        integer*4 iw, ifile, ios
! interesting, adir*120 here, adir*80 is siosub:getdir, why?
        character adir*80, a*1
! 21jul2014: clear the 2 ierrors that could get set in here:
        ierror(7) = 0
        ierror(17) = 0
! 05sep2014  clear the 3 NEW ierrors that could get set in here:
        igderr(1) = 0
        igderr(2) = 0
        igderr(3) = 0

        len_adir = 0
        do 10 i = 1, 80
10      adir(i:i) = ' '

!02jul2014 change the path for seas10/Ibis:
        open(31,file='c:\Users\Public\Documents\siodir.txt',
     $          status='old', form='formatted',err=200,iostat=ios)
        if(ios.ne.0) igderr(1)=ios
        read(31,'(a)',err=201,iostat=ios) adir
        if(ios.ne.0) igderr(2)=ios
        close(31,iostat=ios)
        if(ios.ne.0) igderr(3)=ios

! search for "?" - indicates end of path
        do 20 i = 1, 80
           read(adir(i:i),'(a1)') a
           if(a(1:1).eq.'?') then
            len_adir = i-1
            go to 21
           endif
20      continue
21      continue
! Check to see if last char is a '\'.   If not, add it!
        if(adir(len_adir:len_adir).ne.'\') then
           len_adir = len_adir + 1
           adir(len_adir:len_adir) = '\'
        endif

        go to 250

! error opening 'xbtdirectory'
200     continue
        ierror(7) = 1
        go to 250
! error reading 'xbtdirectory'
201     continue
        ierror(17) = 1
        close(31,iostat=ios)
        if(ios.ne.0) igderr(3)=ios

250     continue
        return
        end
!
!*******************************
!
!23456789012345678901234567890123456789012345678901234567890123456789012
        SUBROUTINE getfilen(afilen,adosday,adosmon,adosyear,
     $                      idy,imn,iyr,adir,len_adir)
! translates dos date to the <date>.nav filename
! IN:   integer*4 iyr(4 digits), imn,idy 
!       adir*(*) character string of current directory
!       len_adir - length of adir
! OUT:  afilen - the <date>.nav file name in char
!       adosday - dos day in char
!       adosmon - dos month in char
!       adosyear - dos year in char (all 4 digits)

       character afilen*(*), adosday*2, adosmon*2, adosyear*4
       character atemp*10
       character*(*) adir
       integer*4 idy,imn,iyr,len,i1,i2,i3,n
       real*4 x

       if(len_adir.gt.0) afilen(1:len_adir) = adir(1:len_adir)

       atemp = '000000.nav'
       adosyear = '0000'
! get year as 2 digit year
       call int2ch(iyr,adosyear,1,len)
       call ch2real(adosyear,3,2,x)
       i1 = int(x)
       i2 = imn
       i3 = idy
       n = 5
       if(i1.lt.10) n = 6
       call int2ch(i1,atemp,n,len)
       n = 3
       if(i2.lt.10) n = 4
       call int2ch(i2,atemp,n,len)
       adosmon = '00'
       n = 1
       if(i2.lt.10) n = 2
       call int2ch(i2,adosmon,n,len)
       n = 1
       if(i3.lt.10) n = 2
       call int2ch(i3,atemp,n,len)
       adosday = '00'
       call int2ch(i3,adosday,n,len)

       afilen(len_adir+1:len_adir+5) = 'Data\' 
       afilen(len_adir+6:len_adir+15) = atemp(1:10)
       return
       end
!
!***************************************************
! NEVER USED- NOT WORKING!!!
!          SUBROUTINE getslen(astring,length)
! as it's named, get the length of the passed string -
! looking for the first blank.   If you want the blank
! part of the string length, will have to do something else...
!       integer length
!       character astring*(*), a*1
!
!       length = 0
!
!c search for " " - hopefully indicates end of string
!       do 20 i = 1, 100
!          read(astring(i:i),'(a1)') a
!          if(a(1:1).eq.' ') then
!             length = i-1
!             go to 21
!          endif
!20       continue
!21       continue
!
!       return
!       end
!
!***************************************************
!
        SUBROUTINE gettmtg(iweekday,ihr,imin,isec,timetag)
! calculate timetag (seconds into the week, Sunday am beginning)
! 86400 seconds in one day
! INPUT:
!       iweekday - integer*4, 0=sun,1=mon,2=tue,3=wed,4=thr,5=fri,6=sat
!       ihr,imin,isec - integer*4, hour, minute, seconds
! OUTPUT:
!       timetag - real*4
       
       integer*4 iweekday, ihr, imin, isec
       real*4 timetag

        timetag = (iweekday) * 86400.0
        timetag = timetag + ihr*3600.0 + imin*60.0 + real(isec)
        return
        end
!
!***************************************************
!
        SUBROUTINE int2ch(ka,a,jpos,len)
! translate integer to character, only works for +-xxxx or less
! IN:    ka = integer*4 to be translated
!        a = passed character string to write to
!        jpos = integer*4 position in character string to start writing at
! OUT:   len - integer*4 length of a
!
!        na = # of places in ka

       character*20 a1
       character*(*) a
       integer*4 ka, len, jpos, i, i0,i1,i2,i3,i4,na

       a1 = '                    '
       len = 1
       if(ka.lt.0) len = len + 1
       if(abs(ka).le.9) go to 50
       len = len + 1
       if(abs(ka).le.99) go to 50
       len = len + 1
       if(abs(ka).le.999) go to 50
       len = len + 1
       if(abs(ka).le.9999) go to 50
       len = len + 1
       if(abs(ka).le.99999) go to 50
       len = len + 1
       if(abs(ka).le.999999) go to 50
50       continue
       a1(jpos:jpos+len-1) = a(jpos:jpos+len-1)
       i = jpos
       i0 = 0
       i1 = 0
       i2 = 0
       i3 = 0
       i4 = 0
       if(ka.lt.0) then
        write(a1(i:i),500) char(45)
500        format(a1)
        i = i + 1
       endif
       if(abs(ka).ge.0.and.abs(ka).lt.10) na = 1
       if(abs(ka).ge.10.and.abs(ka).lt.100) na = 2
       if(abs(ka).ge.100.and.abs(ka).lt.1000) na = 3
       if(abs(ka).ge.1000.and.abs(ka).lt.10000) na = 4
       if(abs(ka).ge.10000.and.abs(ka).lt.100000) na = 5
       if(na.ge.5) then
        i0 = abs(ka)/10000
         write(a1(i:i),500) char(i0+48)
        i = i + 1
       endif
       if(na.ge.4) then
        i1 = (abs(ka) - i0*10000)/1000
         write(a1(i:i),500) char(i1+48)
        i = i + 1
       endif
       if(na.ge.3) then
        i2 = (abs(ka) - i0*10000 - i1*1000)/100
         write(a1(i:i),500) char(i2+48)
        i = i + 1
       endif
        if(na.ge.2) then
        i3 = (abs(ka) - i0*10000 - i1*1000 - i2*100)/10
         write(a1(i:i),500) char(i3+48)
        i = i + 1
       endif
       if(na.ge.1) then
        i4 = mod(abs(ka),10)
         write(a1(i:i),500) char(i4+48)
       endif
       a(jpos:jpos+len-1) = a1(jpos:jpos+len-1)
10       return
       end
!
!*********************************************
!
        SUBROUTINE interp(yrdrop,ylat,ylon,yrsav,zlat,zlon,yrnav,xlat,
     $xlon,iw,ifile)
! 25jul2014 add iw & ifile for writing. This is only called from gpspos
! INPUT:
!       yrdrop, real, decimal date of drop
!       yrsav, real, decimal date of drop past (beyond)
!       zlat, real, lat of past
!       zlon, real, lon of past
!       yrnav, real, decimal date of previous drop
!       xlat, real, lat of prev
!       xlon, real, lon of prev
!       iw, int,=0 no write log, =1 writ log
!       ifile, int, file number of log
! OUTPUT:
!       ylat, real
!       ylon, real

c yrsav,zlat,zlon: past
c yrnav,xlat,xlon: prev
        real*4 ylat,ylon,yrsav,zlat,zlon,yrnav,xlat,xlon
        real*4 frac
        integer*4 iw,ifile
!
        yrdenom = yrnav-yrsav
        if(yrdenom.eq.0.0) yrdenom = 0.001   ! kludge, do not divide by zero
        frac=(yrdrop-yrsav)/yrdenom
        ylat=zlat+(xlat-zlat)*frac
c 08aug2005 LL crossing 0 deg E... gotta fix...do in here instead of outside
       if(abs(xlon-zlon).gt.300.0) then
          if(xlon.gt.300.0)then
             zlon = zlon + 360.0
          elseif(zlon.gt.300.0)then
             xlon = xlon + 360.0
          endif
       endif
        ylon=zlon+(xlon-zlon)*frac
        if(iw.eq.1) then
         write(ifile,*)'    given as time(days), lat, lon '
         write(ifile,'("first fix :",3f13.5)')yrsav,zlat,zlon
         write(ifile,'("interp pos:",3f13.5)')yrdrop,ylat,ylon
         write(ifile,'("second fix:",3f13.5)')yrnav,xlat,xlon
        endif
        return
        end
!
!*******************************
!
        SUBROUTINE lev(aop,ierrlev)
       character*(*) aop
       integer*4 ierrlev

       ierrlev = 0
       if(aop(1:7).eq.'errtest') then
        if(aop(8:8).eq.'1') ierrlev = 1
        if(aop(8:8).eq.'2') ierrlev = 2
        if(aop(8:8).eq.'3') ierrlev = 3
        if(aop(8:8).eq.'4') ierrlev = 4
        if(aop(8:8).eq.'5') ierrlev = 5
        if(aop(8:8).eq.'6') ierrlev = 6
       endif
       return
       end
!
!**************************************************
!
!24jul2014 clean up, make sure all vars defined and
! NO writing to any output, set
! ierr=1 error opening nav file
! ierr=2 error end reading nav file
! ierr=3 error error reading nav file
        SUBROUTINE navopen(iday,imo,iyr,ierr,fnav,adir,len_adir)
! iyr 2 digit, ok for 2000
        character*10 ftemp, clats*2, clons*2
        character fnav*80, adir*80
        integer*4 iday, imo, iyr, ierr, len_adir

        if(len_adir.gt.0) fnav(1:len_adir) = adir(1:len_adir)

        ftemp='000000.nav'
        ierr=0
! open nav file
        if(iday.gt.9)write(ftemp(1:2),'(i2)')iday
        if(iday.lt.10)write(ftemp(2:2),'(i1)')iday
        if(imo.gt.9)write(ftemp(3:4),'(i2)')imo
        if(imo.lt.10)write(ftemp(4:4),'(i1)')imo
        if(iyr.gt.9) write(ftemp(5:6),'(i2)')iyr
        if(iyr.lt.10) write(ftemp(6:6),'(i1)')iyr
        fnav(len_adir+1:len_adir+5) = 'Data\'
        fnav(len_adir+6:len_adir+15) = ftemp(1:10)
        open(8,file=fnav,status='old',err=100)
! let's make sure there is at least one line of data in nav file too
        read(8,500,end=102,err=103)kday,kmo,kyr,khr,kmin,ksec,
     $       klat,zltm,clats,klon,zlnm,clons,spd,dir,nfix
500     format(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)
        rewind(8)
!
        return     ! all is well
100     continue
        ierr=1     ! error opening nav file
        return
102     continue
        close(8)
        ierr=2     ! end reading nav file
        return
103     continue
        close(8)
        ierr=3     ! error reading nav file
        return
        end
!
!*******************************
!
        SUBROUTINE newpos(speed,change,dir,vlat,vlat1,vlon1,aclath,
     $                    ierrlev,ifile)
! translate time just travelled into a new position
! IN:       real*4 speed
!       real*4 change
!       real*4 dir
!       real*4 vlat   AND the previous vlat1 and vlon1!!!
!       int*4 ierrlev - error writing yes or no
!       int*4 ifile - error file number - usually 33

! IN and OUT:
!       real*4 vlat1, vlon1 - new positions (pass in old to add change to
!       			get new

       real*4 speed, dir, change, vlat, vlat1, vlon1
       character*1 aclath
        integer*4 ifile,iw, ierrlev

       deg2rad = 3.141592654/180.0
       aclath = 'N'
       if(vlat.lt.0.0) aclath = 'S'
! translate nm/hour to nm/sec:
       speedsec = speed/3600.0
! distance we just travelled in ichange (time since average was made) seconds:
       distnew = change*speedsec
       if(ierrlev.ge.6) then
        write(ifile,*) ' newpos: distnew =', distnew
       endif
! change that to naut. miles of latitude.
       dxlatnm1 = distnew*cos(dir*deg2rad)
       dxlonnm1 = distnew*sin(dir*deg2rad)
! change naut. mile to degrees: (1 deg lat = 60 nm, 1 deg lon = 60*cos(lat) nm)
       dxlat1 = dxlatnm1/60.0
       x = cos(vlat*deg2rad)
       if(x.eq.0.0) then
        dxlon1 = dxlonnm1
       else
        dxlon1 = dxlonnm1/(60.0 * x)
       endif
! deadreckoning latitude:
! not sure if I really need this direction check, but just for sure:
       if(dir.gt.270.0.or.dir.lt.90.0) then
        vlat1 = vlat1 + abs(dxlat1)
       else
        vlat1 = vlat1 - abs(dxlat1)
       endif
! going E
       if(dir.ge.0.0.and.dir.lt.180.0) then
        vlon1 = vlon1 + abs(dxlon1)
! going W
       else
        vlon1 = vlon1 - abs(dxlon1)
       endif
       if(vlon1.gt.360.0) vlon1 = vlon1 - 360.0
       if(vlon1.lt.0.0) vlon1 = vlon1 + 360.0
       if(ierrlev.ge.6) then
        write(ifile,*)'out newpos,vlat1,vlon1',vlat1,vlon1
       endif

       return
       end
!
!***************************************
!
        SUBROUTINE planinfo(xlat,alath,xlat1,ahemi,aspec,ispec,
     $                      aplandir,iplandir)
! input:
!       xlat = decimal first position in plan.dat
!       alath = character hemisphere of first position in plan.dat
!       xlat1 = decimal second position in plan.dat
! OUTPUT:
!       ahemi = character hemisphere of first position in plan.dat
!       aspec = character "lon" or "lat"
!       ispec = integer of aspec, lat=1, lon=0
!       aplandir = character of direction heading between first and second position
!       iplandir = integer of direction heading between first and second position -
!                       N=1, E=2, S=3, W=4
! Note, when I say first position/second position - it could be the first and second
! in the file, or 2 further down in the file plan.dat since we might change direction
! midway inside plan.dat.

        character ahemi*1, alath*1, aspec*3, aplandir*1
        real*4 xlat, xlat1
        integer*4 ispec, iplandir

! New plan.dat, set ahemi ("base hemisphere" for comparison to see if change direction)
        ahemi = alath
! aspec:
        if(ahemi(1:1).eq.'E'.or.ahemi(1:1).eq.'e'.or.
     $     ahemi(1:1).eq.'W'.or.ahemi(1:1).eq.'w') then
           aspec = 'lon'
        else
           aspec = 'lat'
        endif
! ispec:
        ispec = 1
        if(aspec.eq.'lon') ispec = 0

        if(aspec.eq.'lat') then
         if(xlat.gt.xlat1) then
          aplandir = 'S'
          iplandir = 3
         elseif(xlat.lt.xlat1) then
          aplandir = 'N'
          iplandir = 1
         endif
        else
! 'lon'
         if(xlat.gt.xlat1) then
          aplandir = 'W'
          iplandir = 4
         elseif(xlat.lt.xlat1) then
          aplandir = 'E'
          iplandir = 2
         endif
         if(xlat.gt.350.0.and.xlat1.lt.10.0) then
          aplandir = 'W'
          iplandir = 4
         endif
         if(xlat.lt.10.0.and.xlat1.gt.350.0) then
          aplandir = 'E'
          iplandir = 2
         endif
        endif

        return
        end
!
!*********************************************
!
        SUBROUTINE real2ch(x,a,ipos,nrx,len)
! translate real to character, only works for +-xxxxxx.xxxxxx(or less positions)
! IN:       x = real*4 # to translate
!        a = passed character string to write to
!        integer*4 ipos = position in "a" to start writing 
!        integer*4 nrx = # positions to right of decimal point to translate
! OUT:       a - character out
!       integer*4 len - length of characters written

       real*4 x
       integer*4 ipos,nrx,len
       character*80 a2, a*(*)
       do 1 i = 1, 80
1       write(a2(i:i),'(a1)')' '

       if(nrx.eq.0)write(a2(1:14),'(f14.0)')x
       if(nrx.eq.1)write(a2(1:14),'(f14.1)')x
       if(nrx.eq.2)write(a2(1:14),'(f14.2)')x
       if(nrx.eq.3)write(a2(1:14),'(f14.3)')x
       if(nrx.eq.4)write(a2(1:14),'(f14.4)')x
       if(nrx.eq.5)write(a2(1:14),'(f14.5)')x
       if(nrx.eq.6)write(a2(1:14),'(f14.6)')x

       nb = 0
       len=14

c search for 1st nonblank character
       do 2 i = 1, 14
c put a 0 in front of decimal pt if number less than 0
       if(a2(i:i).eq.'.') then
        nb = nb - 1
        len = len + 1
        a2(i-1:i-1) = '0'
        go to 3
       endif
       if(a2(i:i).ne.' ')go to 3
       nb = nb + 1
       len = len - 1
2       continue

3       continue
! If translating 0 digits to the right of decimal pt, don't write dec pt.
       if(nrx.eq.0) len = len - 1
       if(len.eq.1)write(a(ipos:ipos),'(a1)') a2(1+nb:1+nb)
       if(len.eq.2)write(a(ipos:ipos+1),'(a2)') a2(1+nb:2+nb)
       if(len.eq.3)write(a(ipos:ipos+2),'(a3)') a2(1+nb:3+nb)
       if(len.eq.4)write(a(ipos:ipos+3),'(a4)') a2(1+nb:4+nb)
       if(len.eq.5)write(a(ipos:ipos+4),'(a5)') a2(1+nb:5+nb)
       if(len.eq.6)write(a(ipos:ipos+5),'(a6)') a2(1+nb:6+nb)
       if(len.eq.7)write(a(ipos:ipos+6),'(a7)') a2(1+nb:7+nb)
       if(len.eq.8)write(a(ipos:ipos+7),'(a8)') a2(1+nb:8+nb)
       if(len.eq.9)write(a(ipos:ipos+8),'(a9)') a2(1+nb:9+nb)
       if(len.eq.10)write(a(ipos:ipos+9),'(a10)') a2(1+nb:10+nb)
       if(len.eq.11)write(a(ipos:ipos+10),'(a11)') a2(1+nb:11+nb)
       if(len.eq.12)write(a(ipos:ipos+11),'(a12)') a2(1+nb:12+nb)
       if(len.eq.13)write(a(ipos:ipos+12),'(a13)') a2(1+nb:13+nb)
       if(len.eq.14)write(a(ipos:ipos+13),'(a14)') a2(1+nb:14+nb)

       return
       end
!
!*********************************************
!
! NOT CURRENTLY USING!
!       SUBROUTINE setdosdy(cday,cmon,cyear,i1,i2,i3)
! set dos(pc!) date (setdat(iyr,imo,iday)
! input: cday = real*4 gps day 
!        cmon = real*4 gps month
!        cyear = real*4 gps year 4 digit
!       real*4 cday, cmon, cyear
!       integer*2 j1, j2, j3
! day
!       j1 = hfix(cday)	
!       i1 = int(cday)
! month
!       j2 = hfix(cmon)	
!       i2 = int(cmon)	
! year
!       j3 = hfix(cyear)	
!       i3 = int(cyear)	
! set the pc date to incoming gps date:
!       call setdat(j3,j2,j1)
!       return
!       end
!
!******************************************************
!
        SUBROUTINE timetohms(timetag,ihr,imin,isec)
! translate timetag to ihr:imin:isec
! IN:       real*4 timetag
! OUT:       integer*4 ihr,imin,isec

       real*4 timetag, x
       integer*4 ihr,imin,isec

       ix = int(timetag/86400.0)
       x = timetag
       if(ix.gt.0) x = timetag - float(ix*86400)
       ihr = int(x/3600.0)
       imin = int((x - ihr*3600.0)/60.0)
       isec = int(x - ihr*3600.0 - imin*60.0)
       return
       end
!*****************************************************
! NOT CURRENTLY USING!
!NEW - check type here!
!        SUBROUTINE wrdate(cday,cmon,cyear,agpsdate)
! input: cday - current gps day
!        cmon - current gps month
!        cyear- current gps year
! output: character*8 agpsdate
!        character*8 agpsdate, a
!        real*4 cday, cmon, cyear, cy
!
!        a(1:8) = '00/00/00'
!                 12345678
! day
!        if(cday.le.9.0) then
!           call real2ch(cday,a,2,0,len)
!        else
!           call real2ch(cday,a,1,0,len)
!        endif
! month
!        if(cmon.le.9.0) then
!           call real2ch(cmon,a,5,0,len)
!        else
!           call real2ch(cmon,a,4,0,len)
!        endif
! year
!        if(cyear.ge.2000.0) then
!           cy = cyear - 2000.0
!        else
!           cy = cyear
!        endif
!
!        if(cy.le.9.0) then
!           call real2ch(cy,a,8,0,len)
!        elseif(cy.ge.10.and.cy.le.99) then
!           call real2ch(cy,a,7,0,len)
!        else
!        endif
!
!        agpsdate(1:8) = a(1:8)
!        return
!        end
!
!*****************************************************
!
        SUBROUTINE xbteta(xlatload,vlat1,vlon1,speed,
     $  dir,ctime,ispec,nplan,xlat,xlon,ixhr,ixmin,ixsec,
     $  ierrlev,nlnchr,peta,ifile)

! pass back eta (in hours) of probes in xlatload.   Note - if we start
! changing direction - this won't work for those positions until the
! direction change is "live".
! WILL HAVE TO MODIFY THIS ROUTINE FOR THAT SCENARIO -^
! INPUT: xlatload(nlnchr) - real*4 - positions of loaded launchers read in from plan.dat
!       		including current drop!!  in lat or lon
!       vlat1, vlon1 - last averaged ships position
!        vlat1 = dead reckoning lat of where we're suppose to be now
!       speed - last averaged ships speed
!       dir - last averaged ships direction
!        ctime - real*4 current time
!        ispec(12) - integer*4 - 1=lat or 0=lon - from plan.dat
!        nplan - integer*4 - # loaded probes in launcher (from control.dat only!)
!       xlat - latitude of upcoming xbt drop - should = xlatload(1) !
!       xlon - longitude of upcoming xbt drop - I think.
!       ierrlev - how much garbage to write out to sio.log
!       nlnchr - integer*4 number of launchers (can vary btwn sio and aoml)
!       	Note currently (08mar2005) nlnchr hardcoded to = 12
! OUTPUT:
!       peta(nlnchr) - probe eta in hours
!       ixhr
!       ixmin  these 3 not currently used...
!       ixsec
!
       real*4 xlatload(12),vlat1,vlon1,speed,dir,ctime,xlat,xlon
       real*4 peta(12)
       integer*4 nplan,ierrlev,ixhr,ixmin
       integer*4 ixsec,nlnchr, ispec(12)
       integer*4 iw,ifile
       character*1 alaunch,alatd*3,alatm*5,ahem*1
       character*8 ahour, amin

       deg2rad = 3.141592654/180.0
! calculate eta's for the next nlnchr (or less if near end) positions
! in plan.dat whether launcher loaded or not.

       neta = 0
! zero out peta(12)
       do 10 i = 1, 12
        peta(i) = 0.0
10       continue
       if(ierrlev.ge.6) then
        write(ifile,*) 'inside xbteta,ispec=',ispec
        write(ifile,*)'vlat1=',vlat1,' vlon1=',vlon1
        write(ifile,*)'speed=',speed,' dir=',dir
        write(ifile,*)'xlat=',xlat,' xlon=',xlon
       endif
 
! use nplan+1 because nplans is 2 to end, xlatload starts at 1 (upcoming drop)
        do 360 i = 1, nplan+1
!cc         if(aspec.eq.'lat') then
         if(ispec(i).eq.1) then
          dxlatld = vlat1 - xlatload(i)
          dxlatnml = abs( dxlatld * 60.0)
! if we're suppose to be moving  along latitude and we go directly
! 90 or -90 in direction this gets screwed
          x = cos(dir*deg2rad)
         if(ierrlev.ge.6) then
         write(ifile,*)'  LAT,dxlatld=',dxlatld,' dxlatnml=',dxlatnml,
     $                 ' x=',x
         endif
!23456789012345678901234567890123456789012345678901234567890123456789012
         if(x.ne.0.0) then
           distld = abs(dxlatnml / x)
          if(ierrlev.ge.6) write(ifile,*)' x.ne.0,distld=',distld
         else
! xlon here not defined for xlatload (2->) !!!
! this won't work!!!  If I calc an xlonload this would work, but in the mean
! time If we're going 90 or -90 in direction, then all our travelled distance
! is in the latitude direction, so let's just set distld=abs(dxlatnml).  May
! need to debug later:
           distld = abs(dxlatnml)
          if(ierrlev.ge.6) then
            write(ifile,*)'  x.eq.0,distld=',distld
          endif
! no xlon             dxlonld = vlon1 - xlon
! no xlon             if(abs(dxlonld).ge.300.0) then
! no xlon              dxlonld = 360.0 - (vlon1 - xlon)
! no xlon             endif
! no xlon             dxlonnm1 = abs( dxlonld * (60.0*cos(vlat1*deg2rad)))
! no xlon             distld = abs(dxlonnm1 / (sin(dir*deg2rad)))
         endif
         else
! 'lon'
          dxlonld = vlon1 - xlatload(i)
         if(ierrlev.ge.6) then
           write(ifile,*)'  dxlonld=',dxlonld
          endif
! 6jul2005 e-w and w-e cross 0 fix...
          if(abs(dxlonld).ge.300.0) then
          if(dxlonld.gt.300.0) then
            dxlonld = 360.0 - (vlon1 - xlatload(i))
            write(ifile,*)'  new dxlonld=',dxlonld
          elseif(dxlonld.lt.300.0) then
            dxlonld = 360.0 + (vlon1 - xlatload(i))
            write(ifile,*)'  new dxlonld=',dxlonld
          endif
          endif
          dxlonnm1 = abs( dxlonld * (60.0*cos(vlat1*deg2rad)))
          x = sin(dir*deg2rad)
         if(ierrlev.ge.6) then
!23456789012345678901234567890123456789012345678901234567890123456789012
          write(ifile,*)'  LON',vlon1,xlatload(i),
     $                                 ' =dxlonld=',dxlonld
           write(ifile,*)'  dxlonnm1=',dxlonnm1,' x=',x
         endif
         if(x.ne.0.0) then
           distld = abs(dxlonnm1 / x)
         else
           distld = abs(dxlonnm1)
! same here, see above "no xlon"
!             dxlatld = vlat1 - xlat
!             dxlatnml = abs( dxlatld * 60.0)
!             distld = abs(dxlatnml / cos(dir*deg2rad))
         endif
          if(ierrlev.ge.6) write(ifile,*) '  distld=',distld
         endif
        if(speed.eq.0.0) then
          eta = distld
        else
          eta = distld/speed
        endif
! eta in hour to next probes:
        neta = neta + 1
        peta(neta) = eta
!         if(ierrlev.ge.6) write(ifile,*) '  peta(',neta,')=',eta

355      continue
        
360     continue
100     continue
        return
        end
!*****************************************************
!
! gfortran compatibility wrappers for Lahey/Intel gettim and getdat
! These replace the non-standard compiler intrinsics using
! standard Fortran DATE_AND_TIME.
!
        SUBROUTINE gettim(ihr,imin,isec,ihsec)
        integer*2 ihr,imin,isec,ihsec
        integer idt(8)
        call DATE_AND_TIME(VALUES=idt)
        ihr   = idt(5)
        imin  = idt(6)
        isec  = idt(7)
        ihsec = idt(8) / 10
        return
        end
!
        SUBROUTINE getdat(iyr,imo,iday)
        integer*2 iyr,imo,iday
        integer idt(8)
        call DATE_AND_TIME(VALUES=idt)
        iyr  = idt(1)
        imo  = idt(2)
        iday = idt(3)
        return
        end
