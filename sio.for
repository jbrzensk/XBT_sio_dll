! SEARCH FOR TODO
! 28apr2016 chkprof output log mods only - try recompile on del xps13
!
!08jan2016 working on chkprof, currently just comments. Will note if any
!actuall changes here
!
!05jun2015LL mods to prstat&wrxmit - do NOT zero out all ierror(*)! in prstat&wrxmit -
!   you zero'd out your jptr and icall passing (duh)
!
!19may2015LL, comment out your code for sio,seas2s,wrdrpstn hhmmss.log
!   (never gave to ibis anyway). Keep your changes for chkprof about if
!   the test station is shallower than test depth (mark is bad).
!18aug2014, work on seas2s, wrdrpstn,
!23jul2014 seas2s- add errs 41->47 for open/read/close, plus rdcntrl mods!
!21jul2014 LL try to set ierror=0 at beginning of each routine
!18jul2014 LL wrdrpstn- mods for t700 from seas plus check format spec.
!11jul2014 SioTimeBegin, if ok read stations.dat, skip over 70 cont ierror!
!15-17jun2013 LL try to shutoff the deadmin/dropmin alarms...
! 18jul2005 - add new subroutine SioTimeBegin - this is a "replacement"
! for siobegin while running under Seas Time plan.

! ok now let's do 300 instead of 3 and make the 00 the alarm number...
! 16jun2005 - change to ierror(35), now I'm setting it to 3 whenever
!  I want seas to ALARM IMMEDITATELY! (ack)
! 27may2005 - add ierror(35)=iSIOErrWatchDog!
! search for 19feb2005 LL for dayroll problems.
! search for FIX!   and debug.... 
!***************************************
!23456789012345678901234567890123456789012345678901234567890123456789012       
!        1         2         3         4         5         6         7 
!
        SUBROUTINE siobegin(deadmin,dropmin,relodmin,runsec,xmaxspd,
     $     launcher,igps,xlat,xlatload,nplan,ibuf,
     $     idsec2,ierrlev,alrmtime,ifirst,irollnav,
     $     inav,ispec,dtime,yrday1,ierror,iaveflg,ispd,itime,
     $     idayave,imonave,iyerave,icday1,iplandir,
     $     speed,dir,timeave,vlat,vlon,
     $     nlnchr,nextdrop,iplancnt,iwait,
     $     chr,cmin,csec,cday,cmon,cyear,isio_skip_count)
!
! INPUT from Seas:
! 21sep2004 igps per JB and LL:
!        //igps  =1 if transceiver
!        //igps =2 if DeadReck
!        //igps = 2 if in transition between xver and DeadReck.
!------------------------------------------------------------
!        ((((((OLD-> igps = 1 have a gps, igps = 2 no gps - DR only)))))
!
!        iwait - sio_wait_flag - see below.
!        isio_skip_count - if iwait=5 must look at this counter to  know
!        		HOW MANY positions to skip in plan.dat
!        chr, cmin, csec   - incoming time from Seas  (I now consider these my)
!        cday, cmon, cyear   - incoming date from Seas( DOS day & time...     )

! OUTPUT
!        deadmin - read from control.dat
!        dropmin - read from control.dat
!        relodmin - read from control.dat
!        runsec - read from control.dat
!        xmaxspd - read from control.dat
!        launcher(12) - read from control.dat (23sep2004- not actually using this!)
!        xlat - position (either lat or lon depending on aspec) of next xbt drop
!        xlatload(12) - position (lat or lon) of next 1-x drops (xlatload(1)=xlat)
!        	18oct2004 NOTE - change xlatload to be 0-180 E -1to -180 for west!
!        		when I use it, change it back!!!!  duh.
!        nplan - I think this is how many drops in plan.dat we have loaded xbt's for.
!        ibuf - initialize to 0 here, counter of # of positions in gps buffer to 
!        	calc ave position
!        idsec2 - dos second at time of this being called  CHANGE!!! Don't use DOS!
!        	- ok done, this is JB passed in second at time of this called.
!        ierrlev - set to 6 for debug logging, 0 for no logging. 23sep2004 - I read
!         	Operator name in control.dat.   If="debug" then ierrlev=6
!        alrmtime - currently uses dos time to set an alarm for time past drops
!         	to alert op.   CHECK
!        ifirst - initialize to 0 here, once run thru sioloop once, set to 1 -
!        	check to see if this and irunsec(rm'd 17feb2004) are same now.
!        irollnav - initialize to 0 here.   Set to 1 once it's time to roll nav file
!        inav - init to 0 here.  Set to 1 to write to nav file once a minute if gps input.
!         ispec(12) - 08mar2005 change this to an array so each position read in
!        	from plan.dat has a corresponding direction.   This is to help
!        	with mixed plans (mixed lats and lons) and to help with subroutine xbteta.
!                ispec - aspec=lat or lon - depending on plan.dat.  What about switching INSIDE
!        	of plan.dat.   I tried to account for that.   Not tested.
!        	ispec=0 means plan is longitude based.
!        	ispec=1 means plan is latitude based.
!        dtime - dos time in seconds of current day only  Keep using dostime for
!                my alrmtime and stoptime ONLY
!        yrday1 - yearday of 3 drops previous to most recent drop.
!        ierror(50) - init all to 0 here, set to 1 for various errors
!        	  Recall that in Seas ierror runs 0-39 and I am 1-40.
!                 27may2005 - add ierror(35)=iSIOErrWatchDog! Gauge will send
!        	  ierror(35)=0.   If all is "well" I set ierror(35)=2, if all 
!        	  is NOT well I set ierror(35)=3.   If Gauge gets back a 0 or 3
!         	  then it will stop sending the reset pulse to the watchdog timer.
!        iaveflg - init to 2 here - means NO data to DR from
!        	- set to 1 if have new average position/speed/dir to DR from.
!        	- set to 0 means keep DR'ing from last position
!        ispd - init to 0 here, if I read in a speed from control.dat/nav file,
!        	then set to 1.
!        itime - init to 0 here.   (OLD:Once I have a gps time, set to 1 in sioloop)
!              -25feb2004 NEW- base for timer.  Set to 0 here, increment in seconds
!              - inside sioloop.   Run alrmtime and stoptime off of here.
!        idayave,imonave,iyerave(4) - day,mon,year of last averaged position
!        navday,navmon,navyear(4) - day,mon,year of last position in <date>.nav file
!        icday - incoming day from seas (comes in as cday, icday is int(cday)
!        icday1 - set to icday here in siobegin.   In sioloop compare incoming icday
!        	 to this icday1.   If same, we're on the same day.   If different we
!        	 need to write to the previous date.nav file!
!       iplandir = integer of direction heading between first and second position -
!                       N=1, E=2, S=3, W=4
!                   - it could be the first and second in plan.dat, or 2 further down
!        		 in the file plan.dat since we might change direction
!                         midway inside plan.dat.
!        speed - init to = -0.00009, if read in speed from navtrk or nav, set to
!        	value read in (then set ispd=1 too)
!        dir - init to 0.0, if read in dir from navtrk or nav, set to value read in
!        timeave - time in seconds of last averaged position (1 day of secs) I think.
!        vlat - matching lat to timeave
!        vlon - matching lon to timeave
!        nlnchr - # tubes with xbt - I get this from launcher(12) from control.dat
!            20sep2004 -nope- just setting this to 12 (was 11 ?)
!        nextdrop - telling seas2k what drop number is next in our sequence.
!             It will =1 for first drop, will = next drop number after last drop
!              in stations.dat
!        iplancnt - telling Seas what drop # I'm on inside plan.dat
!        inavfile- what position to use as last averaged position upon startup
!        	=0 (default) use navtrk.dat  (why I have 0 and 2... ?)
!        	=1 use date.nav
!        	=2 error reading date.nav, use navtrk.dat


        parameter(nerr=50)
!GCC$ ATTRIBUTES DLLEXPORT :: siobegin
! OLD:   NEED TO FIX CODE to match JBs definition of igps - basically she will
! be changing it often!
! igps: =1 have incoming messages on the com port - we have a gps plugged in
!       =2 no gps whatsoever - it's been thrown overboard
!          load dos date/time into acmsg and do time loop, and deadreckoning
!          portion of position loop
!
! file numbers opened and closed in siobegin:
!        7=stations.dat (read)
!       10=<date>.nav   (read)
!       13=plan.dat     (read)
!       15=navtrk.dat   (read)
!       22=control.dat (sub rdcntrl) (read)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=sio.log (write)
!
! ierrors set in here:  looks like I'm clearing all at begin
!( 1) - At drop position!  Drop the probe.   (dostime GE stoptime)
!( 2) - error reading <date>.nav file
!( 5) - error opening <date>.nav file
!( 7) - error opening siodir.txt
!(13) - seas year is bad, stop and fix
!(15) - error opening control.dat
!(16) - error reading control.dat
!(17) - error reading siodir.txt
!(19) - error opening plan.dat
!(20) - error reading plan.dat
!(21) - reached end of plan.dat
!(22) - first 2 positions in plan.dat are equal.  Can not do that.
!(23) - error opening navtrk.dat
!(24) - error reading navtrk.dat
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(33) - if operator name = "debug" turn on ierrlev=6 (fill .log files)
!(34) - integer of day (dd) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(35) - iSIOErrWatchDog , set to 2 at end of program if all is well.
!                       set to 3XX where XX is above error number to alert
!                       an error was set
!     = 307,313,315,316,317,319,320,322,325,326
!(36) - integer of month (mm) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(37) - integer of year (yy) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(44) - ALL    - error opening calling routine's .log file (eg sio.log)
!(45) - ALL    - error writing calling routine's .log file (eg sio.log)


        real*4  deadmin, dropmin, relodmin, runsec, xmaxspd
        real*4 xlat, xlatload(12), alrmtime, dtime, yrday1
        real*4 speed, dir, timeave, vlat, vlon
        integer*4 launcher(12), igps, nplan, ibuf
        integer*4 idsec2,ierrlev
        integer*4 ifirst, irollnav, inav, ispec(12)
        integer*4 ierror(nerr), iaveflg, ispd, itime
        integer*4 idayave, imonave, iyerave, icday1
        integer*4 navday, navmon, navyear, isio_skip_count
        integer*4 iplandir,nlnchr,nextdrop,iplancnt, iwait

        character aspec*3, aline*70, aplanline*80
        character afilen*80, adosday*2, adosmon*2, adosyear*4
        character alathnav*1, alonhnav*1
        character alath*1, alath1*1, aplandir*1, ahemi*1
        character avlath*1, avlonh*1
        integer*2 j1, j2, j3, j4
        integer*4 icday,icmon,icyear
        integer*4 ilontest,linecnt,inavfile,i,latd,latd1
        integer*4 ilatdnav,ilondnav,iy,im,id,ih,imi,is
        real*4 xlatmnav,xlonmnav
        real*4 xlatm,xlatm1,xlat1,x

! rdcntrl:
        integer*4 len_acruise
        real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
        real*4 tm_pl_mx, tm_pl_mn
        character*7 acruise

        character*80 asio, anavtrk, aplan, astations
        character*80 adir, acontrol
! 05 & 08sep2014 cleanup...
        integer*4 iw, file, ios, len_adir
        integer*4 igderr(3)
        integer*4 indx, igooddrp
        real*4 chr,cmin,csec, cday,cmon,cyear
        integer*4 iphr, ipmn, ipsc
        integer*4 idhr, idmin, idsec
!
! DIAGNOSTIC
        open(98,file='C:\Temp\sio_called.txt',
     $       form='formatted',status='unknown',err=9997)
        write(98,*) 'siobegin called'
        close(98)
9997    continue
! END DIAGNOSTIC
        speed = -0.00009
        dir = 0.0
        nplan = 0
        iplancnt = 0    ! BRZENSKI initialize to zero, prevent garbage.
        nextdrop = 0
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! fill xlatload with 999.0 for no value
! (-99.0 will not work since that is a valid west longitude)
!   guard against caller passing uninitialized isio_skip_count:
        if(isio_skip_count.lt.0) isio_skip_count = 0
        do 5 i = 1, 12
         xlatload(i) = 999.0
5       continue

! zero out error array:
! this is ok for ierror(35) because Gauge is sending me a 0.
        do 10 i = 1, nerr
           ierror(i) = 0
10      continue
!
! get seas2k path:
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open(7)  or read(17) file siodir.txt containing 'xbtdirectory', 
!   then exit out
        if(ierror(7).eq.1) then
           len_adir = 0
           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
           ierror(35) = 317
           go to 999
        endif

! initialize all path strings to spaces to prevent garbage chars in filenames
        asio      = ' '
        astations = ' '
        aplan     = ' '
        anavtrk   = ' '
        acontrol  = ' '

        if(len_adir.gt.0) then
           asio(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
           aplan(1:len_adir) = adir(1:len_adir)
           anavtrk(1:len_adir) = adir(1:len_adir)
           acontrol(1:len_adir) = adir(1:len_adir)
        endif

        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
        acontrol(len_adir+1:len_adir+16) =  'Data\control.dat'
        aplan(len_adir+1:len_adir+8) =      'plan.dat'
        anavtrk(len_adir+1:len_adir+15) =   'Data\navtrk.dat'

! 05nov2014 write separate sioHHMMSS.log for each time called! fill disk!
!You cannot tie to ierror(33), rdcntrl has not been called yet (duh)
! try to tie this to operator=debug....
! this is basic one:
         asio(len_adir+1:len_adir+12) =      'Data\sio.log'
! this is lab debugging one:
!         asio(len_adir+1:len_adir+18) =      'Data\sio000000.log'
!                                             123456789012345678
!                                                      1
!         CALL gettim(j1,j2,j3,j4)
!         if(j1.le.9) then
!            write(asio(len_adir+10:len_adir+10),'(i1)') j1
!         elseif(j1.gt.9.and.j1.lt.99) then
!            write(asio(len_adir+9:len_adir+10),'(i2)') j1
!         endif
!         if(j2.le.9) then
!            write(asio(len_adir+12:len_adir+12),'(i1)') j2
!         elseif(j2.gt.9.and.j2.lt.99) then
!            write(asio(len_adir+11:len_adir+12),'(i2)') j2
!         endif
!         if(j3.le.9) then
!            write(asio(len_adir+14:len_adir+14),'(i1)') j3
!         elseif(j3.gt.9.and.j3.lt.99) then
!            write(asio(len_adir+13:len_adir+14),'(i2)') j3
!         endif
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
! force-close unit 33 in case sioloop left it open (concurrent call)
        close(ifile,iostat=ios)
        open(ifile,file=asio,form='formatted',access='append',
     $       status='unknown',err=333,iostat=ios)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
! write open failure reason to diagnostic file
        open(98,file='C:\Temp\sio_called.txt',
     $       form='formatted',status='unknown',err=334)
        write(98,*)'siobegin log open failed, iostat=',ios
        close(98)
334     continue
        if(iw.eq.1) then  
         write(ifile,*,err=335)'IN SIOBEGIN'
         if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
         if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
         if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
         write(ifile,*) 'adir=',adir(1:len_adir)
         write(ifile,*) 'asio=',asio(1:len_adir+12)
         write(ifile,*) 'astations=',astations(1:len_adir+17)
         write(ifile,*) 'acontrol=',acontrol(1:len_adir+16)
         write(ifile,*) 'aplan=',aplan(1:len_adir+8)
         write(ifile,*) 'anavtrk=',anavtrk(1:len_adir+15)
         write(ifile,*)'incoming iwait=',iwait
         call flush(ifile)
         go to 336 
335      ierror(45) = 1
         iw = 0
336      continue
        endif 
!
! set ierrlev=0 as default, modify if op="debug"
        ierrlev=0
!
        adosday = '00'
        ifirst = 0
        inav = 0
        iaveflg = 2
        ispd = 0
        itime = 0
        irollnav = 0
        ibuf = 0
! 02jun2005 yikes, all iwait: iplancnt = iplancnt + isioskipcount!
! except iwait=2, then it's just one more
! we are changing this to "sio_wait_flag".   JB sends me a flag of:
!        iwait = 1	Run as normal
!NO DO!        iwait = 2	See iwait=5.  If iwait=2 then we had a water hit AND we have
!        		information written to stations.dat.   So now skip current drop
!                       entirely, go to next drop (think about this carefully!)
!NO DO!        iwait = 3	Run for X minutes past drop.   So if we think drop is now, send
!        		JB drop flag in now + X minutes.
!        iwait = 4	Send JB drop flag immediately!
!        		02jun2006:
!         		hmm.   iwait=4 - send drop flag now, inc iplancnt by skipcount only!
!         		that means do NOT go to next posn in plan.dat then inc by skip count.
!         		???start w/iplancnt = iplancnt -1 (which is last posn done) then inc by skip count.  ???
!        iwait = 5	See iwait=2.  If iwait=5 we had NO water hit so we need to look
!        		at isio_skip_count to determine HOW MANY positions in plan.dat
!        		to skip!!!   Since iwait=2 will only skip 1 position
!        iwait = 6	duplicate of iwait=1 (testing!)
!        iwait = 7	send drop flag NOW, do NOT increment iplancnt
!        isio_skip_count - See iwait=5


! 09jun2006 initialize runsec to 0.0
        runsec = 0.0

        if(iwait.eq.1) then
           if(iw.eq.1)write(ifile,*)'iwait=',iwait,' RUN NORMAL'
        elseif(iwait.eq.2) then
           if(iw.eq.1)write(ifile,*)'initally iwait=',iwait
           iwait = 1
           if(iw.eq.1)write(ifile,*)'iwait=',iwait,' RUN NORMAL'
        elseif(iwait.eq.3) then
c should never happen!
           if(iw.eq.1)write(ifile,*)'initally iwait=',iwait
           iwait = 1
           if(iw.eq.1)write(ifile,*)'iwait=',iwait,' RUN NORMAL'
           if(iw.eq.1)write(ifile,*)'set runsec to 300.0'
!??           write(ifile,*)'iwait=',iwait,' Run 5 min past nrml drp pos'
           runsec = 300.0
        elseif(iwait.eq.4) then
           if(iw.eq.1)then
              write(ifile,*)'iwait=',iwait,' Send drop flag NOW'
              write(ifile,*)'isio_skip_count=',isio_skip_count
           endif
           ierror(1) = 1
        elseif(iwait.eq.5) then
           if(iw.eq.1)then
              write(ifile,*)'iwait=',iwait, 'no water hit, skip'
              write(ifile,*)'isio_skip_count=',isio_skip_count,
     $                      ' positions'
           endif
        elseif(iwait.eq.6) then
           if(iw.eq.1)write(ifile,*)'iwait=',iwait,
     $                   ' RUN NORMAL-I think!Inc by 1 only'
        elseif(iwait.eq.7) then
           if(iw.eq.1)write(ifile,*)'iwait=',iwait,' Send drop flag NOW'
           ierror(1) = 1
        else
! if it's anything else, set it to 1 !
           if(iw.eq.1)write(ifile,*)'initally iwait=',iwait
           iwait = 1
           if(iw.eq.1)write(ifile,*)'iwait=',iwait,' RUN NORMAL'
        endif
!
        CALL rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $         deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $         dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $         ichkprofdepth)
!
        if(ierror(15).ne.0) then           ! error opening control.dat
              ierror(35) = 315
              go to 999
        elseif(ierror(16).ne.0) then           ! error reading control.dat
              ierror(35) = 316
              go to 999
        endif
!
        if(ierror(33).eq.6) then                !  this is "debug"
           ierrlev = 6
        endif
!
        if(iw.eq.1) then
         write(ifile,*)'after rdcntrl'
         write(ifile,'(a7)')'acruise=', acruise
         write(ifile,*)'xmaxspd=', xmaxspd
         write(ifile,*)'deadmin=', deadmin,' dropmin=', dropmin
         write(ifile,*)'relodmin=', relodmin,' tm_pl_mn=', tm_pl_mn
         write(ifile,*)'launcher=', launcher
         call flush(ifile)
        endif
!
        relodsec = relodmin*60.0
        dropsec = 60.0*dropmin
!
! 27aug2003 - JB currently puts only -99's for launcher sequence
! so I guess if 1st value is -99 - we're handlaunching.   But she
! controls the launcher so it doesn't matter if I know we're handlaunching or not!
! nlnchr = # tubes in use
! test:
        nlnchr = 12
! 26jun2003 JB wants etas even if no launchers loaded and/or 
! handlaunching.   Go ahead and try to set nlnchr=12 if
! launcher(1)=0 (hand launch).   See what it breaks...

! If we are hand launching (launcher(1) = 0) 
!8sep2014 comment out:        if(launcher(1).eq.0) go to 32

! new JB - since we are trying to run with aoml we have up to 12
! launchers available.   Ones not in use set to -99, figure out
! 2aug2004        do 26 i = 1, 12
! 2aug2004         if(launcher(i).ne.-99) nlnchr = nlnchr + 1
! 2aug200426        continue
!        if(ierrlev.eq.6) write(ifile,*)'nlnchr =',nlnchr

32        continue
! following 3 lines use to translate DOS to isvars, now it's JB incoming time:
! note, cyear comes in as 4 digit year (plus decimal point.0)
        icyear = int(cyear)
        icmon = int(cmon)
        icday = int(cday)
        icday1 = icday
        if(iw.eq.1)
     $    write(ifile,*)'cyr,cmn,cdy=',icyear,icmon,icday
        call flush(ifile)
! check the year coming in from seas.
! If less than 2003, stop with an error and tell user to reset dos date:
!08jul2013 LL change 2006 to 2013
        if(real(icyear).lt.2014.0) then
          ierror(13) = 1              ! seas incoming year is < 2014
          ierror(35) = 313
          go to 999
        endif
! check for existence of todays date.nav file; if it exists read up 
! to the end (so don't write over old nav data).  calling getfilen with
! the incoming icday,icmon,icyear from seas:
! getfilen just converts incoming seas date to char dat (ados-blah)
        CALL getfilen(afilen,adosday,adosmon,adosyear,icday,
     $                icmon,icyear,adir,len_adir)
        if(iw.eq.1) write(ifile,*)'afilen=', afilen(1:len_adir+15)
        call flush(ifile)
!
!23456789012345678901234567890123456789012345678901234567890123456789012       
! open and read date.nav file, check each line to see if it's
! valid, then rewind file and read number of valid lines.  This
! is to help get past those pesky ctrl-z - note, only really skips
! last line if screwy!
        linecnt = 0
        open(10,file=afilen,form='formatted',status='unknown', err=3,
     $          iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*)'1open10ios=',ios
1       read(10,555,end=2,err=310,iostat=ios) ilontest
555     format(t33,i3)
        if(ilontest.ge.0.and.ilontest.le.360) then
           linecnt = linecnt + 1
           go to 1               ! see if there is another line to read
        endif
2       rewind(10,iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*)'1rewind10ios=',ios
        if(linecnt.gt.2) then
         do 61 i = 1, linecnt-1
61        read(10,*,err=310)      ! skip lines before last line
        endif
! read in last position in nav file.  Check to see if it is more
! current than what is in navtrk.dat.  If so, DR from here.  This is
! in case op used edit to update nav files - then he doesn't do navtrk.dat...
! default set inavfile=0 that means use navtrk.dat
! only do if NO_GPS option set!
        if(linecnt.eq.0) go to 310
        inavfile = 0
        read(10,599,err=310)navday,navmon,navyear,navhr,navmin,navsec,
     $           ilatdnav, xlatmnav, alathnav, ilondnav, xlonmnav,
     $           alonhnav, speednav, dirnav
599        format(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,
     $         i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,1x,
     $         a1,4x,f6.2,f6.1)
        if(iw.eq.1)then
           write(ifile,*)'last nav file entry:'
           write(ifile,599) navday,navmon,navyear, navhr,navmin,navsec,
     $           ilatdnav, xlatmnav, alathnav, ilondnav, xlonmnav,
     $           alonhnav, speednav, dirnav
           call flush(ifile)
        endif
        navyear = navyear + 2000
        go to 4
!
3       ierror(5) = 1               ! error opening date.nav
        ierror(34) = icday         ! pass seas the ddmmyy of date.nav
        ierror(36) = icmon
        ierror(37) = icyear
        inavfile=2                  ! use navtrk.dat for last known position
        if(iw.eq.1)then
           write(ifile,*) 'Error opening ', afilen(1:len_adir+15)
           write(ifile,*) 'use navtrk.dat for last known position'
        endif
        go to 4
!
310     ierror(2) = 1              ! error reading date.nav
        ierror(34) = icday         ! pass seas the ddmmyy of date.nav
        ierror(36) = icmon
        ierror(37) = icyear
        inavfile=2                 ! use navtrk.dat for last known position
        if(iw.eq.1)then
           write(ifile,*) 'Error reading ', afilen(1:len_adir+15)
           write(ifile,*) 'use navtrk.dat for last known position'
        endif
!
4       continue
! 20feb2004 close date.nav file and reopen in sioloop as append!
        close(10,iostat=ios)
        if(iw.eq.1)write(ifile,*) 'closing ', afilen(1:len_adir+15),
     $                            'ios= ', ios

! Read in last updated (and averaged) position info from navtrk.dat.
! If gps not updating when program starts, begin deadreckoning from here. 
! Reading these into the average variables (vlat, vlon, timeave), then
! if program is updating they'll get written over, if not updating
! they'll be saved.
!
! if navtrk.dat does not exist exit with error
        if(iw.eq.1)write(ifile,*) 'opening navtrk.dat:'
        open(15,file=anavtrk,form='formatted',status='old',err=161)
        read(15,508,err=162) idayave,imonave,iyear,
     $             iphr,ipmn,ipsc,vlat,vlon,speed,dir
508     format(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,2f8.3,f6.2,f7.2)
        close(15,iostat=ios)
!
        if(iw.eq.1) then
           write(ifile,*)'closing navtrk.dat, ios= ', ios
           write(ifile,*)'read in from navtrk.dat:'
           write(ifile,508)idayave,imonave,iyear, iphr, ipmn, ipsc,
     $                vlat, vlon, speed, dir
           call flush(ifile)
        endif
!
        timeave = float(iphr*3600 + ipmn*60 + ipsc)
        iyerave = iyear + 2000
        if(iw.eq.1) write(ifile,*)'iyerave=',iyerave
! initially setting avlath here to avoid ? s in nav file
        if(vlat.ge.0.0) then
           avlath(1:1) = 'N'
        else
           avlath(1:1) = 'S'
        endif
! decide which position to use as our most recent position - one
! read in from the nav file, or navtrk.dat
! inavfile=2 - means error reading pos in nav file, so use navtrk.dat
! old old old? rm 23sep2004        if(inavfile.ne.2.and.igps.eq.2) then
        if(inavfile.ne.2) then
         if(speednav.eq.0.0.or.dirnav.eq.0.0) then
          if(iw.eq.1) then
           write(ifile,*)'Using position in navtrk.dat for DR since no'
           write(ifile,*)'speed or direction in last nav file entry   '
          endif
          go to 266
         endif
! compare date/time of last nav file entry and navtrk.dat entry
! subroutine compare just returns iflg=1 if 1st group is later in time
! than second group.   So must be careful the order you send things!
         CALL compare(navday,navmon,navyear,navhr,navmin,navsec,
     $                idayave,imonave,iyerave,iphr,ipmn,ipsc,iflg)
! if iflg=1, use nav file entry
         if(iflg.eq.1) then
          idayave = navday
          imonave = navmon
          iyerave = navyear
          if(iw.eq.1) then
           write(ifile,*)'Using last nav file entry to DR from!!!'
           write(ifile,*)idayave,imonave,iyerave
          endif
          timeave = float(navhr*3600 + navmin*60 + navsec)
          CALL deg2dec(ilatdnav,xlatmnav,alathnav,vlat)
          CALL deg2dec(ilondnav,xlonmnav,alonhnav,vlon)
          speed = speednav
          dir = dirnav
         endif
        endif
! SO HERE IS WHERE WE THINK WE LAST WERE - ALONG WITH SPEED AND DIR!
        if(iw.eq.1) then
          write(ifile,645)idayave,imonave,iyerave,timeave,vlat,
     $                 vlon, speed, dir
645       format('Using: ',i2,'/',i2,'/',i4,1x,f9.2,2f8.2,
     $           f6.2,f7.2,' to DR from')
        endif
!
266     continue

! 08sep2014: what is this?:
        if(speed.ne.-0.00009) then
         iaveflg = 1
         ispd = 1
        endif

! dtime: dos time in seconds, get dos time (gettim uses i*2):
         CALL gettim(j1,j2,j3,j4)
         idhr = int(j1)
         idmin = int(j2)
         idsec = int(j3)
         dtime = float(idhr*3600 + idmin*60 + idsec)
! yes - idsec2 - this is suppose to be seas incoming second:
         idsec2 = int(csec)
         if(iw.eq.1)write(ifile,585)idhr,idmin,idsec,j4,int(dtime)
585      format(' pc h:m:s ',i2,':',i2,':',i2,'.',i2,' dtime=',i6)
!
! read in xbt info
! aspec: specify what we're relying on to drop xbt's (lat or lon)
! 30apr2003 - new plan.dat format.  Need to figure out aspec/ispec from new
! plan.dat.   Have to think about this - if we start changing lat/lon inside 
! the file - then I can't get this from the first 2 positions - as I thought
! I could.
        if(iw.eq.1)write(ifile,*)'opening plan.dat:'
        open(13,file=aplan,form='formatted',status='old',err=315)
! read in first 2 xbt positions to determine direction we're suppose to
! be heading, use that direction for stopping program if past an xbt
! and also when reading stations.dat (control.dat)
! 16jun2005 Skip 4 lines!
        read(13,*,err=316,end=316) 
        read(13,*,err=316,end=316) 
        read(13,*,err=316,end=316) 
        read(13,*,err=316,end=316) 

! 18nov2004 LL more robust reading of plan.dat! (decodeplan)
! 10sep2014 LL: ierrplan-> you are not doing anything with this! 
        read(13,500,err=316,end=316) aplanline
        CALL decodeplan(aplanline,latd,xlatm,alath,ierrplan,ispec(1))

        read(13,500,err=316,end=316) aplanline
        CALL decodeplan(aplanline,latd1,xlatm1,alath1,ierrplan,ispec(2))

500     format(a)
        rewind(13,iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*)'rewind13ios=',ios
! reposition plan.dat to line 5 (first position)
        read(13,*,err=316,end=316) 
        read(13,*,err=316,end=316) 
        read(13,*,err=316,end=316) 
        read(13,*,err=316,end=316) 

        CALL deg2dec(latd,xlatm,alath,xlat)
        CALL deg2dec(latd1,xlatm1,alath1,xlat1)
        if(iw.eq.1) then
         write(ifile,*)'plan pos1:latd ,xlatm ,alath =',
     $                         latd,xlatm,alath, ' = ',xlat
         write(ifile,*)'plan pos2:latd1,xlatm1,alath1=',
     $                         latd1,xlatm1,alath1, ' = ',xlat1
         call flush(ifile)
        endif
        if(xlat.eq.xlat1) go to 318

! figure out aspec, ispec(1), ahemi, aplandir, iplandir
! pass ispec(1) since this is first position
        CALL planinfo(xlat,alath,xlat1,ahemi,aspec,ispec(1),
     $                aplandir,iplandir)

        if(iw.eq.1) then
         write(ifile,*)'aplandir=',aplandir,' iplandir=',iplandir
         write(ifile,*)'aspec=',aspec, ' ispec(1)=',ispec(1)
         call flush(ifile)
        endif

! remember idrp is chkprof code...
! Read through stations.dat looking for last drop (idrp=1 or -1 or -3), then
! read through plan.dat looking for next drop.  Use position read in from 
! navtrk.dat as our current position (or date.nav file if that is more
! recent).  If there is a position in plan.dat
! that is between last good drop and current position, do a drop.
!
! BOGUS BUG FIX here:  Seems that when running on longitude the
! position in stations.dat is ~.005 before the drop, so gps does 
! another drop.  SO, if a last drop in stations.dat is within .01 of
! a drop in plan.dat, call it the same and don't redo it.
!
! igooddrp: set to one once we've read in a previous good drop.
! pxlat:(etc) = Previous Xbt drop position info
        igooddrp = 0
! nextdrop: telling seas2k what drop number is next in our sequence.
! will =1 for first drop, will = next drop number after last drop
! in stations.dat
! next 4 lines for debugging only!:
        if(iw.eq.1) then
          CALL gettim(j1,j2,j3,j4)
          write(ifile,*)'dos time before open stations.dat:',j1,j2,j3,j4
          call flush(ifile)
        endif
!
        nextdrop = 1
        open(7,file=astations,form='formatted',status='old',err=107)
!
! 08sep2014 LL - DO NOT DO THIS ANYMORE, LET SEAS HANDLE THIS ERROR:
! 04mar2005 LL if error opening stations.dat, open as new file and write ENDDATA
! (prev I set ierror(25)=1 and exit, no longer doing that.)
!08sep2014 commentout
!08sep2014 commentout65      write(ifile,*) 'first error opening stations.dat'
!08sep2014 commentout        call flush(ifile)
!08sep2014 commentout        open(7,file=astations,status='unknown',err=866)
!08sep2014 commentout        go to 867
!08sep2014 commentout866     write(ifile,*)'second error opening stations.dat'
!08sep2014 commentout        call flush(ifile)
!08sep2014 commentout867     continue
!08sep2014 commentout        write(7,'(a7)')'ENDDATA'
!08sep2014 commentout        close(7)
!08sep2014 commentout        open(7,file=astations,form='formatted',status='old',err=65)
!
        if(iw.eq.1) write(ifile,*)' rewind & read stations.dat:'
        rewind(7,iostat=ios)
        if(iw.eq.1.and.ios.ne.0) write(ifile,*)'rewind7ios= ',ios
!
        do 105 i = 1, 1000
         indx = i
! TODO do you want an end= here??:
         read(7,'(a70)',err=70) aline(1:70)
         if(iw.eq.1.and.ierrlev.eq.6) write(ifile,'(a70)') aline(1:70)
         if(aline(1:3).eq.'END') go to 69
         read(aline,507,end=69,err=70) ixbt, ipxday, ipxhr,
     $          ipxmin, ipxsec, pxlat, pxlon, idrp, iedt, jnav
507      format(1x,i3,14x,i2,7x,i2,1x,i2,1x,i2,2f9.3,2x,i2,3x,i2,1x,i5)
         if(pxlon.ge.360.0) pxlon = pxlon - 360.0
         if(iw.eq.1) write(ifile,507) ixbt, ipxday, ipxhr, ipxmin, 
     $          ipxsec,pxlat,pxlon, idrp, iedt, jnav
! save the last drop info
         if(idrp.eq.1.or.idrp.eq.-1.or.idrp.eq.-3) then
          pxlats = pxlat
          pxlons = pxlon
          ipxdays = ipxday
          ipxhrs = ipxhr
          ipxmins = ipxmin
          ipxsecs = ipxsec
          inavs = jnav
          igooddrp = 1
         endif
105      continue
!
69       continue
! value of indx here is the next drop number.  Pass it back to JB.
        nextdrop = indx
        if(iw.eq.1)write(ifile,*)'nextdrop=',nextdrop,
     $                           ' igooddrp=',igooddrp
!
! stop autolauncher dumping!
!29oct2014 initialize yrday1 to -1
        yrday1 = -1
        if(indx.ge.3) then
         backspace(7)
         backspace(7)
         backspace(7)
         read(7,510,err=70) id, im, iy, ih, imi, is
! iy is 2 digit year read in from stations.dat:
         CALL yrdy(iy,im,id,ih,imi,is,yrday1)
510      format(18x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)
        endif
        close(7,iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close7ios=',ios

! very first drop
        if(igooddrp.eq.0) then

         read(13,500,err=316,end=316) aplanline
         CALL decodeplan(aplanline,latd,xlatm,alath,ierrplan,ispec(1))
         if(iw.eq.1) write(ifile,*) 'igooddrp=0,aft decodeplan=',
     $                     latd, xlatm, alath, ispec(1)
!
! set "counter" to 1 - this is first position in plan.dat, for JB
         iplancnt = 1
         CALL deg2dec(latd,xlatm,alath,xlat)
! also set dropmin negative so alarm does NOT sound before first drop!!!
         dropmin = -1.0
         if(iw.eq.1)write(ifile,*)'set dropmin negative:',dropmin
        else
! check to be sure drop has been navigated, if not, sound alarm
! 30nov2004 - changed WrDrpStn to write Seas2k position to stations.dat -
! so that if gpspos fails it still has a position.   So, in theory I'm going
! to get rid of this.   risky.... perhaps I should write some other number
! to this inside of gpspos.   So know it at least tried?
!         if(inavs.eq.0) then
!          ierror(27) = 1
!          go to 999
!         endif
! we know last good xbt drop, the direction we're suppose to be heading (aplandir),
! (note that can be different than the actual ship direction.(think circles)..)
! so find the next xbt:
          
! FIX - WHAT ABOUT MULTIPLE CHANGES IN PLAN.DAT hemispheres?
! never mind - loop 104 should take care of that:
         do 104 j = 1, 1000
          iplancnt = j
          read(13,500,err=316,end=317) aplanline
          CALL decodeplan(aplanline,latd,xlatm,alath,ierrplan,ispec(1))
          if(iw.eq.1) write(ifile,*)'aft decodeplan=',
     $                     latd, xlatm, alath,ispec(1)
          CALL deg2dec(latd,xlatm,alath,xlat)
! compare alath(hem just read in from plan.dat) to ahemi(hem at beginning of
! plan.dat) - this is to determine if we changed direction
! inside plan.dat
          if(alath.ne.ahemi) then
           if(iw.eq.1)write(ifile,*)'alath.ne.ahemi',alath,ahemi
! ok, first check that it's not a W ne w etc etc
! also check it's not S to N, or E to W...
! the go to 100 means no change in direction, just in "case"
           if(alath.eq.'W'.and.ahemi.eq.'w') go to 100
           if(alath.eq.'w'.and.ahemi.eq.'W') go to 100
           if(alath.eq.'E'.and.ahemi.eq.'e') go to 100
           if(alath.eq.'e'.and.ahemi.eq.'E') go to 100
           if(alath.eq.'S'.and.ahemi.eq.'s') go to 100
           if(alath.eq.'s'.and.ahemi.eq.'S') go to 100
           if(alath.eq.'N'.and.ahemi.eq.'n') go to 100
           if(alath.eq.'n'.and.ahemi.eq.'N') go to 100

           if(alath.eq.'W'.and.ahemi.eq.'E') go to 100
           if(alath.eq.'E'.and.ahemi.eq.'W') go to 100
           if(alath.eq.'N'.and.ahemi.eq.'S') go to 100
           if(alath.eq.'S'.and.ahemi.eq.'W') go to 100

! if we're here, we're fairly certain we've changed direction inside plan.dat
! need to read next position in plan.dat
           read(13,500,err=316,end=317) aplanline
           CALL decodeplan(aplanline,latd1,xlatm1,alath1,
     $                     ierrplan,ispec(1))
           if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'aft decodeplan=',
     $                     latd1,xlatm1, alath1,ispec(1)
! don't forget to backspace, since we read in one extra:
           backspace(13,iostat=ios)
           if(iw.eq.1.and.ios.ne.0)write(ifile,*)'bckspc13ios=',ios
! translate to decimal
           CALL deg2dec(latd1,xlatm1,alath1,xlat1)
! call our new planinfo: to figure out new aspec, ispec(1), ahemi,
!     aplandir, iplandir
           CALL planinfo(xlat,alath,xlat1,ahemi,aspec,ispec(1),
     $                  aplandir,iplandir)
           if(iw.eq.1) then
            write(ifile,*)'CHANGED DIR IN PLAN.DAT!:'
            write(ifile,*)'aplandir=',aplandir,xlat,xlat1
            write(ifile,*)'iplandir=',iplandir
            write(ifile,*)'aspec=',aspec, 'ispec(1)=',ispec(1)
           endif
          endif       ! endif alath.ne.ahemi

100       continue

          if(aspec.eq.'lat') then
           dxlat = pxlats - xlat
! if we're going N:
           if(aplandir.eq.'N') then
! read in next one if we're past it
! BOGUS BUG FIX - using 0.01 instead of 0.0 here:
            if(dxlat.ge.-0.01) go to 104
           else
            if(dxlat.le.0.01) go to 104
           endif
          endif
!
          if(aspec.eq.'lon') then
           dxlon = pxlons - xlat
           if(iw.eq.1)then
            write(ifile,*)'pxlons-xlat=dxlon ',pxlons,'-',xlat,'=',dxlon
            write(ifile,*)'before xlon=',xlon
           endif
! FIX - may want to double check this!
           xlon = xlat
           if(iw.eq.1)write(ifile,*)'set xlon=xlat, xlon=',xlon
!
           if(aplandir.eq.'E') then
! BOGUS BUG FIX: using -0.01 & 0.01 instead of 0.0 in this if statement:
! this is in case the last drop is .01 IN FRONT of drop position in plan.dat.
!  changed the .01 to 0.0!!!   May want to redo later!
! pxlons is previous drop longitude in 0-360E
! xlon is next xbt long we are searching for in plan.dat
! 'go to 104' means keep looking
! 'go to 6' means this is next xbt drop position
            if(pxlons.ge.340.0.and.xlon.lt.50.0) then
               dxlon = pxlons - (360.0 + xlon)
               if(iw.eq.1)write(ifile,*) 'E at 0 dxlon=', dxlon 
            endif
            if(dxlon.ge.-0.01) go to 104
! this is silly, but it should work (for crossing the 0 line)  NO IT DOES NOT
! at least not by itself.   It used to in k98....
            if(pxlons.le.50.0.and.xlon.gt.50.0) go to 104
           elseif(aplandir.eq.'W')then
            if(dxlon.gt.300.0) then
               if(pxlons.gt.300.0.and.xlon.lt.50.) then
                  dxlon = pxlons - (360.0 + xlon)
                  if(iw.eq.1)write(ifile,*) 'W at 0 dxlon=', dxlon 
               endif
            endif
            if(pxlons.lt.50.0.and.xlon.gt.300.0) then
               dxlon = (360.0 + pxlons) - xlon
               if(iw.eq.1)write(ifile,*) 'W at 0 dxlon=', dxlon 
            endif
! if dxlon in this case is negative or LT 0.01 then consider it done
! and look at next position in plan.dat
            if(dxlon.le.0.01) go to 104
           endif
          endif
! if we are here, we think we found our next drop position in plan.dat
          if(iw.eq.1)write(ifile,*) 'Found next drop:', xlon
! set iplancnt to j counter inside do 104 loop.  Should be right.
! 07mar2005 mv the "iplancnt=j" to very beginning of do 104 loop.
          go to 6
104       continue 
        endif

6       continue
        if(iw.eq.1)write(ifile,*)'iplancnt=',iplancnt

! 26sep2003 LL the iwait=2 skip:
! read the next position in plan.dat!  Skip the one *I* say we should do

        if(iwait.eq.1) then
! isio_skip_count should always be zero for normal run - do not modify iplancnt
          if(iw.eq.1.and.isio_skip_count.ne.0)write(ifile,*)
     $      'WARNING: iwait=1 but isio_skip_count=',isio_skip_count,
     $      ' - ignoring'

! comment out iwait=2 and iwait=3 will never happen:
!        elseif(iwait.eq.2) then
!          read(13,500,err=316,end=317) aplanline
!          CALL decodeplan(aplanline,latd,xlatm,alath,ierrplan,
!     $                    ispec(1))
!          if(iw.eq.1)write(ifile,*) 'aft decodeplan=',latd, xlatm, alath,ispec(1)
!          iplancnt = iplancnt + 1
!          CALL deg2dec(latd,xlatm,alath,xlat)
!          if(aspec.eq.'lon') xlon = xlat
!        elseif(iwait.eq.3) then
! DO NOT USE iwait=3!   set it to 1!
!         iwait = 1
!         if(iw.eq.1)write(ifile,*)'set iwait=1!'
!         iplancnt = iplancnt + isio_skip_count
! 02jun2006 LL
! hmm.   iwait=4 - send drop flag now, inc iplancnt by skipcount only!
        elseif(iwait.eq.4) then
          if(isio_skip_count.gt.0.and.isio_skip_count.lt.1000)
     $      iplancnt = iplancnt + isio_skip_count

        elseif(iwait.eq.5) then
          if(isio_skip_count.gt.0.and.isio_skip_count.lt.1000) then
          do 109 i = 1, isio_skip_count
             read(13,500,err=316,end=317) aplanline
             CALL decodeplan(aplanline,latd,xlatm,alath,ierrplan,
     $                       ispec(1))
             iplancnt = iplancnt + 1
             if(iw.eq.1) then
              write(ifile,*)'aft decodeplan=',latd,xlatm,alath,ispec(1)
              write(ifile,*)isio_skip_count,' iwait=5,readin:',latd,
     $                      xlatm, alath,' iplancnt=',iplancnt
             endif
109       continue
          CALL deg2dec(latd,xlatm,alath,xlat)
          if(aspec.eq.'lon') xlon = xlat
          endif
        elseif(iwait.eq.6) then
          if(isio_skip_count.gt.0.and.isio_skip_count.lt.1000)
     $      iplancnt = iplancnt + isio_skip_count
! if iwait.eq.7 this means last drop failed chkprof!
        elseif(iwait.eq.7) then
          if(isio_skip_count.gt.0.and.isio_skip_count.lt.1000)
     $      iplancnt = iplancnt + isio_skip_count
          if(iw.eq.1)write(ifile,*)'since iwait=7, idrp= ',idrp
        endif

        if(iw.eq.1) then
           write(ifile,*)'final iplancnt = ', iplancnt
           call flush(ifile)
           write(ifile,*)' loaded probe drop positions='
        endif

! read in drops that match up to loaded probes:
! Note here that if we reach the end of plan.dat it's not the end of the
! world.  It is possible we have more probes loaded than needed.
! Also note here that we are reading in drops PAST our current upcoming
! drop.   xlat = our upcoming position.   Here xlatload(nlnchr)
! upcoming drops 1  to 6(or 8 or 12)
! note - xlatload USED TO be upcoming drops 2 to whatever - I changed it to 1 to whatever!
! nplan: is 2 to whatever.  It does not include current upcoming drop.
          xlatload(1) = xlat
! 18oct2004 change xlatload to 0-180 E, -1 to -180 is west!
          if(xlatload(1).gt.180.0.and.xlatload(1).lt.360.0) then
           xlatload(1) = -1. * (360.0 - xlatload(1))
          elseif(xlatload(1).eq.360.0) then
           xlatload(1) = 0.0
          elseif(xlatload(1).gt.360.0) then
           xlatload(1) = 360.0 - xlatload(1)
          endif
          if(iw.eq.1)write(ifile,*)'xlatload    1 ',xlatload(1)

          do 68 i = 1, nlnchr-1
!           read(13,500,err=316,end=317) aplanline
! test end=111 here! 7jul2005
           read(13,500,err=316,end=111) aplanline
           CALL decodeplan(aplanline,latd1,xlatm1,alath1,ierrplan,
     $                     ispec(i+1))
           if(iw.eq.1.and.ierrlev.ge.6) write(ifile,*)'aft decodeplan=',
     $                      latd1,xlatm1, alath1, ispec(i+1)
           CALL deg2dec(latd1,xlatm1,alath1,xlatload(i+1))

! 18oct2004 change xlatload to 0-180 E, -1 to -180 is west!
          if(xlatload(i+1).gt.180.0.and.xlatload(i+1).lt.360.0) then
           xlatload(i+1) = -1. * (360.0 - xlatload(i+1))
          elseif(xlatload(i+1).eq.360.0) then
           xlatload(i+1) = 0.0
          elseif(xlatload(i+1).gt.360.0) then
           xlatload(i+1) = 360.0 - xlatload(i+1)
          endif

          if(iw.eq.1)write(ifile,586)i+1,xlatload(i+1),latd1,xlatm1,
     $               alath1, ispec(i+1)
586        format('xlatload ',i4,f8.3,'=',i3,f6.2,a2,i4)
           nplan = i
68        continue
!
111       continue
          close(13,iostat=ios)
          if(iw.eq.1)then
           write(ifile,*)'1close13ios=',ios
           write(ifile,*) 'nplan =', nplan
           write(ifile,*) 'dropmin=',dropmin,' igooddrp=',igooddrp
          endif
! set timer for alarm to go off in dropmin since last drop (if dropmin not
! lt 0)
! 25feb2004 - using itime=0 as base and running from that.  Meaning set
! itime=0 here, increment it in sioloop from watching the DOS (yes DOS)
! seconds tick by
! 11sep2014 add set alrmtime=dropsec for first drop:
        alrmtime = dropsec      ! will get resent in if stmt below:
!
        if(dropmin.gt.0.0) then
! if we have a previous drop to run from:
         if(igooddrp.eq.1) then
! pxtime: time in seconds into the day that last drop occured
          pxtime = float(ipxhrs*3600 + ipxmins*60 + ipxsecs)
          if(iw.eq.1.and.ierrlev.eq.6) then
           write(ifile,*)'icday,ipxdays',icday,ipxdays
           write(ifile,*)'pxtime,ipxhrs,ipxmins,ipxsecs=',
     $                    pxtime,ipxhrs,ipxmins,ipxsecs
          endif
! if the previous drop was during today:
          if(icday.eq.ipxdays) then
           alrmtime = dropsec - (dtime-pxtime)
           if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'icday=ipxdays,alrmtime',alrmtime
           endif
! else drop was yesterday (big assumption here....)
          else
           alrmtime = dropsec -((86400.0 - pxtime)+dtime)
! if it comes out negative, reset it to dropmin timer
           if(alrmtime.le.0.0) then
            alrmtime = dropsec
           endif
          endif
! else no previous drop, just set dropmin timer from present time:
         else
          alrmtime = dropsec
         endif
        endif
! 23sep2014 add this in while testing with Ibis pc 12 hr diff from GMT:
        if(alrmtime.le.0.0) then
           alrmtime = dropsec
           if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'alrmtime<0, reset to dropsec ',dropsec
              write(ifile,*)'|->This is likely due to pc time ne gmt' 
           endif
         
        endif

!        if(igps.eq.2) then
! ok, so we do not have a gps....
! reset maximum minutes to deadreckon to 8 hours - no matter what op says!
! 17jun2013 LL nah, leave it however operator set it
!         deadmin = 480.0
!        endif

        goto 999
! error set:

315     ierror(19) = 1         ! error opening plan.dat
        ierror(35) = 319
        if(iw.eq.1)write(ifile,*)' ERROR opening plan.dat! '
        go to 999
316     ierror(20) = 1         ! error reading plan.dat
        ierror(35) = 320
        if(iw.eq.1)write(ifile,*)' ERROR reading plan.dat! '
        go to 999
317     ierror(21) = 1         ! end of plan.dat
        if(iw.eq.1)write(ifile,*)' end plan.dat,hopefully not bad '
        go to 999
318     ierror(22) = 1         ! first 2 positions in plan.dat are equal.  bad.
        ierror(35) = 322
        if(iw.eq.1)write(ifile,*)
     $             'first 2 positions in plan.dat are equal, not good'
        go to 999
! 161=error opening navtrk.dat
! don't alarm on this one - navtrk.dat may not exist yet on first run
! fall back to nav file data and continue (do NOT exit siobegin)
161     ierror(23) = 1         !error opening navtrk.dat, do not alarm on this one
        if(iw.eq.1)write(ifile,*)' error opening navtrk.dat '
        if(inavfile.ne.2) then
          idayave = navday
          imonave = navmon
          iyerave = navyear
          timeave = float(navhr*3600 + navmin*60 + navsec)
          CALL deg2dec(ilatdnav,xlatmnav,alathnav,vlat)
          CALL deg2dec(ilondnav,xlonmnav,alonhnav,vlon)
          speed = speednav
          dir = dirnav
          if(iw.eq.1)write(ifile,*)' using nav file pos as fallback'
        endif
        go to 266
! 162=error reading navtrk.dat
! don't alarm on this one
162     ierror(24) = 1              ! error reading navtrk.dat, do not alarm on this one
        if(iw.eq.1)write(ifile,*)' error reading navtrk.dat '
        if(inavfile.ne.2) then
          idayave = navday
          imonave = navmon
          iyerave = navyear
          timeave = float(navhr*3600 + navmin*60 + navsec)
          CALL deg2dec(ilatdnav,xlatmnav,alathnav,vlat)
          CALL deg2dec(ilondnav,xlonmnav,alonhnav,vlon)
          speed = speednav
          dir = dirnav
          if(iw.eq.1)write(ifile,*)' using nav file pos as fallback'
        endif
        go to 266
!
107     ierror(25) = 1              ! error opening stations.dat (add back 08sep2014)
        ierror(35) = 325
        if(iw.eq.1)write(ifile,*)' ERROR opening stations.dat! '
        go to 999
70      ierror(26) = 1              ! error reading stations.dat
        ierror(35) = 326
        if(iw.eq.1)write(ifile,*)' ERROR reading stations.dat! '
        go to 999

999        continue
! If ierror(35) is still = 0 when we get here we think we have
! run thru siobegin successfully, so set it = 2 on exit
        if(ierror(35).eq.0) then
           ierror(35) = 2
        endif

        close(13,iostat=ios)
        if(iw.eq.1)write(ifile,*)'close13ios=',ios

        if(iw.eq.1) then
         write(ifile,587)speed, dir
587         format('spd=',f6.2,' dir=',f7.2)
         write(ifile,*)int(deadmin),int(dropmin),int(relodmin),
     $              ' runsec=',int(runsec)
         write(ifile,*)'igps=',igps,'xlat=',xlat,'ibuf=',ibuf
         write(ifile,*)'idsec2=',idsec2,' ierrlev=',ierrlev,
     $              'alrmtime=',alrmtime
         write(ifile,*)'ifirst=',ifirst,' irollnav=',irollnav
         write(ifile,*)inav,yrday1,iaveflg,ispd,itime
         write(ifile,*)'date ave=',idayave,imonave,iyerave
         write(ifile,*)'vlat=',vlat,' vlon=',vlon
         call flush(ifile)
         write(ifile,*)'errors:'
         do 1010 i = 1, nerr
1010       if(ierror(i).ne.0) write(ifile,*)'ierror(',i,')=',ierror(i)
         write(ifile,*)'LEAVING SIOBEGIN,timeave=',timeave
         call flush(ifile)
        endif

        close(ifile)
        return
        end

!^**** end siobegin  ***********************************************^
! search for FIX!!
!
        SUBROUTINE sioloop(deadmin,dropmin,relodmin,runsec,xmaxspd,
     $     launcher,igps,xlat,xlatload,nplan,ibuf,
     $     idsec2,ierrlev,alrmtime,ifirst,irollnav,
     $     inav,ispec,dtime,yrday1,ierror,iaveflg,ispd,itime,
     $     idayave,imonave,iyerave,icday1,iplandir,
     $     speed,dir,timeave,vlat,vlon,
     $     icday,icmon,icyear,istat,
     $     gpssec,chrsav,icsec1,ctagbuf,clatbuf,clonbuf,
     $     iupdate,clatd,clatm,iclath,clond,clonm,iclonh,
     $     chr,cmin,csec,cday,cmon,cyear,nlnchr,eta,
     $     drlat,drlon,iSIOSpeedAveMin)
!
! INPUT from Seas (this list is MY passing args:)
!        deadmin, dropmin, relodmin, runsec, xmaxspd, launcher,
!        xlat, xlatload, nplan, ibuf,
!        idsec2, ierrlev, alrmtime, ifirst, irollnav, inav,
!        ispec(12), dtime, yrday1, ierror, iaveflg, ispd, itime,
!        idayave, imonave,iyerave, icday1, iplandir,
!        speed, dir, timeave, vlat, vlon, nlnchr
!        gpssec, chrsav, icsec1, ctags
!        icday,icmon,icyear  - I set these from cday,cmon,cyear -
!        	which comes from Seas.  NO NEED TO PASS THESE!
!        istat - I set this depending on igps and iupdate


! INPUT from Seas (this list what SEAS is modifying to send to me:)
!        igps - this can change "on the fly"!
!         iupdate: =1 new position from gps
!                =0 no new position dead reckon from last position
!                   17aug2006 NOTE! Seas is using only 2 digit precision
!        	    to determine if positions is updating, so at SLOW speeds
!        	    even tho it IS updating Seas says it is not.  I guess
!        	    I should check the seconds coming if for the position?
!        clatd, clatm, iclath - incoming position from Seas - use it
!        clond, clonm, iclonh - if iupdate = 1
!        chr, cmin, csec   - incoming time from Seas
!        cday, cmon, cyear   - incoming date from Seas

! OUTPUT
!        eta
!        drlat, drlon

        parameter(nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: sioloop

! main loop portion of sio gps program.
!
!  OLD:   See igps definition at beginning!
! igps: =1 have incoming messages on the com port - we have a gps plugged in
!       =2 no gps whatsoever - it's been thrown overboard load dos date/time
!          into acmsg and
!          do time loop, and deadreckoning portion of position loop
! 06nov2003:
! iupdate: =1 new position from gps
!          =0 no new position dead reckon from last position (see above!)
!
! file numbers opened and closed in sioloop:
!       10=<date>.nav   (write)
!       15=navtrk.dat   (write)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=sio.log (write, append on from siobegin routine)
!
! ierrors set in here:  Note siobegin clears all ierror before calling this
!( 1) - At drop position!  Drop the probe.   (dostime GE stoptime)
!( 5) - error opening <date>.nav file
!( 6) - error writing <date>.nav file
!( 7) - error opening siodir.txt
!( 8) - passed Dead Reckoning time limit!
!(10) - passed dropmin time limit between drops
!(11) - calc'd speed is greater than max knots allowed (something amiss?)
!(12) - change in Dead Reckoning lat or lon is too big (something amiss?)
!(14) - error writing navtrk.dat
!(17) - error reading siodir.txt
!(23) - error opening navtrk.dat
! FIX:TODO: ???? why is ierror(28) not in sioloop???
!(30) - Trying to drop 3rd probe in less than 10 minutes - probably bad.
!(34) - integer of day (dd) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(35) - iSIOErrWatchDog , set to 2 at end of program if all is well.
!                       set to 3XX where XX is above error number to alert
!                       an error was set
!     = 307,312,317
!(36) - integer of month (mm) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(37) - integer of year (yy) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(38) - used for "jptr" in subroutine ave
!(39) - used for "icall" in subroutine ave
!(44) - ALL    - error opening calling routine's .log file (eg sio.log)
!(45) - ALL    - error writing calling routine's .log file (eg sio.log)
!
!
        real*4  deadmin, dropmin, relodmin, runsec, xmaxspd
        real*4 xlat, xlatload(12), alrmtime, dtime, yrday1
        real*4 xlatload360(12)
        real*4 speed, dir, timeave, vlat, vlon
        real*4 gpssec, chrsav
        real*4 clatbuf(200), clonbuf(200), ctagbuf(200)
        real*4 clatd,clatm,clond,clonm
        real*4 chr,cmin,csec, cday,cmon,cyear
        real*4 eta(12)
        real*4 drlat, drlon

        integer*4 launcher(12), igps, nplan, ibuf
        integer*4 idsec2,ierrlev
        integer*4 ifirst, irollnav, inav, ispec(12)
        integer*4 ierror(nerr), iaveflg, ispd, itime
        integer*4 idayave, imonave, iyerave, icday1
        integer*4 iplandir,nlnchr
        integer*4 icday,icmon,icyear,istat
        integer*4 icsec1, iupdate, iclonh,iclath

        integer*2 j1, j2, j3, j4

        integer*4 ihr,imin,isec, ierr, iderr
        integer*4 i1,iyr,imo,iday,idhr,idmin,idsec,jchange
        integer*4 jpos,len,iiyergps,iiyerave,iweekday

        real*4 stoptime,yrday2,s,d,x,timetag
        SAVE stoptime
! BRZENSKI added variables for storing last good vlat and vlon
        real*4 vlat_prev, vlon_prev
        
        character a4*4, aspec*3
        character afilen*80, adosday*2, adosmon*2, adosyear*4
        character aplandir*1
        character avlath*1, avlonh*1
        character aclath*1, aclonh*1
        character astat*3, agpsday*2

        character*80 asio, anavtrk

        character*80 adir
!
        integer*4 iw, ifile, ios
        integer*4 igderr(3)
        integer*4 iSIOSpeedAveMin

        iw = 0                    ! iw=0 no write to log, iw=1 write to log
        deg2rad = 3.141592654/180.0
        do i = 1, 80
           asio(i:i) = ' '
           anavtrk(i:i) = ' '
           adir(i:i) = ' '
        end do
!
! get seas2k path:
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open or read file 'siodir', assume current directory?
        if(ierror(7).eq.1) then
           len_adir = 0
           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
           ierror(35) = 317
           go to 999
        endif
!
        if(len_adir.gt.0) then
           asio(1:len_adir) = adir(1:len_adir)
           anavtrk(1:len_adir) = adir(1:len_adir)
        endif
!
        asio(len_adir+1:len_adir+12) = 'Data\sio.log'
        anavtrk(len_adir+1:len_adir+15) = 'Data\navtrk.dat'
!08sep2014 rm the 5 times itryopen here!

! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=asio,form='formatted',status='unknown',
     $       access='append',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
         write(ifile,*,err=335)'IN SIOLOOP'
         if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
         if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
         if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
         if(ierrlev.eq.6)write(ifile,*)'asio=',asio(1:len_adir+12)
         if(ierrlev.eq.6)write(ifile,*)'anavtrk=',anavtrk(1:len_adir+15)
         call flush(ifile)
         go to 336
335      ierror(45) = 1
         iw = 0
336      continue
        endif
! if we are here we opened sio.log successfully

! 23feb2004- set icsec1 (previous second) to idsec2 (only if ifirst
!             =1).   This may not be needed once debugged...
! idsec2 has 2 functions.   If ifirst=0 it's previous gps seconds.
!         if ifirst=1 it's letting me know stoptime has been set!!!!
        if(ifirst.eq.0) then
           icsec1 = idsec2
           gpssec = idsec2
           idsec2 = 0
           stoptime = 9.9e9 ! BRZENSKI Set this to a big number until we get a 
                            !gps fix and can set it to something real
        endif

! FIX!   Ah - but what about a TIME associated with these:?!
          vlat1 = vlat
          vlon1 = vlon
! I don't think I want to do this because of the "change" in calling
! newpos.   Let's just keep these as vlat,vlon since timeave goes with
! them.   Really drlat and drlon are just for JB to print out and set off
! a drop. 
!nogo?          vlat1 = drlat
!nogo?          vlon1 = drlon
!
        icday = int(cday)
        icmon = int(cmon)
        icyear = int(cyear)
!
!23456789012345678901234567890123456789012345678901234567890123456789012       
        if(iw.eq.1)then
            write(ifile,580)icday,icmon,icyear,int(chr),
     $             int(cmin),int(csec), iupdate
580        format('incm ',i2,'/',i2,
     $        '/',i4,1x,i2,':',i2,':',i2,' iupd ',i2)
           write(ifile,581)clatd,clatm,iclath,clond,clonm,iclonh
581        format('incm cpos',f7.3,1x,f7.3,1x,i1,'/',
     $               f7.3,1x,f7.3,1x,i1)
           call flush(ifile)
        endif
        if(iw.eq.1.and.ierrlev.eq.6) then
         write(ifile,*)'incm icsec1=',icsec1,' iaveflg=',iaveflg
         write(ifile,*)'incm drlat, drlon:', drlat, drlon
         write(ifile,*)'incm vlat, vlon:', vlat, vlon
         write(ifile,587)speed, dir  
587         format('spd=',f6.2,' dir=',f7.2)
         write(ifile,*)'incm ispec:', ispec 
         write(ifile,*)'incm iSIOSpeedAveMin=',iSIOSpeedAveMin
         write(ifile,*)'incm ifirst:',ifirst,' igps=',igps,
     $                 ' icday1',icday1
         write(ifile,*)'incm ierr(38&39) ', ierror(38),ierror(39)
         call flush(ifile)
        endif
! 19may2015 LL pulled this snippet from subroutine ave since amv9 is
!      not initializing iSIOSpeedAveMin [02jun2005 add variable iSIOSpeedAveMin
!   set it to 10 if it's a wierd number...]
        if(iSIOSpeedAveMin.lt.1.or.iSIOSpeedAveMin.gt.10) then
           iSIOSpeedAveMin = 10
        endif
!
! put previous calls dtime into dtime1
        dtime1 = dtime
! dtime: dos time in seconds, get dos time (gettim uses i*2):
! note further down idhr,idmin,idsec are reset to incoming seas
         CALL gettim(j1,j2,j3,j4)
         idhr = int(j1)
         idmin = int(j2)
         idsec = int(j3)
         dtime = float(idhr*3600 + idmin*60 + idsec)
! debugging only! get pc date (getdat uses i*2): j1=4 digit year, j2=month, j3=day
         CALL getdat(j1,j2,j3)
         if(iw.eq.1)write(ifile,582)j3,j2,j1,idhr,idmin,idsec
582      format(' pc ',i2,'/',i2,'/',i4,1x,i2,':',i2,':',i2)
         if(iw.eq.1) call flush(ifile)     ! DIAG checkpoint A
! increment itime to reflect time that has past since last call:
         idchange = 0
         if(dtime.ge.dtime1) then
          idchange = int(dtime) - int(dtime1)
         else
          idchange = int((86400.0 - dtime1)+ dtime)
         endif
         itime = itime + idchange
         if(iw.eq.1.and.ierrlev.eq.6) then
          write(ifile,*)'dtime1=',dtime1,' dtime=',dtime,' itime=',itime
         endif
!
! 20feb2004 reopen date.nav file and append to it!
! if previous icday1 (orig set in siobegin) is not = current icday then
! we've had a day rollover.   We'll need to write our last averaged position
! to the previous date.nav file.
! what about just checking ibuf?   If nothing in the buffer, then no matter?

! if day not equal:
        if(icday1.ne.icday) then
         irollnav = 1
!
         if(iw.eq.1.and.ierrlev.eq.6) then
          write(ifile,*)'icday1neicday irollnav=',irollnav
          write(ifile,*)'icday1=',icday1,' icday =',icday
         endif
         if(icday1.eq.(icday-1)) then
! it's same month, just one day earlier:
          icmon1 = icmon
          icyear1 = icyear

! FIX!!!   What about a bogus date coming in?????? Such as Easymail

! else it's a previous month:
         else
! If January, go back to Dec and back one year:
          if(icmon.eq.1) then
           icmon1 = 12
           icyear1 = icyear - 1
! else months 2-12, just subtract one from month:
          else
           icmon1 = icmon - 1
           icyear1 = icyear
          endif ! (icmon.eq.1)
         endif ! (icday1.ne.(icday-1))

! then set afilen to previous days name (only if igps=1 !):
! and if there is something in the buffer:
         if(igps.eq.1.and.ibuf.ge.1) then
          CALL getfilen(afilen,adosday,adosmon,adosyear,
     $          icday1,icmon1,icyear1,adir,len_adir)
          if(iw.eq.1.and.ierrlev.ge.6) write(ifile,*)
     $          ' after getfilen icday1=',icday1
! if nothing in the buffer then can stick with today's date:
! can unset irollnav, I think and set icday1 to new day:
         elseif(igps.eq.1.and.ibuf.eq.0) then
          irollnav = 0
          icday1 = icday
          if(iw.eq.1.and.ierrlev.ge.6) write(ifile,*)'1set icday1=',
     $           icday1
          CALL getfilen(afilen,adosday,adosmon,adosyear,
     $          icday,icmon,icyear,adir,len_adir)
         elseif(igps.eq.2) then
! we're running on no gps and had a dayrollover, we want to write
! one DR position to our current day date.nav file.
!  How to know if we've got a DR position yet? (ifirst ne 0):
          icday1 = icday
          if(iw.eq.1.and.ierrlev.ge.6) write(ifile,*)'2set icday1=',
     $                                 icday1
          CALL getfilen(afilen,adosday,adosmon,adosyear,
     $          icday,icmon,icyear,adir,len_adir)
         endif !(igps)
! day IS equal:
        else
! then set afilen to todays days name:
         CALL getfilen(afilen,adosday,adosmon,adosyear,
     $          icday,icmon,icyear,adir,len_adir)
        endif ! (icday1.ne.icday)

        if(icyear.gt.2000) iiyergps = icyear - 2000
        if(iyerave.gt.2000) iiyerave = iyerave - 2000
        if(iw.eq.1) write(ifile,*)'diag A2: aft getfilen'    ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG

! translate iplandir to aplandir
        if(iplandir.eq.1) then
         aplandir = 'N'
        elseif(iplandir.eq.2) then
         aplandir = 'E'
        elseif(iplandir.eq.3) then
         aplandir = 'S'
        elseif(iplandir.eq.4) then
         aplandir = 'W'
        endif
! translate iclonh to aclonh
        aclonh = 'E'
        if(iclonh.eq.4) aclonh = 'W'
! translate iclath to aclath
        aclath = 'N'
        if(iclath.eq.3) aclath = 'S'
!        if(iw.eq.1.and.ierrlev.ge.6) write(ifile,*)'aclath,aclonh',aclath,aclonh
! translate istat to astat:
        if(istat.eq.1) then
         astat='NAV'
        else
         astat='UNK'
        endif
        if(iw.eq.1.and.ierrlev.eq.6)then
           write(ifile,*) 'aplandir=',aplandir,' aclonh=',aclonh
           write(ifile,*)'astat=',astat,' aclath=',aclath
        endif
!
        deadsec = 60.0*deadmin
        relodsec = 60.0*relodmin
!
! passing plan.dat next xbt position into here inside variable xlat, make it xlon if needed:
! char change        if(aspec.eq.'lon') xlon = xlat
! change again, I think xlat gets overwritten, use xlatload(1)!
!
! 18oct2004 watch xlatload passed to and from seas in 0-180 E -1 to -180 is West.
! change back to 0-360 E for my calcs...
! if ispec(1) = 0 then the plan is using longitude.   This is a stretch.  What
!     if longs and lats mixed in plan.dat.   This could be a problem...
! first just put xlatload into xlatload360!
        do 685 i = 1, 12
685        xlatload360(i) = xlatload(i)

!        ispec=0=plan longitude based.
!        ispec=1=plan latitude based.
        if(ispec(1).eq.0) then
! 18oct2004 create new variable xlatload360(12) which is xlatload(12) in 0-360 E
! coordinates.   Since xlatload(12) is 0-180 E and -1 to -179 is west...
         do 701 i = 1, 12
           if(xlatload(i).lt.0.0) then
!             using plus here because xlatload is already negative...
              xlatload360(i) = 360.0 + xlatload(i)
           endif
701      continue
         xlon = xlatload360(1)
        endif
!
        if(iw.eq.1) write(ifile,*)'diag A3: bef int2ch'       ! DIAG
        if(iw.eq.1) call flush(ifile)                          ! DIAG
! get pc date (getdat uses i*2): j1=4 digit year, j2=month, j3=day
! 10feb2004 NO MORE, Now use JB passed in day/time:
! JB sends in 4 digit
         i1 = int(cyear)
        if(iw.eq.1) write(ifile,*)'diag A3a cyear=',cyear    ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
! turn pc 4 digit year into a 4 char string
         CALL int2ch(i1,a4,1,len)
        if(iw.eq.1) write(ifile,*)'diag A3b aft int2ch'      ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
! back to real - but only translate back last 2 digits...
         CALL ch2real(a4,3,2,x)
        if(iw.eq.1) write(ifile,*)'diag A3c aft ch2real'     ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
! icyr: seas 2 digit year, imo: month, iday: day
         icyr = int(x)
! idhr: i for integer, d for dos (pc), hr for hour.
! 25feb2004 reset as incoming seas hr,min,sec
         idhr = int(chr)
         idmin = int(cmin)
         idsec = int(csec)
! moved 4 lines from below:
          chrsav = chr
          icsec = int(csec)
! ctime: current gps time in hours
          ctime = chr + (cmin/60.0) + (csec/3600.0)
        if(iw.eq.1) write(ifile,*)'diag A3d ctime=',ctime    ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG

! FIX FIX!!!!!!!:-
         if(igps.eq.2) then
! nothing yet?     ACK
! 27oct2004 this should be ok - don't really need this if(igps=2) elseif(igps=1) !
!        	Leave for now in case I change my mind...
         elseif(igps.eq.1) then
! 24feb2004 - timetag is seconds into WEEK, gpstime, etc is seconds into DAY
! change timetag TO seconds into day.   What will this break???
! this may screw up at midnight???
!          CALL dayofw(iweekday)
!          CALL gettmtg(iweekday,int(chr),int(cmin),int(csec),timetag)
! increment ibuf if gps updating
          if(iupdate.eq.1) then
           ibuf = ibuf + 1
        if(iw.eq.1) write(ifile,*)'diag A3e ibuf=',ibuf      ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
           timetag = (chr + (cmin/60.0) + (csec/3600.0))*3600.0
           CALL deg2dec(int(clatd),clatm,aclath,clat)
        if(iw.eq.1) write(ifile,*)'diag A3f clat=',clat      ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
           CALL deg2dec(int(clond),clonm,aclonh,clon)
        if(iw.eq.1) write(ifile,*)'diag A3g clon=',clon      ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
           if(iw.eq.1.and.ierrlev.eq.6)then
            write(ifile,*)'csec=',int(csec),'timetag=',timetag
            write(ifile,*)'clat=',clat,' clon=',clon
           endif
! don't put duplicate timetags (and positions) into buffers
! nobuf: =0 put latest position into buffer. nobuf =1 don't put into buffers
           nobuf = 0
           if(ibuf.gt.1) then
            if(timetag.eq.ctagbuf(ibuf-1)) then
             nobuf = 1
             ibuf = ibuf - 1
            endif
           endif
!
           if(nobuf.eq.0.and.ibuf.gt.0) then
              if(iw.eq.1.and.ierrlev.eq.6) then
                write(ifile,*)'ad buf,ibuf=', ibuf
             endif
        if(iw.eq.1) write(ifile,*)'diag A3h bef ctagbuf'     ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
             ctagbuf(ibuf) = timetag
        if(iw.eq.1) write(ifile,*)'diag A3i bef clonbuf'     ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
             clonbuf(ibuf) = clon
        if(iw.eq.1) write(ifile,*)'diag A3j bef clatbuf'     ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
             clatbuf(ibuf) = clat
        if(iw.eq.1) write(ifile,*)'diag A3k aft clatbuf'     ! DIAG
        if(iw.eq.1) call flush(ifile)                         ! DIAG
           endif
          endif  !iupdate=1
         endif   !igps=1

        if(iw.eq.1) write(ifile,*)'diag A4: aft GPS buf'      ! DIAG
        if(iw.eq.1) call flush(ifile)                          ! DIAG
         if(iupdate.eq.0) then
! 17aug2006 new - check gps seconds for a change and skip this loop
!                 if we are updating (DANGER DANGER WILL ROBINSON...)
!          csec is incoming gps second, gpssec is previous gps second
          if(csec.ne.gpssec) go to 750

! if we have a timeout on the port, load the previous position message into
! acmsg then go do the deadreckoning.
           if(igps.eq.1) then
! BUT have to do the iaveflg=1 loop once first, how to check?
             iaveflg = 0
             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'set iaveflg=0'
             go to 59
           elseif(igps.eq.2) then
! 28jan2004 - test putting iaveflg=1 here:
! 03jun2015 - PUT BACK iaveflg=1 here (after Ibis changed the incoming
!             current time to continue changing - previously she would
!             stop changing the time unless she checked the gps!)
! 27may2015 - I _think_ this is the problem when I unplug gps and it
! stop dr'ing - so comment this iaveflg=1 out for now and retest!
! comment out 27may2015!              
              iaveflg = 1
        if(iw.eq.1.and.ierrlev.eq.6) 
     $      write(ifile,*)'set iaveflg=1 since igps=2 '
           endif ! igps/else=2
         endif ! iupdate=0
! endif (iupdate)
!
750     continue
!
        if(iw.eq.1) write(ifile,*)'diag A5: bef gpssec'       ! DIAG
        if(iw.eq.1) call flush(ifile)                          ! DIAG
        if(iw.eq.1.and.ierrlev.eq.6) write(ifile,577) inav,iaveflg,ispd
577        format('inav=',i1,' iaveflg=',i1,' ispd=',i1)
!
          gpssec1 = csec
!
! compare gps seconds to see if it changes from 5? to 0?
! gpssec: is the previous gps second, gpssec1: is the current gps second
          if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'gpssec=',
     $                     int(gpssec),' gpssec1=',int(gpssec1)
           if(igps.eq.1) then
            if(gpssec1.lt.10.0.and.gpssec.ge.50.0) then
             inav = 1
             if(iw.eq.1.and.ierrlev.eq.6) then
                write(ifile,*)'5? to 1?  inav=', inav, ' ibuf=',ibuf
             endif
            endif
           endif !igps=1

! calculate speed and direction once a minute:
! If we have more than 7 positions (approx 15 sec if gps running on the 2
! second update rate) in the buffers at the minute rollover call ave, ave calls
! Bruce's slatec routines.
            if(inav.eq.1) then
! 11aug2006 Seas is actually a 2 second update rate if using NMEA GPS
!  If using TnT proprietary it's a 10 second update.   Note when GPS positions
!  are spotty if ibuf.ge.5 is not giving us many positions.   What could I break
! if I lower this even more.... try 4....
! 18aug2006- turns out it's a precision problem w seas so when ship slow I'm
! not getting many updates.   Let's bring this back up to 5:
! note this ibuf.ge.5 is in TWO places....:
             if(ibuf.ge.5) then
              if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*)'bef chkbuf'
               do 754 ik = 1, ibuf
754               write(ifile,756)ik,ctagbuf(ik),clatbuf(ik),clonbuf(ik)
756               format(i3,1x,f7.0,2f9.4)
              endif
! fix ibuf if last value not put in buffers before chkbuf called
              check = abs(ctagbuf(ibuf) - ctagbuf(ibuf-1))
              if(check.gt.100.0) then
                 ibuf = ibuf - 1
                 if(iw.eq.1.and.ierrlev.ge.6) then
                    write(ifile,*) 'ctagbuf(ibuf)=',ctagbuf(ibuf)
                    write(ifile,*) 'ctagbuf(ibuf-1)=',ctagbuf(ibuf-1)
                    write(ifile,*) 'decrement ibuf by 1, ibuf=',ibuf
                 endif
              endif
              if(ctagbuf(ibuf).eq.0.0) then
                 ibuf = ibuf - 1
              endif

! 18aug2006 recall subroutine chkbuf timetags within 200 seconds, up it to 400?
              CALL chkbuf(ibuf,clatbuf,clonbuf,ctagbuf,ierr,iw,ifile)
              if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*)'aft chkbuf, ibuf= ',ibuf,' ierr= ',ierr
               do 755 ik = 1, ibuf
755               write(ifile,756)ik,ctagbuf(ik),clatbuf(ik),clonbuf(ik)
              endif

! FIX JB FIX 10 second update rate!!              if(ibuf.lt.7) then
! this will only happen if I remove from buffer (bad vals)
! 18aug2006 - comment this out:
!              if(ibuf.lt.3) then
!               if(ierrlev.eq.6)write(ifile,*)'ibuf.lt.7,skip ave,zero ibuf'
!               ibuf = 0
!               go to 19
!              endif
!15sep2014 this ierr is from chkbuf, 0=good, 1=bad:
              if(ierr.eq.1) then
               if(iw.eq.1.and.ierrlev.eq.6) then
                write(ifile,*)'all bad by chkbuf:'
                do 877 ib = 1, ibuf
                 write(ifile,878) ib,ctagbuf(ib),clatbuf(ib),clonbuf(ib)
877             continue
878             format(i2,f8.1,2f9.4)
               endif
               ibuf = 0
               go to 21    ! this was 19 (duh)
              endif
! BRZENSKI Save the previous good values of vlat and vlon before calling ave
! then we can restore them if the speed is excessive, like the GPS skips
! or dead reckoning gives some big number.
              vlat_prev = vlat
              vlon_prev = vlon

              CALL ave(ibuf,clatbuf,clonbuf,ctagbuf,avlath,avlonh,
     $            s,d,timeave,vlat,vlon,ierror,iderr,iSIOSpeedAveMin,
     $            iw,ifile)
              if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*)'aft ave, timeave=',timeave,' ibuf=',ibuf
               do 757 ik = 1, ibuf
757               write(ifile,756)ik,ctagbuf(ik),clatbuf(ik),clonbuf(ik)
               if(ctagbuf(1).ge.603000.0) then	
                 write(ifile,*)'bck frm ave, ctagbuf(1)=',ctagbuf(1),
     $           vlat, vlon, timeave
               endif
         write(ifile,*)'ierr(38&39) ', ierror(38),ierror(39)
              endif

! if return from ave with error from dpolft then we've got a screwed
! value in our buffer.  Don't use any of these, clear out buffers.
! This iderr is a return from ave (dpolft) and 1=good, anything else=bad!
              if(iderr.ne.1) then
               if(iw.eq.1.and.ierrlev.eq.6) then
                write(ifile,*) 'iderr frm ave= ', iderr
                do 777 ij = 1, ibuf
                  write(ifile,*) clatbuf(ij), clonbuf(ij), ctagbuf(ij)
777             continue
               endif
               go to 21     !this was 19 (duh)
              endif
!
              iaveflg = 1
!
! 19feb2005 LL I think this is part of my dayroll problem?

! Here is seems we are putting the current date into our "ave" variables
! since ave was called successfully:
              idayave = icday
              imonave = icmon
              iyerave = icyear
              if(iw.eq.1.and.ierrlev.eq.6)write(ifile,*)'new dateaves:',
     $                        idayave,'/',imonave,'/',iyerave
!
! don't use the speed and dir calculated unless have about a full minute
! of positions:
! NOTE!  Using 27 because using the 2 second update rate:
! SEASS change - use 5 since she only updates every 10 seconds...
! note this ibuf.ge.5 is in TWO places....:
              if(ibuf.ge.5) then
               if(s.ne.-99.0) speed = s
               if(speed.gt.xmaxspd) then
                do 778 ij = 1, ibuf
                  if(iw.eq.1.and.ierrlev.eq.6)write(ifile,*)clatbuf(ij),
     $                    clonbuf(ij), ctagbuf(ij)
778             continue
                ierror(11) = 1       ! calc'd speed greater than xmaxspd
! BRZENSKI - if speed is greater than xmaxspd, restore previous good lat and lon
                vlat = vlat_prev
                vlon = vlon_prev
                iaveflg = 0
                ibuf = 0
                go to 21     ! END of BRZENSKI vlat reset
               else
                speed = s
               endif
               if(d.ne.-99.0) dir = d
              endif

! translate timeave:(ave of last min's timetags) to hr:min:sec
              CALL timetohms(timeave,ihr,imin,isec)

! write date, gps time, gps position timetag ave, lat ave, long ave,
! OEM unit status, speed, dir, # fixes, &  current 
! to date.nav file once a minute: 
              CALL dec2deg('lat',ivlatd,vlatm,avlath,vlat)
              CALL dec2deg('lon',ivlond,vlonm,avlonh,vlon)
! change this from checking only speed/dir TO check position/speed/dir!
! 15sep2014  - no, chkall only checks speed and dir!:
              CALL chkall(vlat,vlon,speed,dir,ierrwrite)
              if(iw.eq.1.and.ierrlev.eq.6) then
                 write(ifile,*)'aft chkall,vlat=',vlat,' vlon=',
     $                       vlon,'avlath=',avlath
                 write(ifile,*)' speed=',speed,' dir=',dir,
     $                       'ierrwrite=',ierrwrite
              endif
!
! 15sep2014 this is screwy: 19 is err open date.nav! duh...
              if(ierrwrite.eq.1.or.avlath(1:1).eq.' ') then
                 if(iw.eq.1) then
                  write(ifile,*)'Do not write to ',afilen(1:len_adir+15)
                  write(ifile,*)'ierrwrite= ', ierrwrite
                  write(ifile,*)'avlath(1:1)= ', avlath(1:1)
                 endif
                 go to 21          !this was 19 (duh!)
              endif
!
              if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*)'wrt to date.nav:'
               write(ifile,501) adosday,adosmon,adosyear(3:4),
     $                          ihr,imin,isec,
     $                     abs(ivlatd), vlatm, avlath, ivlond, vlonm,
     $                     avlonh, astat, speed, dir, ibuf
              endif
! open and write date.nav file:
              open(10,file=afilen,form='formatted',status='unknown',
     $          access='append',err=19)
              write(10,501,err=20) adosday,adosmon,adosyear(3:4),
     $                     ihr,imin,isec,
     $                     abs(ivlatd), vlatm, avlath, ivlond, vlonm,
     $                     avlonh, astat, speed, dir, ibuf
501           format(a2,'/',a2,'/',a2,' ',i2,':',i2,':',i2,' ',
     $             i3,' ',f7.4,' ',a1,' ',i3,' ',f7.4,' ',
     $             a1,' ',a3,' ',f5.2,' ',f5.1,i3)
!
              close(10,iostat=ios)
              if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close10ios=',ios
              go to 21        ! if here, all is well so skip setting errors
!
19            ierror(5) = 1       ! error opening date.nav
              ierror(34) = icday         ! pass seas the ddmmyy of date.nav
              ierror(36) = icmon
              ierror(37) = icyear
              go to 21
!
20            ierror(6) = 1       ! error writing date.nav
              ierror(34) = icday         ! pass seas the ddmmyy of date.nav
              ierror(36) = icmon
              ierror(37) = icyear
              close(10,iostat=ios)
              if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close10ios=',ios
!
21            continue
! clear out buffers every minute (if they were used, otherwise if gps is
! updating, but didn't have enough to calc position use these on the next one).
              ibuf = 0
             endif        ! (end if ibuf.ge.5)
             inav = 0
            endif  ! (end if inav.eq.1)
!
          gpssec = gpssec1
59         continue
         if(iw.eq.1) write(ifile,*)'diag B: past gps/inav'  ! DIAG
         if(iw.eq.1) call flush(ifile)                        ! DIAG
!
! Dead reckoning:          
! calculate longitude and eta of next xbt drop if aspec=lat (ispec(1)=1)
! calculate latitude and eta of next xbt drop if aspec=lon (ispec(1)=0)
! NOTE! I'm using the last averaged minute's speed and direction and the
!       most recent gps averaged position to calculate eta.
! if iaveflg=2 we have nothing to deadrec from.
! ??? add timer to alarm for this ???
           if(iaveflg.eq.2) go to 90
! if iaveflg=1 we have new position or speed/dir to deadrec from.
! Every time we get a new position or speed/dir reinitialize deadreckoning
! to start from there.  Subtract timeave from present gpstime then deadrec
! from the ave position to where we are now.  Then watch the dos seconds
! to continue deadrec.
! 28jan2004 - whoa - let's keep iaveflg=1 for no gps so it keeps running
! from navtrk.dat - otherwise the icsec and icsec1 get mixed up.

! NO! For NOGPS - iaveflg=1 at beginning (when read in pos from navtrk.dat)
! NO! so do this if stmt once, then iaveflg set to zero and continue to
! NO! do the BIG else until program stops.
           if(iaveflg.eq.1) then
            if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'DR iaveflg=1'
! if ispd=0 we don't have a speed and dir yet.
            if(ispd.eq.0) go to 90
! 'lat'
            if(ispec(1).eq.1) then
! First calculate the expected longitude of next xbt:
! dxlat: distance (in decimal degrees) of last average lat to next xbt lat.
             if(iw.eq.1.and.ierrlev.eq.6)write(ifile,*)'vlat,xlat ',
     $                                                  vlat,xlat
             dxlat = vlat - xlat
! lat & lon distance in nautical miles:
             dxlatnm = abs(dxlat * 60.0)
! I'm leaving this as tan since this is only for the xbt lon position and
! for the first cruise we're going NE.
             dxlonnm = dxlatnm * tan(dir*deg2rad)
! 9jun2006 add div by 0 check:
             x = (60.0 * cos(vlat*deg2rad))
             if(x.ne.0.0) then
               dxlon = dxlonnm / x
             else
               dxlon = dxlonnm
             endif
! going E
             if(dir.ge.0.0.and.dir.lt.180.0) then
              xlon = vlon + abs(dxlon)
             else
! going W
              xlon = vlon - abs(dxlon)
             endif
             if(xlon.gt.360.0) xlon = xlon - 360.0
             if(xlon.lt.0.0) xlon = 360.0 + xlon
! is this right for divide by zero?:
             x = cos(dir*deg2rad)
             if(x.ne.0.0) then
              dist = abs(dxlatnm / x)
             else
              dist = abs(dxlatnm)
             endif
            else
! 'lon'
! 28jan2004 LL whoa - this dxlon will never change in my new igps=2
! continue dr'ing using this part of the loop.  And since I look for the
! stop using dxlon, that's all wrong...
! add extra calc after calling newpos...   scheez
             dxlon = vlon - xlon
             if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'posloop iaveflg=1,dxlon=',dxlon
              write(ifile,*)'vlon=',vlon,' xlon=',xlon
             endif
! 07jul2005 TEST
             if(abs(dxlon).gt.300.0) then
                 if(dxlon.gt.300.0) then
                  dxlon = 360.0 - (vlon - xlon)
                 elseif(dxlon.lt.300.0) then
                  dxlon = 360.0 + (vlon - xlon)
                 endif
             endif

             if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'new dxlon=',dxlon
             endif
!-------------------------------
             dxlonnm = abs(dxlon * (60.0*cos(vlat*deg2rad)))
             x = tan(dir*deg2rad)
             if(x.ne.0.0) then
              dxlatnm = abs(dxlonnm / x)
              dxlat = dxlatnm / 60.0
             else
! is this actually ok to do?
              dxlat = vlat - xlat
              dxlatnm = abs(dxlat * 60.0)
             endif
              if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'dxlonnm=',dxlonnm,'dxlatnm=',dxlatnm
              write(ifile,*)'dxlt=',dxlat, 'dir' ,dir ,'vlt',vlat
             endif
             if(dir.ge.90.0.and.dir.le.270.0) then
              xlat = vlat - abs(dxlat)
             else
              xlat = vlat + abs(dxlat)
             endif

!             dist = dxlonnm / (sin(dir*deg2rad))
             x = cos(dir*deg2rad)
             if(x.ne.0.0) then
              dist = abs(dxlatnm / x)
             else
              dist = abs(dxlatnm)
             endif
             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'dist=',dist
            endif
! xalarm: last ave timetag (converted to one day) plus deadmin (deadsec)
! 86400 secs in one day
! translate last timeave to one day of seconds (x)
            if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'timeave=',
     $             timeave,' ctime=',ctime
! see if timeave if greater than one day:
             ix = int(timeave/86400.0)
             x = timeave
             if(ix.gt.0) x = timeave - float(ix*86400)
             xalarm = x + deadsec
! gpstime: current gps time in one day of seconds 
             gpstime = ctime*3600.0
! csec: is current gps second
             icsec1 = icsec
             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)
     $              'set icsec1=icsec=', icsec1
             change = gpstime - x
! KLUDGE FIX
! compare dates of timeave and gpstime here at beginning
             if(change.lt.-80000.0.or.idayave.ne.icday) then
              if(iw.eq.1) then
               write(ifile,*)'date not equal?', idayave,' ',icday
               write(ifile,*)'  or change<-80000 ',change
              endif
! assume it's only one day previous....
! 25oct2004  bad bad bad find out how many days:
! x here is from above which is = timeave
! timeave is seconds into the day of the last ave'd position
! 86400.0 seconds in one day
c 
! 19feb2005 LL icday and idayave are already same day at day roll so
! this is not working!
              idaychange = icday - idayave
              if(iw.eq.1.and.ierrlev.ge.6) write(ifile,*)'idayave=',
     $             idayave,' icday=',icday,' idaychange=',idaychange
! 19feb2005 LL
! add this to check the change instead:
              if(idaychange.eq.0)then
!              NORMAL - 1 day passed
               x = 86400.0 - x
              elseif(idaychange.eq.1) then
!              NORMAL - 1 day passed
               x = 86400.0 - x
! FIX!   These really need to be fixed!!!::::
              elseif(idaychange.ge.2) then
!               more than 1 day passed
                  x = (idaychange*86400.0) - x
              elseif(idaychange.lt.0) then
!               this is a month rollover, check how many days:
                idaychangemonth = idayave + idaychange
                if(idaychangemonth.eq.1) then
!        	  NORMAL 1 day change
                  x = 86400.0 - x
                elseif(idaychangemonth.ge.2) then
!        	  more than one day passed during month roll
                  x = (idaychangemonth*86400.0) - x
                endif
              endif
! whoa - test this ^
              change = x + gpstime
             endif
             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'change=',
     $                                    change
! put previous average lat/lon into vlat1,vlon1 to pass to newpos to be updated:
             vlat1 = vlat
             vlon1 = vlon
! so here, vlat and vlat1 are the same before calling newpos:
! the "change" here is what is important.   Going in and out of sioloop means
! "change" will be screwy.   How to handle?
             if(iw.eq.1) write(ifile,*)'diag C: bef newpos1'  ! DIAG
             if(iw.eq.1) call flush(ifile)                     ! DIAG
             CALL newpos(speed,change,dir,vlat,vlat1,vlon1,aclath,
     $                    ierrlev,ifile)
             if(iw.eq.1) write(ifile,*)'diag D: aft newpos1'  ! DIAG
             if(iw.eq.1) call flush(ifile)                     ! DIAG
             if(aclath.eq.'N') iclath = 1
             if(aclath.eq.'S') iclath = 3
!             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'1aft newpos', vlat1, vlon1,
!     $                        aclath, ' ',speed

! translate xlon to
! correct hemisphere, degrees and minutes then to char, xeta to char to print.
! xbteta calc's eta for next 5 drops in plan.dat:
             if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'calline xbteta,nlnchr=',nlnchr
              write(ifile,*)'   xlatload360(1)=',xlatload360(1)
!              write(ifile,*)'   vlat1=',vlat1,'vlon1=',vlon1,'speed=',speed
!              write(ifile,*)'   dir=',dir,'ctime=',ctime,'ispec(1)=',ispec(1)
              write(ifile,*)'   nplan=',nplan,' ispec(1)=',ispec(1)
              write(ifile,*)'   xlat=',xlat,'xlon=',xlon
             endif
             if(iw.eq.1) write(ifile,*)'diag E: bef xbteta1'  ! DIAG
             if(iw.eq.1) call flush(ifile)                     ! DIAG
              CALL xbteta(xlatload360,vlat1,vlon1,speed,
     $                      dir,ctime,ispec,nplan,
     $                      xlat,xlon,ixhr,ixmin,ixsec,ierrlev,
     $                      nlnchr,eta,ifile)
             if(iw.eq.1) write(ifile,*)'diag F: aft xbteta1'  ! DIAG
             if(iw.eq.1) call flush(ifile)                     ! DIAG
!              if(iw.eq.1.and.ierrlev.eq.6) then
!               write(ifile,*) 'after xlatload, etas:'
!               do 6000 ie = 1, nlnchr
!                write(ifile,*) '     ',xlatload(ie),eta(ie)
!6000               continue
!              endif
! the BIG else... this is the part of the loop we do when we don't have
! a new position/speed/dir to deadreckon from.
! just going to watch the gps seconds to countdown to the xbt drop (or alarm)
! that way don't have to worry about the day rollover...

! this is the (if iaveflg=0) part:

            else

!23456789012345678901234567890123456789012345678901234567890123456789012       
             if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)
     $          'DR iaveflg=0,icsec=',icsec
             endif
             if(icsec.ne.icsec1) then
! 23feb2004 - next 25 line cp'd from iaveflg=1 !!!
! translate last timeave to one day of seconds (x)
            if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'2 timeave=',
     $               timeave,' ctime=',ctime
             ix = int(timeave/86400.0)
             x = timeave
             if(ix.gt.0) x = timeave - float(ix*86400)
             xalarm = x + deadsec
! gpstime: current gps time in one day of seconds 
             gpstime = ctime*3600.0
! csec: is current gps second
             icsec1 = icsec
             if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*)'set icsec1 = icsec = ', icsec1
               write(ifile,*)'x=',x
             endif
             change = gpstime - x
! KLUDGE FIX
! compare dates of timeave and gpstime here at beginning
             if(change.lt.-80000.or.idayave.ne.icday) then
              if(iw.eq.1) then
                write(ifile,*)'date not equal?', idayave,' ',icday
                write(ifile,*)'  or change<-80000 ',change
              endif
! assume it's only one day previous....
              x = 86400.0 - x
              change = x + gpstime
             endif
             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'change=',change
             else
              go to 90
             endif
! Translate the time we've just travelled into  a new position.
             x = change
             CALL newpos(speed,x,dir,vlat,vlat1,vlon1,aclath,
     $                    ierrlev,ifile)
             if(aclath.eq.'N') iclath = 1
             if(aclath.eq.'S') iclath = 3
             if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'CALL xbteta,'
              write(ifile,*)xlatload360(1),xlatload360(nlnchr),vlat1,
     $                     vlon1,speed,dir,ctime,ispec,nplan,xlat,xlon
             endif
              CALL xbteta(xlatload360,vlat1,vlon1,speed,
     $                      dir,ctime,ispec,nplan,
     $                      xlat,xlon,ixhr,ixmin,ixsec,ierrlev,
     $                      nlnchr,eta,ifile)
              if(iw.eq.1.and.ierrlev.eq.6) then
               write(ifile,*) 'xlatload360, eta'
               do 6001 ie = 1, nlnchr
                write(ifile,*) xlatload360(ie),eta(ie)
6001           continue
              endif
          if(deadmin.gt.0.0.and.gpstime.ge.xalarm.and.iupdate.eq.0)then
           ierror(8) = 1          ! gpstime>=xalarm - passed DR time limit
          endif
         endif
!        end iaveflg else part

! FIX WHOA - xlat = xlon here:   so one or other is wrong:
          dxlat = vlat1 - xlat
          dxlon = vlon1 - xlon
! 07jul2005 TEST
             if(abs(dxlon).gt.300.0) then
                 if(dxlon.gt.300.0) then
                  dxlon = (vlon1 - xlon) - 360.0
                 elseif(dxlon.lt.300.0) then
                  dxlon = 360.0 + (vlon1 - xlon)
                 endif
             endif
!
             if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'new dxlon=',dxlon
             endif
!-------------------------------
! 01Oct2004- ship circles - try this: only check if actual ship "dir" is same
!            as "aplandir" (risky...)
! tried subroutine - need gauge rebuild          CALL comparedir(ispec,iplandir,dir,idirck) 
! idirck=1 means we think ship heading same direction as plan:
        idirck = 1
! remember iplandir: N=1, E=2 , S=3 , W=4
! our dirs are N=0deg, E=90deg, S=180deg, W=270deg
!    set idirck(=0) only if bad (meaning ship is going different direction than
!    what we think plan.dat is heading (this is for ship circles...):
! ispec=0 means plan is longitude based
        if(ispec(1).eq.0) then
! dir is direction ship is heading (could come in from navtrk.dat, user, or gps)
! iplandir is direction the plan.dat is going!
! so this is if plan is longitude based and ship is heading E and plan says
! we should be going W, set idirck=0:
           if(dir.ge.0.0.and.dir.le.180.0.and.iplandir.eq.4) idirck=0
! so this is if plan is longitude based and ship is heading W and plan says
! we should be going E, set idirck=0:
           if(dir.le.360.0.and.dir.ge.180.0.and.iplandir.eq.2) idirck=0
! ispec=1 means plan is latitude based
        elseif(ispec(1).eq.1) then
           if(dir.ge.270.0.and.dir.le.90.0.and.iplandir.eq.3) idirck=0
           if(dir.le.270.0.and.dir.ge.90.0.and.iplandir.eq.1) idirck=0
        endif

          if(iw.eq.1.and.ierrlev.eq.6) then
             write(ifile,*)' xlat=',xlat,' dxlat=',dxlat
             write(ifile,*)' xlon=',xlon,' dxlon=',dxlon
             write(ifile,*)'chk loc idirck=',idirck
          endif
! If ship is heading in same direction that plan thinks we should be going,
! then check if we are past xbt location.
! BRZENSKI: BUT also check that xmaxspd is not exceeded, otherwise
! the drop may be triggered if GPS cuts out and back in, making a large
! instantaneous speed. This will pause and wait for the speed to settle
! before trying to check to trigger a drop.
          if(idirck.eq.1.and.speed.le.xmaxspd) then
! check to see if we're past xbt location, if so, set stoptime
           if((aplandir.eq.'N'.and.dxlat.ge.0.0).or.
     $       (aplandir.eq.'S'.and.dxlat.le.0.0).or.
     $       (aplandir.eq.'W'.and.
     $           dxlon.le.0.0.and.abs(dxlon).le.20.0).or.
     $       (aplandir.eq.'E'.and.
     $           dxlon.ge.0.0.and.abs(dxlon).le.20.0) ) then
! set the timer to run for runsec after we've past the xbt location:
             if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)
     $            'setting stoptime!'
             stoptime = itime + runsec
! idsec2's other self - let's me know stoptime has been set!
             idsec2 = 1
           endif
          endif

          if(iw.eq.1.and.ierrlev.eq.6) write(ifile,*)'stoptime=',
     $        int(stoptime), ' idsec2=',idsec2

! gps status
         if(igps.eq.1) then
          if(iupdate.eq.1) then
           astat='NAV'
           istat=1
!          else
!           astat='UNK'
!           istat=2
          endif
         endif
! 24feb2004 - keep these before "90" so if do not update, don't write 
! them to drlat and drlon!
! put vlat1 and vlon1 into drlat and drlon: c I think...
        drlat = vlat1
        drlon = vlon1
! 18oct2004 - JB needs Longitude: 1-180 positive is East, Negative 0-180 is west 
! so change drlon only - I do not want to change my whole code and this is 
! all she uses.   So far.
        if(drlon.gt.180.0.and.drlon.lt.360.0) then
           drlon = -1. * (360.0 - drlon)
        elseif(drlon.eq.360.0) then
           drlon = 0.0
        elseif(drlon.gt.360.0) then
           drlon = 360.0 - drlon
        endif
90      continue
         if(iw.eq.1) write(ifile,*)'diag G: at lbl 90'       ! DIAG
         if(iw.eq.1) call flush(ifile)                         ! DIAG
c 28jan2004 - moved this here - will it work?:
! Here is the normal exit from program once reached xbt location:
! 20jul2004 - add - if(ierror(21)=0) because if ierror(21)=1 we've
! reached the end of plan.dat so DO NOT SEND A ierror(1)=1 (dropNOW!)
! Want to just keep running... (try .ne.2 since will stop for ALL
! other end of plan.dats)
! 7mar2005 remove the "ierror(21).ne.2" since I no longer set it...
! OLD!!!    if(idsec2.eq.1.and.itime.ge.stoptime.and.ierror(21).ne.2) then
            if(idsec2.eq.1.and.itime.ge.stoptime) then
             if(iw.eq.1)write(ifile,*)'stoptime=',stoptime,
     $         'itime=',itime
! check current dos date/time against 3 drops ago from stations.dat,
! be sure at least 10 minutes has past.  Stop autolauncher dumping.
! Don't exit out of program here, let operator decide what to do
! yrday1: comes from gpsbegin- yearday of 3 drops ago
! iiyergps is 2 digit year from seas
             CALL yrdy(iiyergps,icmon,icday,idhr,idmin,idsec,yrday2)
!29oct2014 check that yrday1>0.0...
             if(yrday1.gt.0.0) then
              if((yrday2-yrday1).lt.0.0069444) then
               ierror(30) = 1         ! trying to drop 3rd probe in < 10 min
              endif
             endif
             ierror(1) = 1
             go to 101
            endif

           if(dropmin.gt.0.0.and.itime.ge.alrmtime) then
            ierror(10) = 1          ! passed dropmin time limit betwn drops
            if(iw.eq.1)write(ifile,*)'itime.ge.alrmtime',
     $         itime, alrmtime
           endif

! End of main loop.  From here down is closing program:
        go to 101

101        continue
         if(iw.eq.1) write(ifile,*)'diag H: at lbl 101'      ! DIAG
         if(iw.eq.1) call flush(ifile)                         ! DIAG
!
         if(iw.eq.1) then
          write(ifile,587)speed, dir
          write(ifile,568) drlat, drlon, vlat, vlon
568       format('out drlt,drln,vlt,vln: ',f7.3,f9.3,f7.3,f9.3)
         endif

! 20 feb2004 set ifirst here!  for all igps
        ifirst = 1
! only want to do this if running on DR (igps=2) and
! it's just past midnight and there are no positions in date.nav
! 25oct2004 - let's update navtrk.dat too!  yikes
! NO GPS: write one line to new nav file:
        if(igps.eq.2.and.irollnav.eq.1) then
          irollnav = 0
          CALL dec2deg('lat',ivlatd,vlatm,avlath,vlat1)
          CALL dec2deg('lon',ivlond,vlonm,avlonh,vlon1)
!
          open(10,file=afilen,form='formatted',status='unknown',
     $         err=545)
          write(10,543,err=546)adosday,adosmon,adosyear(3:4),'00:00:01',
     $                  abs(ivlatd), vlatm, avlath, ivlond, vlonm,
     $                  avlonh, 'DED', speed, dir, 0
543                format(a2,'/',a2,'/',a2,' ',a8,' ',
     $             i3,' ',f7.4,' ',a1,' ',i3,' ',f7.4,' ',
     $             a1,' ',a3,' ',f5.2,' ',f5.1,i3)
          go to 547          ! all is well, skip errors
!
545       ierror(5) = 1          ! error opening date.nav
          ierror(34) = icday         ! pass seas the ddmmyy of date.nav
          ierror(36) = icmon
          ierror(37) = icyear
          if(iw.eq.1) write(ifile,*)'error opening(for DED)',
     $                               afilen(1:len_adir+15)
          go to 547
!
546       ierror(6) = 1         ! error writing date.nav
          ierror(34) = icday         ! pass seas the ddmmyy of date.nav
          ierror(36) = icmon
          ierror(37) = icyear
          if(iw.eq.1) write(ifile,*)'error writing(for DED)',
     $                               afilen(1:len_adir+15)
!
547       continue
          close(10,iostat=ios)
          if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close10DEDios=',ios
          if(iw.eq.1) write(ifile,*)'wrote DED to ',
     $                               afilen(1:len_adir+15)
!
! now write to navtrk.dat:
          open(15,file=anavtrk,form='formatted',status='unknown',
     $         err=550)
          rewind(15,iostat=ios)
          write(15,509,err=551)adosday,adosmon,adosyear(3:4),'00:00:01',
     $                  vlat1,vlon1,speed,dir
509       format(a2,'/',a2,'/',a2,' ',a8,' ',f7.3,f8.3,f6.2,f7.2)
          go to 552          ! all is well, skip errors
!
550       ierror(23) = 1     ! error opening navtrk.dat
          if(iw.eq.1) write(ifile,*)'error opening(for DED)',
     $                              anavtrk(1:len_adir+15)
          go to 552          
551       ierror(14) = 1     ! error writing navtrk.dat
          if(iw.eq.1) write(ifile,*)'error writing(for DED)',
     $                               anavtrk(1:len_adir+15)
552       continue
          close(15,iostat=ios)
          if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close15DEDios=',ios
          if(iw.eq.1) write(ifile,*)'wrote DR pos to navtrk.dat'
        endif ! (igps=2 & irollnav=1)
!
! 19feb2005 LL this must be part of my new dayroll problem!
!
! 11nov2004 LL pulled this out of inav=1 loop since dayroll NOT working -
! I was not setting icday1=icday after a dayroll (does this go before 59 or after?)
! this for the dayrollover handling:
! set ibuf = 0 if not written so don't screw up new date.nav file
             if(irollnav.eq.1) then
                if(iw.eq.1) write(ifile,*)
     $           'unset irollnav,icday1,ibuf',irollnav,icday1,ibuf
               irollnav = 0
               icday1 = icday
               ibuf = 0
               if(iw.eq.1)write(ifile,*)'they now=',irollnav,icday1,ibuf
             endif
! 
        if(iw.eq.1.and.ierrlev.ge.6) then
         write(ifile,*)' outg icday1=',icday1,' idayave=',idayave
         write(ifile,*)'timeave=',timeave
         write(ifile,*)'LV SIOLOOP,ifirst=',ifirst
        endif
!jun2012 LL this is a pain-in-b alarm when traveling in a direction
!   other than what plan states. How to allow op to disable? Maybe just tie
!   into the 'passed deadreckoning time limit'?
! ierror(12)="change in dead reckoning lat or lon is too big"
!            ierror(12) is set in subroutine ave
! this is the old (pre june 2013):        if(ierror(12).eq.1) then
! TODO FIX THIS WHILE WORKING WITH IBIS 09sep2014
        if(ierror(12).eq.1.and.deadmin.gt.0.0) then
           ierror(35) = 312
        endif
! If ierror(35) is still = 0 when we get here we think we've
! run thru siobegin successfully, so set it = 2 on exit
        if(ierror(35).eq.0) then
           ierror(35) = 2
        endif

999     continue
        if(iw.eq.1) then
         do 1010 i = 1, nerr
1010       if(ierror(i).ne.0) write(ifile,*)'ierror(',i,')=',ierror(i)
         call flush(ifile)
        endif

        close(ifile)
        return
        end

!^*************end sioloop ********************************************^

!23456789012345678901234567890123456789012345678901234567890123456789012       
!
        SUBROUTINE sioend(igps,ibuf,ierrlev,ierror,idayave,
     $    imonave,iyerave,speed,dir,timeave,vlat,vlon,
     $    icday,icmon,icyear,istat,ctagbuf,clatbuf,clonbuf,
     $    iSIOSpeedAveMin)
!
        parameter(nerr=50)
!
!GCC$ ATTRIBUTES DLLEXPORT :: sioend
!
        character avlath*1, avlonh*1
        character*8 adateave
        character astat*3, agpsdate*8
        real*4 clatbuf(200), clonbuf(200), ctagbuf(200)
        integer*4 ierror(nerr), igps, ihr, imin, isec, ibuf
        integer*4 icday,icmon,icyear,istat
        integer*4 ierrlev, idayave,imonave,iyerave
        real*4 timeave,speed,dir,vlat,vlon

        character adir*80, anavtrk*80, asio*80
        character afilen*80, adosday*2, adosmon*2, adosyear*4
        integer*2 j1, j2, j3, j4
!
        integer*4 iw, ifile, ios
        integer*4 igderr(3)
        integer*4 iSIOSpeedAveMin
        integer*4 i
        integer*4 ierr,iderr
        integer*4 jpos,len,len_adir
!
! file numbers opened and closed in sioend:
!       10=<date>.nav   (write)
!       15=navtrk.dat   (write)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=sio.log (write, append on from siobegin&sioloop routine)

! ierrors set in here:  Note siobegin clears all ierror before calling this
!( 5) - error opening <date>.nav file
!( 6) - error writing <date>.nav file
!( 7) - error opening siodir.txt
!(12) - change in Dead Reckoning lat or lon is too big (something amiss?)
!(14) - error writing navtrk.dat
!(17) - error reading siodir.txt
!(23) - error opening navtrk.dat
!(28) - speed and/or direction values screwed up - not writing to navtrk.dat or <date>.nav
!(34) - integer of day (dd) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(35) - iSIOErrWatchDog , set to 2 at end of program if all is well.
!                       set to 3XX where XX is above error number to alert
!                       an error was set
!     = 307,312,317
!(36) - integer of month (mm) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(37) - integer of year (yy) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(38) - used for "jptr" in subroutine ave
!(39) - used for "icall" in subroutine ave
!(44) - ALL    - error opening calling routine's .log file (eg sio.log)
!(45) - ALL    - error writing calling routine's .log file (eg sio.log)
!
! 05sep2014 clean up a bit...
!
!11sep2014 rm zero out ierror from this routine
!
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
        do i = 1, 80
           asio(i:i) = ' '
           anavtrk(i:i) = ' '
           afilen(i:i) = ' '
           adir(i:i) = ' '
        end do
! get seas2k path:
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open or read file 'siodir', assume current directory?
! if I can not open or read file 'siodir.txt'
        if(ierror(7).eq.1) then
           len_adir = 0
           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
           ierror(35) = 317
           go to 999
        endif

        if(len_adir.gt.0) then
           asio(1:len_adir) = adir(1:len_adir)
           anavtrk(1:len_adir) = adir(1:len_adir)
        endif

        asio(len_adir+1:len_adir+12) = 'Data\sio.log'
        anavtrk(len_adir+1:len_adir+15) = 'Data\navtrk.dat'

! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=asio,form='formatted',
     $             access='append',status='unknown',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334 
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then 
           write(ifile,*,err=335)'Inside sioend (append to sio.log): '
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           call flush(ifile)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif
!
        if(iw.eq.1) then
         write(ifile,*)'BEGIN SIOEND,istat=',istat
         write(ifile,*)'speed,dir=',speed, dir
         write(ifile,*) 'igps=',igps, ' ibuf=',ibuf
         write(ifile,*) 'ierrlev=',ierrlev, ' istat=',istat
         write(ifile,*) 'icday,icmon,icyear=',icday,icmon,icyear
         write(ifile,*) 'ctagbuf 1=',ctagbuf(1)
         write(ifile,*) 'clatbuf 1=',clatbuf(1)
         write(ifile,*) 'clonbuf 1=',clonbuf(1)
         write(ifile,*) 'iday,mon,yer ave=',idayave,imonave,iyerave
         write(ifile,*) 'timeave=',timeave
         write(ifile,*) 'vlat=',vlat, ' vlon=',vlon
         write(ifile,*)'anavtrk=',anavtrk(1:len_adir+15)
        endif

! 20feb2004 reopen date.nav file and append to it!
        CALL getfilen(afilen,adosday,adosmon,adosyear,
     $          icday,icmon,icyear,adir,len_adir)

! translate icday, icmon, icyear to agpsdate:
        agpsdate = '00/00/00'
        agpsdate(1:2) = adosday(1:2)
        agpsdate(4:5) = adosmon(1:2)
        agpsdate(7:8) = adosyear(3:4)
        if(iw.eq.1) then
           write(ifile,*) 'agpsdate',agpsdate,'******'
           write(ifile,*)'idayave,imonave,iyerave=',
     $                    idayave,imonave,iyerave
        endif
! translate idayave, imonave, iyerave to adateave:
        adateave = '00/00/00'
        jpos = 2
        if(idayave.gt.9) jpos = 1
        CALL int2ch(idayave,adateave,jpos,len)
        jpos = 5
        if(imonave.gt.9) jpos = 4
        CALL int2ch(imonave,adateave,jpos,len)
        jpos = 8
        if(iyerave.gt.2000) iiyerave = iyerave - 2000
        if(iiyerave.gt.9) jpos = 7
        CALL int2ch(iiyerave,adateave,jpos,len)
        if(iw.eq.1)write(ifile,*)'adateave=',adateave
! translate istat to astat:
        if(istat.eq.1) then
         astat='NAV'
        else
         astat='UNK'
        endif
        if(iw.eq.1)write(ifile,*)'astat=',astat

! if running on NO GPS then don't write any position/speed/dir values
! to navtrk.dat OR date.nav
        if(igps.ne.1) go to 999     !16sep2014 was 451(duh)

        if(ibuf.ge.5) then
         if(iw.eq.1)write(ifile,*)'ibuf=',ibuf,' call chkbuf:'
         CALL chkbuf(ibuf,clatbuf,clonbuf,ctagbuf,ierr,iw,ifile)
! ierr from chkbuf, ierr=1 bad, ierr=0 good
         if(ierr.eq.1) go to 102
         CALL ave(ibuf,clatbuf,clonbuf,ctagbuf,avlath,avlonh,s,d,
     $            timeave,vlat,vlon,ierror,iderr,iSIOSpeedAveMin,
     $            iw,ifile)
! ierror(12)="change in dead reckoning lat or lon is too big"
!            ierror(12) is set in subroutine ave
         if(ierror(12).eq.1) then
           ierror(35) = 312
         endif
! iderr comes from dpolft. iderr=1 good, iderr=anything-else bad:
         if(iderr.ne.1) go to 102
! put date into adateave if ave was called:
         adateave(1:8) = agpsdate(1:8)
! don't use the speed and dir calculated unless have about a full minute
! of positions:
         if(s.ne.-99.0) speed = s
         if(d.ne.-99.0) dir = d
        endif
!
102     continue
!
! translate timeave to hr:min:sec
         if(iw.eq.1)write(ifile,*)'call time,timeave=',timeave
         CALL timetohms(timeave,ihr,imin,isec)
! write updated averaged position/speed/dir to navtrk.dat:
! FIX figure out what to do here:
! change this from checking only speed/dir TO check position/speed/dir!
! 16sep2014, chkall only checks speed and dir, do I want to check position???
         if(iw.eq.1)write(ifile,*)'call chkall',vlat,vlon,speed,dir
         CALL chkall(vlat,vlon,speed,dir,ierrwrite)
!
! 10nov2004 Ll somehow after a dayroll I wrote the current date with
! the previous days time to navtrk.dat.   This really messed with the
! program!   I must check date and time of what is being written to
! I guess both navtrk.dat AND *.nav file.   How to do.   How about as
! kludge fix DO NOT write either if I'm trying to write something with 
! a time that is, say, 30 seconds into the future?   Since we're talking
! the current time is 00:00:30 (or similar) and I'm trying to write
! last nights 23:59:00.   What will this break....
! SO a timeave = 86371=23:59:31 and if dtime is low, say 100=00:01:40
! then we have a problem
!
! ADD FIX TO CORRECT DATE FOR THIS CASE!
!
! dtime: dos time in seconds, get dos time (gettim uses i*2):
         CALL gettim(j1,j2,j3,j4)
         idhr = int(j1)
         idmin = int(j2)
         idsec = int(j3)
         dtime = float(idhr*3600 + idmin*60 + idsec)
!
! debugging:
         CALL getdat(j1,j2,j3)
         if(iw.eq.1)write(ifile,582)j3,j2,j1,idhr,idmin,idsec
582      format(' pc ',i2,'/',i2,'/',i4,1x,i2,':',i2,':',i2)
!
         if(dtime.le.1000.0.and.timeave.ge.85000.0) then
           if(iw.eq.1)write(ifile,*) 'sioend, dtime=',dtime,
     $                               ' timeave=',timeave
           ierrwrite=1    
         endif
! ierrwrite is either set in routine chkall, so ierrwrite=1 bad,
!    and ierrwrite=0 is good
!    The if stmt above can also set ierrwrite, if timeave is
!    yesterday and dostime is today, then do not write it.
         if(ierrwrite.eq.1) then
          ierror(28) = 1
          if(iw.eq.1) then
           write(ifile,*) 'Speed and/or direction values screwed up!'
           write(ifile,*) 'Not writing updated values to navtrk.dat!'
           write(ifile,*) 'OR <date>.nav file!'
          endif
          goto 999             !16sep2014 was 451(duh)
         endif
!
         if(iw.eq.1)write(ifile,*)'open navtrk.dat 1'
!
         open(15,file=anavtrk,form='formatted',status='unknown',
     $       err=161,iostat=ios)
         if(iw.eq.1)write(ifile,*)'open15ios=',ios
         rewind(15,iostat=ios)
         if(iw.eq.1)write(ifile,*)'rewind15ios=',ios
!
         write(15,509,err=162)adateave,ihr,imin,isec,vlat,vlon,speed,dir
509      format(a8,' ',i2,':',i2,':',i2,' ',f7.3,f8.3,f6.2,f7.2)
!
        close(15,iostat=ios)
        if(iw.eq.1)write(ifile,*)'close15ios=',ios
!
        go to 170   ! all is well, skip errors
!
161     ierror(23) = 1    ! error opening navtrk.dat
        if(iw.eq.1)write(ifile,*)
     $           'error opening navtrk.dat, will try to write date.nav'
        go to 170 
!
162     ierror(14) = 1    ! error writing navtrk.dat
        if(iw.eq.1)write(ifile,*)
     $           'error writing navtrk.dat, will try to write date.nav'
        close(15,iostat=ios)
        if(iw.eq.1)write(ifile,*)'close15ios=',ios
!
170     continue
!
! write date, gps time, gps position timetag ave, lat ave, long ave,
! OEM unit status, gps quality, iequal to date.nav file
! at end of program:
!
        CALL dec2deg('lat',ivlatd,vlatm,avlath,vlat)
        CALL dec2deg('lon',ivlond,vlonm,avlonh,vlon)
! FIX - why were these commented out?
        if(avlath(1:1).eq.' ') then
          if(iw.eq.1)write(ifile,*)'avlath(1:1) bad, no write date.nav'
          go to 999
        endif
!
        if(ibuf.ge.5) then
         open(10,file=afilen,form='formatted',status='unknown',
     $           access='append', err=451,iostat=ios)
          if(iw.eq.1.and.ios.ne.0)write(ifile,*)'open10ios=',ios
          write(10,501,err=180,iostat=ios)adateave(1:8),ihr,imin, isec,
     $                abs(ivlatd), vlatm, avlath, ivlond, vlonm, avlonh,
     $                astat, speed, dir, ibuf
         if(iw.eq.1.and.ios.ne.0)write(ifile,*)'write10ios=',ios
         close(10,iostat=ios)
         if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close10ios=',ios
501      format(a8,' ',i2,':',i2,':',i2,' ',
     $          i3,' ',f7.4,' ',a1,' ',i3,' ',f7.4,' ',
     $          a1,' ',a3,' ',f5.2,' ',f5.1,i3)
        endif    ! endif ibuf.ge.5
        go to 999     ! skip errors, all is well
!
451     ierror(5) = 1    ! error opening date.nav
        ierror(34) = icday         ! pass seas the ddmmyy of date.nav
        ierror(36) = icmon
        ierror(37) = icyear
        if(iw.eq.1)write(ifile,*)'error opening 10:',
     $             afilen(1:len_adir+15)
        go to 999
180     ierror(6) = 1    ! error writing date.nav
        ierror(34) = icday         ! pass seas the ddmmyy of date.nav
        ierror(36) = icmon
        ierror(37) = icyear
        if(iw.eq.1)write(ifile,*)'error writing 10:',
     $                            afilen(1:len_adir+15)
        close(10,iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*)'close10ios=',ios
!
999     continue
!
! If ierror(35) is still = 0 when we get here we think we've
! run thru siobegin successfully, so set it = 2 on exit
        if(ierror(35).eq.0) then
           ierror(35) = 2
        endif

        if(iw.eq.1) then
         write(ifile,*)'errors:'
         do 1010 i = 1, nerr
1010      if(ierror(i).ne.0) write(ifile,*)'ierror(',i,')=',ierror(i)
        endif

! debugging:
         if(iw.eq.1) then
          CALL gettim(j1,j2,j3,j4)
          idhr = int(j1)
          idmin = int(j2)
          idsec = int(j3)
          CALL getdat(j1,j2,j3)
          write(ifile,582)j3,j2,j1,idhr,idmin,idsec
          write(ifile,*)'End sioend'
          call flush(ifile)
         endif
        close(ifile)
! test this - why?? 16sep2014
        ibuf = 0
! make sure all are closed:
        close(10)
        close(15)
        return
        end

!^*************** end sioend ****************************************^

        SUBROUTINE gpspos(ierror,ireturn,ichoosedrop)

! ireturn=0 -> ran ok,  ireturn=1 -> error, check ierror(nerr) codes
        parameter(nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: gpspos

! 21jan2005 - ichoosedrop - let operator choose which drop to 
!        renavigate.   If ichoosedrop .lt. 0 (negative) then 
!        run as "usual" - search for drop that needs to be
!        navigated.

! 07may01 LL - variable length cruise name
! 2-27-98, modify so can run with microsoft fortran.  That means if
! we write to a file, we need to read all that files contents, save,
! and rewrite, since msf is so stupid it only keeps the lines before
! the line we want to change..  Don't put nav position in s file.
! Do not want to risk losing it. LL
!         compile with gpp gpspos.for (\msf5\bin\fl /c /AL /Gt %1)
!         link with lnkmsf gpspos
!#  GPSPOS.FOR   version 3.1
!# find an un-navigated xbt drop by looking at stations.dat
!# navigate using interpolation or extrapolation
!# write station position to stations.dat, and edited file
!# read cruise designator from control.dat
!# altered to look for previous day nav file if present day non-existing
!
        character f1*80, chr58*58, fnav*80,chr35*35, clat*2, clats*2
        character ch4*4, clon*2, clons*2, chr70(999)*70, chr21*21
        character chr9(1000)*9, chr1*1
        integer*4 ierror(nerr), ireturn, ichoosedrop
        integer*2 j1,j2,j3,j4
! rdcntrl:
        integer*4 len_acruise
        real*4 xmaxspd, deadmin, dropmin, relodmin
        real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
        real*4 tm_pl_mx, tm_pl_mn
        real*4 yrdrop
        real*4 ylat,ylon,yrsav,zlat,zlon,yrnav,xlat,xlon
        character*7 acruise
        character*80 adir, agpspos*80, astations*80, acontrol*80
!
        integer*4 iw, ifile, ios
        integer*4 iprofcnt
        integer*4 iyr, imo, iday, ihr,imin,isec,len_adir
        integer*4 igderr(3)
        integer*4 launcher(12)
!
! file numbers opened and closed in gpspos:
!        7=stations.dat (read and write)
!        8=<date>.nav file (read only, could be 3 diff nav files)
!        9=sio e file (if exist, read and write) not important
!       22=control.dat (sub rdcntrl) (read)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=gpspos.log
!         
! ierrors set in here:  looks like I'm clearing all at begin
!( 2) - gpspos - error reading today's <date>.nav file
!( 3) - gpspos - end of file in stations.dat - no profile to navigate
!( 4) - gpspos - values calc'd are bad -manually edit <date>.nav file (del lines)
!( 5) - gpspos - cannot open (iday,imo,iyr) .nav file
!( 7) - error opening siodir.txt
!(15) - error opening control.dat
!(16) - error reading control.dat
!(17) - error reading siodir.txt
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(29) - error writing stations.dat, wrxmit, gpspos
!(34) - integer of day (dd) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(36) - integer of month (mm) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(37) - integer of year (yy) of <date>.nav file if ierror(2 or 5 or 6) set (ddmmyy.nav)
!(44) - ALL    - error opening calling routine's .log file (eg gpspos.log)
!(45) - ALL    - error writing calling routine's .log file (eg gpspos.log)
!
        f1 = '            '
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
        do 10 i = 1, nerr
           ierror(i) = 0
10      continue

! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open or read file 'siodir', assume current directory?
! 12sep2014 no, put in proper error exit:
! if I can't open(7)  or read(17) file siodir.txt containing 'xbtdirectory',
!   then exit out
        if(ierror(7).eq.1) then
           len_adir = 0
! do not set yet, is this critical?           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0 
! do not set yet, is this critical?           ierror(35) = 317
           go to 999
        endif 
!
! initialize all path strings to spaces to prevent garbage chars in filenames
        agpspos   = ' '
        astations = ' '
        acontrol  = ' '
        f1        = ' '

        if(len_adir.gt.0) then
           agpspos(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
           acontrol(1:len_adir) = adir(1:len_adir)
           f1(1:len_adir) = adir(1:len_adir)
        endif
! add Data\ to data files paths!!!
        f1(len_adir+1:len_adir+5) = 'Data\'
        len_f1 = len_adir + 5

        agpspos(len_adir+1:len_adir+15) = 'Data\gpspos.log'
        acontrol(len_adir+1:len_adir+16) = 'Data\control.dat'
        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'

        ireturn = 0
        deg2rad = 3.141592654/180.0
        iprofcnt = 0
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=agpspos,status='unknown',
     $             form='formatted',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
           write(ifile,*,err=335) 'BEGIN GPSPOS'
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif

        if(iw.eq.1) then
         write(ifile,*)'astations=',astations(1:len_adir+17)
         write(ifile,*)'ichoosedrop=',ichoosedrop
         call flush(ifile)
        endif

! get cruise designator from control.dat
! recall rdcntrl clear/set ierror: 14,15,16,33
        CALL rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $         deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $         dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $         ichkprofdepth)
!
        if(ierror(33).eq.6) then                !  this is "debug"
           ierrlev = 6
        endif
!
        if(ierror(15).ne.0) then                ! error opening control.dat
! do not use yet:              ierror(35) = 315
              go to 999
        elseif(ierror(16).ne.0) then            ! error reading control.dat
! do not use yet:              ierror(35) = 316
              go to 999
        endif

! new:
! don't forget added \Data\ above...
        f1(len_f1+1:len_f1+len_acruise) = acruise
        len_f1 = len_f1 + len_acruise

        write(f1(len_f1+1:len_f1+5),'(a5)') 's.000'

1        open(7,file=astations,status='old',err=107)
        if(ichoosedrop.gt.0) then
! op choose a drop to navigate:
! skip ichoosedrop-1 lines in stations.dat:
           do 3 kk = 1, ichoosedrop-1
3           read(7,*,end=101,err=108)

! read in drop op said to renavigate:
           read(7,'(1x,a3,a58,i6,1x,a1)',end=101,err=108)
     $      f1(len_f1+3:len_f1+5), chr58,inav,chr1
           iline = ichoosedrop
        else
! default mode - LOOK in stations.dat for drop to navigate:

! find first un-navigated drop (10th col in stations.dat = 0 is unnavigated)
         iline = 0
         do 410 i=1,1000
           read(7,'(a4)',err=108)ch4
!           if(iw.eq.1)write(ifile,*)'reading ', ch4
! exit with error if do not navigate any profiles
           if(iprofcnt.eq.0.and.ch4.eq.'ENDD')go to 101
! exit normally if have navigated at least one profile before reaching ENDDATA
           if(iprofcnt.ne.0.and.ch4.eq.'ENDD')go to 999
           backspace(7)
           read(7,'(1x,a3,a58,i6,1x,a1)',end=101,err=108)
     $      f1(len_f1+3:len_f1+5), chr58,inav,chr1
           iline = iline + 1

           if(inav.eq.0)go to 11          ! found one to navigate
410         continue
11         continue

        endif

        if(iw.eq.1) then
         write(ifile,*) 'Renavigating:'
         write(ifile,*) f1(len_f1+3:len_f1+5),chr58,inav,chr1
         write(ifile,*) ' iline=',iline
         call flush(ifile)
        endif

! iline is line # in stations.dat we are currently modifying
! iline2 is # of lines in stations.dat past iline, (minus ENDDATA)
! read in rest of stations.dat and save (msf5 change)
        iline2 = 0
        do 30 i = 1, 1000
           read(7,'(a4)',err=108)ch4
!           if(iw.eq.1)write(ifile,*)'iline2 reading ', ch4
           if(ch4.eq.'ENDD') go to 31
           backspace(7)
           iline2 = iline2 + 1
! CHECK - where do I really want to go if error here?
           read(7,'(a70)',err=31) chr70(iline2)
30        continue
31        continue
        close(7)
! 
! translate date and time to years after start of 1987
! iyr is 2 digit year:
        read(chr58,'(14x,6(i2,1x))',err=108)iday,imo,iyr,ihr,imin,isec
! yrdrop is time of drop position (in years):
        CALL yrdy(iyr,imo,iday,ihr,imin,isec,yrdrop)
        if(iw.eq.1)then
           write(ifile,*)iday,imo,iyr,ihr,imin,isec,' yrdrop= ',yrdrop
           call flush(ifile)
        endif
! now open appropriate .nav file(s)
! Find a fix before and after the drop for interpolation
! If no fix after, find fix before and extrapolate using spd and dir
! Open same day nav file as drop:
        CALL navopen(iday,imo,iyr,ierr,fnav,adir,len_adir)
        if(iw.eq.1) then
           write(ifile,*)'same day nav is=',fnav(1:len_adir+15)
           write(ifile,*)'ierr=',ierr
           if(ierr.eq.0) then
              write(ifile,*)'and it appears to exist with some data'
           elseif(ierr.eq.1) then
              write(ifile,*)'Could not open it, trying yesterdays nav'
           elseif(ierr.eq.2) then
              write(ifile,*)'Reached end of it, error, try yesterday'
           elseif(ierr.eq.3) then
              write(ifile,*)'Error reading it, try yesterday'
           endif
           call flush(ifile)
        endif
! if error opening same day nav file, then
! use nav file from previous day and extrapolate using spd and dir
        if(ierr.ge.1) then
! figure out what previous day is:
           iday=iday-1
           if(iday.eq.0)then
              imo=imo-1
              if(imo.eq.0)then
                 iyr=iyr-1
                 imo=12
              endif
              iday=31
              if(imo.eq.4.or.imo.eq.6.or.imo.eq.9.or.imo.eq.11)iday=30
              if(imo.eq.2.and.mod(iyr,4).ne.0)iday=28
              if(imo.eq.2.and.mod(iyr,4).eq.0)iday=29
           endif
! open previous day nav file:
! 24jul2014 why are you calling navopen twice here???? REMOVE second call!
           CALL navopen(iday,imo,iyr,ierr,fnav,adir,len_adir)
           if(iw.eq.1) then
              write(ifile,*)'yesterday nav is=',fnav(1:len_adir+15)
              write(ifile,*)'ierr=',ierr
              if(ierr.eq.0) then
                 write(ifile,*)'and it appears to exist with some data'
              elseif(ierr.eq.1) then
                 write(ifile,*)'Could not open yesterdays nav'
              elseif(ierr.eq.2) then
                 write(ifile,*)'Reached end of yesterdays nav'
              elseif(ierr.eq.3) then
                 write(ifile,*)'Error reading yesterday'
              endif
              call flush(ifile)
           endif
           if(ierr.ge.1) go to 111               ! can't open today or yesterday, exit
!
! if we are here, could not open today nav, able to open yesteray nav, read yesterday:
           do 125 j=1,10000
              read(8,500,end=126,err=109)kday,kmo,kyr,khr,kmin,ksec,
     $             klat,zltm,clats,klon,zlnm,clons,spd,dir,nfix
500           format(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)
125        continue
126        continue
           close(8)
!
! yrsav is last position in previous day nav file:
           CALL yrdy(kyr,kmo,kday,khr,kmin,ksec,yrsav)
           go to 29           ! go to extrapolation
        endif                 ! endif for ierr=1 (using nav file from previous day)
!v---------------------
!25jul2014 stop using chknav
!NEVER WORKED:      CALL chknav(iday,imo,iyr,ierr,fnav,len_adir,adir,iw,ifile)
!NEVER WORKED:      if(ierr.eq.1) go to 112
!
! If we are here, success opening today nav file:
! Read same day nav file as drop (yrdrop)
        do 20 i=1,10000
           read(8,500,end=21,err=109)jday,jmo,jyr,jhr,jmin,jsec,
     $       jlat,xltm,clat,jlon,xlnm,clon,spd,dir,nfix
           CALL yrdy(jyr,jmo,jday,jhr,jmin,jsec,yrnav)
           if(iw.eq.1.and.ierrlev.eq.6) then
              write(ifile,*)'today.nav:yr,mo,dy',jyr,jmo,jday,
     $                                           jhr,jmin,jsec,
     $        ' yrnav = ',yrnav
              call flush(ifile)
           endif
! found drop position between 2 same day nav file positions:
           if(i.gt.1.and.yrsav.le.yrdrop.and.yrnav.gt.yrdrop)then
! position past drop:
        	xlat=real(jlat)+xltm/60.0
        	if(clat.eq.' S')xlat=-1.*xlat
        	xlon=real(jlon)+xlnm/60.0
                if(clon.eq.' W') xlon = 360.0 - xlon
! position before drop:
        	zlat=real(klat)+zltm/60.0
        	if(clats.eq.' S')zlat=-1.*zlat
        	zlon=real(klon)+zlnm/60.0
                if(clons.eq.' W') zlon = 360.0 - zlon
                CALL interp(yrdrop,ylat,ylon,yrsav,zlat,zlon,
     $                      yrnav,xlat,xlon,iw,ifile)
        	tnav=(yrnav-yrsav)*1440.0
        	tnav=amax1(tnav,1.0)
                close(8)
        	go to 102    ! go to write stations.dat
           endif
! if we are here, we are still in today's nav file, still looking
!  for surrounding drops:
           yrsav=yrnav
           klat=jlat
           zltm=xltm
           clats=clat
           klon=jlon
           zlnm=xlnm
           clons=clon
! if first fix occurs after drop, need previous day
!v------begin open prev day if first pos in current day past drop--------
           if(i.eq.1.and.yrnav.gt.yrdrop)then
              iday=iday-1
              if(iday.eq.0)then
                 imo=imo-1
                 if(imo.eq.0)then
                    iyr=iyr-1
                    imo=12
                 endif
                 iday=31
                 if(imo.eq.4.or.imo.eq.6.or.imo.eq.9.or.imo.eq.11)iday=30
                 if(imo.eq.2.and.mod(iyr,4).ne.0)iday=28
                 if(imo.eq.2.and.mod(iyr,4).eq.0)iday=29
              endif		
              close(8)
              if(iw.eq.1) then
                 write(ifile,*)'First fix in today nav is after drop,
     $ try opening previous day nav file:'
              endif
              CALL navopen(iday,imo,iyr,ierr,fnav,adir,len_adir)
              if(iw.eq.1)write(ifile,*)'fnav=',fnav(1:len_adir+15)
              if(ierr.ge.1) then
                 write(ifile,*)'Prev day nav does not exist, exiting,
     $ no navigation done'
                 go to 111
              endif
!NEVER WORKED       CALL chknav(iday,imo,iyr,ierr,fnav,len_adir,adir,iw,ifile)
!NEVER WORKED       if(ierr.eq.1) go to 112
              do 25 j=1,10000
                 read(8,500,end=26,err=109)kday,kmo,kyr,khr,kmin,ksec,
     $                  klat,zltm,clats,klon,zlnm,clon,spd,dir,nfix
25              continue
26              CALL yrdy(kyr,kmo,kday,khr,kmin,ksec,yrsav)
              close(8)
! position from same day nav file that is later than drop position:
              xlat=real(jlat)+xltm/60.0
              if(clat.eq.' S')xlat=-1.*xlat
              xlon=real(jlon)+xlnm/60.0
              if(clon.eq.' W') xlon = 360.0 - xlon
! position from prev day nav file that is before drop positions:
              zlat=real(klat)+zltm/60.0
              if(clats.eq.' S')zlat=-1.*zlat
              zlon=real(klon)+zlnm/60.0
              if(clons.eq.' W') zlon = 360.0 - zlon
              CALL interp(yrdrop,ylat,ylon,yrsav,zlat,zlon,yrnav,
     $                    xlat,xlon,iw,ifile)
              tnav=(yrnav-yrsav)*1440.0
              tnav=amax1(tnav,1.0)
              go to 102     ! go to write stations.dat
           endif
!^------end open prev day if first pos in current day past drop--------
20        continue      ! continue reading current day nav file
!
! if last fix occurs before drop, need next day or extrapolation
21      continue           !last fix in current day nav is before drop
        yrsav=yrnav
        klat=jlat
        zltm=xltm
        clats=clat
        klon=jlon
        zlnm=xlnm
        clons=clon
! calculate next day:
        iday=iday+1
        if(iday.eq.32)go to 27
        if(iday.eq.31.and.imo.eq.4)go to 27
        if(iday.eq.31.and.imo.eq.6)go to 27
        if(iday.eq.31.and.imo.eq.9)go to 27
        if(iday.eq.31.and.imo.eq.11)go to 27
        if(iday.eq.29.and.imo.eq.2.and.mod(iyr,4).ne.0)go to 27
        if(iday.eq.30.and.imo.eq.2.and.mod(iyr,4).eq.0)go to 27
        go to 28
27      iday=1
        imo=imo+1
        if(imo.eq.13)iyr=iyr+1
        if(imo.eq.13)imo=1
28      close(8)
        if(iw.eq.1) then
           write(ifile,*)'Last nav file fix occurs before drop, try
     $opening next day nav file:'
        endif
! open next day nav file:
        CALL navopen(iday,imo,iyr,ierr,fnav,adir,len_adir)
        if(iw.eq.1) then
           write(ifile,*)'fnav=',fnav(1:len_adir+15)
           write(ifile,*)'ierr=',ierr,' Next day nav does not exist'
           call flush(ifile)
        endif
        if(ierr.ge.1)go to 29        ! go to extrapolation 
!
! NEVER WORKED:     CALL chknav(iday,imo,iyr,ierr,fnav,len_adir,adir,iw,ifile)
! NEVER WORKED:        if(ierr.eq.1) go to 112
!
! 25jul2014 LL I changed err=109 to err=29 here: as really an
!  error opening or reading first line of next day nav file, you should
!  just extrapolate using last position in current day nav file
        read(8,500,err=29)jday,jmo,jyr,jhr,jmin,jsec,
     $       jlat,xltm,clat,jlon,xlnm,clon,spd,dir,nfix
        close(8)
!
! position past drop: yrnav, xlat, xlon   (using jlat,xltm,jlon,xlnm)
! FIX I think this yrnav should be yrsav?:
        CALL yrdy(jyr,jmo,jday,jhr,jmin,jsec,yrnav)
        xlat=real(jlat)+xltm/60.0
        if(clat.eq.' S')xlat=-1.*xlat
        xlon=real(jlon)+xlnm/60.0
        if(clon.eq.' W') xlon = 360.0 - xlon
! position before drop (from same day nav file as drop): zlat, zlon
!                                         (using klat,zltm,klon,zlonm)
        zlat=real(klat)+zltm/60.0
        if(clats.eq.' S')zlat=-1.*zlat
        zlon=real(klon)+zlnm/60.0
        if(clons.eq.' W') zlon = 360.0 - zlon
        CALL interp(yrdrop,ylat,ylon,yrsav,zlat,zlon,yrnav,
     $              xlat,xlon,iw,ifile)
        tnav=(yrnav-yrsav)*1440.0
        tnav=amax1(tnav,1.0)
        go to 102                 ! write to stations/etc
!
! need to extrapolate
29      continue
        if(iw.eq.1)write(ifile,*)'Extrapolate from last know fix'
        close(8)
        tnav=-1.*(yrdrop-yrsav)*1440.0
        tnav=amin1(tnav,-1.0)
        tnavh=-1.*tnav/60.0
        zlat=real(klat)+zltm/60.0
        if(clats.eq.' S')zlat=-1.*zlat
        zlon=real(klon)+zlnm/60.0
        if(clons.eq.' W') zlon = 360.0 - zlon
        ylat=zlat+tnavh*spd*cos(dir*deg2rad)/60.0
        ylon=zlon+tnavh*spd*sin(dir*deg2rad)/
     $      (cos(ylat*deg2rad)*60.0)
!
        if(iw.eq.1) then
           if(ierrlev.eq.6)write(ifile,*) cos(ylat*deg2rad)
           if(ierrlev.eq.6)write(ifile,*) sin(dir*deg2rad)
           if(ierrlev.eq.6)write(ifile,*) zlon, tnavh, spd
           write(ifile,*)' first fix: time(days) lat, lon, speed,
     $  direction '
           write(ifile,'(3f15.5,2f12.2)')yrsav,zlat,zlon,spd,dir
           write(ifile,*)' dr time(days), elapsed time(hrs), lat, lon '
           write(ifile,'(4f15.5)')yrdrop,tnavh,ylat,ylon
           call flush(ifile)
        endif
!
! Here begins the end... write to stations.dat:
102     continue
        clat=' N'
        if(ylat.lt.0.0)clat=' S'
        clats=' E'
        ylata=abs(ylat)
        ylatd=real(int(ylata))
        ylatm=(ylata-ylatd)*60.0
! 08aug2005 LL - if ylon gt 360 change to 0 E ! 
        if(ylon.gt.360.0) then
           ylon = ylon - 360.0
        endif
        ylond=real(int(ylon))
        ylonm=(abs(ylon)-abs(ylond))*60.0
        if(iw.eq.1) then
           write(ifile,'(a17,i6,2(f5.0,f6.2,a2))')' tnav, lat, lon: ',
     $            nint(tnav),ylatd,ylatm,clat,ylond,ylonm,clats
           call flush(ifile)
        endif
        write(chr58(32:40),'(f9.3)')ylat
        write(chr58(41:49),'(f9.3)')ylon
        CALL chkwrite(ylat,ylon,ierr)
        if(iw.eq.1.and.ierrlev.eq.6)write(ifile,*)
     $                             '102 chkwrite ierr=',ierr
        if (ierr.ge.1) go to 110
! check value of tnav to be sure can be written in (i6) format!
! make it i5 format writable...
        itest = nint(tnav)
        if(itest.gt.99999) itest = 99999
        if(itest.lt.-9999) itest = -9999
! 30nov2004 add if itest=0, set it to -9999 - this is because I am 
! now writing Seas2k lat/lon to stations.dat before running gpspos.
! this is to try to keep program running even if gpspos fails.   Probably
! risky but worth a try
        if(itest.eq.0) itest = -9999
!
        if(iw.eq.1)then
           write(ifile,*)'open stations.dat to modify drop: ', iline
           write(ifile,'(1x,a3,a58,i6,1x,a1)',err=129)
     $                   f1(len_f1+3:len_f1+5),chr58,itest,chr1
        endif
! write to stations.dat:
! open, read to iline-1, write new line, write iline2 lines that were saved.
        open(7,file=astations,status='old',err=107)
        do 60 i = 1, iline-1
60        read(7,*,err=108)
! write the new line:
        write(7,'(1x,a3,a58,i6,1x,a1)',err=129)
     $    f1(len_f1+3:len_f1+5),chr58,itest,chr1
! write the iline2 lines that were saved:
        do 61 i = 1, iline2
61        write(7,'(a70)',err=129) chr70(i)
        write(7,'(a7)',err=129)'ENDDATA'
        close(7)
!
! write to S. and E. file(s)
! No longer writing to s file while at sea!!!  2-27-98 msf5 doesn't save
! what's in file after where we are writing.  Too dangerous to read and 
! save.  What if we crash here, then we lose the s file.  LL
!        if(iw.eq.1) write(ifile,*)' open file ',f1
! s file:
!        open(9,file=f1,status='old',err=104)
!        read(9,'(a35)')chr35
!        backspace 9
!        write(chr35(20:35),'(2f8.2)')ylat,ylon
!        write(9,'(a35)')chr35
!        close (9)
! e file:
        f1(len_f1+1:len_f1+1)='E'
        if(iw.eq.1) write(ifile,*)' open file ',f1
        open(9,file=f1,status='old',err=104)
! read whole file and save (msf5 change)
        read(9,'(a35)',end=104,err=104)chr35
        read(9,'(a21)',end=104,err=104)chr21
        do 70 i = 1, 1000
70        read(9,'(a9)',end=71,err=71) chr9(i)
71        continue
        ic = i-1
        close (9)
        write(chr35(20:35),'(2f8.2)')ylat,ylon
! 23jul2014 shouldn't 'old' = 'unknown' if writing to it:
!        open(9,file=f1,status='old',err=104)
        open(9,file=f1,status='unknown',err=104)
! 25jul2014 add err=81, should never happen, but just in case:
        write(9,'(a35)',err=81)chr35
        write(9,'(a21)',err=81)chr21
        do 80 i = 1, ic
80       write(9,'(a9)',err=81) chr9(i)
81      close (9)
        go to 105
! if e file doesn't exist - should be able to continue navigating...
104     continue
        if(iw.eq.1) then
           write(ifile,*)'can not open or read(no big deal)',f1
           call flush(ifile)
        endif
!
105     continue
        iprofcnt = iprofcnt + 1
        if(ichoosedrop.gt.0) go to 999

        go to 1

101     ierror(3) = 1                       ! end of stations.dat, no profile to navigate
        if(iw.eq.1)write(ifile,*)'END OF FILE STATIONS.DAT,
     $  NO PROFILE TO NAVIGATE'
        ireturn = 1
        go to 999
!
107     ierror(25) = 1                      ! error opening stations.dat
        if(iw.eq.1)write(ifile,*)' GPSPOS! ERROR OPENING STATIONS.DAT! '
        ireturn = 1
        go to 999
!
108     ierror(26) = 1                      ! error reading stations.dat
        if(iw.eq.1)write(ifile,*)' GPSPOS! ERROR READING STATIONS.DAT! '
        ireturn = 1
        go to 999
!
109     ierror(2) = 1                       ! error reading nav file
        ierror(34) = iday                   ! pass actual filename in 3 ierrors
        ierror(36) = imo
        ierror(37) = iyr
        if(iw.eq.1)write(ifile,*)' GPSPOS! ERROR READING NAV FILE! '
        ireturn = 1
        go to 999
!
!
110     ierror(4) = 1                       ! values calc'd are bad, manual edit nav file
        if(iw.eq.1) then
           write(ifile,*)'THE VALUES I JUST CALCULATED ARE MESSED UP,'
           write(ifile,*)'EXIT OUT OF MAIN ENTIRELY AND MANUALLY EDIT'
           write(ifile,*)' <DATE>.NAV FILE AND LOOK FOR AND DELETE BAD'
           write(ifile,*)'VALUES'
        endif
        ireturn = 1
        go to 999
!
111     ierror(5) = 1                       ! can not open nav file
        ierror(34) = iday                   ! pass actual filename in 3 ierrors
        ierror(36) = imo
        ierror(37) = iyr
        ireturn = 1
        go to 999
!
129     ierror(29) = 1                      ! error writing stations.dat
        if(iw.eq.1)write(ifile,*)' GPSPOS! ERROR witing stations.dat '
        ireturn = 1
        go to 999
!
999     continue
        close(7)                      ! close stations.dat at end
        close(8)                      ! close nav file at end
        close(9)                      ! sio e file at end
        if(iw.eq.1) then
           write(ifile,*)'END GPSPOS'
           write(ifile,*)'closing stations.dat'
        endif
        close(ifile)                  ! close gpspos.log
        return
        end
!^*****************end gpspos *********************************************^
!
        SUBROUTINE YRDY(KKYR,KMO,KDAY,KHR,KMN,KSC,YRDAY)
! 21jul2014 - this is not callable by seas, so why in sio.for and not siosub.for??
! KKYR is 2 digit year, I think
! converts input integer year, month, day, hour, minute, second to
! yearday. uses an offset for year to minimize precision problems
! remove most of year, for precision
!
        kyr = kkyr
! yr 2000 "fix" or fudge...won't work past 2086! not my prob by then...
        if(kyr.lt.87) kyr = kyr + 100
        nyr = kyr - 97
      YRDAY = (real(nyr))*365.0
      FDAY = real(KMO-1)*31.0 + real(KDAY) + real(KHR)/24.0
     *  + (real(KMN) + real(KSC)/60. )/(60.*24.)
! take account of months with 30 days:
      IF (KMO .GT. 4) FDAY = FDAY - 1.0
      IF (KMO .GT. 6) FDAY = FDAY - 1.0
      IF (KMO .GT. 9) FDAY = FDAY - 1.0
      IF (KMO .GT. 11) FDAY = FDAY - 1.0
! TAKE ACCOUNT OF LEAP YEAR
! subtract 2 days for each feb in leap year, 3 days for each feb in nonleap year
! 24 tied to 1997 above, just TRY and follow this...:
        nleap = int( real(kyr)/4.0 ) - 24
        if (kmo .gt. 2) then
           fday = fday - 
     $         3.*(real(nyr) + real(nleap) - real(2.*nleap) - 1.)
     $          - real(nleap*2)
        else
           if(mod(kyr,4).eq.0) then
              fday = fday - 
     $         3.*(real(nyr) + real(nleap) - real(2.*nleap) - 1.)
     $          - real((nleap-1)*2)
           else
              fday = fday - 
     $         3.*(real(nyr) + real(nleap) - real(2.*nleap))
     $          - real(nleap*2)

           endif
        ENDIF
!
      YRDAY = YRDAY + FDAY
      RETURN
      END
        
!^*****************end yrdy ***************************************************^

        SUBROUTINE chkprof(ierror,ireturn,ichoosedrop)
        
        parameter(nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: chkprof
!06nov2014- ok, so if f2 (profile we are checking) is less than ichkprofdepth
! (depth to check profile TO, usually 700m) then I exit out and say chkprof
! not run "for other reasons", but I'm not putting a code into the ichk col!
! So next drop rechecks the same short drop (duh). Obviously this program 
! needs a major rewrite (think mark profs good, but may only be good to 600m)
! and think "use the 'e' if it exists, so can see qc codes" but that's in
! the future. For NOW we need to put a code in ichk col if f2-test prof is
! too shallow.
!
!19aug2014 begin iw/ifile improvements, add closing chkprof.log and
!  opening chkprof.XXX if have a profile to check.
!  TODO - open/read/write error handle!!! NEEDS WORK!!!
!       - think about comparison profile - if it's marked as good, but
!         only good to, say, 500m, I'm still using it cuz all data in s file!
! NOTES- Uses only 's' files, and now our s file contains all data, good&bad.
!      - Once operator sets a good profile (iedt=1) that becomes reference.
!      - Next drop after op sets reference - if it passes chkprof - it becomes
!        reference, and so on.
!      - Recall only testing bwteen ~200-~700m (look in code to verify)
!
! 21jan2005 - add ichoosedrop so can run this program from a seas/gauge
! pulldown menu!  But still want it to function as the normal subroutine
! CALL as before.

! 19mar2003 LL - per JB - some probes go below 999 m.   Told her to
! keep our i3 format and write anything above 999 without first digit,
! so 1001 would be written at 001 (below 999, of course).   
! This doesn't actually impact this particular program since we're only
! going down to 720 m.   But for the future...

!c 20feb03 LL changed to ireturn=0,1,2 for subroutine purposes
! stop 0 all ok
! stop 1 failed chkprof
! stop 2 couldn't run chkprof for other reasons
!
! 07may01 LL - variable length cruise name
! 01mar01 LL - do NOT exit w/1(error) if no previous profile
! for comparison (DC&KN req)
! 22jan01 LL - simplify error messages to screen.  Keep lengthy
! error messages to output chkprof.log
! 21nov00 LL 
! 01jun00 LL add dtmx700 for Dave.  
! 8-31-98 at code "-3" to chkprof which means "no profile for 
!         comparison.  LL
! 5-20-98 check both chkprof AND edit column to determine last good drop-LL
! 3-11-98 - mod for msf5-ll
! modified 4-23-97 - so write correct profile name on screen -
! program worked correctly previously though - ll
! compile with msf5 chkprof.for
! link with: lnkmsf chkprof 
!# chkprof.for version 3.1
!# test program compares 'present' drop with 'last good' drop to determine
!# if present drop is good. drops are subjected to 4 tests, using only
!# points from 200 to 700m:
!# tdzmx - maximum vertical displacement relative to last good drop
!# tdzrms - maximum rms displacement relative to last good drop
!# dtdzmn - minimum acceptable value of dtdz
!# dtdzth - minimum value of dtdz to be used in displacement calculation
!# dtmx - maximum temperature difference with respect to last good drop
!# dtmx700 - maximum temperature difference with respect to last good drop
!#        at 700 m depth only!, ok, 699.
!# all of these parameters are found on line 5 of control.dat
!INPUT from SEAS:
!        ichoosedrop = if < 0 (use -99)then auto look for unnavigated drop (SIO CALLS)
!                    = if = a drop number, rerun chkprof onthat particular drop-
!                        this is a gauge dropdown option (I think)
!OUTPUT - the usual ierror(50) array
!        ireturn= 0 all ok (profile passed chkprof)
!        ireturn= 1 profile failed chkprof
!        ireturn= 2 couldn't run chkprof for other reasons
!
! file numbers opened and closed in chkprof:
!        7=stations.dat, (read and write)
!       22=control.dat (sub rdcntrl) (read)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=chkprof.log (write) (overwritten each time chkprof run)
!       33=chkprof.XXX (write) (hopefully a log of what chkprof found on each drop XXX)
!
! ierrors set in here:  looks like I'm clearing all at begin
!( 7) - error opening siodir.txt
!(15) - error opening control.dat
!(16) - error reading control.dat
!(17) - error reading siodir.txt
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(29) - error writing stations.dat
!(31) - no profile for comparison, put -3 in col 57 of stations.dat 
!        - remind operator to run wexbt once in deep water (>700m)!
!        - do NOT sound alarm (do not scare officers)
!(33) - if operator name = "debug" turn on ierrlev=6 (fill .log files)
!(44) - ALL    - error opening calling routine's .log file (eg chkprof.log)
!(45) - ALL    - error writing calling routine's .log file (eg chkprof.log)
!
        parameter (nzmx=999)

        integer*4 ierror(nerr), ireturn, ichoosedrop
        dimension avt(nzmx),rft(nzmx)
        character f1*80, f2*80, chr3*3, chr49*49, chr8*8,chr4*4
        character*70 chr70(nzmx)
        integer*2 j1,j2,j3,j4
        integer*4 iw, ifile
! rdcntrl:
        integer*4 len_acruise
        real*4 xmaxspd, deadmin, dropmin, relodmin
        real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
        real*4 tm_pl_mx, tm_pl_mn
        character*7 acruise

        character adir*80, achkprof*80, astations*80
        character acontrol*80
        integer*4 igderr(3)
        integer*4 launcher(12)
        integer*4 ichkprof, i, ichk, iedt, npts, nptsrms

! tdzmx is maximum allowed displacement
! tdzrms is maximum allowed rms displacement
! dtdzmn = min dtdz to allow
! dtdzth = min dtdz for which to calc displacement
! dtmx,= max  absolute temperature difference from ref prof
        data tdzmx/80./,tdzrms/40./,dtdzmn/-5.e-4/
        data dtdzth/2.e-4/,dtmx/2.0/
        data dtmx700/2.0/
! 21jan2005 addition:
        data itestdepth/720/

        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! blank out filenames:
        do 5 i = 1, 80
           f1(i:i) = ' '
5          f2(i:i) = ' '

        ireturn = 0
! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
! (siodir.txt)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can not open or read file 'siodir.txt'
        if(ierror(7).eq.1) then
           len_adir = 0
! do not do in here:           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
! do not do in here:           ierror(35) = 317
           go to 999
        endif
!
        achkprof  = ' '
        astations = ' '
        acontrol  = ' '
        f1        = ' '
        if(len_adir.gt.0) then
           achkprof(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
           acontrol(1:len_adir) = adir(1:len_adir)
           f1(1:len_adir) = adir(1:len_adir)
        endif
! add Data\ to data files paths!!!
        f1(len_adir+1:len_adir+5) = 'Data\'
        len_f1 = len_adir + 5

        achkprof(len_adir+1:len_adir+16) = 'Data\chkprof.log'
        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
        acontrol(len_adir+1:len_adir+16) = 'Data\control.dat'

! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=achkprof,status='unknown',
     $             form='formatted',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
           write(ifile,*,err=335) 'BEGIN chkprof'
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           go to 336
335        ierror(45) = 1        ! error write log file, do not write
           iw = 0
336        continue
        endif
!
        if(iw.eq.1) then
         write(ifile,*) 'NOTE: chkprof only compares ~200m~700m'
         write(ifile,*) '      chkprof excludes the surface layer'
         write(ifile,*) 'achkprof=',achkprof(1:len_adir+16)
         write(ifile,*) 'astations=',astations(1:len_adir+17)
         write(ifile,*) 'Incoming ichoosedrop=',ichoosedrop
        endif
! 21jan2005:
        if(ichoosedrop.le.0) then
           ichoosedrop = 1000
        endif
! have added dtdznm =min dtdz to accept to control.dat 
! also dtdzth = min dtdz for which to make displacement calcs

        CALL rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $         deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $         dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $         ichkprofdepth)
!
        if(ierror(15).ne.0) then                ! error opening control.dat
! do not do in here:              ierror(35) = 315
              go to 999
        elseif(ierror(16).ne.0) then            ! error reading control.dat
! do not do in here:              ierror(35) = 316
              go to 999
        endif
!21sep2014 add :
        if(ierror(33).eq.6) then                !  this is "debug"
           ierrlev = 6
        endif

        itestdepth = ichkprofdepth
!
        if(iw.eq.1) then
         write(ifile,*)'back from rdcntrl: ', acruise(1:len_acruise)
         write(ifile,*)'itestdepth=', itestdepth
        endif
! if op sets icheckprof depth on control.dat < 250m, do not run
! 105 sets ireturn=2 and exits:
        if(itestdepth.lt.250) go to 105
!
! iread is 1/2 of itestdepth, because our s file is every 2 m per line
! itest, not sure why I subtract 10 on itest... it'll come to me...
        iread = itestdepth/2
        itest = iread - 10
!        if(iw.eq.1) then
!        write(ifile,*)'iread=',iread
!        write(ifile,*)'itest=',itest
!        endif

! don't forget added Data\ above...
        f1(len_f1+1:len_f1+len_acruise) = acruise(1:len_acruise)
        len_f1 = len_f1 + len_acruise
! here is where we set it as 's' file:
        write(f1(len_f1+1:len_f1+5),'(a5)') 's.000'
        f2(1:len_f1+5)=f1(1:len_f1+5)
!
        open(unit=7,file=astations,status='old',err=112)
        if(iw.eq.1) write(ifile,*)'Success opening stations.dat...'
! iline is counter of lines in stations.dat
        iline = 0
! ichk = chkprof code
! iedt = edit code
! 21jan2005 change the  outer limit of 1000 to ichoosedrop,
!        if seas sends in ichoosedrop=-99 then reset to 1000
!        and run as normal
! old:        do 10 i=1,1000
! v------------read each line of stations.dat looking for reference and current prof---v
        do 10 i=1,ichoosedrop
           read(7,'(a4)',end=113,err=113)chr4
           if(chr4.eq.'ENDD')go to 111
           backspace(7)
           read(7,547,end=11,err=113) chr3,chr49,ichk,iedt,chr8
!           if(iw.eq.1)write(ifile,547) chr3,chr49,ichk,iedt,chr8
547        format(1x,a3,a49,i4,i5,a8)
           iline = iline + 1
! DO NOT use if iedt=-1!
! NOTE here that if you have an edited profile where edt=1 and it's
! lt 700 m we have a problem.
! search for last "checked" profile (ichk=1).or.(ichk=-1 && iedt=1)
! .or.(ichk=-3 && iedt=1)
! if(ichk=-3 && iedt=0) let's CALL it "no profile for comparison".
! code for ichoosedrop:
           if(i.eq.ichoosedrop) then
              f2(len_f1+3:len_f1+5)=chr3(1:3)
              go to 11
           endif
!
           if(iedt.eq.-1) go to 10             ! skip profile if op edit and mark bad
!
           if((ichk.eq.1).or.                   ! ichk=1 =passed chkprof
     $        (ichk.eq.-1.and.iedt.eq.1).or.    ! ichk=-1=fail chkprof, iedt=1=op mark good
     $        (ichk.eq.-3.and.iedt.eq.1) ) then ! ichk=-3=chkprof no run, iedt=1=op mark good
              f1(len_f1+3:len_f1+5)=chr3(1:3)
! search for current profile that needs checking: (ichk=0)
           elseif(ichk.eq.0) then
              f2(len_f1+3:len_f1+5)=chr3(1:3)
              go to 11
           endif
10        continue
11        continue
! ^------------end read of stations.dat looking for reference and current prof-------^
!    Note stations.dat is open here and just read in current profile
!
! v ------------If NO previous profile for comparison:-------------------------------v
        if(f1(len_f1+3:len_f1+5).eq.'000')then
          ierror(31) = 1   ! no profile for comparison, chkprof not run
          if(iw.eq.1) then
           write(ifile,*)'No previous profile for comparison, write -3'
           write(ifile,*)'  to stations.dat chkprof column  (col8)    '
          endif
!   check for more lines in stations.dat and save:
           iline2 = 0
           do 130 i = 1, 1000
              read(7,'(a4)',err=113)chr4
              if(chr4.eq.'ENDD') go to 131
              backspace(7)
              iline2 = iline2 + 1
              read(7,'(a70)',err=113) chr70(iline2)
130           continue
131           continue
           close(7)
! write a -3 to stations.dat:
! open, read to iline-1, write new line, write iline2 line that were saved after.
          CALL gettim(j1,j2,j3,j4)                               !debug only
          if (iw.eq.1)write(ifile,*)
     $          'dos date before writing stations.dat:',j1,j2,j3,j4
! v-------------write -3 to stations.dat-------------------------------------------------v
           ichkprof = -3
           open(7,file=astations,status='old',err=112)
           do 260 i = 1, iline-1
260        read(7,*,err=113)                 !read lines before profile to write
           write(7,547,err=114)chr3,chr49,ichkprof,iedt,chr8  !write currect profile info
           do 261 i = 1, iline2      
261           write(7,'(a70)',err=114) chr70(i)            !write lines after current prof
           write(7,'(a7)',err=114)'ENDDATA'
           close(7)
! ^-------------end write -3 to stations.dat---------------------------------------------^
!
           if(iw.eq.1) then
           write(ifile,*)'no profile for comparison, comparison profile'
           write(ifile,*)'identified by 1 in col 8 (57) of stations.dat'
           write(ifile,*)'I put a -3 in col 8 (57) for this drop, once '
           write(ifile,*)'you get to deep water be sure to run exbt!'
           write(ifile,*)'(Main Menu, option 12)'
           endif
! do not error out and ring alarm here (don't scare the officers!)
           go to 999
        endif
! ^ ------------end if NO previous profile for comparison:------------------------------^
!
! If NO profile identified by a "0" in col 8 (57) of stations.dat, exit
        if(f2(len_f1+3:len_f1+5).eq.'000')go to 107
!
! read in rest of stations.dat (past current drop) and save (msf5 change)
! iline is line # in stations.dat we are currently modifying
! iline2 is # of lines in stations.dat past iline, (minus ENDDATA)
        iline2 = 0
        do 30 i = 1, 1000
           read(7,'(a4)',err=113,end=113)chr4                !added err&end 20aug2014
           if(chr4.eq.'ENDD') go to 31
           backspace(7)
           iline2 = iline2 + 1
!CHECK - where do I really want to go if error here?         !20aug2014 change 31 to 113
           read(7,'(a70)',err=113) chr70(iline2)
30        continue
31        continue
        close(7)
!
! If we get to here, we have a profile to check:
        if(iw.eq.1) then
         write(ifile,*)'Have a profile to check! Closing chkprof.log'
         write(ifile,*)'and opening chkprof.',f2(len_f1+3:len_f1+5)
        endif
        close(ifile)
!
! create 'new' chkprof.log filename to match drop: chkprof.000 or 120, etc
        achkprof(len_adir+14:len_adir+16) = f2(len_f1+3:len_f1+5)
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=achkprof,status='unknown',
     $             form='formatted',err=1333)
        iw = 1                    ! success open log file, set to write
        go to 1334
1333     ierror(44) = 1           ! error open log file, do not write
1334     continue
        if(iw.eq.1) then
           write(ifile,*,err=1335) 
     $          'BEGIN again chkprof for drop ',f2(len_f1+3:len_f1+5)
           go to 1336
1335        ierror(45) = 1        ! error write log file, do not write
           iw = 0
1336        continue
        endif
!
        if(iw.eq.1) then
         write(ifile,*)'NOTE: chkprof only compares depths: 200m to "Che
     $ck Profile Depth" =', ichkprofdepth,' for this profile'
         write(ifile,*) 'chkprof excludes the surface layer (0-200m)'
         write(ifile,*)'reference profile: ', f1
         write(ifile,*)'testing profile  : ', f2
         write(ifile,*)'The following 6 values are set in "Check Profile
     $ configuration":'
!
         write(ifile,703) tdzmx
703      format('dispmax=', f6.0,' (Max vertical displacement allowed r
     $elative to reference profile)')
!
         write(ifile,702) tdzrms
702      format('rmsmax= ', f6.0,' (maximum rms displacement allowed r
     $elative to reference profile)')
!
         write(ifile,700) dtdzmn
700      format('dtdzmin=  ', f6.3,' (minimum acceptable value of temper
     $ature gradient)')
!
         write(ifile,701) dtdzth
701      format('dtdzmind=  ',f6.4,' (minimum acceptable value of refere
     $nce profile temperature gradient to be used in displacement calcul
     $ation)')
!
         write(ifile,704) dtmx
704      format('deltmax=  ', f5.2,' (Max absolute tem diff from referen
     $ce prof btwn 199m and icheckprof depth)')
!
         write(ifile,705) dtmx700
705      format('dtmx700=  ', f5.2,' (Max 700m tem diff allowed between 
     $reference and current profile)')
!
         write(ifile,*) 'For a ridiculous amount of info, change operato
     $r name to "debug" (no quotes)'
         write(ifile,*) 'Results for this chkprof run:'
         write(ifile,*) '-----------------------------'
        endif
!
! clear vars:
        ibad=0
        idifmx = 0
        npts = 0
        nptsrms = 0
        dzrms = 0.
        dzmx = 0.
        itdif = 0
        dtmax = 0.
! just some varaibles to save tem gradient values for printing:
        dtdzsav = 999.0
        idepdtdzsav = 0
! is it a good drop? 
!
! f2=current profile to test (avt())
! open file for drop to test (current profile to check)
        open (unit=14,file=f2,status='old',err=900)
        read(14,*,err=901)         !skip header
        read(14,*,err=901)         !skip header
! read current profile down to itestdepth (old default=720) meters
        do 930 i=1,iread
           read(14,361,end=902,err=901)id,it
930        avt(i)=float(it)/1000.
        close(14)
!
! read last good station (reference profile) and test from 200 to 700 m
! 17aug00 if last good stn doesn't go to 700 m, error out, tell op
! open file for last good drop (reference profile)
        open(unit=19,file=f1,status='old',err=991)
        read(19,*,err=992)         !skip header
        read(19,*,err=992)         !skip header
!recall itest is iread-10
        do 915 i=1,itest
         read(19,361,end=993,err=992)idl,itl
915      rft(i)=float(itl)/1000.
361      format(i3,i6)
        close(19)
!
! add Dave Cutchin's 700m temperature difference test here:
        i700m = 0
! need to be sure read in enough depths!        
        if(iread.ge.350) then
         tdif700 = abs(rft(350)-avt(350))
         if(tdif700.gt.dtmx700) then
          i700m = 1
          if(iw.eq.1)then
             write(ifile,*) 'Profile fails 700m temperature diff test:'
             write(ifile,*) ' Tem diff at 700m=',tdif700
             write(ifile,*) ' dtmx700 is set to =',dtmx700
          endif
         endif
        endif
! exclude surface layer
! i=100=199m   (note 179m to 719m actually checked in dtdz)
! i=350=699m
! recall anything with 'r' in name is reference prof, 'a' is current prof
      do 920 i=100,itest
       npts=npts+1
       nptsrms=nptsrms+1
       tdif=abs(rft(i)-avt(i))           ! tem diff at depth i btwn ref and current
!TESTING:
       if(iw.eq.1) write(ifile,*)'------------------------'
       if(iw.eq.1.and.ierrlev.eq.6) write(ifile,600)2*i-1,tdif
600    format('tdif @',i4,' = ',f9.4,'  (Tem diff between test and refer
     $ence prof)')
!
       if (abs(tdif) .gt. dtmax) then    ! First time thru dtmax=0
        dtmax = abs(tdif)                ! Here you are finding max tem diff btwn ref & test
!                                        ! btwn i=100 (199m) and itest (set by op, minus 10) 
        itdif = i
        if(dtmax.gt.dtmx)then            ! dtmx is 'deltmax' set by operator
          if(iw.eq.1.and.ierrlev.eq.6)write(ifile,601) tdif,2*i-1, dtmx
601       format(f10.3, ' = temp diff at ',i4,' m, greater then op set 
     $deltmax', f10.3)
        endif
       endif
!
       dtdz=(avt(i-10)-avt(i+10))/40.    !dtdz is tem gradient of current prof at dep=i
!                                           using 40 m depth
! just save dtdz and depth for printing at end:
       if(dtdz.lt.dtdzsav) then
        dtdzsav = dtdz
        idepdtdzsav = i
       endif
!TESTING:
       if(iw.eq.1.and.ierrlev.eq.6) write(ifile,602)2*i-1,dtdz
602    format('dtdz @',i4,' = ', f10.5,' (temperature gradient)')
!
! check for temperature gradient less than min allowed, or less than twice the
! gradient in the reference profile
       dtrdz=(rft(i-10)-rft(i+10))/40.    !dtrdz is tem gradient of reference at dep=i
!TESTING:
       if(iw.eq.1.and.ierrlev.eq.6) write(ifile,603)2*i-1,dtrdz
603    format('dtrdz@',i4,' = ',f10.5,' (tem gradient of ref prof)')
!
       dtst = dtdzmn         ! dtdzmn set by op=min tem gradient
       if(dtrdz.lt.dtst) then
          dtst = 2.*dtrdz
          if(iw.eq.1)then
           write(ifile,*) 'Reference profile tem gradient is < min set:'
           write(ifile,*) ' so multiply ref prof tem gradient by 2 and '
           write(ifile,*) ' compare test prof tem gradient to that     '
           write(ifile,*)' (at i=',i,')'
        endif
       endif
!
       if(dtdz.lt.dtst)then     ! if tem gradient of test profile lt
        if(iw.eq.1)then         !  either ref prof gradient or 2xref prof gradient
           write(ifile,*) 'Profile fails temperature gradient test:'
           write(ifile,*)dtdz,' dtdz < dtst ',dtst,' at ',2*i-1,' m'
        endif
        ibad=1
        idtdz=i
        go to 920
       endif
! only compute displacement if have a decent gradient to work from
! could use dtrdz, but if this profile is good, its dtdz should work
! ACTUALLY we ARE using reference profile gradient here (dtrdz) and always have!
       if (abs(dtrdz) .gt. dtdzth) then       !-ref prof gradient acceptable to use
        dz=tdif/dtrdz                         !-displacement calc
!
        dz1=tdif/dtdz                         !-test compare disp calc with THIS profiles tem gradient
!TESTING:
        if(iw.eq.1.and.ierrlev.eq.6) then
          write(ifile,744)2*i-1,dz
744       format('dz   @',i4,' = ',f10.5,' (vertical displacement(ref gr
     $ad))')
          write(ifile,745)2*i-1,dz1
745       format('dz1  @',i4,' = ',f10.5,' (vertical displacement(this 
     $profile grad)')
          write(ifile,746)abs(dtrdz-dtdz)
746       format('dtrdz-dtdz = ',f10.5,' (diff of tem gradients)')

        endif
!
        dzrms = dzrms + dz**2                 !-summing for displacement rms calc later
        if (dz .gt. dzmx) then                !-set dzmx as max displacement found
          dzmx = dz
          izdz = 2*i-1                        !  and this is depth of max displacement
        endif
       else
        nptsrms = nptsrms - 1  ! add this 17sep2014, since only used in rms calc!
        if(iw.eq.1) write(ifile,*)
     $      ' no displacement test at ',2*i-1,' m; dtrdz = ',dtrdz
      endif
920   continue         ! end do i = 199m to 699m
!
!27apr2016 add info
        if(iw.eq.1) write(ifile,605) tdif700
605     format(' Tem diff at 700m=',f10.3)
!
        if(iw.eq.1) write(ifile,606) dtmax,2*itdif-1
606     format(' Max tem diff =',f10.3,' at ',i4,' m (btwn 200m->chkprof
     $depth)')
!
        if(iw.eq.1) write(ifile,609) dtdzsav,2*idepdtdzsav-1
609     format(' Min tem gradient =',f10.5,' at ',i4,' m (btwn 200m->chk
     $prof depth)')
!
! this ibad=1 was set above for failing temperature gradient test:
        if(ibad.eq.1) then
           ibaddep = 2*idtdz-1
           if(iw.eq.1) then
            write(ifile,801) ibaddep
801         format('Failed temperature gradient test at depth ', i4)
            write(ifile,*)'Profile fails chkprof '
           endif
           go to 997
        endif
!
        if (abs(dtmax) .gt. dtmx) then
           ibaddep = 2*itdif-1
           if(iw.eq.1) then
           write(ifile,610)ibaddep
610        format('Temperature difference at depth  ',i4,' m')
           write(ifile,611)dtmx
611        format(' exceeds Max delta set ( ',f5.2,')')
           write(ifile,*)'Profile fails chkprof '
           endif
           go to 997
        endif
!
        dzrms = sqrt(dzrms/float(nptsrms))
        if(iw.eq.1) then
         write(ifile,*)'------------------------'
         write(ifile,*) ' no. of points tested for rms displacement= ',
     $                   nptsrms
         write(ifile,*)' rms displacement = ', dzrms
        endif
!
        if (dzrms .gt. tdzrms) then
         ibad = 1
         if(iw.eq.1) then
          write(ifile,*)'------------------------'
          write(ifile,*) dzrms,' rms displacement difference too large'
          write(ifile,*)'Profile fails chkprof '
         endif
        endif
!
        write(ifile,*)'------------------------'
        if(iw.eq.1) write(ifile,607) dzmx, izdz
607     format(' Maximum vertical displacement =',f10.3,' at ',i4,' m')

        if(dzmx.gt.tdzmx) then
         ibaddep = izdz
         if(iw.eq.1) then
          write(ifile,*)' profile fails displacement test'
          write(ifile,*)'at depth ', ibaddep
          write(ifile,*)'Profile fails chkprof '
         endif
         go to 997
        endif

        if(i700m.eq.1)then
            ibad = 1
            ibaddep=699
        endif
!
        if(ibad.eq.0)then
! write to stations.dat:
! open, read to iline-1, write new line, write iline2 line that were saved.
! CHECK errors here
           ichkprof = 1
           open(7,file=astations,status='old',err=112)
           do 60 i = 1, iline-1
60           read(7,*,err=113)

           write(7,'(1x,a3,a49,i4,i5,a8)',err=114)chr3,chr49,ichkprof,
     $            iedt,chr8
           do 61 i = 1, iline2
61           write(7,'(a70)',err=114) chr70(i)
           write(7,'(a7)',err=114)'ENDDATA'
           close(7)
           write(ifile,*)'Profile is acceptable, wrote to stations.dat'
           go to 999
        endif

        if(ibad.eq.1)go to 997
!
900     if(iw.eq.1)
     $     write(ifile,*)'error opening (status=old) test profile: ',f2
        ireturn = 2
        go to 999      ! no change to stations.dat
!
901     if(iw.eq.1)
     $     write(ifile,*)'error reading (status=old) test profile: ',f2
        ireturn = 2
        go to 999      ! no change to stations.dat
!
902     close(14)             !  close test profile if err/end on read
! 6nov2014 call this bad!!!
        ibad = 1
        if(iw.eq.1)write(ifile,*)' station ends above ', itestdepth,' m'
        ireturn = 1
!           this is note to me: DO I WANT TO GO TO 997 and do I want ireturn=2 here?
!           before 06nov2014 I used this:   ireturn = 2
!           before 06nov2014 I used this:   go to 999      ! no change to stations.dat
        go to 997         !06nov2014 write ichk=-1 to stations.dat
!
991     if(iw.eq.1)
     $     write(ifile,*)'error opening (status=old) reference profile '
     $                   ,f1
        ireturn = 2
        go to 999      ! no change to stations.dat
!
992     if(iw.eq.1)
     $     write(ifile,*)'error reading (status=old) reference profile '
     $                   ,f1
        ireturn = 2
        go to 999      ! no change to stations.dat
!
993     close(19)             !  close reference profile if err/end on read
        if(iw.eq.1)write(ifile,*)
     $     ' comparison profile ends above ', itestdepth,' m, chkprof
     $  will not run!'
        ireturn = 2
        go to 999      ! no change to stations.dat
!
997        continue
! if we are here then ibad=1
        ibad = 1
        if(i700m.eq.1) then
         if(iw.eq.1) write(ifile,*) tdif700,'>',dtmx700,
     $'Profile fails 700m temperature difference test'
        endif
! write to stations.dat:
! open, read to iline-1, write new line, write iline2 line that were saved.
        ichkprof = -1
        open(7,file=astations,status='old',err=112)
        do 264 i = 1, iline-1
264        read(7,*,err=113)
        write(7,'(1x,a3,a49,i4,i5,a8)',err=114)
     $         chr3,chr49,ichkprof,iedt,chr8
        do 161 i = 1, iline2
161        write(7,'(a70)',err=114) chr70(i)
        write(7,'(a7)',err=114)'ENDDATA'
        close(7)
        ireturn = 1
        go to 999
! end of exit if ibad=1
105     ireturn = 2
        if(iw.eq.1) then
         write(ifile,*)' Operator set checkprofile depth < 250m    '
         write(ifile,*)' Do not run chkprof, so just exiting '
        endif
        go to 999
!
107     ireturn = 2
        if(iw.eq.1) then
         write(ifile,*)' no new profile identified, new profile is '
         write(ifile,*)' identified by 0 in col 57 of stations.dat '
        endif
        go to 999
!
106     ireturn = 2
        if(iw.eq.1)write(ifile,*)'error opening or reading control.dat '
        go to 999
!
111     ireturn = 2
        if(iw.eq.1)write(ifile,*)' end of file in stations.dat'
        go to 999
!
112     ierror(25) = 1             ! error opening stations.dat
        if(iw.eq.1)write(ifile,*)' error opening stations.dat, exiting'
        ireturn = 2
        go to 999
!
113     ierror(26) = 1             ! error reading stations.dat
        if(iw.eq.1)write(ifile,*)' error reading stations.dat, exiting'
        ireturn = 2
!
114     ierror(29) = 1             ! error writing stations.dat
        if(iw.eq.1)write(ifile,*)' error writing stations.dat, exiting'
        ireturn = 2

999     continue
        close(7)
        if(iw.eq.1) then
         write(ifile,*)'ireturn=',ireturn
         write(ifile,*)'closing stations.dat'
         write(ifile,*)'End chkprof'
        endif
        close(ifile)

        return
        end
!^**********************end  chkprof ************************************^

!23456789012345678901234567890123456789012345678901234567890123456789012
        SUBROUTINE wrdrpstn(nextdrop,itube,t700,iday,imon,iyer,
     $             ihr,imin,isec,ierror,xlat,xlon)
        parameter (nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: wrdrpstn
!3sep2014 LL add (new) ierror(40) error read s file (no biggy)
!            add ierror(46) error open s file (no biggy)
!            (no biggy on s file as seas now sends decent t700)
!31jul2013 LL add the iw/ifile clean up here (15aug-not sure I finished, but I
!               did recompile on 01aug and send to Ibis, maybe that is problem?)
!
!18jul2014 LL wrdrpstn- mods for t700 from seas plus check format spec.
! 03oct2003 add writing to sst.dat - this file will be a matching file to
! stations.dat.   For each line/drop in stations.dat there will be a line
! in sst.dat.   1st col will be drop number, just like in stations.dat.
! 2nd col will be sea surface temperature.   This is so each time Seas calls
! prstat - prstat will _not_ have to open, read and close EACH s file.

! subroutine for JB to write the line to stations.dat that
! we used to write from inside dropmk12
!/ WrDrpStn will write one line in file stations.dat for the drop
!/ just done.   Seas2k must pass it:
!/ nextdrop = int - the value used for the drop (came from siobegin)
!/ itube = int - the launcher tube used for the drop 0-6
!/ t700 = float - the 700m temperature of the drop (make up for now)
!/ the following should be a timestamp of the moment the drop began:
!/ iday = int - day of drop
!/ imon = int - month of drop
!/ iyer = int - year of drop (4 digit, 2003)
!/ ihr = int - hour of drop
!/ imin = int - minute of drop
!/ isec = int - second of drop
!/ xlat = real - latitude of drop from Seas  (Note - I used to write zero's here!)
!/ xlon = real - longitude of drop from Seas     "   Note Seas passes in 
!/         negative numbers for West - I need to translate that to < 180.
!/ OUTPUT - the usual ierror(50) array
!Other notes:
!18jul2014 add t700s - that's the t700 value from the 's' file
!
! file numbers opened and closed in wrdrpstn:
!        7=stations.dat, (read and write)
!       17=sst.dat, (read and write)
!       19=f1 (SIO s file .<nextdrop>) (read) 
!       22=control.dat (sub rdcntrl) (read)
!       31=siodir.txt (sub getdir) (read)
!       33=wrdrpstn.log (ifile) (write)
! ierrors set in here:  looks like I'm clearing all at begin
!( 7) - error opening siodir.txt
!(15) - error opening control.dat
!(16) - error reading control.dat
!(17) - error reading siodir.txt
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(29) - error writing stations.dat
!(32) - wrdrpstn - could not find specified drop in stations.dat! used seas "nextdrop"
!(35) - iSIOErrWatchDog , set to 2 at end of program if all is well.
!                       set to 3XX where XX is above error number to alert
!                       an error was set
!     = 307,317
!(40) - error reading SIO s file (no big deal)
!(44) - ALL    - error opening calling routine's .log file (eg wrdrpstn.log)
!(45) - ALL    - error writing calling routine's .log file (eg wrdrpstn.log)
!(46) - error opening SIO s file (no big deal)
!(48) - error opening sst.dat (no big deal)
!(49) - error reading sst.dat (no big deal)
!(50) - error writing sst.dat (no big deal)
!
        integer*4 ierror(nerr)
        integer*4 nextdrop, itube, iday, imon, iyer
        integer*4 ihr, imin, isec
        real*4 t700, xlat, xlon
        real*4 t700s
        character ch3*3, chr4*4

        character aday*2,amon*2,ayer*2,ahr*2,amin*2,asec*2

        character adir*80, awrdrpstn*80, astations*80
        character asst*80, acontrol*80, f1*80
        integer*2 j1,j2,j3,j4

! rdcntrl:
        integer*4 len_acruise
        real*4 xmaxspd, deadmin, dropmin, relodmin
        real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
        real*4 tm_pl_mx, tm_pl_mn
        character*7 acruise
!
        integer*4 iw, ifile, len
        integer*4 igderr(3)
        integer*4 len_adir, len_f1, i
        integer*4 launcher(12)

        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
        do 10 i = 1, nerr
           ierror(i) = 0
10        continue

        do 5 i = 1, 80
5          f1(i:i) = ' '

        do i = 1, 80
           adir(i:i) = ' '
           awrdrpstn(i:i) = ' '
           astations(i:i) = ' '
           acontrol(i:i) = ' '
           asst(i:i) = ' '
        end do

! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open or read file 'siodir.txt', error out!
        if(ierror(7).eq.1) then
           len_adir = 0
           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
           ierror(35) = 317
           go to 999
        endif
!
        if(len_adir.gt.0) then
           awrdrpstn(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
           asst(1:len_adir) = adir(1:len_adir)
           acontrol(1:len_adir) = adir(1:len_adir)
           f1(1:len_adir) = adir(1:len_adir)
        endif
! add Data\ to data files paths!!!
        f1(len_adir+1:len_adir+5) = 'Data\'
        len_f1 = len_adir + 5

! this is basic one:
        awrdrpstn(len_adir+1:len_adir+17) = 'Data\wrdrpstn.log'
! this is lab debugging one:
!         awrdrpstn(len_adir+1:len_adir+23) =  'Data\wrdrpstn000000.log'
!                                              12345678901234567890123
!                                                      1
!         CALL gettim(j1,j2,j3,j4)
!         if(j1.le.9) then
!            write(awrdrpstn(len_adir+15:len_adir+15),'(i1)') j1
!         elseif(j1.gt.9.and.j1.lt.99) then
!            write(awrdrpstn(len_adir+14:len_adir+15),'(i2)') j1
!         endif
!         if(j2.le.9) then
!            write(awrdrpstn(len_adir+17:len_adir+17),'(i1)') j2
!         elseif(j2.gt.9.and.j2.lt.99) then
!            write(awrdrpstn(len_adir+16:len_adir+17),'(i2)') j2
!         endif
!         if(j3.le.9) then
!            write(awrdrpstn(len_adir+19:len_adir+19),'(i1)') j3
!         elseif(j3.gt.9.and.j3.lt.99) then
!            write(awrdrpstn(len_adir+18:len_adir+19),'(i2)') j3
!         endif

        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
        acontrol(len_adir+1:len_adir+16) = 'Data\control.dat'
        asst(len_adir+1:len_adir+12) = 'Data\sst.dat'

! Our old default values for stations.dat:
! 29Nov2004 - set w/Seas position so if gpspos craps out things still run.
! NO NO NO USE Seas position!        xlat = 99.0
! NO NO NO USE Seas position!        xlon = 0.0
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=awrdrpstn,status='unknown',
     $             form='formatted',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
           write(ifile,*,err=335) 'BEGIN WrDrpStn'
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           go to 336
335        ierror(45) = 1        ! error write log file, do not write
           iw = 0
336        continue
        endif

        if(iw.eq.1) then
           write(ifile,*) 'BEGIN WRDRPSTN, incoming from Seas:'
           write(ifile,*) 'astations=',astations(1:len_adir+17)
           write(ifile,*)'nextdrop=',nextdrop
           write(ifile,*)'itube=',itube
           write(ifile,*)'t700=',t700
           write(ifile,*)'iday=',iday
           write(ifile,*)'imon=',imon
           write(ifile,*)'iyer=',iyer
           write(ifile,*)'ihr=',ihr
           write(ifile,*)'imin=',imin
           write(ifile,*)'isec=',isec
           write(ifile,*)'xlat=',xlat
           write(ifile,*)'xlon=',xlon
           call flush(ifile)
        endif
! check incoming longitude: change to > 180 deg if negative (for west)
        if(xlon.lt.0.0) then
           xlon = 360.0 + xlon
           if(iw.eq.1) write(ifile,*)'new xlon=',xlon
        endif

        CALL rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $         deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $         dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $         ichkprofdepth)
!
        if(ierror(15).ne.0) then                ! error opening control.dat
              ierror(35) = 315
              go to 999
        elseif(ierror(16).ne.0) then            ! error reading control.dat
              ierror(35) = 316
              go to 999
        endif

! don't forget added Data\ above...
        f1(len_f1+1:len_f1+len_acruise) = acruise(1:len_acruise)
        len_f1 = len_f1 + len_acruise

        write(f1(len_f1+1:len_f1+5),'(a5)') 's.000'

        if(iw.eq.1)write(ifile,*) 'before adding nextdrop, f1=',f1
! create s filename from incoming nextdrop:
        ch3(1:3) = '000'
        if(nextdrop.le.9) write(ch3(3:3),'(i1)') nextdrop
        if(nextdrop.ge.10.and.nextdrop.le.99) 
     $       write(ch3(2:3),'(i2)') nextdrop
        if(nextdrop.ge.100) write(ch3(1:3),'(i3)') nextdrop

! open and read sst from s file (nextdrop)
        f1(len_f1+3:len_f1+5)=ch3(1:3)
        if(iw.eq.1) then
           write(ifile,*)'opening f1: ',f1
           call flush(ifile)
        endif

! if no data for csst (current sst in s file): set to no value:
        csst = -9.999
! if no data for t700s (current t700 in s file): set to no value:
        t700s = -9999.0

!18jul2014 comment out (added yesterday) Ibis will send:       t700 = -9.99
!v-----------------begin  open/read s file ----------------------------v
        open(19,file=f1,status='old',form='formatted',err=402)
        if(iw.eq.1)write(ifile,*)'success opening ', f1
        read(19,*,end=403,err=403)          ! skip header line 1
        read(19,*,end=403,err=403)          ! skip header line 2
! read 3 m value to avoid spikes (skip 1m)
        read(19,*,end=403,err=403)          ! skip first 1m value
        read(19,'(i3,i6)',err=403,end=403) idep, item
! 18jul2014 LL - try using Ibis t700! so next 2 lines are seas2k notes(buggy)
! 07mar2005 LL read 700 m temp also- replace t700 incoming from Seas
! with this.   Their t700 is buggy.
! using 348 here because we've read dep 1 & 3 above (indx 1&2 in file)
! depth 700m (699m) = indx 350 in s file.
        do 380 ii = 1, 348
           read(19,'(i3,i6)',end=401,err=403) idep, it700
380        continue
401     continue
        if(iw.eq.1)write(ifile,*)'reading f1, ii, idep, it700=',
     $                           ii,idep,it700
        csst = float(item) * 0.001
        t700s = float(it700)     ! Lisa, you're multiplying by 0.001 below 
        go to 404
!
402     ierror(46) = 1           ! if error opening s file end up here, no big deal
        if(iw.eq.1)write(ifile,*)'error opening',f1,' no big deal'
        go to 404
403     ierror(40) = 1           ! if error reading s file end up here, no big deal
        if(iw.eq.1)write(ifile,*)'error reading',f1,' no big deal'
404     continue
        close(19)
!^-------------------end  open/read s file -----------------------------^
!
!
! v---------begin    open and write to sst.dat ----------------------------v
!08jul2013 LL change file 7 to file 17 : 
! 18jul2014 add err=461 to open...
        if(iw.eq.1)write(ifile,*)'opening asst: ',asst
        open(17,file=asst,status='unknown',err=461)
        rewind(17)
! skip over previous lines:
        do 450 i = 1, nextdrop-1
450     read(17,*,end=458,err=458)

        write(17,'(1x,a3,1x,f7.3)',err=460) ch3, csst
        go to 462           ! all is well, do not set ierrors for sst.dat

458     ierror(49) = 1      ! error reading sst.dat, no big deal
        go to 462           ! all is well, do not set ierrors for sst.dat

460     ierror(50) = 1      ! error writing sst.dat, no big deal
        go to 462           ! all is well, do not set ierrors for sst.dat

461     ierror(48) = 1      ! error opening sst.dat, no big deal
462     continue
        close(17)
! ^---------end      open and write to sst.dat ---------------------------^

! v---------t700 logic for seas t700 or s t700 ---------------------------v
        if(iw.eq.1)write(ifile,*)'seas:t700=',t700,' s:t700=',t700s
! check t700 (from Ibis) and t700s (from s file):
        if(t700.eq.-9999.0) then                  ! > no t700 from seas
           t700 = t700s                           !   then set to t700 from s file 
           t700 = t700 * 0.001                    ! > change to my format -9.999
        elseif(t700.lt.-2.0.or.t700.gt.40.0) then ! > if t700 is out of range
           t700 = t700s                           !   then set to t700 from s file 
           t700 = t700 * 0.001                    ! > change to my format -9.999
        else                                      ! > we think good t700 from Ibis
!          do nothing, don't multiply seas t700 as it's in correct format coming in:
           if(iw.eq.1)write(ifile,*)'use seas t700'
        endif
! 15jul2014 LL if t700 not f7.3, write -9.999
        if(t700.gt.99.99.or.t700.le.-10.0) then
           t700=-9.999
           if(iw.eq.1)write(ifile,*)'t700 outside format spec,
     $  change to -9.999'
        endif
! ^---------t700 logic for seas t700 or s t700 ---------------------------^

! v---------'fix' format of date/time for stations.dat--------------------v
! ok - let's put zeros in front of date/time if only 1 digit integers...
        aday(1:2) = '00'
        amon(1:2) = '00'
        ayer(1:2) = '00'
        ahr(1:2) =  '00'
        amin(1:2) = '00'
        asec(1:2) = '00'

        if(iday.le.9) then
           write(aday(2:2),'(i1)') iday
        else
           write(aday(1:2),'(i2)') iday
        endif
        if(imon.le.9) then
           write(amon(2:2),'(i1)') imon
        else
           write(amon(1:2),'(i2)') imon
        endif
! iyer came in as 4 digit integer, change to 4 digit char, then
!  copy the last 2 digits
        CALL int2ch(iyer,chr4,1,len)
        ayer(1:2) = chr4(3:4)
        if(ihr.le.9) then
           write(ahr(2:2),'(i1)') ihr
        else
           write(ahr(1:2),'(i2)') ihr
        endif
        if(imin.le.9) then
           write(amin(2:2),'(i1)') imin
        else
           write(amin(1:2),'(i2)') imin
        endif
        if(isec.le.9) then
           write(asec(2:2),'(i1)') isec
        else
           write(asec(1:2),'(i2)') isec
        endif
! ^-----end-'fix' format of date/time for stations.dat--------------------^


! v---------begin    open and write to stations.dat ---------------------------v
        open(7,file=astations,status='old',err=914)
        rewind(7)
        if(iw.eq.1) write(ifile,*)'found and opened stations.dat'
        go to 916       ! success opening stations.dat, go to 916 to read it

! Do I really want to do this in seas2k??
! error opening stations.dat, open as new file and write ENDDATA
! 18aug2014 - stop doing this! just set ierror(25)=1 and exit back to seas
! 18aug2014 914     open(7,file=astations)
! 18aug2014         if(iw.eq.1)write(ifile,*)'no stations.dat, creating new one'
! 18aug2014         write(7,'(a7)')'ENDDATA'
! 18aug2014         backspace(7)

914     ierror(25) = 1            ! error opening stations.dat
        ierror(35) = 325
        if(iw.eq.1) write(ifile,*)'cannot open stations.dat, exit'
        go to 950

916     continue     ! if here, opened stations.dat, try to read it:

! How many lines in stations.dat?
! 18aug2014 add err=911 to this read, do I care enough to set a flag????
        do 910 i=1,1000
           indx=i
           read(7,'(a4)',end=911,err=911) chr4
           if(iw.eq.1)write(ifile,*)'reading stations.dat: ', chr4
           if(chr4.eq.'ENDD') go to 912
910     continue

! if we are here, no ENDDATA, no biggy, I hope.
911     indx = indx - 1
        if(iw.eq.1)write(ifile,*)'new indx=',indx
!
! 18aug2014 make sure indx >= 1 :
        if(indx.lt.1) then
           ierror(26) = 1             ! error reading stations.dat
           ierror(35) = 326
           if(iw.eq.1) write(ifile,*)
     $        'Error reading stations.dat, indx<1, exiting'
           go to 950
        endif

912     backspace(7)
!
! indx is the ENDDATA line, so we write new drop on indx line
! and then add ENDDATA at indx+1 lines...
        if(iw.eq.1)write(ifile,*)'indx=',indx,' nextdrop=',nextdrop

! check if what JB used to name s files is what we are putting in stations.dat.
! if not, then what??
        if(indx.ne.nextdrop) then
           ierror(32) = 1       ! could not find specified drop in stations.dat
           if(iw.eq.1) then
              write(ifile,*)'nextdrop from Seas=',nextdrop
              write(ifile,*)'stations.dat numbering (indx) is =',indx
              write(ifile,*)'I used nextdrop in stations.dat'
           endif
        endif
!
        if(iw.eq.1) then
           write(ifile,*) 'Writing next line to stations.dat:'
           write(ifile,363) ch3(1:3),itube,t700,aday,amon,ayer,
     $               ahr,amin,asec,xlat,xlon,0,0,0,'n'
! next 2 lines for debugging only!:
           CALL gettim(j1,j2,j3,j4)
           write(ifile,*)'dos date before write stations.dat:',
     $                              j1,j2,j3,j4
        endif

! write line to stations.dat, already backspaced from ENDDATA:
        write(7,363,err=365) ch3(1:3),itube,t700,aday,amon,ayer,
     $               ahr,amin,asec,xlat,xlon,0,0,0,'n'

363     format(1x,a3,i6,f7.3,1x,a2,1h/,a2,1h/,a2,1x,a2,1h:,a2,1h:,a2,
     $         f9.3,f9.3,i4,i5,i6,1x,a1)

        if(iw.eq.1)write(ifile,'(a7)')'Write ENDDATA to stations.dat:'
        write(7,'(a7)',err=365)'ENDDATA'
        go to 950     
!
!18aug2014 if error writing stations.dat:
365     ierror(29)=1     ! error writing stations.dat, exit without writing drop!
        ierror(35) = 329
        if(iw.eq.1)write(ifile,*)
     $         'Error writing stations.dat, I did NOT write ',ch3(1:3)
!
950     close(7)
! 12sep2014 add 999 and ierror(35)=2 here....
999     continue
! If ierror(35) is still = 0 when we get here we think we have
! run thru siobegin successfully, so set it = 2 on exit
        if(ierror(35).eq.0) then
           ierror(35) = 2
        endif
! FIX: YOU MIGHT WANT TO ADD CLOSE 17 and 19 here:
        if(iw.eq.1)then
         do 1010 i = 1, nerr
1010       if(ierror(i).ne.0) write(ifile,*)'ierror(',i,')=',ierror(i)
           write(ifile,*)'END WrDrpStn'
           call flush(ifile)
        endif
!
        close(ifile)
        return
        end
!^*********************end  wrdrpstn **********************************^
!
! search for FIX!
!23456789012345678901234567890123456789012345678901234567890123456789012       
! think about - do we want this also to write to <date>.nav file?
! currently IS doing (08dec2004)
!---------------------------------------------------------------------------
!/ WrNavFls "Write Navigation Files" (note, this is different than WrDrpStn!)
!/
!/ WrNavFls will write the *users* entered position/date/time/speed/direction
!/ to the files navtrk.dat and <date>.nav for SIO xbt data collection
!/ Seas2k must pass it:
!/ INPUT:
!/ iday = int - day of users inputted position
!/ imon = int - month of users inputted position
!/ iyer = int - year of users inputted position (4 digit, 2003)
!/ ihr = int - hour of users inputted position
!/ imin = int - minute of users inputted position
!/ isec = int - second of users inputted position
!/ clatd = float - latitude degrees of users inputted position
!/ clatm = float - latitude minutes of users inputted position
!/ iclath = int -
!/ clond = float - longitude degrees of users inputted position
!/ clonm = float - longitude minutes of users inputted position
!/ iclonh = int -
!/ speed = float - ship speed of users inputted position
!/ idr = float - ship direction of users inputted position
!
!/ OUTPUT - the usual ierror(50) array
!///////////////////////////////////////////////
!
! file numbers opened and closed in wrnavfls:
!       10=<date>.nav (read,write)
!       15=navtrk.dat (read,write)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=wrnavfls.log (write)
!
! ierrors set in here:  looks like I'm clearing all at begin
!( 7) - error opening siodir.txt
!(14) - error writing navtrk.dat 
!(17) - error reading siodir.txt
!(23) - error opening navtrk.dat
!(24) - error reading navtrk.dat
!(28) - speed and/or direction values screwed up - not writing to navtrk.dat or <date>.nav
!(44) - ALL    - error opening calling routine's .log file (eg wrnavfls.log)
!(45) - ALL    - error writing calling routine's .log file (eg wrnavfls.log)

        SUBROUTINE wrnavfls(ierror,iday,imon,iyer,ihr,imin,isec,
     $    clatd,clatm,iclath,clond,clonm,iclonh,speed,dir)

        parameter(nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: wrnavfls
!
!        10 = date.nav
!        22 = control.dat
!        15 = navtrk.dat
!        33 = wrnavfls.log

        character aclath*1, aclonh*1, afilen*80
        character astat*3, adate*8, atime*8, adatein*8
        character adosday*2, adosmon*2, adosyear*4
        integer*4 ierror(nerr), ihr, imin, isec
        integer*4 iday,imon,iyer
        real*4 speed,dir
        real*4 vlat, vlon
        real*4 clatd,clatm,clond,clonm
        integer*4 iclath,iclonh
        integer*4 iclatd,iclond
        integer*4 jpos,len, ibuf
        integer*2 j1,j2,j3,j4 
        integer*4 nhr,nmin,nsec,iflg
        character a64(1500)*64
        integer*4 iw, ifile, i, len_adir, ierrwrite, ios
        integer*4 nday,nmon,nyear
        real*4 vlatin,vlonin, speedin, dirin
        integer*4 idone,ibefore
        integer*4 igderr(3)

        character*80 adir, awrnavfls*80, anavtrk*80
        adir = ' '
        awrnavfls = ' '
        anavtrk = ' '
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
        do 10 i = 1, nerr
           ierror(i) = 0
10        continue
!
! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can not open or read file 'siodir.txt'
        if(ierror(7).eq.1) then
           len_adir = 0
! do not do in here:           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
! do not do in here:           ierror(35) = 317
           go to 999
        endif
!
        if(len_adir.gt.0) then
           awrnavfls(1:len_adir) = adir(1:len_adir)
           anavtrk(1:len_adir) = adir(1:len_adir)
        endif

        anavtrk(len_adir+1:len_adir+15) =   'Data\navtrk.dat'
! if you want multiple outputs named with hhmmss switch these:
        awrnavfls(len_adir+1:len_adir+17) = 'Data\wrnavfls.log'

!hhmmss        awrnavfls(len_adir+1:len_adir+23) = 'Data\wrnavfls000000.log'
!                                            12345678901234567890123
!                                                     1         2
!hhmmss         CALL gettim(j1,j2,j3,j4)
!hhmmss         if(j1.le.9) then
!hhmmss            write(awrnavfls(len_adir+15:len_adir+15),'(i1)') j1
!hhmmss         elseif(j1.gt.9.and.j1.lt.99) then
!hhmmss            write(awrnavfls(len_adir+14:len_adir+15),'(i2)') j1
!hhmmss         endif
!hhmmss         if(j2.le.9) then
!hhmmss            write(awrnavfls(len_adir+17:len_adir+17),'(i1)') j2
!hhmmss         elseif(j2.gt.9.and.j2.lt.99) then
!hhmmss            write(awrnavfls(len_adir+16:len_adir+17),'(i2)') j2
!hhmmss         endif
!hhmmss         if(j3.le.9) then
!hhmmss            write(awrnavfls(len_adir+19:len_adir+19),'(i1)') j3
!hhmmss         elseif(j3.gt.9.and.j3.lt.99) then
!hhmmss            write(awrnavfls(len_adir+18:len_adir+19),'(i2)') j3
!hhmmss         endif
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=awrnavfls,form='formatted',
     $             status='unknown',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then 
           write(ifile,*,err=335)'Inside wrnavfls: '
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           call flush(ifile)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif

        if(iw.eq.1) then
         write(ifile,*)'awrnavfls=',awrnavfls(1:len_adir+17)
         write(ifile,*)'anavtrk=', anavtrk(1:len_adir+17)
         write(ifile,*)'incoming iday,imon,iyer', iday,imon,iyer
         write(ifile,*)'incoming ihr,imin,isec',ihr,imin,isec
         write(ifile,*)'incoming clatd,clatm,iclath',
     $               clatd, clatm, iclath
         write(ifile,*)'incoming clond,clonm,iclonh',
     $               clond, clonm, iclonh
         write(ifile,*)'incoming speed,dir', speed,dir
         call flush(ifile)
        endif

! check for existence of date.nav file; if it exists read up 
! to the end (so don't write over old nav data).
! create name of passed in date's date.nav file:
        CALL getfilen(afilen,adosday,adosmon,adosyear,iday,
     $                imon,iyer,adir,len_adir)
        if(iw.eq.1)write(ifile,*)'back from getfilen ',
     $                            afilen(1:len_adir+15)

! do need this because of slashes:
! translate iday, imon, iyer to adate:
        adate = '00/00/00'
        jpos = 2
        if(iday.gt.9) jpos = 1
        CALL int2ch(iday,adate,jpos,len)
        jpos = 5
        if(imon.gt.9) jpos = 4
        CALL int2ch(imon,adate,jpos,len)
        CALL ch2real(adosyear,3,2,x)
        i1 = int(x)
        jpos = 8
        if(i1.gt.9) jpos = 7
        CALL int2ch(i1,adate,jpos,len)
        if(iw.eq.1) write(ifile,*)'adate=', adate

        iclatd = int(clatd)
        iclond = int(clond)

! translate iclonh to aclonh
        aclonh = 'E'
        if(iclonh.eq.4) aclonh = 'W'
! translate iclath to aclath
        aclath = 'N'
        if(iclath.eq.3) aclath = 'S'

! if we're calling this, it's a manual entry
        astat='MAN'
        ibuf = 0

! translate input position to decimal
        CALL deg2dec(int(clatd),clatm,aclath,vlat)
        CALL deg2dec(int(clond),clonm,aclonh,vlon)
        if(iw.eq.1) then
         write(ifile,*) 'vlat=',vlat,' vlon=',vlon
         write(ifile,*) 'calling chkall:'
         call flush(ifile)
        endif

! change this from checking only speed/dir TO check position/speed/dir!

         CALL chkall(vlat,vlon,speed,dir,ierrwrite)
         if(iw.eq.1) then
          write(ifile,*) 'after chkall ierrwrite=',ierrwrite
          write(ifile,*) 'after chkall vlat=',vlat
          write(ifile,*) 'after chkall vlon=',vlon
          write(ifile,*) 'after chkall speed=',speed
          write(ifile,*) 'after chkall dir=',dir
          call flush(ifile)
         endif

         if(ierrwrite.eq.1) then
          ierror(28) = 1
          if(iw.eq.1) then
           write(ifile,*) 'Speed and/or direction values screwed up!'
           write(ifile,*) 'Not writing updated values to navtrk.dat!'
           call flush(ifile)
          endif
          goto 700
         endif

! open, read, compare, write if entered pos more recent, close navtrk.dat:

!FIX  ??
         if(iw.eq.1)write(ifile,*) 'opening anavtrk:'
         open(15,file=anavtrk,form='formatted',status='unknown',
     $       err=161)
         if(iw.eq.1)write(ifile,*) 'reading anavtrk:'

         read(15,502,end=161,err=161)nday,nmon,nyear,nhr,nmin,
     $         nsec,vlatin,vlonin, speedin, dirin
502      format(i2,1x,i2,1x,i2,t10,i2,1x,i2,1x,
     $         i2,t19,f7.3,f8.3,f6.2,f7.2)
! note, nyear is READ in from navtrk as 2 digit.   Change to 4 digit:
         nyear = nyear + 2000
         if(iw.eq.1) then
          write(ifile,*)'read in from navtrk:'
          write(ifile,*)nday,nmon,nyear,nhr,nmin,nsec,vlatin,
     $              vlonin,speedin,dirin
         endif
!
         close(15,iostat=ios)
! Is entered position more recent than what is in navtrk.dat? if so,
! write entered value to navtrk.dat

         if(iw.eq.1) then
            write(ifile,*)'1close15ios=',ios
            write(ifile,*) 'call compare:'
         endif
! subroutine compare just returns iflg=1 if 1st group is later in time
! than second group.   So must be careful the order you send things!
! switched these 08oct2004 - used to be nday 1st and iday 2nd - ERROR
! also note compare expects 4 digit years from iyer and nyear
! 19aug2014 "i" is passed in date, "n" is from navtrk.dat, if exist
        CALL compare(iday,imon,iyer,ihr,imin,isec,
     $                nday,nmon,nyear,nhr,nmin,nsec,iflg)

        if(iw.eq.1)then
           write(ifile,*)'back from compare,iflg=',iflg
           write(ifile,*)
     $      'if iflg=1, passed in date is later than navtrk'
        endif
        go to 170
161     continue   !error opening or reading navtrk.dat, try to create new one:

! If error opening or reading navtrk.dat just write a new one!   So set
!    iflg = 1    27Jan2004 LL  
        close(15,iostat=ios)
        if(iw.eq.1)then
          write(ifile,*)'2close15ios=',ios
          write(ifile,*)
     $        'navtrk.dat does not exist, set iflg=1, create navtrk'
        endif
        iflg = 1

170     continue

        if(iflg.eq.1) then
         open(15,file=anavtrk,form='formatted',status='unknown',err=180,
     $        iostat=ios)
         if(iw.eq.1) then
            write(ifile,*) 'open15ios=',ios
            write(ifile,*) 'iflg=1, writing to navtrk.dat'
            write(ifile,509) adate,ihr,imin,isec,vlat,vlon,speed,dir
            call flush(ifile)
         endif
         write(15,509,err=185) adate,ihr,imin,isec,vlat,vlon,speed,dir
509      format(a8,' ',i2,':',i2,':',i2,' ',f7.3,f8.3,f6.2,f7.2)
        endif
        close(15,iostat=ios)
        if(iw.eq.1) write(ifile,*) '3close15ios=',ios
!
        go to 190        !all is well, skip errors
!
180     ierror(23)=1     ! error opening navtrk.dat
        if(iw.eq.1)write(ifile,*)
     $           'error opening navtrk.dat, will try to write date.nav'
        go to 190
185     ierror(14)=1     ! error writing navtrk.dat
        if(iw.eq.1)write(ifile,*)
     $          'error writing navtrk.dat, will try to write date.nav'
        close(15,iostat=ios)
        if(iw.eq.1) write(ifile,*) '4close15ios=',ios
190     continue

! write date, gps time, gps position timetag ave, lat ave, long ave,
! OEM unit status, gps quality, iequal to date.nav file

! idone, set to 1 once found position to write to
! ibefore, # lines in nav file before new write position
! i-1 = total # lines in nav file
        idone = 0
        ibefore = 0

        if(iw.eq.1) write(ifile,*)
     $       'opening date.nav file:', afilen(1:len_adir+15)
        open(10,file=afilen,form='formatted',status='unknown',
     $          err=680,iostat=ios)
        if(iw.eq.1) write(ifile,*)'1open10ios=',ios
        rewind(10,iostat=ios)
        if(iw.eq.1) write(ifile,*)'1rewind10=',ios
        if(iw.eq.1) write(ifile,*)
     $       'reading date.nav file:', afilen(1:len_adir+15)

        do 656 i = 1, 1500
         read(10,'(a64)',end=657,err=690,iostat=ios) a64(i)
         if(iw.eq.1) then
            write(ifile,*)'a64',i,':',a64(i)
            write(ifile,*)'read10ios=',ios
         endif
         if(idone.eq.0) then
!19aug2014 TEST, add err=690 here: what will this break?
          read(a64(i),'(9x,i2,1x,i2,1x,i2)',err=690) nhr, nmin, nsec
          CALL findtime(nhr,nmin,nsec,ihr,imin,isec,iflg)
          if(iw.eq.1) write(ifile,*)'aft findtime,iflg=',
     $                iflg,nhr,nmin,nsec,ihr,imin,isec
! if time less than what just read, write before
          if(iflg.eq.0) then
           idone = 1
! if time greater, keep looking
          elseif(iflg.eq.1) then
           ibefore = ibefore + 1
          endif
         endif
656     continue

657     continue
        if(iw.eq.1)write(ifile,*)'idone=',idone,' ibefore=',ibefore
! if idone = 0 - just write line at end of file
        if(idone.eq.0) then
         if(iw.eq.1) then
           write(ifile,*)
     $      'Entered position is later than last position in date.nav'
           write(ifile,*)'write to end of ',adate(1:8),'.nav file' 
         endif
         backspace(10,iostat=ios)
         if(iw.eq.1) write(ifile,*)'backspace10ios=',ios
         write(10,599,err=695,iostat=ios) adate(1:8), ihr,imin,isec,
     $           abs(iclatd), clatm, aclath, iclond, clonm,
     $                aclonh, 'MAN', speed, dir, 0
599      format(a8,' ',i2,':',i2,':',i2,' ',
     $          i3,' ',f7.4,' ',a1,' ',i3,' ',f7.4,' ',
     $          a1,' ',a3,f6.2,f6.1,i3)
         if(iw.eq.1) write(ifile,*)'write10ios=',ios

! if idone = 1, close file, open, read ibefore lines, write new line,
! write the rest (i-1-ibefore) lines
        else
         close(10,iostat=ios)
         if(iw.eq.1) then
           write(ifile,*)'close10ios= ',ios 
           write(ifile,*)
     $      'Entered position is before than last position in date.nav'
           write(ifile,*)'write to middle of ',adate(1:8),'.nav file' 
         endif
         open(10,file=afilen,form='formatted',status='unknown',err=680,
     $           iostat=ios)
         if(iw.eq.1) write(ifile,*)'2open10ios=',ios
         do 670 j = 1, ibefore
670      write(10,'(a64)',err=695) a64(j)
         write(10,599,err=695) adate(1:8), ihr,imin,isec,
     $           abs(iclatd), clatm, aclath, iclond, clonm,
     $                aclonh, 'MAN', speed, dir, 0
         do 671 j = ibefore+1, i-1
671         write(10,'(a64)',err=695) a64(j)
        endif
        go to 700
!
680     ierror(5) = 1               ! error opening date.nav
        ierror(34) = iday         ! pass seas the ddmmyy of date.nav
        ierror(36) = imon
        ierror(37) = iyer
        if(iw.eq.1)write(ifile,*)'Error opening',adate(1:8),'.nav'
        go to 700
!
690     ierror(2) = 1              ! error reading date.nav
        ierror(34) = iday         ! pass seas the ddmmyy of date.nav
        ierror(36) = imon
        ierror(37) = iyer
        if(iw.eq.1)write(ifile,*)'Error reading',adate(1:8),'.nav'
        go to 700
!
695     ierror(6) = 1              ! error writing date.nav
        ierror(34) = iday         ! pass seas the ddmmyy of date.nav
        ierror(36) = imon
        ierror(37) = iyer
        if(iw.eq.1)write(ifile,*)'Error writing',adate(1:8),'.nav'

!
700     continue

! ok just close them all here:
        close(10,iostat=ios)
           if(iw.eq.1) write(ifile,*)'ENDclose10ios= ',ios 
        close(15,iostat=ios)    !added 3sep2014
           if(iw.eq.1) write(ifile,*)'ENDclose15ios= ',ios 
        if(iw.eq.1)write(ifile,*)'end wrnavfls'
        close(ifile)
!
999     continue
        return 
        end
        
!^******************end  wrnavfls  *********************************^

        SUBROUTINE prstat(ido,iDropNo,iTubeNo,c700m,cLat,cLon,ihour,
     $                    imin,isec,iday,imonth,iyear,icheckprof,
     $                    iedited,iNavNo,csst,ixmit,ierror)
! 18aug2014 add iw/ifile and errors for sst.dat:
! send data back to seas2k of
! oops, no JB only wants last 10 drops.

! INPUT - none
! OUTPUT
!        ido - number in array I am passing back (<=10)
!        iDropNo(ido) = drop number in stations.dat
!        iTubeNo(ido) = tube number this drop shot from
!        c700m(ido) = 700 m temperature of this drop
!        cLat(ido) = latitude of this drop      
!        cLon(ido) = longitude of this drop      
!        ihour(ido) = hour of this drop
!        imin(ido) = minute of this drop
!        isec(ido) = second of this drop         
!        iday(ido) = day of this drop         
!        imonth(ido) = month of this drop         
!        iyear(ido) = year of this drop         2003, etc
!        icheckprof(ido) = chkprof code of this drop         
!        iedited(ido) = edit code of this drop         
!        iNavNo(ido) = navigation code of this drop         
!        csst(ido) = sea surface(3m) temperature of this drop ,if exist, if not exist = -999.9
!        ixmit(ido) = transmit code of this drop, 1=y, 0=no
!        ierror(nerr) - the usual error code sending output
!
! file numbers opened and closed in prstat:
!        7=stations.dat (read)
!       17=sst.dat (read)
!       22=control.dat (sub rdcntrl) (read)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=prstat.log (write)
!
! ierrors set in here:  looks like I'm clearing all at begin
! 5jun2015 do not clear ierror(38 & 39) !!!
!( 7) - error opening siodir.txt
!(15) - error opening control.dat
!(16) - error reading control.dat
!(17) - error reading siodir.txt
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(33) - if operator name = "debug" turn on ierrlev=6 (fill .log files)
!(35) - iSIOErrWatchDog , set to 2 at end of program if all is well.
!                       set to 3XX where XX is above error number to alert
!                       an error was set
!     = 307,315,316,317,325,326
!(44) - ALL    - error opening calling routine's .log file (eg prstat.log)
!(45) - ALL    - error writing calling routine's .log file (eg prstat.log)
!(48) - error opening sst.dat (no big deal)
!(49) - error reading sst.dat (no big deal)
!
! how many to send back:
        parameter(nr=10)
        parameter(nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: prstat

!23456789012345678901234567890123456789012345678901234567890123456789012       
        character axmit*1
        character aline*70, axbt*3, ch3*3
        integer*4 iDropNo(nr), iTubeNo(nr),iday(nr),imonth(nr),iyear(nr)
        integer*4 ihour(nr),imin(nr),isec(nr)
        integer*4 icheckprof(nr), iedited(nr), iNavNo(nr), ixmit(nr)
        real*4 cLat(nr), cLon(nr), c700m(nr),csst(nr)
        integer*4 ierror(nerr), launcher(12)
        integer*4 iw, ifile

        character*80 adir, astations*80, acruise*7, aprstat*80
        character asst*80
        character acontrol*80
        integer*2 j1, j2, j3, j4
        integer*4 igderr(3)
        integer*4 ido, iskip
        integer*4 i, len_adir
! rdcntrl:
        integer*4 len_acruise, ichkprofdepth
        real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
        real*4 tm_pl_mx, tm_pl_mn
        real*4 xmaxspd, deadmin, dropmin, relodmin
!
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
!05jun2015 LL only zero out ones that might get set in here:
        do 10 i = 1, nerr
           ierror(7) = 0
           ierror(15) = 0
           ierror(16) = 0
           ierror(17) = 0
           ierror(25) = 0
           ierror(26) = 0
           ierror(44) = 0
           ierror(45) = 0
           ierror(48) = 0
           ierror(49) = 0
10        continue
!
! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can not open or read file 'siodir.txt'
        if(ierror(7).eq.1) then
           len_adir = 0
           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
           ierror(35) = 317
           go to 999
        endif

        astations = ' '
        asst      = ' '
        acontrol  = ' '
        aprstat   = ' '
        if(len_adir.gt.0) then
           astations(1:len_adir) = adir(1:len_adir)
           asst(1:len_adir) = adir(1:len_adir)
           acontrol(1:len_adir) = adir(1:len_adir)
           aprstat(1:len_adir) = adir(1:len_adir)
        endif

        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
        asst(len_adir+1:len_adir+12) = 'Data\sst.dat'
        acontrol(len_adir+1:len_adir+16) = 'Data\control.dat'
        aprstat(len_adir+1:len_adir+15) = 'Data\prstat.log'
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=aprstat,form='formatted',
     $             status='unknown',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then 
           write(ifile,*,err=335)'Inside prstat: calling rdcntrl'
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           call flush(ifile)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif
!
        if(iw.eq.1) then
           CALL gettim(j1,j2,j3,j4)
           write(ifile,*)'Begin prstat, dos time:',j1,j2,j3,j4
           write(ifile,*) 'aprstat=', aprstat(1:len_adir+15)
           write(ifile,*) 'astations=', astations(1:len_adir+17)
           write(ifile,*) 'calling rdcntrl= '
           write(ifile,*) 'ierror(15)=', ierror(15)
           write(ifile,*) 'ierror(16)=', ierror(16)
        endif

        ido = 0

        CALL rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $         deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $         dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $         ichkprofdepth)
!
        if(iw.eq.1) then
           write(ifile,*) 'return rdcntrl= ierror(15->16)='
           write(ifile,*) 'ierror(15)=', ierror(15)
           write(ifile,*) 'ierror(16)=', ierror(16)
        endif
!
        if(ierror(15).ne.0) then                ! error opening control.dat
              ierror(35) = 315
              go to 999
        elseif(ierror(16).ne.0) then            ! error reading control.dat
              ierror(35) = 316
              go to 999
        endif
!   this is "debug"
        if(ierror(33).eq.6) ierrlev = 6

! first, how many lines in stations.dat:
        open(7,file=astations,form='formatted',status='old',err=65)
        rewind(7)
! sst.dat, just set error flag, but do not exit:
        open(17,file=asst,form='formatted',status='unknown',err=48)
        rewind(17)
        go to 448
48      ierror(48) = 1     ! error opening sst.dat, no big deal
        if(iw.eq.1) write(ifile,*) 'error opening sst.dat, no big deal'
448     continue

        do 105 i = 1, 1000
         indx = i
         read(7,'(a70)',err=70) aline(1:70)
         if(aline(1:3).eq.'END') go to 106
105     continue

106     continue
        rewind(7)

! indx-1 is number of lines in stations.dat (indx line = ENDDATA)
        if(indx.eq.1) then
         ido = 0
        elseif(indx.gt.1) then
         iskip = (indx - 1) - nr

         if(iskip.le.0) then
          ido = indx - 1
          iskip = 0
         elseif(iskip.gt.0) then
          ido = nr
c         and iskip = iskip...
         endif

        if(iw.eq.1) write(ifile,*)'indx=',indx,
     $                           ' iskip=',iskip,' ido=',ido

! skip iskip lines in both stations.dat AND sst.dat
! 27sep2005 LL - program hanging while reading 12-I think it's here...
        if(iskip.ge.1) then
         do 320 i = 1, iskip
          if(ierrlev.eq.6.and.iw.eq.1) write(ifile,*)'skip ', i
          read(7,*,end=320,err=320)
          if(ierror(48).eq.0) read(17,*,end=320,err=320)
320      continue
        endif

        if(iw.eq.1) write(ifile,*)'Read from stations.dat'
        do 325 i = 1, ido
! start out with sst set as no value for JB
         csst(i) = -999.9
         read(7,507,err=70)axbt,iTubeNo(i),c700m(i),iday(i),
     $                imonth(i),iy,
     $                ihour(i),imin(i),isec(i), cLat(i), cLon(i),
     $                icheckprof(i), iedited(i), iNavNo(i), axmit
507      format(1x,a3,i6,f7.3,1x,i2,1x,i2,1x,i2,1x,
     $          i2,1x,i2,1x,i2,f9.3,f9.3,
     $          i4,i5,i6,1x,a1)
         read(axbt(1:3),'(i3)') iDropNo(i) 

         if(iw.eq.1) then
          write(ifile,*)'read in i=',i
          write(ifile,507)axbt,iTubeNo(i),c700m(i),iday(i),
     $                imonth(i),iy,
     $                ihour(i),imin(i),isec(i), cLat(i), cLon(i),
     $                icheckprof(i), iedited(i), iNavNo(i), axmit
          write(ifile,*)'test iDropNo(i)=',iDropNo(i)
         endif

         iyear(i) = 2000 + iy

         if(axmit.eq.'y'.or.axmit.eq.'Y') then
           ixmit(i) = 1
         else
           ixmit(i) = 0
         endif
         if(iw.eq.1) write(ifile,*)i,'set', 
     $                    iDropNo(i),iyear(i),ixmit(i)

! read sst from sst.dat:
         if(ierror(48).eq.0) then    ! if able to open sst.dat, then read:
          read(17,'(1x,a3,1x,f7.3)',err=49,end=49) ch3, csst1
         endif
         if(ch3(1:3).eq.axbt(1:3)) then
          csst(i) = csst1
         endif
!
         if(iw.eq.1) then
          write(ifile,'(1x,a3,1x,f7.3)') ch3, csst1
          write(ifile,*)i,'setit ', csst(i)
         endif
         go to 325
49       ierror(49) = 1               ! error reading sst.dat, no big deal
         if(iw.eq.1) write(ifile,*) 'error reading sst.dat, no big deal'

325     continue
        endif

        close(7,iostat=ios)
         if(iw.eq.1) write(ifile,*) 'close7ios=',ios
        close(17,iostat=ios)
         if(iw.eq.1) write(ifile,*) 'close17ios=',ios
! 18oct2004 Change cLon to be 0-180 E and 181-360 to be negative 0-180!
        do 32 i = 1, ido
           if(cLon(i).gt.180.0.and.cLon(i).lt.360.0) then
              cLon(i) = -1. * (360.0 - cLon(i))
           elseif(cLon(i).eq.360.0) then
              cLon(i) = 0.0
           elseif(cLon(i).gt.360.0) then
              cLon(i) = 360.0 - cLon(i)
           endif
32        continue

        if(iw.eq.1) then
         write(ifile,*)'sending back to Seas, ido=',ido
         write(ifile,*)'iDropNo,iTubeNo,c700m,iday,imonth,iyear'
         write(ifile,*)'        ihour,imin,isec,cLat,cLon,icheckprof'
         write(ifile,*)'         iedited,iNavNo,ixmit'

         do 350 i = 1, ido
          write(ifile,*)iDropNo(i),iTubeNo(i),c700m(i),iday(i),
     $                imonth(i),iyear(i),
     $                ihour(i),imin(i),isec(i), cLat(i), cLon(i),
     $                icheckprof(i), iedited(i), iNavNo(i), ixmit(i)
350      continue
        endif

        go to 600

65      ierror(25) = 1             ! error opening stations.dat
        ierror(35) = 325
        if(iw.eq.1) write(ifile,*)'error opening stations.dat'
        go to 600
!
70      ierror(26) = 1             ! error reading stations.dat
        ierror(35) = 326
        if(iw.eq.1) write(ifile,*)'error reading stations.dat'
        close(7)

600     continue
        


999     continue
!        if ierror(35) has not been set in here then we think 
!        we've run smoothly so set ierror(35)=2
        if(ierror(35).eq.0) then
           ierror(35) = 2
           if(iw.eq.1) write(ifile,*)'all is well, set ierror(35)=2'
        endif
! ok, just close them all here for safety:
        close(7,iostat=ios)
         if(iw.eq.1) write(ifile,*) 'ENDclose7ios=',ios
        close(17,iostat=ios)
         if(iw.eq.1) write(ifile,*) 'ENDclose17ios=',ios
! next 2 lines for debugging only!:
        if(iw.eq.1) then
           do 1010 i = 1, nerr
1010           if(ierror(i).eq.1) write(ifile,*)'ierror(',i,')=1'
           CALL gettim(j1,j2,j3,j4)
           write(ifile,*)'end prstat, dos time:',j1,j2,j3,j4
        endif
        close(ifile)
!
        return
        end
!
!^********************* end  prstat  ********************************^
!
        SUBROUTINE wrxmit(iday,imon,iyer,ihr,imin,isec,ierror,
     $                    ichoosedrop)
!
        parameter (nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: wrxmit

! 21jan2005 - add ichoosedrop - so op can enter which drop was xmitted.
!        Seas should use ichoosedrop=-99 for regular use!

! subroutine for JB to write change the transmitted? "N" in stations.dat
! to a "Y" if drop was transmitted.   We are using date/time to figure out
! which drop to change since that is what is identical for us between
! seas drops and sio drops.
!//////////////////////////////////////////////////////////////////
!/ WrXmit - this routine will change 'n' to 'y' inside SIO's main
!/ stations.dat file for the "transmitted" column (last col).  Once
!/ changed -when prstat is called you'll get a ixmit=1 (for yes).
!/ Seas2k must pass it the timestamp of the drop that was transmitted:
!/     (meaning the timestamp when Seas launched the drop!)
!/ INPUT:
!/ iday = int - day of drop
!/ imon = int - month of drop
!/ iyer = int - year of drop (4 digit, 2003)
!/ ihr = int - hour of drop
!/ imin = int - minute of drop
!/ isec = int - second of drop
!/ ichoosedrop = int - op change particular drop
!/ OUTPUT: - the usual ierror(40) array
!/     NEW: ierror(32)=1 means I did NOT find this particular drop!

        integer*4 ierror(nerr), ichoosedrop
        integer*4 iday, imon, iyer
        integer*4 ihr, imin, isec
        character adir*80, astations*80, awrxmit*80
        character (len=70), dimension(999) :: aline
        integer*4 iw, ifile
        integer*4 igderr(3)
!
! file numbers opened and closed in wrxmit:
!        7=stations.dat, (read and write)
!       31=siodir.txt (sub getdir) (read)
!       33=awrxmit.log (ifile) (write)
! ierrors set in here:  looks like I'm clearing all at begin
!( 7) - error opening siodir.txt
!(17) - error reading siodir.txt
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(29) - error writing stations.dat
!(32) - wrxmit - could not find specified drop in stations.dat!  NO change made.
!(44) - ALL    - error opening calling routine's .log file (eg wrxmit.log)
!(45) - ALL    - error writing calling routine's .log file (eg wrxmit.log)
!
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
! 5jun2015LL NO, only zero out ones that get set in here:
        do 10 i = 1, nerr
           ierror(25) = 0
           ierror(26) = 0
           ierror(29) = 0
           ierror(32) = 0
           ierror(44) = 0
           ierror(45) = 0
10        continue
!
! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can not open or read file 'siodir.txt'
        if(ierror(7).eq.1) then
           len_adir = 0
! do not do in here:           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0
! do not do in here:           ierror(35) = 317
           go to 999
        endif
!
        awrxmit   = ' '
        astations = ' '
        if(len_adir.gt.0) then
           awrxmit(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
        endif
        awrxmit(len_adir+1:len_adir+17) = 'Data\wrxmit.log'
        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
                    
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=awrxmit,status='unknown',
     $             form='formatted',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
           write(ifile,*,err=335) 'BEGIN WRXMIT'
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif
!
        if(iw.eq.1) then
           write(ifile,*) 'astations=',astations(1:len_adir+17)
           write(ifile,*) 'ichoosedrop=',ichoosedrop
           write(ifile,*) 'Seas says the following drop transmitted:'
           write(ifile,*) 'iday=',iday, ' imon=',imon,' iyer=',iyer
           write(ifile,*) 'ihr=',ihr,' imin=',imin,' isec=',isec
           call flush(ifile)
         endif
! iyer came in as 4 digit integer, change to 2 digit int
        iyer = iyer - 2000
        write(ifile,*)' changed iyer to ', iyer
!         
        open(7,file=astations,status='old',err=914)
        rewind(7)
        if(iw.eq.1) then
           write(ifile,*)'found stations.dat, reading:'
           call flush(ifile)
        endif
         
        icount = 0
! read in entire stations.dat
        do 110 i = 1, 1000
         read(7,'(a70)',err=70,end=15) aline(i)
         if(iw.eq.1) write(ifile,'(a70)') aline(i)
         icount = icount + 1
110      continue
15      continue
! close stations.dat:
        close(7)

! icount is total number of lines in stations.dat, including the ENDDATA.
        if(ichoosedrop.ge.icount) go to 299      
!
        if(ichoosedrop.gt.0) then
! write "y" to op choosen drop (if exist!)
         write(aline(ichoosedrop)(70:70),'(a1)') 'y'
         if(iw.eq.1) write(ifile,'(a70)')aline(i)(1:70)
         go to 300
        else
! search thru aline(i) for matching date/time:
         do 20 i = 1, icount
          if(aline(i)(1:3).eq.'END') go to 299
          read(aline(i)(1:70),510) id, im, iy, ih, imi, is
          if(iw.eq.1) write(ifile,*) id,im,iy,ih,imi,is
510       format(18x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)
! find drop that matches date and time:
          if((id.eq.iday).and.(im.eq.imon).and.(iy.eq.iyer).and.
     $      (ih.eq.ihr).and.(imi.eq.imin)) then
! change aline 'n' to 'y':
           write(aline(i)(70:70),'(a1)') 'y'
           if(iw.eq.1) then
              write(ifile,*)'found match!'
              write(ifile,'(a70)')aline(i)(1:70)
           endif
           go to 300
          endif
20       continue

        endif           ! ichoosedrop ge icount
          
299     if(iw.eq.1) write(ifile,*)'could not find specified drop in
     $   stations.dat'
         ierror(32) = 1              ! could not find specified drop in stations.dat
         close(7)
         go to 400

300     continue

! found the drop, write out entire stations.dat:     POSSIBLE FOR ERRORS
! This is possible source of errors in stations.dat since it must write
! entire file
! 23jul2014 add ierror(29) for error writing stations.dat
        open(7,file=astations,status='old',err=914)
        rewind(7)
        do 60 i = 1, icount-1
         write(7,'(a70)',err=377) aline(i)(1:70)
60      continue
        write(7,'(a7)',err=377)'ENDDATA'
! close stations.dat:
        close(7)
        go to 400
!         
70      if(iw.eq.1)write(ifile,*)'error reading stations.dat!'
        ierror(26) = 1             ! error reading stations.dat
        close(7)
        go to 400
!
377     if(iw.eq.1)write(ifile,*)'error writing stations.dat! Bad'
        ierror(29) = 1             ! error writing stations.dat
        close(7)
        go to 400
!
914     if(iw.eq.1)write(ifile,*)'no stations.dat found!'
        ierror(25) = 1             ! error opening stations.dat
        go to 400
         
400     continue
        close(7)
999     continue
        if(iw.eq.1)write(ifile,*)'End wrxmit'
        close(ifile)
        return
        end
!
!^***********************end  wrxmit  ***************************************^
!
        SUBROUTINE seas2s(ierror,nextdrop)
!08jun2015 if idep>999 set idep=idep-1000 -> since I only write depths
! as format(i3), so you'll have dep=999 & tem, then dep=001 & tem deep
! in file...
! 18aug2014- SRP-V2 format: NO: "Drop No:" so use nextdrop as adrop(1:3)
!      CAN delete ierror(34) (or use as something else as no longer relevant)
!          -also SRP-V2 Long is _080.777_W     (JB was _0080.777_W)
! 16aug2005 - only use 1st 7 chars of cruise name to read JB SRP file...
! read seas raw file, write sio s file:
! INPUT from SEAS:
!         nextdrop - is the actually drop number we just completed.   The drop I
!                     am currently translating.
! OUTPUT the usual ierror array...
!
! file numbers opened and closed in seas2s: 
!       14=SIO <nextdrop>s file (write)
!       22=control.dat (sub rdcntrl) (read)
!       23=SEAS <nextdrop>_SRP file (read)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=seas2s.log
!
! ierrors set in here:  looks like I'm clearing all at begin
!( 7) - error opening siodir.txt
!(15) - error opening control.dat
!(16) - error reading control.dat
!(17) - error reading siodir.txt
!(33) - rdcntrl - if operator name = "debug" turn on ierrlev=6 (fill .log files)
!DELETE, NO LONGER USED AS THIS: (34) - subroutine seas2s - idrop is NOT EQUAL to nextdrop
!(41) - seas2s - error opening SRP file
!(42) - seas2s - error reading SRP file
!(43) - seas2s - error, reached end of SRP file, should be more data
!(44) - ALL    - error opening calling routine's .log file (eg seas2s.log)
!(45) - ALL    - error writing calling routine's .log file (eg seas2s.log)
!(46) - seas2s - error opening SIO s file
!(47) - seas2s - error writing SIO s file
!   7,17 getdir
!  14,15,16,33 rdcntrl
!  34 seas2s
! need to add ierror for open & read srp file
!   41,42,43
! try adding err open & write seas2s.log
!   44,45
! err open & write sio s file
!   46, 47

        parameter (nlines=18)
        parameter (nerr=50)
        parameter (nlnchrs=12)
        parameter (maxtems=3000)

!GCC$ ATTRIBUTES DLLEXPORT :: seas2s

        integer*4 la(nlines)
        integer*4 ierror(nerr)
        integer*4 len_adir
        integer*4 nextdrop
        integer*4 item(maxtems)
! this is a relic - just setting isbn=0 - we should write something else...
        integer*4 isbn

        real*4 a, b, raw(maxtems)
        real*4 xlat, xlon
! this is a relic - just setting xcal=0 - we should write something else...
        real*4 xcal

        integer*2 j1, j2, j3, j4
        character*100 a100, ablank
        character aline(nlines)*34
        character*80 adir, aseas2s, astations
        character*3 adrop, adropsec*2, astatline*70
        character*1 alat
        character*1 alon
!lisa:
        character*80 adate
        integer*4 iw, ifile, ierrlev
        integer*4 igderr(3)

! rdcntrl:
        integer*4 len_acruise
        integer*4 launcher(nlnchrs)
        real*4 tdzmx,tdzrms,dtdzmn,dtdzth,dtmx,dtmx700
        real*4 tm_pl_mx, tm_pl_mn
        real*4 xmaxspd, deadmin, dropmin, relodmin
        character*7 acruise

        character*80 arawfile, asfile

        character*80 acontrol

! 05 Oct 2004 SEAS2K: sample header format:
!         SEAS Version:  6.12
!         Ship Name: Polynesia
!         Call Sign: V2CA2
!         Lloyds Number:  9127019
!         Date/Time(dd/mm/yyyy): 26/08/2004 15:49 GMT
!         Latitude(ddd.ddd): 027.591 N
!         Longitude(ddd.ddd): 0128.451 W
!         Drop No: 004
!         Probe Type: Sippican Deep Blue
!         Probe Code:  52
!         Acoeff:  6.691
!         Bcoeff:  -2.25
!         Probe Serial No: -000001
!         Recorder Type: Sippican MK-21
!         Recorder Code:  6
!         Bottom Depth: 9999 M
!         SEAS ID: 35609CC6
!        XBT: 1310
!
! Summer 2014 seas 9x sample header:
! SEAS Version:  9.20
! Ship Name: Horizon Navigator
! Call Sign: TEST
! Lloyds Number:  7116315
! Date/Time(dd/mm/yyyy): 08/09/2014 18:49 GMT
! Latitude(ddd.ddd): 025.735 N
! Longitude(ddd.ddd): 080.162 W
! Probe Type: Sippican Deep Blue
! Probe Code:  52
! Probe Serial No: -000001
! Recorder Type: Sippican MK-21
! Recorder Code:  6
! Bottom Depth:    2 M
! SEAS ID: B3D39764
! Ship Speed at Launch (knots):  0.00
! Ship Direction at Launch (Degrees):   55
! Sequence Number:   149
! Transect Number:    1
! Launch Height (Meters): 12.50
! SOOP Line: AX10
!
!
! use our OLD COEFFICIENTS:
        data a/0.00216/, b/6.472/
        data ibins/0/
        data isbn/0/
        data xcal/0.0/

        data  aline(1)/' SEAS Version: '/
        data  aline(2)/' Ship Name: '/
        data  aline(3)/' Call Sign: '/
        data  aline(4)/' Lloyds Number:  '/
        data  aline(5)/' Date/Time(dd/mm/yyyy): '/
        data  aline(6)/' Latitude(ddd.ddd): '/
        data  aline(7)/' Longitude(ddd.ddd): '/
        data  aline(8)/' Drop No: '/
        data  aline(9)/' Probe Type: '/
        data aline(10)/' Probe Code: '/
        data aline(11)/' Acoeff: '/
        data aline(12)/' Bcoeff: '/
        data aline(13)/' Probe Serial No: '/
        data aline(14)/' Recorder Type: '/
        data aline(15)/' Recorder Code: '/
        data aline(16)/' Bottom Depth: '/
        data aline(17)/' SEAS ID: '/
        data aline(18)/'XBT: '/
        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! blank out ablank(1:100)
        do 1 i=1,100
1        ablank(i:i) = ' '
!
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
        do 10 i = 1, nerr
           ierror(i) = 0
10        continue
!
        if(ierror(33).eq.6) then
!            this is "debug"
           ierrlev = 6
        endif

! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)

! if I can't open or read file 'siodir', assume current directory?
! 12sep2014 no, put in proper error exit:
! if I can't open(7)  or read(17) file siodir.txt containing 'xbtdirectory',
!   then exit out
        if(ierror(7).eq.1) then
           len_adir = 0 
! do not set yet, is this critical?           ierror(35) = 307
           go to 999
        endif
        if(ierror(17).eq.1) then
           len_adir = 0 
! do not set yet, is this critical?           ierror(35) = 317
           go to 999
        endif
!
        aseas2s  = ' '
        arawfile = ' '
        acontrol = ' '
        if(len_adir.gt.0) then
           aseas2s(1:len_adir) = adir(1:len_adir)
           arawfile(1:len_adir) = adir(1:len_adir)
           acontrol(1:len_adir) = adir(1:len_adir)
        endif
!
! this is basic one:
        aseas2s(len_adir+1:len_adir+15) = 'Data\seas2s.log'
! this is lab debugging one:
!         aseas2s(len_adir+1:len_adir+21) =  'Data\seas2s000000.log'
!                                            123456789012345678901
!                                                      1
!         CALL gettim(j1,j2,j3,j4)
!         if(j1.le.9) then
!            write(aseas2s(len_adir+13:len_adir+13),'(i1)') j1
!         elseif(j1.gt.9.and.j1.lt.99) then
!            write(aseas2s(len_adir+12:len_adir+13),'(i2)') j1
!         endif 
!         if(j2.le.9) then
!            write(aseas2s(len_adir+15:len_adir+15),'(i1)') j2
!         elseif(j2.gt.9.and.j2.lt.99) then
!            write(aseas2s(len_adir+14:len_adir+15),'(i2)') j2
!         endif
!         if(j3.le.9) then
!            write(aseas2s(len_adir+17:len_adir+17),'(i1)') j3
!         elseif(j3.gt.9.and.j3.lt.99) then
!            write(aseas2s(len_adir+16:len_adir+17),'(i2)') j3
!         endif

        acontrol(len_adir+1:len_adir+16) = 'Data\control.dat'
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=aseas2s,form='formatted',
     $             status='unknown',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
           write(ifile,*,err=335)'Inside seas2s: calling rdcntrl'
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           write(ifile,*)'incoming nextdrop=',nextdrop
           call flush(ifile)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif
! recall rdcntrl clear/set ierror: 14,15,16,33
        CALL rdcntrl(ierror,len_acruise,acruise,xmaxspd,launcher,
     $         deadmin,dropmin,relodmin,tdzmx,tdzrms,dtdzmn,dtdzth,
     $         dtmx,dtmx700,tm_pl_mn,tm_pl_mx,acontrol,ifile,
     $         ichkprofdepth)
!
! 12sep2014 add exit if error on control.dat:
        if(ierror(15).ne.0) then           ! error opening control.dat
! do not use yet              ierror(35) = 315
              go to 999
        elseif(ierror(16).ne.0) then           ! error reading control.dat
! do not use yet              ierror(35) = 316
              go to 999
        endif
! 16aug2005 set len_acruise <= 7...   
        if(len_acruise.gt.7) len_acruise = 7 
!
! acruise is cruise name, len_acruise is length of acruise <= 7
! so lets try to fill in entire name of raw file:

        arawfile(len_adir+1:len_adir+5) = 'Data\'
        arawfile(len_adir+6:len_adir+6+len_acruise) 
     $                                  = acruise(1:len_acruise)
        asfile(1:len_adir+6+len_acruise) = 
     $                            arawfile(1:len_adir+6+len_acruise)
!
! whoa 21oct2004 Seas added an "r".....
!                                                        123456789
! here is where they differ.  The raw file is "CruiseNamer_000.SRP"
! whereas our "s" file is                     "CruiseNames.000"
!                                     lenraw =          ^
        lenraw = len_adir+5+len_acruise
        arawfile(lenraw+1:lenraw+9) = 'r_000.SRP'
        asfile(lenraw+1:lenraw+5)   = 's.000'
! define adrop(1:3) = nextdrop since no "Drop No" in SRP V2:
        adrop(1:3)   = '000'
!
c now tack on nextdrop to get final arawfile name.   whew.
        if(nextdrop.le.9) then
           write(arawfile(lenraw+5:lenraw+5),'(i1)') nextdrop
           write(asfile(lenraw+5:lenraw+5),'(i1)') nextdrop
           write(adrop(3:3),'(i1)') nextdrop
        elseif(nextdrop.ge.10.and.nextdrop.le.99) then
           write(arawfile(lenraw+4:lenraw+5),'(i2)') nextdrop
           write(asfile(lenraw+4:lenraw+5),'(i2)') nextdrop
           write(adrop(2:3),'(i2)') nextdrop
        elseif(nextdrop.ge.100.and.nextdrop.le.999) then
           write(arawfile(lenraw+3:lenraw+5),'(i3)') nextdrop
           write(asfile(lenraw+3:lenraw+5),'(i3)') nextdrop
           write(adrop(1:3),'(i3)') nextdrop
        endif
! 
        if(iw.eq.1) then
         write(ifile,*) 'seas file=',arawfile(1:lenraw+9)
         write(ifile,*) 's file=',asfile(1:lenraw+5)
        endif
!
! 07mar2004 WHOA - NO GO ON READING seconds from stations.dat.
! Seas has not written the line to it yet...  So keep as 00...
! 04mar2005 LL - SRP file does not have the "seconds" (time) of the
! drop, so open and read stations.dat to get that.   IF failure to
! get this from stations.dat do not panic, just move on.
!        open(7,file=astations,form='formatted',status='old',
!     $       err=69)
!        if(iw.eq.1)write(ifile,*)' read in stations.dat:'
!        rewind(7)
!        do 105 i = 1, 1000
!         read(7,'(a70)',err=69) astatline(1:70)
!         if(ierrlev.ge.6.and.iw.eq.1) write(ifile,'(a70)') astatline(1:70)
!         if(astatline(1:3).eq.'END') go to 69
!         read(astatline,510,end=69,err=69) idropnum, idday, idhr,
!     $          idmin, adropsec(1:2)
!         if(iw.eq.1)write(ifile,*)'idropnum=',idropnum,' adropsec=',adropsec
!         if(idropnum.eq.nextdrop) go to 69
!510      format(1x,i3,14x,i2,7x,i2,1x,i2,1x,a2)
!105        continue
!69        continue
!        if(iw.eq.1)write(ifile,*)'adropsec=',adropsec
!        close(7)
!
! arawfile is "srp" file:
        open(23,file=arawfile,form='formatted',status='old',
     $          err=201)       ! 201 sets ierror(41)=1 and return
!
! read through Seas raw file:
        do 100 i = 1, 999
           a100(1:100) = ablank(1:100)            !18aug2014 blank out a100 each time
           read(23,'(a)',end=203,err=202) a100    ! 202 ier42=1 & rtn
!                                                 ! 203 ier43=1 & rtn
! aline(5)
           if(a100(1:4).eq.'Date'.or.a100(2:5).eq.'Date') then
!             1234567890123456789012345678901234567890
!SEAS fmt:   ' Date/Time(dd/mm/yyyy): 26/08/2004 15:49 GMT'
!S    fmt:   ' 29- 5-2004   3:51:12'
             adate(1:21) = ' 00-00-0000  00:00:00'
!                           123456789012345678901
! well this is stupid.   Not very robust...
             if(a100(1:1).eq.'D') then
                adate(2:3) = a100(24:25)
                adate(5:6) = a100(27:28)
                adate(8:11) = a100(30:33)
                adate(14:15) = a100(35:36)
                adate(17:18) = a100(38:39)
             elseif(a100(2:2).eq.'D') then
                adate(2:3) = a100(25:26)
                adate(5:6) = a100(28:29)
                adate(8:11) = a100(31:34)
                adate(14:15) = a100(36:37)
                adate(17:18) = a100(39:40)
             endif
           elseif(a100(1:4).eq.'Lati'.or.a100(2:5).eq.'Lati') then
! SEAS fmt   ' Latitude(ddd.ddd): 027.591 N'
! S    fmt   '  030      0   .000   34.02  226.00
! S    fmt     drop #               lat    lon
             if(a100(1:1).eq.'L') then
                read(a100(20:28),'(f7.3,1x,a1)') xlat, alat
!             if(iw.eq.1)write(ifile,*)'1 xlat=',xlat, ' alat=',alat
             elseif(a100(2:2).eq.'L') then
                read(a100(21:29),'(f7.3,1x,a1)') xlat, alat
!             if(iw.eq.1)write(ifile,*)'2 xlat=',xlat, ' alat=',alat
             endif
             if(alat(1:1).eq.'S'.or.alat(1:1).eq.'s') then
                xlat = -xlat
             endif
             if(iw.eq.1) write(ifile,*)'xlat=',xlat, ' alat=',alat
           elseif(a100(1:4).eq.'Long'.or.a100(2:5).eq.'Long') then
! SEAS V1      ' Longitude(ddd.ddd): 0128.451 W'
! SEAS V2      ' Longitude(ddd.ddd): 128.451 W'
!               123456789012345678901234567890
             if(a100(1:1).eq.'L') then
                read(a100(21:30),'(f7.3,1x,a1)') xlon, alon
                if(iw.eq.1)write(ifile,*)'1 xlon=',xlon, ' alon=',alon
             elseif(a100(2:2).eq.'L') then
                read(a100(22:31),'(f7.3,1x,a1)') xlon, alon
                if(iw.eq.1)write(ifile,*)'2 xlon=',xlon, ' alon=',alon
             endif
! convert to E longitude:
             if(alon(1:1).eq.'W'.or.alon(1:1).eq.'w') then
                xlon = 360.0 - xlon
             endif
             if(iw.eq.1)write(ifile,*) 'xlon=',xlon
!
!18aug2014           elseif(a100(1:4).eq.'Drop'.or.a100(2:5).eq.'Drop') then
!18aug2014! Get the drop number:
!18aug2014! SEAS       ' Drop No: 004'
!18aug2014              if(a100(1:4).eq.'Drop') then
!18aug2014                 read(a100(10:12),'(a3)') adrop(1:3)
!18aug2014              elseif(a100(2:5).eq.'Drop') then
!18aug2014                 read(a100(11:13),'(a3)') adrop(1:3)
!18aug2014              endif
!18aug2014              read(adrop(1:3),'(i3)') idrop
!18aug2014! this will never happen...
!18aug2014              if(idrop.ne.nextdrop) ierror(34) = 1
!
           elseif(a100(1:4).eq.'XBT:'.or.a100(2:5).eq.'XBT:') then
! then we've found the raw temperature data.   Decode.
              if(iw.eq.1)write(ifile,*)'found ', a100(1:5)
              if(a100(1:4).eq.'XBT:') then
!lisa FIX:
                    read(a100(6:9),'(i4)') numtems
              endif
              if(iw.eq.1)write(ifile,*)'numtems= ', numtems

! since we know on a full line there are twenty 5 digits tems per line,
!    find out how many lines to read:

              x = real(numtems)/20.0
              numtemlines = int(x)
              xremainder = x - real(numtemlines)
! if xremainder.eq.0 then numtemlines = exact number lines to read
! if xremainder.ne.0 then must read numtemlines + 1 where the extra has a few tems.
! figure out how many tems on "extra" one line:
              nremain = 0
              if(xremainder.ne.0.0) then
                 nremain = int(xremainder*20.0)
              endif
              if(iw.eq.1) then
              write(ifile,*)'x ','numtemlines ','xremainder ','nremain:'
              write(ifile,*)x, numtemlines, xremainder, nremain
              endif
! now read them:
              ii = 1
              do 90 k = 1, numtemlines
                 read(23,'(20i5)',end=203,err=202)(item(jj),jj=ii,ii+19)
                 if(ierrlev.eq.6.and.iw.eq.1) then
                  write(ifile,*) 'k=',k,' ',(item(jj),jj=ii,ii+19)
                 endif
                 ii = ii + 20
90              continue
              if(nremain.ne.0) then
                 if(iw.eq.1)write(ifile,*)'try reading last line:'
                 read(23,'(a)',end=203,err=202) a100
                 if(iw.eq.1)write(ifile,500)a100
500              format('a100=*',a100,'*')
! Now read that last "shorter" line of tems:
!            ij is point inside of line to start reading
                 ij = 1
                 do 92 jj = 1, nremain
                    read(a100(ij:ij+4),'(i5)') item(ii) 
!                    if(iw.eq.1)write(ifile,*)'item(',ii,')=',item(ii)
                    ij = ij + 5
                    ii = ii + 1
92               continue
              endif
! if we are here then we've read all the data, exit out of 100 loop
           go to 102
           endif

100        continue

102        continue
        close(23,iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*) 'close23SRPios=',ios

! we know we have item(numtems)
! convert to reals with decimal points (seas passes 5 digit int,
! so 23456 = 23.456 deg):
        do 130 i = 1, numtems
           raw(i) = real(item(i)) * .001
           if(ierrlev.eq.6.and.iw.eq.1) then
            write(ifile,*)'raw(',i,')=',raw(i)
           endif
130        continue

! open our raw "s" file:
        if(iw.eq.1)write(ifile,*)'open ',asfile(1:lenraw+5)

        open(14,file=asfile,status='unknown',
     $          form='formatted',err=204)  ! 204 ierr46=1 & rtn

! need to write header:
        write(14,362,err=205)adrop(1:3),isbn,xcal,xlat,xlon
362     format(2x,a3,i7,f7.3,2f8.2)
        write(14,364,err=205)adate(1:21)
364     format(a21)

! Here we need to put in the old code from dropmk12 - first it calculates
! the depth using old coefficients then bins into 2m bins!
! I know ibins is set in data stmt, but it does not reset in subroutine mode.
! whew - zero them all out!
        ibins = 0
        nbin = 0
        avt = 0.0

        do 310 j=1,numtems
        temp=raw(j)
        time=float(j)/10.
        depth=b*time-a*time*time
        ibin=ifix(depth/2.)+1
        if(ierrlev.eq.6.and.iw.eq.1) then
         write(ifile,*) 'temp,time,depth,ibin',temp, time, depth, ibin
        endif
        if(ibin.eq.ibins.or.ibins.eq.0)then
                nbin=nbin+1
                avt=avt+temp
        else
                avt=avt/float(nbin)
                iavt=nint(1000.*avt)
                idep=2*ibin-3
! 08jun2015 if idep>999, set so idep=idep-1000
                if(idep.gt.999) idep = idep - 1000
                write(14,'(i3,i6)',err=205)idep,iavt
                if(ibin.eq.350)t700=avt
                avt=temp
                nbin=1
        endif
        ibins=ibin
310     continue
!
        close(14,iostat=ios)
        if(iw.eq.1.and.ios.ne.0)write(ifile,*)'closed sio s file:',
     $              asfile(1:lenraw+5),' ,ios=',ios
!
        go to 300
!
201     continue     ! error opening raw srp file
        ierror(41) = 1
        if(iw.eq.1)write(ifile,*)'error opening raw file'
        go to 300
202     continue     ! error reading raw srp file
        ierror(42) = 1
        if(iw.eq.1)write(ifile,*)'error reading raw file'
        go to 300
203     continue     ! error, reached end of SRP, should be more data
        ierror(43) = 1
        if(iw.eq.1)write(ifile,*)'end of raw file'
        go to 300
204     continue     ! error opening sio s file
        ierror(46) = 1
        if(iw.eq.1)write(ifile,*)'error opening sio s file'
        go to 300
205     continue     ! error writing sio s file
        ierror(47) = 1
        if(iw.eq.1)write(ifile,*)'error writing sio s file'
        go to 300

300     continue
! 22jul2014 close the blasted srp file at end in case errors above:
        close(23)
! same for s file:
        close(14)
!
! 12sep2014 add 999 for exit out:
999     continue
! If ierror(35) is still = 0 when we get here we think we have
! run thru siobegin successfully, so set it = 2 on exit
!not used yet        if(ierror(35).eq.0) then
!not used yet           ierror(35) = 2
!not used yet        endif
!
        if(iw.eq.1) then
         do 1010 i = 1, nerr
1010       if(ierror(i).ne.0) write(ifile,*)'ierror(',i,')=',ierror(i)
         write(ifile,*)'closing seas2s.log now, bye!'
        endif
        close(ifile)
        return
        end
!
!^******************end  seas2s  *********************************************^
!
!23456789012345678901234567890123456789012345678901234567890123456789012
!        1         2         3         4         5         6         7
! 18jul2005 - new routine for running in Seas time plan - this
! just returns 'nextdrop' from stations.dat to seas. 
        SUBROUTINE SioTimeBegin(nextdrop,ierror)

        parameter(nerr=50)
!GCC$ ATTRIBUTES DLLEXPORT :: SioTimeBegin

        integer*4 ierror(nerr), nextdrop
        integer*4 len_adir, idrp, iedt, jnav
        integer*4 ixbt, ipxday, ipxhr, ipxmin, ipxsec
        real*4 pxlat, pxlon
        character*80 adir, asiotime, astations, aline*70
        integer*4 iw, ifile, ios, indx, i
        integer*2 j1, j2, j3, j4
        integer*4 igderr(3)
!
! file numbers opened and closed in siotimebegin:
!        7=stations.dat (read)
!       31=siodir.txt (sub getdir) (read)
! ifile=33=siotime.log (write)
!
! ierrors set in here:  looks like I'm clearing all at begin
!( 7) - error opening siodir.txt
!(17) - error reading siodir.txt
!(25) - error opening stations.dat
!(26) - error reading stations.dat
!(35) - iSIOErrWatchDog , set to 2 at end of program if all is well.
!                       set to 3XX where XX is above error number to alert
!                       an error was set, eg 307, 317, etc 325, 326
!(44) - ALL    - error opening calling routine's .log file (eg siotime.log)
!(45) - ALL    - error writing calling routine's .log file (eg siotime.log)

        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014 zero out error array:
        do 10 i = 1, nerr
           ierror(i) = 0
10      continue

! get seas2k path:
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open or read file 'siodir', assume current directory?
! error opening siodir.txt:
        if(ierror(7).eq.1) then
           len_adir = 0
           ierror(35) = 307
           go to 999
        endif
! error reading siodir.txt:
        if(ierror(17).eq.1) then
           len_adir = 0
           ierror(35) = 317
           go to 999
        endif
                    
        asiotime  = ' '
        astations = ' '
        if(len_adir.gt.0) then
           asiotime(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
        endif

        asiotime(len_adir+1:len_adir+16) = 'Data\siotime.log'
        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
!
! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=asiotime,form='formatted',
     $             status='unknown',err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then 
           write(ifile,*,err=335)'Inside siotimebegin: '
           call flush(ifile)
           if(igderr(1).ne.0) write(ifile,*)'igderr1ios=',igderr(1)
           if(igderr(2).ne.0) write(ifile,*)'igderr2ios=',igderr(2)
           if(igderr(3).ne.0) write(ifile,*)'igderr3ios=',igderr(3)
           go to 336
335        ierror(45) = 1
           iw = 0
336        continue
        endif
!
        if(iw.eq.1) then
         write(ifile,*) 'adir=',adir(1:len_adir)
         write(ifile,*) 'asiotime=',asiotime(1:len_adir+16)
         write(ifile,*) 'astations=',astations(1:len_adir+17)
         CALL gettim(j1,j2,j3,j4)
         write(ifile,*)'dos time before open stations.dat:',j1,j2,j3,j4
         call flush(ifile)
        endif

! nextdrop: telling seas2k what drop number is next in our sequence.
! will =1 for first drop, will = next drop number after last drop
! in stations.dat
! default to next drop # = 1
        nextdrop = 1
        open(7,file=astations,form='formatted',status='old',err=65,
     $         iostat=ios)
        go to 66

! 18aug2014 - no - go back to set ierror(25)=1 and exit! Let Ibis handle!
! 04mar2005 LL if error opening stations.dat, open as new file and write ENDDATA
! (prev I set ierror(25)=1 and exit, no longer doing that.)

65      if(iw.eq.1) write(ifile,*) 
     $   'Error opening stations.dat. Exiting, open 7 ios= ',ios
        ierror(25) = 1             ! error opening stations.dat
        ierror(35) = 325
        go to 999
!
66      continue     ! if we are here success opening stations.dat
        if(iw.eq.1)write(ifile,*)'Read in stations.dat-open 7 ios=',ios
        rewind(7,iostat=ios)
        if(iw.eq.1)write(ifile,*)'rewind 7 ios=',ios

        do 105 i = 1, 1000
         indx = i
         read(7,'(a70)',err=70,iostat=ios) aline(1:70)
         if(iw.eq.1)then
            write(ifile,'(a70)') aline(1:70)
            write(ifile,*)'read7ios=',ios
         endif
         if(aline(1:3).eq.'END') go to 69
         read(aline,507,end=69,err=70) ixbt, ipxday, ipxhr,
     $          ipxmin, ipxsec, pxlat, pxlon, idrp, iedt, jnav
507      format(1x,i3,14x,i2,7x,i2,1x,i2,1x,i2,2f9.3,2x,i2,3x,i2,1x,i5)
         if(iw.eq.1)write(ifile,507) ixbt, ipxday, ipxhr, ipxmin, 
     $          ipxsec,pxlat,pxlon,idrp, iedt, jnav
105     continue

69      continue
        close(7,iostat=ios)
        if(iw.eq.1)write(ifile,*)'1close7 ios=',ios

! value of indx here is the next drop number.  Pass it back to JB.
        nextdrop = indx
        if(iw.eq.1)write(ifile,*)'nextdrop=',nextdrop
!11jul2014 LL- if we are here, we read stations.dat successfully, so 
! we should skip to 999 and not go thru the 70=error reading stations.dat
        go to 999

! 70=error reading stations.dat
70      ierror(26) = 1             ! error reading stations.dat
        ierror(35) = 326

999     continue

! If ierror(35) is still = 0 when we get here we think we've
! run thru siobegin successfully, so set it = 2 on exit
        if(ierror(35).eq.0) then
           ierror(35) = 2
        endif
! close them all to be safe:
        close(7,iostat=ios)
        if(iw.eq.1)write(ifile,*)'2close7 ios=',ios
        if(iw.eq.1)write(ifile,*)'end siotimebegin'
        close(ifile)
        
        return
        end
!^********************   end  siotimebegin ***********************************^
        SUBROUTINE tstwrstn(ierror)
        parameter (nerr=50)

!GCC$ ATTRIBUTES DLLEXPORT :: tstwrstn
!
        integer*4 ierror(nerr)

        character adir*80, awrdrpstn*80, astations*80
        character chr4*4

        integer*4 iw, ifile, len_adir, indx, ios
        integer*2 j1, j2, j3, j4

        iw = 0                    ! iw=0 no write to log, iw=1 write to log
! 21jul2014: zero out error array:
! ?? IS ?? this is ok for ierror(35) ?because Gauge is sending me a 0?
        do 10 i = 1, nerr
           ierror(i) = 0
10        continue


! get seas2k path: (recall getdir clears ierror:7,17 and set if needed)
        CALL getdir(adir,len_adir,ierror,igderr)
! if I can't open or read file 'siodir.txt', assume current directory?
        if(ierror(7).eq.1.or.ierror(17).eq.1) then
           len_adir = 0
        endif
        awrdrpstn = ' '
        astations = ' '
        if(len_adir.gt.0) then
           awrdrpstn(1:len_adir) = adir(1:len_adir)
           astations(1:len_adir) = adir(1:len_adir)
        endif
! add Data\ to data files paths!!!
        astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
        awrdrpstn(len_adir+1:len_adir+23) = 'Data\tstwrstn000000.log'
!                                            12345678901234567890123
!                                                     1         2   
         CALL gettim(j1,j2,j3,j4)
         if(j1.le.9) then
            write(awrdrpstn(len_adir+15:len_adir+15),'(i1)') j1
         elseif(j1.gt.9.and.j1.lt.99) then
            write(awrdrpstn(len_adir+14:len_adir+15),'(i2)') j1
         endif
         if(j2.le.9) then
            write(awrdrpstn(len_adir+17:len_adir+17),'(i1)') j2
         elseif(j2.gt.9.and.j2.lt.99) then
            write(awrdrpstn(len_adir+16:len_adir+17),'(i2)') j2
         endif
         if(j3.le.9) then
            write(awrdrpstn(len_adir+19:len_adir+19),'(i1)') j3
         elseif(j3.gt.9.and.j3.lt.99) then
            write(awrdrpstn(len_adir+18:len_adir+19),'(i2)') j3
         endif

! iw=0 no write to log, iw=1 write to log
        iw = 0
        ifile = 33
        open(ifile,file=awrdrpstn,status='unknown',
     $             form='formatted',iostat=ios,err=333)
        iw = 1                   ! success open log file, set to write
        go to 334
333     ierror(44) = 1           ! error open log file, do not write
334     continue
        if(iw.eq.1) then
           write(ifile,*,err=335) 'BEGIN tstwrstn,ios=',ios 
           go to 336
335        ierror(45) = 1        ! error write log file, do not write
           iw = 0
336        continue
        endif

        if(iw.eq.1) then
           write(ifile,*) 'BEGIN tstwrstn, :'
           write(ifile,*) 'astations=',astations(1:len_adir+17)
           call flush(ifile)
        endif

! v---------begin    open and write to stations.dat ---------------------------v
        open(7,file=astations,status='old',err=914,iostat=ios)
        if(iw.eq.1) write(ifile,*)'found/opened stations.dat,ios=',ios
        rewind(7)
        go to 916       ! success opening stations.dat, go to 916 to read it

914     ierror(25) = 1            ! error opening stations.dat
        if(iw.eq.1) write(ifile,*)'cannot open stations.dat, exit'
        go to 950

916     continue     ! if here, opened stations.dat, try to read it:

! How many lines in stations.dat?
! 18aug2014 add err=911 to this read, do I care enough to set a flag????
        do 910 i=1,1000
           indx=i
           read(7,'(a4)',end=911,err=911,iostat=ios) chr4
           if(iw.eq.1)write(ifile,*)'rd stations.dat: ',chr4,' ios=',ios
           if(chr4.eq.'ENDD') go to 912
910     continue

! if we are here, no ENDDATA, no biggy, I hope.
911     indx = indx - 1
        if(iw.eq.1)write(ifile,*)'new indx=',indx
!
! 18aug2014 make sure indx >= 1 :
        if(indx.lt.1) then
           ierror(26) = 1             ! error reading stations.dat
           if(iw.eq.1) write(ifile,*)
     $        'Error reading stations.dat, indx<1, exiting'
           go to 950
        endif
!
! indx is the ENDDATA line, so we write new drop on indx line
! and then add ENDDATA at indx+1 lines...
912     if(iw.eq.1)then
            write(ifile,*)'indx=',indx
            write(ifile,*)'backspace(7):'
            call flush(ifile)
        endif
        backspace(7,err=366,iostat=ios)

        if(iw.eq.1) then
           write(ifile,*) 'Writing stations.dat: bkspc ios=', ios
           call flush(ifile)
        endif

! write line to stations.dat, already backspaced from ENDDATA:
        write(7,'(a7)',err=365,iostat=ios)'ENDDATA'
        if(iw.eq.1)write(ifile,*)'after writing stations.dat, ios=',ios
        go to 950
!
!18aug2014 if error writing stations.dat:
365     ierror(29)=1     ! error writing stations.dat, exit without writing drop!
        if(iw.eq.1)write(ifile,*)'Error writing stations.dat'
        go to 950
!
366     ierror(29)=1     ! error writing stations.dat, exit without writing drop!
        if(iw.eq.1)write(ifile,*)'Error backspacing stations.dat'
        go to 950
!
950     close(7,iostat=ios)
        if(iw.eq.1)then
           write(ifile,*)'Closed stations.dat,ios=',ios
           write(ifile,*)'END tstwrstn'
           call flush(ifile)
        endif
!
        close(ifile)
        return
        end

