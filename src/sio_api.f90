! src/sio_api.f90
! Thin DLL wrapper -- bare subroutines, no module statement.
! Seas2k calls siobegin once at startup, then sioloop each GPS tick.
! SioTimeBegin reads stations.dat and sets nextdrop.

! ======================================================================
!  siobegin -- one-time startup: read config, plan, nav, set nextdrop
! ======================================================================
  subroutine siobegin(deadmin, dropmin, relodmin, runsec, xmaxspd, &
       launcher, igps, xlat, xlatload, nplan, ibuf, &
       idsec2, ierrlev, alrmtime, ifirst, irollnav, &
       inav, ispec, dtime, yrday1, ierror, iaveflg, ispd, itime, &
       idayave, imonave, iyerave, icday1, iplandir, &
       speed, dir, timeave, vlat, vlon, &
       nlnchr, nextdrop, iplancnt, iwait, &
       chr, cmin, csec, cday, cmon, cyear, isio_skip_count)
    use sio_core
    use sio_io,      only: rdcntrl, getdir, chknav, getfilen, decodeplan, navopen
    use sio_nav,     only: ave, newpos, xbteta, interp, planinfo, chkall, chkbuf, chkwrite
    use sio_time,    only: gettim, getdat, dayofw, gettmtg, timetohms, yrdy, compare, findtime
    use sio_convert, only: ch2real, real2ch, int2ch, dec2deg, deg2dec, findspace, lev
    implicit none
    integer, parameter :: nerr = 50
!GCC$ ATTRIBUTES DLLEXPORT :: siobegin

    ! Arguments
    real,    intent(inout) :: deadmin, dropmin, relodmin, runsec, xmaxspd
    real,    intent(inout) :: xlat, xlatload(12), alrmtime, dtime, yrday1
    real,    intent(inout) :: speed, dir, timeave, vlat, vlon
    real,    intent(inout) :: chr, cmin, csec, cday, cmon, cyear
    integer, intent(inout) :: launcher(12), igps, nplan, ibuf
    integer, intent(inout) :: idsec2, ierrlev
    integer, intent(inout) :: ifirst, irollnav, inav, ispec(12)
    integer, intent(inout) :: ierror(nerr), iaveflg, ispd, itime
    integer, intent(inout) :: idayave, imonave, iyerave, icday1
    integer, intent(inout) :: iplandir, nlnchr, nextdrop, iplancnt, iwait
    integer, intent(inout) :: isio_skip_count

    ! Locals -- rdcntrl
    integer :: len_acruise, iSIOSpeedAveMin
    real    :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real    :: tm_pl_mn, tm_pl_mx
    character(len=7) :: acruise

    ! Locals -- file paths
    character(len=80) :: asio, anavtrk, aplan, astations
    character(len=80) :: adir, acontrol, afilen
    character(len=2)  :: adosday, adosmon
    character(len=4)  :: adosyear

    ! Locals -- nav file reading
    integer :: navday, navmon, navyear, navhr, navmin, navsec
    integer :: ilatdnav, ilondnav
    real    :: xlatmnav, xlonmnav, speednav, dirnav
    character(len=1) :: alathnav, alonhnav

    ! Locals -- plan.dat reading
    integer :: latd, latd1, ierrplan
    real    :: xlatm, xlatm1, xlat1
    character(len=1) :: alath, alath1, aplandir, ahemi
    character(len=3) :: aspec
    character(len=80) :: aplanline

    ! Locals -- stations.dat reading
    character(len=70) :: aline
    integer :: ixbt, ipxday, ipxhr, ipxmin, ipxsec, idrp, iedt, jnav
    real    :: pxlat, pxlon, pxlats, pxlons
    integer :: ipxdays, ipxhrs, ipxmins, ipxsecs, inavs, igooddrp
    integer :: indx

    ! Locals -- misc
    integer :: iw, ifile, ios, len_adir
    integer :: igderr(3)
    integer :: icday, icmon, icyear
    integer :: inavfile, linecnt, ilontest, iflg, i, j
    integer :: iphr, ipmn, ipsc, iyear
    integer :: idhr, idmin, idsec, id, im, iy, ih, imi, is
    integer :: len
    real    :: relodsec, dropsec, pxtime, x, dxlat, dxlon, xlon
    real    :: yrday2
    character(len=1) :: avlath, avlonh
    integer(2) :: j1, j2, j3, j4

    ! ---- Initialization ----
    speed = -0.00009
    dir = 0.0
    nplan = 0
    iplancnt = 0
    nextdrop = 0
    iw = 0
    ! fill xlatload with 999.0 for no value
    if (isio_skip_count < 0) isio_skip_count = 0
    xlatload = 999.0
    ierror = 0

    ! ---- Get seas2k path ----
    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
       len_adir = 0
       ierror(35) = 307
       ! -> cleanup
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if
    if (ierror(17) == 1) then
       len_adir = 0
       ierror(35) = 317
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if

    ! Initialize all path strings
    asio      = ' '
    astations = ' '
    aplan     = ' '
    anavtrk   = ' '
    acontrol  = ' '

    if (len_adir > 0) then
       asio(1:len_adir)      = adir(1:len_adir)
       astations(1:len_adir) = adir(1:len_adir)
       aplan(1:len_adir)     = adir(1:len_adir)
       anavtrk(1:len_adir)   = adir(1:len_adir)
       acontrol(1:len_adir)  = adir(1:len_adir)
    end if

    astations(len_adir+1:len_adir+17) = 'Data\stations.dat'
    acontrol(len_adir+1:len_adir+16)  = 'Data\control.dat'
    aplan(len_adir+1:len_adir+8)      = 'plan.dat'
    anavtrk(len_adir+1:len_adir+15)   = 'Data\navtrk.dat'
    asio(len_adir+1:len_adir+12)      = 'Data\sio.log'

    ! ---- Open log file ----
    iw = 0
    ifile = 33
    close(ifile, iostat=ios)
    open(ifile, file=asio, form='formatted', access='append', &
         status='unknown', iostat=ios)
    if (ios == 0) then
       iw = 1
    else
       ierror(44) = 1
    end if

    if (iw == 1) then
       write(ifile, *, iostat=ios) 'IN SIOBEGIN'
       if (ios /= 0) then
          ierror(45) = 1
          iw = 0
       else
          if (igderr(1) /= 0) write(ifile, *) 'igderr1ios=', igderr(1)
          if (igderr(2) /= 0) write(ifile, *) 'igderr2ios=', igderr(2)
          if (igderr(3) /= 0) write(ifile, *) 'igderr3ios=', igderr(3)
          write(ifile, *) 'adir=', adir(1:len_adir)
          write(ifile, *) 'asio=', asio(1:len_adir+12)
          write(ifile, *) 'astations=', astations(1:len_adir+17)
          write(ifile, *) 'acontrol=', acontrol(1:len_adir+16)
          write(ifile, *) 'aplan=', aplan(1:len_adir+8)
          write(ifile, *) 'anavtrk=', anavtrk(1:len_adir+15)
          write(ifile, *) 'incoming iwait=', iwait
          call flush(ifile)
       end if
    end if

    ! ---- Set defaults ----
    ierrlev = 0
    adosday = '00'
    ifirst = 0
    inav = 0
    iaveflg = 2
    ispd = 0
    itime = 0
    irollnav = 0
    ibuf = 0
    runsec = 0.0

    ! ---- Handle iwait flags ----
    if (iwait == 1) then
       if (iw == 1) write(ifile, *) 'iwait=', iwait, ' RUN NORMAL'
    else if (iwait == 2) then
       if (iw == 1) write(ifile, *) 'initally iwait=', iwait
       iwait = 1
       if (iw == 1) write(ifile, *) 'iwait=', iwait, ' RUN NORMAL'
    else if (iwait == 3) then
       if (iw == 1) write(ifile, *) 'initally iwait=', iwait
       iwait = 1
       if (iw == 1) write(ifile, *) 'iwait=', iwait, ' RUN NORMAL'
       if (iw == 1) write(ifile, *) 'set runsec to 300.0'
       runsec = 300.0
    else if (iwait == 4) then
       if (iw == 1) then
          write(ifile, *) 'iwait=', iwait, ' Send drop flag NOW'
          write(ifile, *) 'isio_skip_count=', isio_skip_count
       end if
       ierror(1) = 1
    else if (iwait == 5) then
       if (iw == 1) then
          write(ifile, *) 'iwait=', iwait, 'no water hit, skip'
          write(ifile, *) 'isio_skip_count=', isio_skip_count, ' positions'
       end if
    else if (iwait == 6) then
       if (iw == 1) write(ifile, *) 'iwait=', iwait, &
            ' RUN NORMAL-I think!Inc by 1 only'
    else if (iwait == 7) then
       if (iw == 1) write(ifile, *) 'iwait=', iwait, ' Send drop flag NOW'
       ierror(1) = 1
    else
       if (iw == 1) write(ifile, *) 'initally iwait=', iwait
       iwait = 1
       if (iw == 1) write(ifile, *) 'iwait=', iwait, ' RUN NORMAL'
    end if

    ! ---- Read control.dat ----
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
         deadmin, dropmin, relodmin, runsec, &
         tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
         tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
         len_adir, adir, iw, ifile)

    if (ierror(15) /= 0) then
       ierror(35) = 315
       call siobegin_cleanup(iw, ifile, ierror)
       return
    else if (ierror(16) /= 0) then
       ierror(35) = 316
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if

    if (ierror(33) == 6) ierrlev = 6

    if (iw == 1) then
       write(ifile, *) 'after rdcntrl'
       write(ifile, '(a8,a7)') 'acruise=', acruise
       write(ifile, *) 'xmaxspd=', xmaxspd
       write(ifile, *) 'deadmin=', deadmin, ' dropmin=', dropmin
       write(ifile, *) 'relodmin=', relodmin, ' tm_pl_mn=', tm_pl_mn
       write(ifile, *) 'launcher=', launcher
       call flush(ifile)
    end if

    relodsec = relodmin * 60.0
    dropsec  = 60.0 * dropmin
    nlnchr   = 12

    ! ---- Convert incoming date ----
    icyear = int(cyear)
    icmon  = int(cmon)
    icday  = int(cday)
    icday1 = icday
    if (iw == 1) write(ifile, *) 'cyr,cmn,cdy=', icyear, icmon, icday
    call flush(ifile)

    ! Check year
    if (real(icyear) < 2014.0) then
       ierror(13) = 1
       ierror(35) = 313
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if

    ! ---- Build date filename using make_dos_date helper ----
    call make_dos_date(icday, icmon, icyear, adosday, adosmon, adosyear)
    call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)
    if (iw == 1) write(ifile, *) 'afilen=', afilen(1:len_adir+15)
    call flush(ifile)

    ! ---- Read date.nav file ----
    linecnt = 0
    inavfile = 0
    navday = 0; navmon = 0; navyear = 0
    navhr = 0; navmin = 0; navsec = 0
    speednav = 0.0; dirnav = 0.0
    ilatdnav = 0; ilondnav = 0
    xlatmnav = 0.0; xlonmnav = 0.0
    alathnav = ' '; alonhnav = ' '

    open(10, file=afilen, form='formatted', status='unknown', iostat=ios)
    if (ios /= 0) then
       ! Error opening date.nav
       ierror(5) = 1
       ierror(34) = icday
       ierror(36) = icmon
       ierror(37) = icyear
       inavfile = 2
       if (iw == 1) then
          write(ifile, *) 'Error opening ', afilen(1:len_adir+15)
          write(ifile, *) 'use navtrk.dat for last known position'
       end if
    else
       ! Count valid lines
       do i = 1, 100000
          read(10, '(t33,i3)', iostat=ios) ilontest
          if (ios /= 0) exit
          if (ilontest >= 0 .and. ilontest <= 360) then
             linecnt = linecnt + 1
          end if
       end do
       rewind(10, iostat=ios)
       if (linecnt > 2) then
          do i = 1, linecnt - 1
             read(10, *, iostat=ios)
             if (ios /= 0) exit
          end do
       end if

       if (linecnt == 0) then
          ierror(2) = 1
          ierror(34) = icday
          ierror(36) = icmon
          ierror(37) = icyear
          inavfile = 2
          if (iw == 1) then
             write(ifile, *) 'Error reading ', afilen(1:len_adir+15)
             write(ifile, *) 'use navtrk.dat for last known position'
          end if
       else
          read(10, '(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,' // &
               'i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,1x,a1,4x,f6.2,f6.1)', &
               iostat=ios) &
               navday, navmon, navyear, navhr, navmin, navsec, &
               ilatdnav, xlatmnav, alathnav, ilondnav, xlonmnav, &
               alonhnav, speednav, dirnav
          if (ios /= 0) then
             ierror(2) = 1
             ierror(34) = icday
             ierror(36) = icmon
             ierror(37) = icyear
             inavfile = 2
             if (iw == 1) then
                write(ifile, *) 'Error reading ', afilen(1:len_adir+15)
                write(ifile, *) 'use navtrk.dat for last known position'
             end if
          else
             if (iw == 1) then
                write(ifile, *) 'last nav file entry:'
                write(ifile, '(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,' // &
                     'i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,1x,a1,4x,f6.2,f6.1)') &
                     navday, navmon, navyear, navhr, navmin, navsec, &
                     ilatdnav, xlatmnav, alathnav, ilondnav, xlonmnav, &
                     alonhnav, speednav, dirnav
                call flush(ifile)
             end if
             navyear = navyear + 2000
          end if
       end if
       close(10, iostat=ios)
    end if

    if (iw == 1) write(ifile, *) 'closing ', afilen(1:len_adir+15), 'ios= ', ios

    ! ---- Read navtrk.dat ----
    if (iw == 1) write(ifile, *) 'opening navtrk.dat:'
    open(15, file=anavtrk, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
       ! Error opening navtrk.dat -- fallback to nav file data
       ierror(23) = 1
       if (iw == 1) write(ifile, *) ' error opening navtrk.dat '
       if (inavfile /= 2) then
          idayave = navday
          imonave = navmon
          iyerave = navyear
          timeave = real(navhr*3600 + navmin*60 + navsec)
          call deg2dec(ilatdnav, xlatmnav, alathnav, vlat)
          call deg2dec(ilondnav, xlonmnav, alonhnav, vlon)
          speed = speednav
          dir = dirnav
          if (iw == 1) write(ifile, *) ' using nav file pos as fallback'
       end if
       ! Skip to post-navtrk section (label 266 equivalent)
    else
       read(15, '(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,2f8.3,f6.2,f7.2)', &
            iostat=ios) idayave, imonave, iyear, iphr, ipmn, ipsc, &
            vlat, vlon, speed, dir
       close(15, iostat=ios)
       if (ios /= 0) then
          ! Error reading navtrk.dat -- fallback
          ierror(24) = 1
          if (iw == 1) write(ifile, *) ' error reading navtrk.dat '
          if (inavfile /= 2) then
             idayave = navday
             imonave = navmon
             iyerave = navyear
             timeave = real(navhr*3600 + navmin*60 + navsec)
             call deg2dec(ilatdnav, xlatmnav, alathnav, vlat)
             call deg2dec(ilondnav, xlonmnav, alonhnav, vlon)
             speed = speednav
             dir = dirnav
             if (iw == 1) write(ifile, *) ' using nav file pos as fallback'
          end if
       else
          if (iw == 1) then
             write(ifile, *) 'closing navtrk.dat, ios= ', ios
             write(ifile, *) 'read in from navtrk.dat:'
             write(ifile, '(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,2f8.3,f6.2,f7.2)') &
                  idayave, imonave, iyear, iphr, ipmn, ipsc, vlat, vlon, speed, dir
             call flush(ifile)
          end if
          timeave = real(iphr*3600 + ipmn*60 + ipsc)
          iyerave = iyear + 2000
          if (iw == 1) write(ifile, *) 'iyerave=', iyerave

          ! Set avlath
          if (vlat >= 0.0) then
             avlath = 'N'
          else
             avlath = 'S'
          end if

          ! Decide which position to use as most recent
          if (inavfile /= 2) then
             if (speednav == 0.0 .or. dirnav == 0.0) then
                if (iw == 1) then
                   write(ifile, *) 'Using position in navtrk.dat for DR since no'
                   write(ifile, *) 'speed or direction in last nav file entry   '
                end if
                ! Fall through to label-266 equivalent (skip compare)
             else
                call compare(navday, navmon, navyear, navhr, navmin, navsec, &
                     idayave, imonave, iyerave, iphr, ipmn, ipsc, iflg)
                if (iflg == 1) then
                   idayave = navday
                   imonave = navmon
                   iyerave = navyear
                   if (iw == 1) then
                      write(ifile, *) 'Using last nav file entry to DR from!!!'
                      write(ifile, *) idayave, imonave, iyerave
                   end if
                   timeave = real(navhr*3600 + navmin*60 + navsec)
                   call deg2dec(ilatdnav, xlatmnav, alathnav, vlat)
                   call deg2dec(ilondnav, xlonmnav, alonhnav, vlon)
                   speed = speednav
                   dir = dirnav
                end if
             end if
          end if

          if (iw == 1) then
             write(ifile, '(a,i2,a,i2,a,i4,1x,f9.2,2f8.2,f6.2,f7.2,a)') &
                  'Using: ', idayave, '/', imonave, '/', iyerave, timeave, vlat, &
                  vlon, speed, dir, ' to DR from'
          end if
       end if
    end if

    ! ---- Label 266 equivalent: check speed for DR ----
    if (speed /= -0.00009) then
       iaveflg = 1
       ispd = 1
    end if

    ! ---- Get dos time ----
    call gettim(j1, j2, j3, j4)
    idhr  = int(j1)
    idmin = int(j2)
    idsec = int(j3)
    dtime = real(idhr*3600 + idmin*60 + idsec)
    idsec2 = int(csec)
    if (iw == 1) write(ifile, '(a,i2,a,i2,a,i2,a,i2,a,i6)') &
         ' pc h:m:s ', idhr, ':', idmin, ':', idsec, '.', int(j4), ' dtime=', int(dtime)

    ! ---- Read plan.dat ----
    if (iw == 1) write(ifile, *) 'opening plan.dat:'
    open(13, file=aplan, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
       ierror(19) = 1
       ierror(35) = 319
       if (iw == 1) write(ifile, *) ' ERROR opening plan.dat! '
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if

    ! Skip 4 header lines
    do i = 1, 4
       read(13, *, iostat=ios)
       if (ios /= 0) then
          ierror(20) = 1
          ierror(35) = 320
          if (iw == 1) write(ifile, *) ' ERROR reading plan.dat! '
          close(13, iostat=ios)
          call siobegin_cleanup(iw, ifile, ierror)
          return
       end if
    end do

    ! Read first 2 positions to determine direction
    read(13, '(a)', iostat=ios) aplanline
    if (ios /= 0) then
       ierror(20) = 1; ierror(35) = 320
       if (iw == 1) write(ifile, *) ' ERROR reading plan.dat! '
       close(13, iostat=ios)
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if
    call decodeplan(aplanline, latd, xlatm, alath, ierrplan, ispec(1))

    read(13, '(a)', iostat=ios) aplanline
    if (ios /= 0) then
       ierror(20) = 1; ierror(35) = 320
       if (iw == 1) write(ifile, *) ' ERROR reading plan.dat! '
       close(13, iostat=ios)
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if
    call decodeplan(aplanline, latd1, xlatm1, alath1, ierrplan, ispec(2))

    rewind(13, iostat=ios)
    ! Reposition to line 5 (first position)
    do i = 1, 4
       read(13, *, iostat=ios)
       if (ios /= 0) then
          ierror(20) = 1; ierror(35) = 320
          close(13, iostat=ios)
          call siobegin_cleanup(iw, ifile, ierror)
          return
       end if
    end do

    call deg2dec(latd, xlatm, alath, xlat)
    call deg2dec(latd1, xlatm1, alath1, xlat1)
    if (iw == 1) then
       write(ifile, *) 'plan pos1:latd ,xlatm ,alath =', latd, xlatm, alath, ' = ', xlat
       write(ifile, *) 'plan pos2:latd1,xlatm1,alath1=', latd1, xlatm1, alath1, ' = ', xlat1
       call flush(ifile)
    end if

    if (xlat == xlat1) then
       ierror(22) = 1
       ierror(35) = 322
       if (iw == 1) write(ifile, *) 'first 2 positions in plan.dat are equal, not good'
       close(13, iostat=ios)
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if

    ! Figure out aspec, ispec(1), ahemi, aplandir, iplandir
    call planinfo(xlat, alath, xlat1, alath1, aspec, ispec(1), iplandir, vlat, vlon)
    ! Derive ahemi from alath for hemisphere comparisons
    ahemi = alath
    ! Convert iplandir to aplandir
    if (iplandir == 1) then
       aplandir = 'N'
    else if (iplandir == 2) then
       aplandir = 'E'
    else if (iplandir == 3) then
       aplandir = 'S'
    else if (iplandir == 4) then
       aplandir = 'W'
    end if

    if (iw == 1) then
       write(ifile, *) 'aplandir=', aplandir, ' iplandir=', iplandir
       write(ifile, *) 'aspec=', aspec, ' ispec(1)=', ispec(1)
       call flush(ifile)
    end if

    ! ---- Read stations.dat ----
    igooddrp = 0
    pxlats = 0.0; pxlons = 0.0
    ipxdays = 0; ipxhrs = 0; ipxmins = 0; ipxsecs = 0; inavs = 0
    if (iw == 1) then
       call gettim(j1, j2, j3, j4)
       write(ifile, *) 'dos time before open stations.dat:', j1, j2, j3, j4
       call flush(ifile)
    end if

    nextdrop = 1
    open(7, file=astations, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
       ierror(25) = 1
       ierror(35) = 325
       if (iw == 1) write(ifile, *) ' ERROR opening stations.dat! '
       close(13, iostat=ios)
       call siobegin_cleanup(iw, ifile, ierror)
       return
    end if

    if (iw == 1) write(ifile, *) ' rewind & read stations.dat:'
    rewind(7, iostat=ios)

    do i = 1, 1000
       indx = i
       read(7, '(a70)', iostat=ios) aline(1:70)
       if (ios /= 0) then
          ! Error reading stations.dat
          ierror(26) = 1
          ierror(35) = 326
          if (iw == 1) write(ifile, *) ' ERROR reading stations.dat! '
          close(7, iostat=ios)
          close(13, iostat=ios)
          call siobegin_cleanup(iw, ifile, ierror)
          return
       end if
       if (iw == 1 .and. ierrlev == 6) write(ifile, '(a70)') aline(1:70)
       if (aline(1:3) == 'END') exit
       read(aline, '(1x,i3,14x,i2,7x,i2,1x,i2,1x,i2,2f9.3,2x,i2,3x,i2,1x,i5)', &
            iostat=ios) ixbt, ipxday, ipxhr, ipxmin, ipxsec, pxlat, pxlon, idrp, iedt, jnav
       if (ios /= 0) then
          ierror(26) = 1
          ierror(35) = 326
          if (iw == 1) write(ifile, *) ' ERROR reading stations.dat! '
          close(7, iostat=ios)
          close(13, iostat=ios)
          call siobegin_cleanup(iw, ifile, ierror)
          return
       end if
       if (pxlon >= 360.0) pxlon = pxlon - 360.0
       if (iw == 1) write(ifile, '(1x,i3,14x,i2,7x,i2,1x,i2,1x,i2,2f9.3,2x,i2,3x,i2,1x,i5)') &
            ixbt, ipxday, ipxhr, ipxmin, ipxsec, pxlat, pxlon, idrp, iedt, jnav
       ! Save last drop info
       if (idrp == 1 .or. idrp == -1 .or. idrp == -3) then
          pxlats  = pxlat
          pxlons  = pxlon
          ipxdays = ipxday
          ipxhrs  = ipxhr
          ipxmins = ipxmin
          ipxsecs = ipxsec
          inavs   = jnav
          igooddrp = 1
       end if
    end do

    ! indx is the next drop number
    nextdrop = indx
    if (iw == 1) write(ifile, *) 'nextdrop=', nextdrop, ' igooddrp=', igooddrp

    ! Stop autolauncher dumping
    yrday1 = -1
    if (indx >= 3) then
       backspace(7)
       backspace(7)
       backspace(7)
       read(7, '(18x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            id, im, iy, ih, imi, is
       if (ios /= 0) then
          ierror(26) = 1
          ierror(35) = 326
          if (iw == 1) write(ifile, *) ' ERROR reading stations.dat! '
          close(7, iostat=ios)
          close(13, iostat=ios)
          call siobegin_cleanup(iw, ifile, ierror)
          return
       end if
       call yrdy(iy, im, id, ih, imi, is, yrday1)
    end if
    close(7, iostat=ios)

    ! ---- Find next drop in plan.dat ----
    if (igooddrp == 0) then
       ! Very first drop
       read(13, '(a)', iostat=ios) aplanline
       if (ios /= 0) then
          ierror(20) = 1; ierror(35) = 320
          if (iw == 1) write(ifile, *) ' ERROR reading plan.dat! '
          close(13, iostat=ios)
          call siobegin_cleanup(iw, ifile, ierror)
          return
       end if
       call decodeplan(aplanline, latd, xlatm, alath, ierrplan, ispec(1))
       if (iw == 1) write(ifile, *) 'igooddrp=0,aft decodeplan=', &
            latd, xlatm, alath, ispec(1)
       iplancnt = 1
       call deg2dec(latd, xlatm, alath, xlat)
       dropmin = -1.0
       if (iw == 1) write(ifile, *) 'set dropmin negative:', dropmin
    else
       ! Find next position in plan.dat past last good drop
       xlon = xlat  ! initialize xlon in case aspec is 'lon'
       do j = 1, 1000
          iplancnt = j
          read(13, '(a)', iostat=ios) aplanline
          if (ios < 0) then
             ! End of plan.dat
             ierror(21) = 1
             if (iw == 1) write(ifile, *) ' end plan.dat,hopefully not bad '
             exit
          end if
          if (ios > 0) then
             ierror(20) = 1; ierror(35) = 320
             if (iw == 1) write(ifile, *) ' ERROR reading plan.dat! '
             close(13, iostat=ios)
             call siobegin_cleanup(iw, ifile, ierror)
             return
          end if
          call decodeplan(aplanline, latd, xlatm, alath, ierrplan, ispec(1))
          if (iw == 1) write(ifile, *) 'aft decodeplan=', latd, xlatm, alath, ispec(1)
          call deg2dec(latd, xlatm, alath, xlat)

          ! Check for hemisphere change in plan.dat
          if (alath /= ahemi) then
             if (iw == 1) write(ifile, *) 'alath.ne.ahemi', alath, ahemi
             ! Check if it's just a case difference or E/W <-> S/N swap
             if (.not. siobegin_same_hemi(alath, ahemi)) then
                ! Direction change in plan.dat -- read next position
                read(13, '(a)', iostat=ios) aplanline
                if (ios < 0) then
                   ierror(21) = 1
                   if (iw == 1) write(ifile, *) ' end plan.dat,hopefully not bad '
                   exit
                end if
                if (ios > 0) then
                   ierror(20) = 1; ierror(35) = 320
                   close(13, iostat=ios)
                   call siobegin_cleanup(iw, ifile, ierror)
                   return
                end if
                call decodeplan(aplanline, latd1, xlatm1, alath1, ierrplan, ispec(1))
                backspace(13, iostat=ios)
                call deg2dec(latd1, xlatm1, alath1, xlat1)
                call planinfo(xlat, alath, xlat1, alath1, aspec, ispec(1), &
                     iplandir, vlat, vlon)
                ahemi = alath
                if (iplandir == 1) then
                   aplandir = 'N'
                else if (iplandir == 2) then
                   aplandir = 'E'
                else if (iplandir == 3) then
                   aplandir = 'S'
                else if (iplandir == 4) then
                   aplandir = 'W'
                end if
                if (iw == 1) then
                   write(ifile, *) 'CHANGED DIR IN PLAN.DAT!:'
                   write(ifile, *) 'aplandir=', aplandir, xlat, xlat1
                   write(ifile, *) 'iplandir=', iplandir
                   write(ifile, *) 'aspec=', aspec, 'ispec(1)=', ispec(1)
                end if
             end if
          end if

          ! Check if we are past this position
          if (aspec == 'lat') then
             dxlat = pxlats - xlat
             if (aplandir == 'N') then
                if (dxlat >= -0.01) cycle
             else
                if (dxlat <= 0.01) cycle
             end if
          end if

          if (aspec == 'lon') then
             dxlon = pxlons - xlat
             if (iw == 1) then
                write(ifile, *) 'pxlons-xlat=dxlon ', pxlons, '-', xlat, '=', dxlon
             end if
             xlon = xlat
             if (iw == 1) write(ifile, *) 'set xlon=xlat, xlon=', xlon

             if (aplandir == 'E') then
                if (pxlons >= 340.0 .and. xlon < 50.0) then
                   dxlon = pxlons - (360.0 + xlon)
                   if (iw == 1) write(ifile, *) 'E at 0 dxlon=', dxlon
                end if
                if (dxlon >= -0.01) cycle
                if (pxlons <= 50.0 .and. xlon > 50.0) cycle
             else if (aplandir == 'W') then
                if (dxlon > 300.0) then
                   if (pxlons > 300.0 .and. xlon < 50.0) then
                      dxlon = pxlons - (360.0 + xlon)
                      if (iw == 1) write(ifile, *) 'W at 0 dxlon=', dxlon
                   end if
                end if
                if (pxlons < 50.0 .and. xlon > 300.0) then
                   dxlon = (360.0 + pxlons) - xlon
                   if (iw == 1) write(ifile, *) 'W at 0 dxlon=', dxlon
                end if
                if (dxlon <= 0.01) cycle
             end if
          end if

          ! Found next drop position
          if (iw == 1) write(ifile, *) 'Found next drop:', xlon
          exit
       end do
    end if

    if (iw == 1) write(ifile, *) 'iplancnt=', iplancnt

    ! ---- Handle iwait skip logic ----
    if (iwait == 1) then
       if (iw == 1 .and. isio_skip_count /= 0) write(ifile, *) &
            'WARNING: iwait=1 but isio_skip_count=', isio_skip_count, ' - ignoring'
    else if (iwait == 4) then
       if (isio_skip_count > 0 .and. isio_skip_count < 1000) &
            iplancnt = iplancnt + isio_skip_count
    else if (iwait == 5) then
       if (isio_skip_count > 0 .and. isio_skip_count < 1000) then
          do i = 1, isio_skip_count
             read(13, '(a)', iostat=ios) aplanline
             if (ios < 0) then
                ierror(21) = 1
                if (iw == 1) write(ifile, *) ' end plan.dat,hopefully not bad '
                exit
             end if
             if (ios > 0) then
                ierror(20) = 1; ierror(35) = 320
                close(13, iostat=ios)
                call siobegin_cleanup(iw, ifile, ierror)
                return
             end if
             call decodeplan(aplanline, latd, xlatm, alath, ierrplan, ispec(1))
             iplancnt = iplancnt + 1
             if (iw == 1) then
                write(ifile, *) 'aft decodeplan=', latd, xlatm, alath, ispec(1)
                write(ifile, *) isio_skip_count, ' iwait=5,readin:', latd, &
                     xlatm, alath, ' iplancnt=', iplancnt
             end if
          end do
          call deg2dec(latd, xlatm, alath, xlat)
          if (aspec == 'lon') xlon = xlat
       end if
    else if (iwait == 6) then
       if (isio_skip_count > 0 .and. isio_skip_count < 1000) &
            iplancnt = iplancnt + isio_skip_count
    else if (iwait == 7) then
       if (isio_skip_count > 0 .and. isio_skip_count < 1000) &
            iplancnt = iplancnt + isio_skip_count
       if (iw == 1) write(ifile, *) 'since iwait=7, idrp= ', idrp
    end if

    if (iw == 1) then
       write(ifile, *) 'final iplancnt = ', iplancnt
       call flush(ifile)
       write(ifile, *) ' loaded probe drop positions='
    end if

    ! ---- Read in drops matching loaded probes ----
    xlatload(1) = xlat
    if (xlatload(1) > 180.0 .and. xlatload(1) < 360.0) then
       xlatload(1) = -1.0 * (360.0 - xlatload(1))
    else if (xlatload(1) == 360.0) then
       xlatload(1) = 0.0
    else if (xlatload(1) > 360.0) then
       xlatload(1) = 360.0 - xlatload(1)
    end if
    if (iw == 1) write(ifile, *) 'xlatload    1 ', xlatload(1)

    do i = 1, nlnchr - 1
       read(13, '(a)', iostat=ios) aplanline
       if (ios /= 0) exit   ! end of plan.dat is ok
       call decodeplan(aplanline, latd1, xlatm1, alath1, ierrplan, ispec(i+1))
       if (iw == 1 .and. ierrlev >= 6) write(ifile, *) 'aft decodeplan=', &
            latd1, xlatm1, alath1, ispec(i+1)
       call deg2dec(latd1, xlatm1, alath1, xlatload(i+1))

       if (xlatload(i+1) > 180.0 .and. xlatload(i+1) < 360.0) then
          xlatload(i+1) = -1.0 * (360.0 - xlatload(i+1))
       else if (xlatload(i+1) == 360.0) then
          xlatload(i+1) = 0.0
       else if (xlatload(i+1) > 360.0) then
          xlatload(i+1) = 360.0 - xlatload(i+1)
       end if

       if (iw == 1) write(ifile, '(a,i4,f8.3,a,i3,f6.2,a2,i4)') &
            'xlatload ', i+1, xlatload(i+1), '=', latd1, xlatm1, alath1, ispec(i+1)
       nplan = i
    end do

    close(13, iostat=ios)
    if (iw == 1) then
       write(ifile, *) '1close13ios=', ios
       write(ifile, *) 'nplan =', nplan
       write(ifile, *) 'dropmin=', dropmin, ' igooddrp=', igooddrp
    end if

    ! ---- Set alarm timer ----
    dropsec = 60.0 * dropmin
    alrmtime = dropsec

    if (dropmin > 0.0) then
       if (igooddrp == 1) then
          pxtime = real(ipxhrs*3600 + ipxmins*60 + ipxsecs)
          if (iw == 1 .and. ierrlev == 6) then
             write(ifile, *) 'icday,ipxdays', icday, ipxdays
             write(ifile, *) 'pxtime,ipxhrs,ipxmins,ipxsecs=', &
                  pxtime, ipxhrs, ipxmins, ipxsecs
          end if
          if (icday == ipxdays) then
             alrmtime = dropsec - (dtime - pxtime)
          else
             alrmtime = dropsec - ((86400.0 - pxtime) + dtime)
             if (alrmtime <= 0.0) alrmtime = dropsec
          end if
       else
          alrmtime = dropsec
       end if
    end if

    if (alrmtime <= 0.0) then
       alrmtime = dropsec
       if (iw == 1 .and. ierrlev == 6) then
          write(ifile, *) 'alrmtime<0, reset to dropsec ', dropsec
          write(ifile, *) '|->This is likely due to pc time ne gmt'
       end if
    end if

    ! ---- Final cleanup ----
    call siobegin_cleanup(iw, ifile, ierror)
    return

  contains

    ! Cleanup helper: set watchdog, close units, write log trailer
    subroutine siobegin_cleanup(iw_arg, ifile_arg, ierror_arg)
      integer, intent(in)    :: iw_arg, ifile_arg
      integer, intent(inout) :: ierror_arg(nerr)
      integer :: ios_l, i_l

      if (ierror_arg(35) == 0) ierror_arg(35) = 2

      close(13, iostat=ios_l)
      if (iw_arg == 1) write(ifile_arg, *) 'close13ios=', ios_l

      if (iw_arg == 1) then
         write(ifile_arg, '(a,f6.2,a,f7.2)') 'spd=', speed, ' dir=', dir
         write(ifile_arg, *) int(deadmin), int(dropmin), int(relodmin), &
              ' runsec=', int(runsec)
         write(ifile_arg, *) 'igps=', igps, 'xlat=', xlat, 'ibuf=', ibuf
         write(ifile_arg, *) 'idsec2=', idsec2, ' ierrlev=', ierrlev, &
              'alrmtime=', alrmtime
         write(ifile_arg, *) 'ifirst=', ifirst, ' irollnav=', irollnav
         write(ifile_arg, *) inav, yrday1, iaveflg, ispd, itime
         write(ifile_arg, *) 'date ave=', idayave, imonave, iyerave
         write(ifile_arg, *) 'vlat=', vlat, ' vlon=', vlon
         call flush(ifile_arg)
         write(ifile_arg, *) 'errors:'
         do i_l = 1, nerr
            if (ierror_arg(i_l) /= 0) write(ifile_arg, *) 'ierror(', i_l, ')=', ierror_arg(i_l)
         end do
         write(ifile_arg, *) 'LEAVING SIOBEGIN,timeave=', timeave
         call flush(ifile_arg)
      end if
      close(ifile_arg, iostat=ios_l)
    end subroutine siobegin_cleanup

    ! Check if two hemisphere characters are equivalent (case or E/W, N/S swaps)
    logical function siobegin_same_hemi(a, b)
      character(len=1), intent(in) :: a, b
      character(len=1) :: au, bu
      au = a; bu = b
      ! Uppercase both
      if (au >= 'a' .and. au <= 'z') au = char(ichar(au) - 32)
      if (bu >= 'a' .and. bu <= 'z') bu = char(ichar(bu) - 32)
      siobegin_same_hemi = .false.
      ! Same letter (case insensitive)
      if (au == bu) then
         siobegin_same_hemi = .true.
         return
      end if
      ! E/W swap or N/S swap -- treat as no real direction change
      if ((au == 'W' .and. bu == 'E') .or. (au == 'E' .and. bu == 'W')) then
         siobegin_same_hemi = .true.
         return
      end if
      if ((au == 'N' .and. bu == 'S') .or. (au == 'S' .and. bu == 'N')) then
         siobegin_same_hemi = .true.
         return
      end if
    end function siobegin_same_hemi

  end subroutine siobegin


! ======================================================================
!  sioloop -- main GPS tick handler: position, DR, ETA, nav writes
! ======================================================================
  subroutine sioloop(deadmin, dropmin, relodmin, runsec, xmaxspd, &
       launcher, igps, xlat, xlatload, nplan, ibuf, &
       idsec2, ierrlev, alrmtime, ifirst, irollnav, &
       inav, ispec, dtime, yrday1, ierror, iaveflg, ispd, itime, &
       idayave, imonave, iyerave, icday1, iplandir, &
       speed, dir, timeave, vlat, vlon, &
       icday, icmon, icyear, istat, &
       gpssec, chrsav, icsec1, ctagbuf, clatbuf, clonbuf, &
       iupdate, clatd, clatm, iclath, clond, clonm, iclonh, &
       chr, cmin, csec, cday, cmon, cyear, nlnchr, eta, &
       drlat, drlon, iSIOSpeedAveMin)
    use sio_core
    use sio_io,      only: rdcntrl, getdir, chknav, getfilen, decodeplan, navopen
    use sio_nav,     only: ave, newpos, xbteta, interp, planinfo, chkall, chkbuf, chkwrite
    use sio_time,    only: gettim, getdat, dayofw, gettmtg, timetohms, yrdy, compare, findtime
    use sio_convert, only: ch2real, real2ch, int2ch, dec2deg, deg2dec, findspace, lev
    implicit none
    integer, parameter :: nerr = 50
!GCC$ ATTRIBUTES DLLEXPORT :: sioloop

    ! Arguments
    real,    intent(inout) :: deadmin, dropmin, relodmin, runsec, xmaxspd
    real,    intent(inout) :: xlat, xlatload(12), alrmtime, dtime, yrday1
    real,    intent(inout) :: speed, dir, timeave, vlat, vlon
    real,    intent(inout) :: gpssec, chrsav
    real,    intent(inout) :: ctagbuf(200), clatbuf(200), clonbuf(200)
    real,    intent(inout) :: clatd, clatm, clond, clonm
    real,    intent(inout) :: chr, cmin, csec, cday, cmon, cyear
    real,    intent(inout) :: eta(12)
    real,    intent(inout) :: drlat, drlon
    integer, intent(inout) :: launcher(12), igps, nplan, ibuf
    integer, intent(inout) :: idsec2, ierrlev
    integer, intent(inout) :: ifirst, irollnav, inav, ispec(12)
    integer, intent(inout) :: ierror(nerr), iaveflg, ispd, itime
    integer, intent(inout) :: idayave, imonave, iyerave, icday1
    integer, intent(inout) :: iplandir, nlnchr
    integer, intent(inout) :: icday, icmon, icyear, istat
    integer, intent(inout) :: icsec1, iupdate, iclath, iclonh
    integer, intent(inout) :: iSIOSpeedAveMin

    ! Locals
    real    :: xlatload360(12)
    real    :: stoptime, clat, clon, timetag
    real    :: s, d, x, ctime, gpstime, xalarm, change
    real    :: dtime1, gpssec1, check
    real    :: deadsec, relodsec, dxlat, dxlon, xlon
    real    :: dist, dxlatnm, dxlonnm
    real    :: vlat1, vlon1, vlat_prev, vlon_prev, yrday2
    real    :: deg2rad
    save       stoptime

    integer :: iw, ifile, ios, len_adir
    integer :: igderr(3)
    integer :: ihr, imin, isec, ierr, iderr
    integer :: i1, icyr, imo, iday, idhr, idmin, idsec, jchange
    integer :: jpos, len, iiyergps, iiyerave, iweekday
    integer :: nobuf, icsec, idchange, idirck, ix, i
    integer :: ierrwrite
    integer :: ivlatd, ivlond
    integer :: icmon1, icyear1, ik, ij, ib, ie
    real    :: vlatm, vlonm
    character(len=4) :: a4
    character(len=3) :: aspec, astat
    character(len=80) :: asio, anavtrk, adir, afilen
    character(len=2) :: adosday, adosmon, agpsday
    character(len=4) :: adosyear
    character(len=1) :: aplandir, avlath, avlonh, aclath, aclonh
    integer(2) :: j1, j2, j3, j4

    ! ---- Initialization ----
    iw = 0
    deg2rad = 3.141592654 / 180.0
    asio = ' '
    anavtrk = ' '
    adir = ' '

    ! ---- Get seas2k path ----
    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
       len_adir = 0
       ierror(35) = 307
       call sioloop_close(iw, ifile, ierror)
       return
    end if
    if (ierror(17) == 1) then
       len_adir = 0
       ierror(35) = 317
       call sioloop_close(iw, ifile, ierror)
       return
    end if

    if (len_adir > 0) then
       asio(1:len_adir)    = adir(1:len_adir)
       anavtrk(1:len_adir) = adir(1:len_adir)
    end if
    asio(len_adir+1:len_adir+12)    = 'Data\sio.log'
    anavtrk(len_adir+1:len_adir+15) = 'Data\navtrk.dat'

    ! ---- Open log file ----
    iw = 0
    ifile = 33
    open(ifile, file=asio, form='formatted', status='unknown', &
         access='append', iostat=ios)
    if (ios == 0) then
       iw = 1
    else
       ierror(44) = 1
    end if

    if (iw == 1) then
       write(ifile, *, iostat=ios) 'IN SIOLOOP'
       if (ios /= 0) then
          ierror(45) = 1
          iw = 0
       else
          if (igderr(1) /= 0) write(ifile, *) 'igderr1ios=', igderr(1)
          if (igderr(2) /= 0) write(ifile, *) 'igderr2ios=', igderr(2)
          if (igderr(3) /= 0) write(ifile, *) 'igderr3ios=', igderr(3)
          if (ierrlev == 6) write(ifile, *) 'asio=', asio(1:len_adir+12)
          if (ierrlev == 6) write(ifile, *) 'anavtrk=', anavtrk(1:len_adir+15)
          call flush(ifile)
       end if
    end if

    ! ---- Set icsec1/gpssec if first call ----
    if (ifirst == 0) then
       icsec1 = idsec2
       gpssec = idsec2
       idsec2 = 0
       stoptime = 9.9e9
    end if

    vlat1 = vlat
    vlon1 = vlon

    icday  = int(cday)
    icmon  = int(cmon)
    icyear = int(cyear)

    if (iw == 1) then
       write(ifile, '(a,i2,a,i2,a,i4,1x,i2,a,i2,a,i2,a,i2)') &
            'incm ', icday, '/', icmon, '/', icyear, int(chr), ':', &
            int(cmin), ':', int(csec), ' iupd ', iupdate
       write(ifile, '(a,f7.3,1x,f7.3,1x,i1,a,f7.3,1x,f7.3,1x,i1)') &
            'incm cpos', clatd, clatm, iclath, '/', clond, clonm, iclonh
       call flush(ifile)
    end if

    if (iSIOSpeedAveMin < 1 .or. iSIOSpeedAveMin > 10) iSIOSpeedAveMin = 10

    ! ---- dos time and itime increment ----
    dtime1 = dtime
    call gettim(j1, j2, j3, j4)
    idhr  = int(j1)
    idmin = int(j2)
    idsec = int(j3)
    dtime = real(idhr*3600 + idmin*60 + idsec)
    call getdat(j1, j2, j3)
    if (iw == 1) write(ifile, '(a,i2,a,i2,a,i4,1x,i2,a,i2,a,i2)') &
         ' pc ', int(j3), '/', int(j2), '/', int(j1), idhr, ':', idmin, ':', idsec
    if (iw == 1) call flush(ifile)

    idchange = 0
    if (dtime >= dtime1) then
       idchange = int(dtime) - int(dtime1)
    else
       idchange = int((86400.0 - dtime1) + dtime)
    end if
    itime = itime + idchange
    if (iw == 1 .and. ierrlev == 6) then
       write(ifile, *) 'dtime1=', dtime1, ' dtime=', dtime, ' itime=', itime
    end if

    ! ---- Day rollover handling ----
    if (icday1 /= icday) then
       irollnav = 1
       if (iw == 1 .and. ierrlev == 6) then
          write(ifile, *) 'icday1neicday irollnav=', irollnav
          write(ifile, *) 'icday1=', icday1, ' icday =', icday
       end if
       if (icday1 == (icday - 1)) then
          icmon1  = icmon
          icyear1 = icyear
       else
          if (icmon == 1) then
             icmon1  = 12
             icyear1 = icyear - 1
          else
             icmon1  = icmon - 1
             icyear1 = icyear
          end if
       end if
       call make_dos_date(icday1, icmon1, icyear1, adosday, adosmon, adosyear)
       if (igps == 1 .and. ibuf >= 1) then
          call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)
       else if (igps == 1 .and. ibuf == 0) then
          irollnav = 0
          icday1 = icday
          call make_dos_date(icday, icmon, icyear, adosday, adosmon, adosyear)
          call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)
       else if (igps == 2) then
          icday1 = icday
          call make_dos_date(icday, icmon, icyear, adosday, adosmon, adosyear)
          call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)
       end if
    else
       call make_dos_date(icday, icmon, icyear, adosday, adosmon, adosyear)
       call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)
    end if

    if (icyear > 2000) iiyergps = icyear - 2000
    if (iyerave > 2000) iiyerave = iyerave - 2000

    ! ---- Translate iplandir to aplandir ----
    if (iplandir == 1) then
       aplandir = 'N'
    else if (iplandir == 2) then
       aplandir = 'E'
    else if (iplandir == 3) then
       aplandir = 'S'
    else if (iplandir == 4) then
       aplandir = 'W'
    end if
    aclonh = 'E'
    if (iclonh == 4) aclonh = 'W'
    aclath = 'N'
    if (iclath == 3) aclath = 'S'
    if (istat == 1) then
       astat = 'NAV'
    else
       astat = 'UNK'
    end if

    deadsec  = 60.0 * deadmin
    relodsec = 60.0 * relodmin

    ! ---- Convert xlatload to 0-360 E ----
    do i = 1, 12
       xlatload360(i) = xlatload(i)
    end do
    if (ispec(1) == 0) then
       do i = 1, 12
          if (xlatload(i) < 0.0) then
             xlatload360(i) = 360.0 + xlatload(i)
          end if
       end do
       xlon = xlatload360(1)
    end if

    ! ---- Year/time conversions ----
    i1 = int(cyear)
    call int2ch(i1, a4, 1, len)
    call ch2real(a4, 3, 2, x)
    icyr = int(x)
    idhr  = int(chr)
    idmin = int(cmin)
    idsec = int(csec)
    chrsav = chr
    icsec  = int(csec)
    ctime  = chr + (cmin / 60.0) + (csec / 3600.0)

    ! ---- GPS buffer handling ----
    if (igps == 1) then
       if (iupdate == 1) then
          ibuf = ibuf + 1
          timetag = (chr + (cmin / 60.0) + (csec / 3600.0)) * 3600.0
          call deg2dec(int(clatd), clatm, aclath, clat)
          call deg2dec(int(clond), clonm, aclonh, clon)
          if (iw == 1 .and. ierrlev == 6) then
             write(ifile, *) 'csec=', int(csec), 'timetag=', timetag
             write(ifile, *) 'clat=', clat, ' clon=', clon
          end if
          nobuf = 0
          if (ibuf > 1) then
             if (timetag == ctagbuf(ibuf - 1)) then
                nobuf = 1
                ibuf = ibuf - 1
             end if
          end if
          if (nobuf == 0 .and. ibuf > 0) then
             ctagbuf(ibuf) = timetag
             clonbuf(ibuf) = clon
             clatbuf(ibuf) = clat
          end if
       end if
    end if

    ! ---- No-update / DR branch ----
    if (iupdate == 0) then
       if (csec /= gpssec) then
          ! Continue to position section (label 750)
       else
          if (igps == 1) then
             iaveflg = 0
             if (iw == 1 .and. ierrlev == 6) write(ifile, *) 'set iaveflg=0'
             ! Skip to DR section (label 59)
          else if (igps == 2) then
             iaveflg = 1
             if (iw == 1 .and. ierrlev == 6) &
                  write(ifile, *) 'set iaveflg=1 since igps=2 '
          end if
       end if
    end if

    ! ---- Label 750: GPS second comparison ----
    gpssec1 = csec
    if (igps == 1) then
       if (gpssec1 < 10.0 .and. gpssec >= 50.0) then
          inav = 1
          if (iw == 1 .and. ierrlev == 6) &
               write(ifile, *) '5? to 1?  inav=', inav, ' ibuf=', ibuf
       end if
    end if

    ! ---- Minute calculation: call ave ----
    if (inav == 1) then
       if (ibuf >= 5) then
          check = abs(ctagbuf(ibuf) - ctagbuf(ibuf - 1))
          if (check > 100.0) ibuf = ibuf - 1
          if (ctagbuf(ibuf) == 0.0) ibuf = ibuf - 1

          call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, iw, ifile)
          if (ierr == 1) then
             ibuf = 0
             ! Skip to label 21
          else
             vlat_prev = vlat
             vlon_prev = vlon

             call ave(ibuf, clatbuf, clonbuf, ctagbuf, avlath, avlonh, &
                  s, d, timeave, vlat, vlon, ierror, iderr, iSIOSpeedAveMin, &
                  iw, ifile)

             if (iderr /= 1) then
                ibuf = 0
                ! Skip to label 21
             else
                iaveflg = 1
                idayave = icday
                imonave = icmon
                iyerave = icyear

                if (ibuf >= 5) then
                   if (s /= -99.0) then
                      if (s > xmaxspd) then
                         ierror(11) = 1
                         vlat = vlat_prev
                         vlon = vlon_prev
                         iaveflg = 0
                         ibuf = 0
                         ! Skip to label 21
                      else
                         speed = s
                      end if
                   end if
                   if (d /= -99.0) dir = d
                end if

                if (iaveflg == 1 .and. speed <= xmaxspd) then
                   ! Translate timeave to hr:min:sec
                   call timetohms(timeave, ihr, imin, isec)

                   call dec2deg('lat', ivlatd, vlatm, avlath, vlat)
                   call dec2deg('lon', ivlond, vlonm, avlonh, vlon)
                   call chkall(vlat, vlon, speed, dir, ierrwrite)

                   if (ierrwrite /= 1 .and. avlath(1:1) /= ' ') then
                      ! Write to date.nav
                      open(10, file=afilen, form='formatted', status='unknown', &
                           access='append', iostat=ios)
                      if (ios /= 0) then
                         ierror(5) = 1
                         ierror(34) = icday
                         ierror(36) = icmon
                         ierror(37) = icyear
                      else
                         write(10, '(a2,a,a2,a,a2,a,i2,a,i2,a,i2,a,' // &
                              'i3,a,f7.4,a,a1,a,i3,a,f7.4,a,a1,a,a3,a,f5.2,a,f5.1,i3)', &
                              iostat=ios) &
                              adosday, '/', adosmon, '/', adosyear(3:4), ' ', &
                              ihr, ':', imin, ':', isec, ' ', &
                              abs(ivlatd), ' ', vlatm, ' ', avlath, ' ', &
                              ivlond, ' ', vlonm, ' ', avlonh, ' ', &
                              astat, ' ', speed, ' ', dir, ibuf
                         if (ios /= 0) then
                            ierror(6) = 1
                            ierror(34) = icday
                            ierror(36) = icmon
                            ierror(37) = icyear
                         end if
                         close(10, iostat=ios)
                      end if

                      ! Write to navtrk.dat
                      open(15, file=anavtrk, form='formatted', status='unknown', &
                           iostat=ios)
                      if (ios /= 0) then
                         ierror(23) = 1
                      else
                         rewind(15, iostat=ios)
                         write(15, '(a2,a,a2,a,a2,a,i2,a,i2,a,i2,a,f7.3,f8.3,f6.2,f7.2)', &
                              iostat=ios) &
                              adosday, '/', adosmon, '/', adosyear(3:4), ' ', &
                              ihr, ':', imin, ':', isec, ' ', vlat, vlon, speed, dir
                         if (ios /= 0) ierror(14) = 1
                         close(15, iostat=ios)
                      end if
                   end if
                end if
             end if
          end if

          ! Label 21: clear buffers
          ibuf = 0
       end if
       inav = 0
    end if

    ! ---- Label 59: DR section ----
    gpssec = gpssec1

    ! Dead reckoning
    if (iaveflg /= 2) then
       if (iaveflg == 1) then
          if (ispd /= 0) then
             ! Calculate from latest averaged position
             if (ispec(1) == 1) then
                ! lat-based plan
                dxlat = vlat - xlat
                dxlatnm = abs(dxlat * 60.0)
                dxlonnm = dxlatnm * tan(dir * deg2rad)
                x = 60.0 * cos(vlat * deg2rad)
                if (x /= 0.0) then
                   dxlon = dxlonnm / x
                else
                   dxlon = dxlonnm
                end if
                if (dir >= 0.0 .and. dir < 180.0) then
                   xlon = vlon + abs(dxlon)
                else
                   xlon = vlon - abs(dxlon)
                end if
                if (xlon > 360.0) xlon = xlon - 360.0
                if (xlon < 0.0) xlon = 360.0 + xlon
                x = cos(dir * deg2rad)
                if (x /= 0.0) then
                   dist = abs(dxlatnm / x)
                else
                   dist = abs(dxlatnm)
                end if
             else
                ! lon-based plan
                dxlon = vlon - xlon
                if (abs(dxlon) > 300.0) then
                   if (dxlon > 300.0) then
                      dxlon = 360.0 - (vlon - xlon)
                   else
                      dxlon = 360.0 + (vlon - xlon)
                   end if
                end if
                dxlonnm = abs(dxlon * (60.0 * cos(vlat * deg2rad)))
                x = tan(dir * deg2rad)
                if (x /= 0.0) then
                   dxlatnm = abs(dxlonnm / x)
                   dxlat = dxlatnm / 60.0
                else
                   dxlat = vlat - xlat
                   dxlatnm = abs(dxlat * 60.0)
                end if
                if (dir >= 90.0 .and. dir <= 270.0) then
                   xlat = vlat - abs(dxlat)
                else
                   xlat = vlat + abs(dxlat)
                end if
                x = cos(dir * deg2rad)
                if (x /= 0.0) then
                   dist = abs(dxlatnm / x)
                else
                   dist = abs(dxlatnm)
                end if
             end if

             ! Alarm and newpos
             ix = int(timeave / 86400.0)
             x = timeave
             if (ix > 0) x = timeave - real(ix * 86400)
             xalarm = x + deadsec
             gpstime = ctime * 3600.0
             icsec1 = icsec
             change = gpstime - x

             if (change < -80000.0 .or. idayave /= icday) then
                idchange = icday - idayave
                if (idchange == 0 .or. idchange == 1) then
                   x = 86400.0 - x
                else if (idchange >= 2) then
                   x = real(idchange * 86400) - x
                else
                   ! Month rollover
                   iday = idayave + idchange
                   if (iday == 1) then
                      x = 86400.0 - x
                   else if (iday >= 2) then
                      x = real(iday * 86400) - x
                   end if
                end if
                change = x + gpstime
             end if

             vlat1 = vlat
             vlon1 = vlon
             call newpos(speed, change, dir, vlat, vlat1, vlon1, aclath, &
                  ierrlev, ifile)
             if (aclath == 'N') iclath = 1
             if (aclath == 'S') iclath = 3

             call xbteta(xlatload360, vlat1, vlon1, speed, &
                  ispec, iplandir, iw, ifile, ierror)
          end if
          ! else ispd==0: skip to label 90

       else
          ! iaveflg == 0: watch seconds
          if (icsec /= icsec1) then
             ix = int(timeave / 86400.0)
             x = timeave
             if (ix > 0) x = timeave - real(ix * 86400)
             xalarm = x + deadsec
             gpstime = ctime * 3600.0
             icsec1 = icsec
             change = gpstime - x
             if (change < -80000.0 .or. idayave /= icday) then
                x = 86400.0 - x
                change = x + gpstime
             end if
             x = change
             call newpos(speed, x, dir, vlat, vlat1, vlon1, aclath, &
                  ierrlev, ifile)
             if (aclath == 'N') iclath = 1
             if (aclath == 'S') iclath = 3
             call xbteta(xlatload360, vlat1, vlon1, speed, &
                  ispec, iplandir, iw, ifile, ierror)
             if (deadmin > 0.0 .and. gpstime >= xalarm .and. iupdate == 0) then
                ierror(8) = 1
             end if
          end if
          ! else icsec==icsec1: skip to label 90
       end if

       ! ---- Check if past xbt location ----
       dxlat = vlat1 - xlat
       dxlon = vlon1 - xlon
       if (abs(dxlon) > 300.0) then
          if (dxlon > 300.0) then
             dxlon = (vlon1 - xlon) - 360.0
          else
             dxlon = 360.0 + (vlon1 - xlon)
          end if
       end if

       ! Ship direction check
       idirck = 1
       if (ispec(1) == 0) then
          if (dir >= 0.0 .and. dir <= 180.0 .and. iplandir == 4) idirck = 0
          if (dir <= 360.0 .and. dir >= 180.0 .and. iplandir == 2) idirck = 0
       else if (ispec(1) == 1) then
          if (dir >= 270.0 .and. dir <= 90.0 .and. iplandir == 3) idirck = 0
          if (dir <= 270.0 .and. dir >= 90.0 .and. iplandir == 1) idirck = 0
       end if

       if (idirck == 1 .and. speed <= xmaxspd) then
          if ((aplandir == 'N' .and. dxlat >= 0.0) .or. &
              (aplandir == 'S' .and. dxlat <= 0.0) .or. &
              (aplandir == 'W' .and. dxlon <= 0.0 .and. abs(dxlon) <= 20.0) .or. &
              (aplandir == 'E' .and. dxlon >= 0.0 .and. abs(dxlon) <= 20.0)) then
             stoptime = itime + runsec
             idsec2 = 1
          end if
       end if

       ! GPS status
       if (igps == 1 .and. iupdate == 1) then
          astat = 'NAV'
          istat = 1
       end if

       drlat = vlat1
       drlon = vlon1
       if (drlon > 180.0 .and. drlon < 360.0) then
          drlon = -1.0 * (360.0 - drlon)
       else if (drlon == 360.0) then
          drlon = 0.0
       else if (drlon > 360.0) then
          drlon = 360.0 - drlon
       end if
    end if

    ! ---- Label 90: check stoptime for drop ----
    if (idsec2 == 1 .and. itime >= stoptime) then
       call yrdy(iiyergps, icmon, icday, idhr, idmin, idsec, yrday2)
       if (yrday1 > 0.0) then
          if ((yrday2 - yrday1) < 0.0069444) then
             ierror(30) = 1
          end if
       end if
       ierror(1) = 1
    end if

    if (dropmin > 0.0 .and. itime >= alrmtime) then
       ierror(10) = 1
    end if

    ! ---- Label 101: closing ----
    ifirst = 1

    ! NO GPS day rollover: write DR position to nav file
    if (igps == 2 .and. irollnav == 1) then
       irollnav = 0
       call dec2deg('lat', ivlatd, vlatm, avlath, vlat1)
       call dec2deg('lon', ivlond, vlonm, avlonh, vlon1)

       open(10, file=afilen, form='formatted', status='unknown', iostat=ios)
       if (ios /= 0) then
          ierror(5) = 1
          ierror(34) = icday; ierror(36) = icmon; ierror(37) = icyear
       else
          write(10, '(a2,a,a2,a,a2,1x,a8,1x,i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,' // &
               '1x,a1,1x,a3,1x,f5.2,1x,f5.1,i3)', iostat=ios) &
               adosday, '/', adosmon, '/', adosyear(3:4), '00:00:01', &
               abs(ivlatd), vlatm, avlath, ivlond, vlonm, avlonh, 'DED', speed, dir, 0
          if (ios /= 0) then
             ierror(6) = 1
             ierror(34) = icday; ierror(36) = icmon; ierror(37) = icyear
          end if
       end if
       close(10, iostat=ios)

       ! Write to navtrk.dat
       open(15, file=anavtrk, form='formatted', status='unknown', iostat=ios)
       if (ios /= 0) then
          ierror(23) = 1
       else
          rewind(15, iostat=ios)
          write(15, '(a2,a,a2,a,a2,1x,a8,1x,f7.3,f8.3,f6.2,f7.2)', iostat=ios) &
               adosday, '/', adosmon, '/', adosyear(3:4), '00:00:01', &
               vlat1, vlon1, speed, dir
          if (ios /= 0) ierror(14) = 1
       end if
       close(15, iostat=ios)
    end if

    ! Reset irollnav
    if (irollnav == 1) then
       irollnav = 0
       icday1 = icday
       ibuf = 0
    end if

    ! DR alarm
    if (ierror(12) == 1 .and. deadmin > 0.0) ierror(35) = 312

    ! Watchdog
    if (ierror(35) == 0) ierror(35) = 2

    ! ---- Close log ----
    call sioloop_close(iw, ifile, ierror)
    return

  contains

    subroutine sioloop_close(iw_arg, ifile_arg, ierror_arg)
      integer, intent(in) :: iw_arg, ifile_arg
      integer, intent(in) :: ierror_arg(nerr)
      integer :: ios_l, i_l
      if (iw_arg == 1) then
         do i_l = 1, nerr
            if (ierror_arg(i_l) /= 0) write(ifile_arg, *) 'ierror(', i_l, ')=', ierror_arg(i_l)
         end do
         call flush(ifile_arg)
      end if
      close(ifile_arg, iostat=ios_l)
    end subroutine sioloop_close

  end subroutine sioloop


! ======================================================================
!  SioTimeBegin -- read stations.dat, set nextdrop
! ======================================================================
  subroutine SioTimeBegin(nextdrop, ierror)
    use sio_io,      only: getdir
    use sio_time,    only: gettim
    implicit none
    integer, parameter :: nerr = 50
!GCC$ ATTRIBUTES DLLEXPORT :: SioTimeBegin

    integer, intent(inout) :: nextdrop
    integer, intent(inout) :: ierror(nerr)

    ! Locals
    integer :: len_adir, idrp, iedt, jnav
    integer :: ixbt, ipxday, ipxhr, ipxmin, ipxsec
    real    :: pxlat, pxlon
    character(len=80) :: adir, asiotime, astations
    character(len=70) :: aline
    integer :: iw, ifile, ios, indx, i
    integer(2) :: j1, j2, j3, j4
    integer :: igderr(3)

    iw = 0
    ! Zero out error array
    ierror = 0

    ! Get seas2k path
    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
       len_adir = 0
       ierror(35) = 307
       call stb_cleanup()
       return
    end if
    if (ierror(17) == 1) then
       len_adir = 0
       ierror(35) = 317
       call stb_cleanup()
       return
    end if

    asiotime  = ' '
    astations = ' '
    if (len_adir > 0) then
       asiotime(1:len_adir)  = adir(1:len_adir)
       astations(1:len_adir) = adir(1:len_adir)
    end if

    asiotime(len_adir+1:len_adir+16)  = 'Data\siotime.log'
    astations(len_adir+1:len_adir+17) = 'Data\stations.dat'

    ! Open log file
    iw = 0
    ifile = 33
    open(ifile, file=asiotime, form='formatted', status='unknown', iostat=ios)
    if (ios == 0) then
       iw = 1
    else
       ierror(44) = 1
    end if

    if (iw == 1) then
       write(ifile, *, iostat=ios) 'Inside siotimebegin: '
       if (ios /= 0) then
          ierror(45) = 1
          iw = 0
       else
          call flush(ifile)
          if (igderr(1) /= 0) write(ifile, *) 'igderr1ios=', igderr(1)
          if (igderr(2) /= 0) write(ifile, *) 'igderr2ios=', igderr(2)
          if (igderr(3) /= 0) write(ifile, *) 'igderr3ios=', igderr(3)
       end if
    end if

    if (iw == 1) then
       write(ifile, *) 'adir=', adir(1:len_adir)
       write(ifile, *) 'asiotime=', asiotime(1:len_adir+16)
       write(ifile, *) 'astations=', astations(1:len_adir+17)
       call gettim(j1, j2, j3, j4)
       write(ifile, *) 'dos time before open stations.dat:', j1, j2, j3, j4
       call flush(ifile)
    end if

    ! Default next drop = 1
    nextdrop = 1
    open(7, file=astations, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
       if (iw == 1) write(ifile, *) &
            'Error opening stations.dat. Exiting, open 7 ios= ', ios
       ierror(25) = 1
       ierror(35) = 325
       call stb_cleanup()
       return
    end if

    ! Success opening
    if (iw == 1) write(ifile, *) 'Read in stations.dat-open 7 ios=', ios
    rewind(7, iostat=ios)
    if (iw == 1) write(ifile, *) 'rewind 7 ios=', ios

    do i = 1, 1000
       indx = i
       read(7, '(a70)', iostat=ios) aline(1:70)
       if (ios /= 0) then
          ! Error reading stations.dat
          ierror(26) = 1
          ierror(35) = 326
          exit
       end if
       if (iw == 1) then
          write(ifile, '(a70)') aline(1:70)
          write(ifile, *) 'read7ios=', ios
       end if
       if (aline(1:3) == 'END') exit
       read(aline, '(1x,i3,14x,i2,7x,i2,1x,i2,1x,i2,2f9.3,2x,i2,3x,i2,1x,i5)', &
            iostat=ios) ixbt, ipxday, ipxhr, ipxmin, ipxsec, pxlat, pxlon, idrp, iedt, jnav
       if (ios /= 0) then
          ierror(26) = 1
          ierror(35) = 326
          exit
       end if
       if (iw == 1) write(ifile, '(1x,i3,14x,i2,7x,i2,1x,i2,1x,i2,2f9.3,2x,i2,3x,i2,1x,i5)') &
            ixbt, ipxday, ipxhr, ipxmin, ipxsec, pxlat, pxlon, idrp, iedt, jnav
    end do

    close(7, iostat=ios)
    if (iw == 1) write(ifile, *) '1close7 ios=', ios

    ! If no error, set nextdrop
    if (ierror(26) == 0) then
       nextdrop = indx
       if (iw == 1) write(ifile, *) 'nextdrop=', nextdrop
    end if

    call stb_cleanup()
    return

  contains

    subroutine stb_cleanup()
      integer :: ios_l
      if (ierror(35) == 0) ierror(35) = 2
      close(7, iostat=ios_l)
      if (iw == 1) write(ifile, *) '2close7 ios=', ios_l
      if (iw == 1) write(ifile, *) 'end siotimebegin'
      close(ifile, iostat=ios_l)
    end subroutine stb_cleanup

  end subroutine SioTimeBegin
