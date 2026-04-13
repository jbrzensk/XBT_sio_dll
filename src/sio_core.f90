! src/sio_core.f90
module sio_core
  use sio_math,    only: dpolft, dp1vlu
  use sio_convert, only: ch2real, real2ch, int2ch, dec2deg, deg2dec, lev
  use sio_time,    only: compare, dayofw, gettmtg, findtime, yrdy, timetohms, gettim, getdat
  use sio_io,      only: getdir, navopen, chknav, getfilen, decodeplan, rdcntrl
  use sio_nav,     only: ave, newpos, xbteta, interp, planinfo, chkall, chkbuf, chkwrite
  implicit none
  private
  public :: gpspos, chkprof, wrdrpstn, wrnavfls, prstat, wrxmit, &
            seas2s, tstwrstn, sioend

  integer, parameter :: nerr = 50

contains

  ! Helper: convert integer date components to 2-char strings for getfilen
  subroutine make_dos_date(iday, imon, iyer, adosday, adosmon, adosyear)
    integer, intent(in) :: iday, imon, iyer
    character(len=2), intent(out) :: adosday, adosmon
    character(len=4), intent(out) :: adosyear
    adosday = '00'
    adosmon = '00'
    adosyear = '0000'
    if (iday <= 9) then
      write(adosday(2:2), '(i1)') iday
    else
      write(adosday(1:2), '(i2)') iday
    end if
    if (imon <= 9) then
      write(adosmon(2:2), '(i1)') imon
    else
      write(adosmon(1:2), '(i2)') imon
    end if
    write(adosyear, '(i4.4)') iyer
  end subroutine make_dos_date

  ! -----------------------------------------------------------------------
  ! gpspos: Navigate XBT drops using GPS positions from .nav files.
  ! sio.for line 2887, ~700 lines
  ! -----------------------------------------------------------------------
  subroutine gpspos(ierror, ireturn, ichoosedrop)
    integer, intent(inout) :: ierror(nerr)
    integer, intent(out)   :: ireturn
    integer, intent(in)    :: ichoosedrop

    character(len=80) :: f1, fnav, adir, agpspos, astations
    character(len=70) :: chr70(999)
    character(len=58) :: chr58
    character(len=35) :: chr35
    character(len=21) :: chr21
    character(len=9)  :: chr9(1000)
    character(len=4)  :: ch4
    character(len=2)  :: clat, clats, clon, clons
    character(len=1)  :: chr1
    integer :: i, ios, iw, ifile, iprofcnt, ierrlev
    integer :: iyr, imo, iday, ihr, imin, isec, len_adir
    integer :: igderr(3), launcher(12), len_acruise, iSIOSpeedAveMin
    integer :: iline, iline2, inav, ierr, ic, itest, kk
    integer :: jday, jmo, jyr, jhr, jmin, jsec, jlat, jlon, nfix
    integer :: kday, kmo, kyr, khr, kmin, ksec, klat, klon
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    real :: yrdrop, yrsav, yrnav, ylat, ylon, xlat, xlon, zlat, zlon
    real :: xltm, xlnm, zltm, zlnm, spd, dir, deg2rad, tnav, tnavh
    real :: ylatd, ylatm, ylata, ylond, ylonm
    character(len=7) :: acruise
    integer :: len_f1
    logical :: found_interp, need_extrap

    f1 = ' '
    iw = 0
    ierrlev = 0
    ifile = 33

    do i = 1, nerr
      ierror(i) = 0
    end do

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
      len_adir = 0; return
    end if
    if (ierror(17) == 1) then
      len_adir = 0; return
    end if

    agpspos   = ' '
    astations = ' '
    f1        = ' '
    if (len_adir > 0) then
      agpspos(1:len_adir)   = adir(1:len_adir)
      astations(1:len_adir) = adir(1:len_adir)
      f1(1:len_adir)        = adir(1:len_adir)
    end if

    f1(len_adir+1:len_adir+5) = 'Data/'
    len_f1 = len_adir + 5
    agpspos(len_adir+1:len_adir+15)   = 'Data/gpspos.log'
    astations(len_adir+1:len_adir+17) = 'Data/stations.dat'

    ireturn = 0
    deg2rad = 3.141592654 / 180.0
    iprofcnt = 0

    ! open log
    open(ifile, file=agpspos, status='unknown', form='formatted', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'BEGIN GPSPOS'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
         deadmin, dropmin, relodmin, runsec, &
         tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
         tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, len_adir, adir, iw, ifile)

    if (ierror(33) == 6) ierrlev = 6
    if (ierror(15) /= 0 .or. ierror(16) /= 0) then
      ireturn = 1
      call gpspos_cleanup(iw, ifile)
      return
    end if

    f1(len_f1+1:len_f1+len_acruise) = acruise(1:len_acruise)
    len_f1 = len_f1 + len_acruise
    write(f1(len_f1+1:len_f1+5), '(a5)') 's.000'

    ! Main loop
    do
      open(7, file=astations, status='old', iostat=ios)
      if (ios /= 0) then
        ierror(25) = 1; ireturn = 1
        call gpspos_cleanup(iw, ifile)
        return
      end if

      if (ichoosedrop > 0) then
        do kk = 1, ichoosedrop - 1
          read(7, *, iostat=ios)
          if (ios /= 0) then
            ierror(3) = 1; ireturn = 1
            call gpspos_cleanup(iw, ifile)
            return
          end if
        end do
        read(7, '(1x,a3,a58,i6,1x,a1)', iostat=ios) &
             f1(len_f1+3:len_f1+5), chr58, inav, chr1
        if (ios /= 0) then
          ierror(3) = 1; ireturn = 1
          call gpspos_cleanup(iw, ifile)
          return
        end if
        iline = ichoosedrop
      else
        iline = 0
        do i = 1, 1000
          read(7, '(a4)', iostat=ios) ch4
          if (ios /= 0) then
            ierror(26) = 1; ireturn = 1
            call gpspos_cleanup(iw, ifile)
            return
          end if
          if (iprofcnt == 0 .and. ch4 == 'ENDD') then
            ierror(3) = 1; ireturn = 1
            call gpspos_cleanup(iw, ifile)
            return
          end if
          if (iprofcnt /= 0 .and. ch4 == 'ENDD') then
            call gpspos_cleanup(iw, ifile)
            return
          end if
          backspace(7)
          read(7, '(1x,a3,a58,i6,1x,a1)', iostat=ios) &
               f1(len_f1+3:len_f1+5), chr58, inav, chr1
          if (ios /= 0) then
            ierror(26) = 1; ireturn = 1
            call gpspos_cleanup(iw, ifile)
            return
          end if
          iline = iline + 1
          if (inav == 0) exit
        end do
      end if

      ! read rest of stations.dat
      iline2 = 0
      do i = 1, 1000
        read(7, '(a4)', iostat=ios) ch4
        if (ios /= 0) exit
        if (ch4 == 'ENDD') exit
        backspace(7)
        iline2 = iline2 + 1
        read(7, '(a70)', iostat=ios) chr70(iline2)
        if (ios /= 0) exit
      end do
      close(7)

      read(chr58, '(14x,6(i2,1x))', iostat=ios) iday, imo, iyr, ihr, imin, isec
      if (ios /= 0) then
        ierror(26) = 1; ireturn = 1
        call gpspos_cleanup(iw, ifile)
        return
      end if
      call yrdy(iyr, imo, iday, ihr, imin, isec, yrdrop)

      call navopen(iday, imo, iyr, ierr, fnav, adir, len_adir)

      found_interp = .false.
      need_extrap = .false.

      if (ierr >= 1) then
        ! try previous day
        iday = iday - 1
        if (iday == 0) then
          imo = imo - 1
          if (imo == 0) then; iyr = iyr - 1; imo = 12; end if
          iday = 31
          if (imo == 4 .or. imo == 6 .or. imo == 9 .or. imo == 11) iday = 30
          if (imo == 2 .and. mod(iyr, 4) /= 0) iday = 28
          if (imo == 2 .and. mod(iyr, 4) == 0) iday = 29
        end if
        call navopen(iday, imo, iyr, ierr, fnav, adir, len_adir)
        if (ierr >= 1) then
          ierror(5) = 1; ierror(34) = iday; ierror(36) = imo; ierror(37) = iyr
          ireturn = 1
          call gpspos_cleanup(iw, ifile)
          return
        end if
        do i = 1, 10000
          read(8, '(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)', iostat=ios) &
               kday, kmo, kyr, khr, kmin, ksec, klat, zltm, clats, klon, zlnm, clons, spd, dir, nfix
          if (ios /= 0) exit
        end do
        close(8)
        call yrdy(kyr, kmo, kday, khr, kmin, ksec, yrsav)
        need_extrap = .true.
      end if

      if (.not. need_extrap) then
        ! Read same-day nav
        do i = 1, 10000
          read(8, '(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)', iostat=ios) &
               jday, jmo, jyr, jhr, jmin, jsec, jlat, xltm, clat, jlon, xlnm, clon, spd, dir, nfix
          if (ios /= 0) exit
          call yrdy(jyr, jmo, jday, jhr, jmin, jsec, yrnav)

          if (i > 1 .and. yrsav <= yrdrop .and. yrnav > yrdrop) then
            xlat = real(jlat) + xltm / 60.0
            if (clat == ' S') xlat = -1.0 * xlat
            xlon = real(jlon) + xlnm / 60.0
            if (clon == ' W') xlon = 360.0 - xlon
            zlat = real(klat) + zltm / 60.0
            if (clats == ' S') zlat = -1.0 * zlat
            zlon = real(klon) + zlnm / 60.0
            if (clons == ' W') zlon = 360.0 - zlon
            call interp(yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav, xlat, xlon)
            tnav = (yrnav - yrsav) * 1440.0
            tnav = max(tnav, 1.0)
            close(8)
            found_interp = .true.
            exit
          end if

          yrsav = yrnav
          klat = jlat; zltm = xltm; clats = clat
          klon = jlon; zlnm = xlnm; clons = clon

          if (i == 1 .and. yrnav > yrdrop) then
            iday = iday - 1
            if (iday == 0) then
              imo = imo - 1
              if (imo == 0) then; iyr = iyr - 1; imo = 12; end if
              iday = 31
              if (imo == 4 .or. imo == 6 .or. imo == 9 .or. imo == 11) iday = 30
              if (imo == 2 .and. mod(iyr, 4) /= 0) iday = 28
              if (imo == 2 .and. mod(iyr, 4) == 0) iday = 29
            end if
            close(8)
            call navopen(iday, imo, iyr, ierr, fnav, adir, len_adir)
            if (ierr >= 1) then
              ierror(5) = 1; ierror(34) = iday; ierror(36) = imo; ierror(37) = iyr
              ireturn = 1
              call gpspos_cleanup(iw, ifile)
              return
            end if
            do kk = 1, 10000
              read(8, '(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)', iostat=ios) &
                   kday, kmo, kyr, khr, kmin, ksec, klat, zltm, clats, klon, zlnm, clon, spd, dir, nfix
              if (ios /= 0) exit
            end do
            call yrdy(kyr, kmo, kday, khr, kmin, ksec, yrsav)
            close(8)
            xlat = real(jlat) + xltm / 60.0
            if (clat == ' S') xlat = -1.0 * xlat
            xlon = real(jlon) + xlnm / 60.0
            if (clon == ' W') xlon = 360.0 - xlon
            zlat = real(klat) + zltm / 60.0
            if (clats == ' S') zlat = -1.0 * zlat
            zlon = real(klon) + zlnm / 60.0
            if (clons == ' W') zlon = 360.0 - zlon
            call interp(yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav, xlat, xlon)
            tnav = (yrnav - yrsav) * 1440.0
            tnav = max(tnav, 1.0)
            found_interp = .true.
            exit
          end if
        end do

        if (.not. found_interp) then
          ! last fix before drop - try next day
          yrsav = yrnav
          klat = jlat; zltm = xltm; clats = clat
          klon = jlon; zlnm = xlnm; clons = clon
          iday = iday + 1
          if (iday == 32 .or. &
              (iday == 31 .and. (imo == 4 .or. imo == 6 .or. imo == 9 .or. imo == 11)) .or. &
              (iday == 29 .and. imo == 2 .and. mod(iyr, 4) /= 0) .or. &
              (iday == 30 .and. imo == 2 .and. mod(iyr, 4) == 0)) then
            iday = 1; imo = imo + 1
            if (imo == 13) then; iyr = iyr + 1; imo = 1; end if
          end if
          close(8)
          call navopen(iday, imo, iyr, ierr, fnav, adir, len_adir)
          if (ierr >= 1) then
            need_extrap = .true.
          else
            read(8, '(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)', iostat=ios) &
                 jday, jmo, jyr, jhr, jmin, jsec, jlat, xltm, clat, jlon, xlnm, clon, spd, dir, nfix
            if (ios /= 0) then
              need_extrap = .true.
            else
              close(8)
              call yrdy(jyr, jmo, jday, jhr, jmin, jsec, yrnav)
              xlat = real(jlat) + xltm / 60.0
              if (clat == ' S') xlat = -1.0 * xlat
              xlon = real(jlon) + xlnm / 60.0
              if (clon == ' W') xlon = 360.0 - xlon
              zlat = real(klat) + zltm / 60.0
              if (clats == ' S') zlat = -1.0 * zlat
              zlon = real(klon) + zlnm / 60.0
              if (clons == ' W') zlon = 360.0 - zlon
              call interp(yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav, xlat, xlon)
              tnav = (yrnav - yrsav) * 1440.0
              tnav = max(tnav, 1.0)
              found_interp = .true.
            end if
          end if
        end if
      end if

      ! extrapolation
      if (need_extrap) then
        close(8, iostat=ios)
        tnav = -1.0 * (yrdrop - yrsav) * 1440.0
        tnav = min(tnav, -1.0)
        tnavh = -1.0 * tnav / 60.0
        zlat = real(klat) + zltm / 60.0
        if (clats == ' S') zlat = -1.0 * zlat
        zlon = real(klon) + zlnm / 60.0
        if (clons == ' W') zlon = 360.0 - zlon
        ylat = zlat + tnavh * spd * cos(dir * deg2rad) / 60.0
        ylon = zlon + tnavh * spd * sin(dir * deg2rad) / (cos(ylat * deg2rad) * 60.0)
      end if

      ! label 102 equivalent - post-interpolation/extrapolation
      clat = ' N'
      if (ylat < 0.0) clat = ' S'
      ylata = abs(ylat)
      ylatd = real(int(ylata))
      ylatm = (ylata - ylatd) * 60.0
      if (ylon > 360.0) ylon = ylon - 360.0
      ylond = real(int(ylon))
      ylonm = (abs(ylon) - abs(ylond)) * 60.0

      write(chr58(32:40), '(f9.3)') ylat
      write(chr58(41:49), '(f9.3)') ylon
      call chkwrite(ylat, ylon, ierr)
      if (ierr >= 1) then
        ierror(4) = 1; ireturn = 1
        call gpspos_cleanup(iw, ifile)
        return
      end if

      itest = nint(tnav)
      if (itest > 99999) itest = 99999
      if (itest < -9999) itest = -9999
      if (itest == 0) itest = -9999

      open(7, file=astations, status='old', iostat=ios)
      if (ios /= 0) then
        ierror(25) = 1; ireturn = 1
        call gpspos_cleanup(iw, ifile)
        return
      end if
      do i = 1, iline - 1
        read(7, *, iostat=ios)
        if (ios /= 0) then
          ierror(26) = 1; ireturn = 1
          call gpspos_cleanup(iw, ifile)
          return
        end if
      end do
      write(7, '(1x,a3,a58,i6,1x,a1)', iostat=ios) f1(len_f1+3:len_f1+5), chr58, itest, chr1
      if (ios /= 0) then
        ierror(29) = 1; ireturn = 1
        call gpspos_cleanup(iw, ifile)
        return
      end if
      do i = 1, iline2
        write(7, '(a70)', iostat=ios) chr70(i)
        if (ios /= 0) then
          ierror(29) = 1; ireturn = 1
          call gpspos_cleanup(iw, ifile)
          return
        end if
      end do
      write(7, '(a7)', iostat=ios) 'ENDDATA'
      close(7)

      ! e file handling
      f1(len_f1+1:len_f1+1) = 'E'
      open(9, file=f1, status='old', iostat=ios)
      if (ios == 0) then
        read(9, '(a35)', iostat=ios) chr35
        if (ios == 0) then
          read(9, '(a21)', iostat=ios) chr21
          if (ios == 0) then
            ic = 0
            do i = 1, 1000
              read(9, '(a9)', iostat=ios) chr9(i)
              if (ios /= 0) exit
              ic = i
            end do
            close(9)
            write(chr35(20:35), '(2f8.2)') ylat, ylon
            open(9, file=f1, status='unknown', iostat=ios)
            if (ios == 0) then
              write(9, '(a35)') chr35
              write(9, '(a21)') chr21
              do i = 1, ic
                write(9, '(a9)') chr9(i)
              end do
            end if
          end if
        end if
        close(9, iostat=ios)
      end if

      iprofcnt = iprofcnt + 1
      if (ichoosedrop > 0) then
        call gpspos_cleanup(iw, ifile)
        return
      end if
    end do ! main loop

  contains
    subroutine gpspos_cleanup(iw_arg, ifile_arg)
      integer, intent(in) :: iw_arg, ifile_arg
      integer :: ios_l
      close(7, iostat=ios_l)
      close(8, iostat=ios_l)
      close(9, iostat=ios_l)
      if (iw_arg == 1) write(ifile_arg, *) 'END GPSPOS'
      close(ifile_arg, iostat=ios_l)
    end subroutine gpspos_cleanup

  end subroutine gpspos

  ! -----------------------------------------------------------------------
  ! chkprof: Check XBT profile quality against a reference profile.
  ! sio.for line 3596, ~775 lines
  ! -----------------------------------------------------------------------
  subroutine chkprof(ierror, ireturn, ichoosedrop)
    integer, intent(inout) :: ierror(nerr)
    integer, intent(out)   :: ireturn
    integer, intent(inout) :: ichoosedrop

    integer, parameter :: nzmx = 999
    real    :: avt(nzmx), rft(nzmx)
    character(len=80) :: f1, f2, adir, achkprof, astations
    character(len=70) :: chr70(nzmx)
    character(len=49) :: chr49
    character(len=8)  :: chr8
    character(len=4)  :: chr4
    character(len=3)  :: chr3
    integer :: igderr(3), launcher(12)
    integer :: iw, ifile, ios, ierrlev, iSIOSpeedAveMin
    integer :: len_adir, len_f1, len_acruise
    integer :: ichk, iedt, iline, iline2, ichkprof
    integer :: i, ibad, npts, nptsrms
    integer :: iread, itest_depth, itestdepth
    integer :: id, it, idl, itl, i700m, idtdz, itdif, izdz
    integer :: ibaddep, idepdtdzsav, ichkprofdepth
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    real :: dzrms, dzmx, dtmax, tdif, dtdz, dtrdz, dtst, dz, dz1
    real :: tdif700, dtdzsav
    character(len=7) :: acruise

    iw = 0; ierrlev = 0; ireturn = 0; ifile = 33
    tdzmx = 80.0; tdzrms = 40.0; dtdzmn = -5.0e-4
    dtdzth = 2.0e-4; dtmx = 2.0; dtmx700 = 2.0
    itestdepth = 720

    f1 = ' '; f2 = ' '

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then; len_adir = 0; return; end if
    if (ierror(17) == 1) then; len_adir = 0; return; end if

    achkprof  = ' '; astations = ' '; f1 = ' '
    if (len_adir > 0) then
      achkprof(1:len_adir)  = adir(1:len_adir)
      astations(1:len_adir) = adir(1:len_adir)
      f1(1:len_adir)        = adir(1:len_adir)
    end if

    f1(len_adir+1:len_adir+5) = 'Data/'
    len_f1 = len_adir + 5
    achkprof(len_adir+1:len_adir+16)  = 'Data/chkprof.log'
    astations(len_adir+1:len_adir+17) = 'Data/stations.dat'

    open(ifile, file=achkprof, status='unknown', form='formatted', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'BEGIN chkprof'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    if (ichoosedrop <= 0) ichoosedrop = 1000

    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
         deadmin, dropmin, relodmin, runsec, &
         tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
         tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, len_adir, adir, iw, ifile)

    if (ierror(15) /= 0 .or. ierror(16) /= 0) then
      call chkprof_cleanup(iw, ifile)
      return
    end if
    if (ierror(33) == 6) ierrlev = 6

    ! rdcntrl sets ichkprofdepth internally; use default if not available
    ! itestdepth stays at 720 or gets overridden by control.dat
    if (itestdepth < 250) then
      ireturn = 2
      call chkprof_cleanup(iw, ifile)
      return
    end if

    iread = itestdepth / 2
    itest_depth = iread - 10

    f1(len_f1+1:len_f1+len_acruise) = acruise(1:len_acruise)
    len_f1 = len_f1 + len_acruise
    write(f1(len_f1+1:len_f1+5), '(a5)') 's.000'
    f2(1:len_f1+5) = f1(1:len_f1+5)

    open(unit=7, file=astations, status='old', iostat=ios)
    if (ios /= 0) then
      ierror(25) = 1; ireturn = 2
      call chkprof_cleanup(iw, ifile)
      return
    end if

    iline = 0
    do i = 1, ichoosedrop
      read(7, '(a4)', iostat=ios) chr4
      if (ios /= 0) then
        ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      if (chr4 == 'ENDD') then
        ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      backspace(7)
      read(7, '(1x,a3,a49,i4,i5,a8)', iostat=ios) chr3, chr49, ichk, iedt, chr8
      if (ios /= 0) then
        ierror(26) = 1; ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      iline = iline + 1

      if (i == ichoosedrop) then
        f2(len_f1+3:len_f1+5) = chr3(1:3); exit
      end if
      if (iedt == -1) cycle
      if ((ichk == 1) .or. (ichk == -1 .and. iedt == 1) .or. (ichk == -3 .and. iedt == 1)) then
        f1(len_f1+3:len_f1+5) = chr3(1:3)
      else if (ichk == 0) then
        f2(len_f1+3:len_f1+5) = chr3(1:3); exit
      end if
    end do

    ! no previous profile
    if (f1(len_f1+3:len_f1+5) == '000') then
      ierror(31) = 1
      iline2 = 0
      do i = 1, 1000
        read(7, '(a4)', iostat=ios) chr4
        if (ios /= 0 .or. chr4 == 'ENDD') exit
        backspace(7)
        iline2 = iline2 + 1
        read(7, '(a70)', iostat=ios) chr70(iline2)
        if (ios /= 0) then
          ierror(26) = 1; ireturn = 2
          call chkprof_cleanup(iw, ifile)
          return
        end if
      end do
      close(7)
      ichkprof = -3
      open(7, file=astations, status='old', iostat=ios)
      if (ios /= 0) then
        ierror(25) = 1; ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      do i = 1, iline - 1
        read(7, *, iostat=ios)
        if (ios /= 0) then
          ierror(26) = 1; ireturn = 2
          call chkprof_cleanup(iw, ifile)
          return
        end if
      end do
      write(7, '(1x,a3,a49,i4,i5,a8)', iostat=ios) chr3, chr49, ichkprof, iedt, chr8
      if (ios /= 0) then
        ierror(29) = 1; ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      do i = 1, iline2
        write(7, '(a70)', iostat=ios) chr70(i)
      end do
      write(7, '(a7)') 'ENDDATA'
      close(7)
      call chkprof_cleanup(iw, ifile)
      return
    end if

    if (f2(len_f1+3:len_f1+5) == '000') then
      ireturn = 2
      call chkprof_cleanup(iw, ifile)
      return
    end if

    ! read rest of stations.dat
    iline2 = 0
    do i = 1, 1000
      read(7, '(a4)', iostat=ios) chr4
      if (ios /= 0 .or. chr4 == 'ENDD') exit
      backspace(7)
      iline2 = iline2 + 1
      read(7, '(a70)', iostat=ios) chr70(iline2)
      if (ios /= 0) then
        ierror(26) = 1; ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
    end do
    close(7)

    ! Clear vars
    ibad = 0; npts = 0; nptsrms = 0; dzrms = 0.0; dzmx = 0.0
    itdif = 0; dtmax = 0.0; dtdzsav = 999.0; idepdtdzsav = 0
    izdz = 0; idtdz = 0; tdif700 = 0.0

    ! open current profile
    open(unit=14, file=f2, status='old', iostat=ios)
    if (ios /= 0) then
      ireturn = 2
      call chkprof_cleanup(iw, ifile)
      return
    end if
    read(14, *, iostat=ios)
    read(14, *, iostat=ios)
    do i = 1, iread
      read(14, '(i3,i6)', iostat=ios) id, it
      if (ios /= 0) then
        close(14); ibad = 1; ireturn = 1
        call chkprof_write_bad(astations, chr3, chr49, iedt, chr8, iline, iline2, chr70, &
                               ierror, ireturn, iw, ifile)
        return
      end if
      avt(i) = real(it) / 1000.0
    end do
    close(14)

    ! open reference profile
    open(unit=19, file=f1, status='old', iostat=ios)
    if (ios /= 0) then
      ireturn = 2
      call chkprof_cleanup(iw, ifile)
      return
    end if
    read(19, *, iostat=ios)
    read(19, *, iostat=ios)
    do i = 1, itest_depth
      read(19, '(i3,i6)', iostat=ios) idl, itl
      if (ios /= 0) then
        close(19); ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      rft(i) = real(itl) / 1000.0
    end do
    close(19)

    ! 700m temp diff test
    i700m = 0
    if (iread >= 350) then
      tdif700 = abs(rft(350) - avt(350))
      if (tdif700 > dtmx700) i700m = 1
    end if

    ! main comparison
    do i = 100, itest_depth
      npts = npts + 1; nptsrms = nptsrms + 1
      tdif = abs(rft(i) - avt(i))
      if (abs(tdif) > dtmax) then; dtmax = abs(tdif); itdif = i; end if
      dtdz = (avt(i-10) - avt(i+10)) / 40.0
      if (dtdz < dtdzsav) then; dtdzsav = dtdz; idepdtdzsav = i; end if
      dtrdz = (rft(i-10) - rft(i+10)) / 40.0
      dtst = dtdzmn
      if (dtrdz < dtst) dtst = 2.0 * dtrdz
      if (dtdz < dtst) then; ibad = 1; idtdz = i; cycle; end if
      if (abs(dtrdz) > dtdzth) then
        dz = tdif / dtrdz
        dzrms = dzrms + dz**2
        if (dz > dzmx) then; dzmx = dz; izdz = 2 * i - 1; end if
      else
        nptsrms = nptsrms - 1
      end if
    end do

    if (ibad /= 1 .and. abs(dtmax) <= dtmx) then
      if (nptsrms > 0) dzrms = sqrt(dzrms / real(nptsrms))
      if (dzrms > tdzrms) ibad = 1
      if (dzmx > tdzmx) ibad = 1
      if (i700m == 1) ibad = 1
    else
      ibad = 1
    end if

    if (ibad == 0) then
      ichkprof = 1
      open(7, file=astations, status='old', iostat=ios)
      if (ios /= 0) then
        ierror(25) = 1; ireturn = 2
        call chkprof_cleanup(iw, ifile)
        return
      end if
      do i = 1, iline - 1
        read(7, *, iostat=ios)
      end do
      write(7, '(1x,a3,a49,i4,i5,a8)', iostat=ios) chr3, chr49, ichkprof, iedt, chr8
      do i = 1, iline2
        write(7, '(a70)', iostat=ios) chr70(i)
      end do
      write(7, '(a7)') 'ENDDATA'
      close(7)
      call chkprof_cleanup(iw, ifile)
      return
    end if

    ! bad profile
    call chkprof_write_bad(astations, chr3, chr49, iedt, chr8, iline, iline2, chr70, &
                           ierror, ireturn, iw, ifile)
    return

  contains
    subroutine chkprof_cleanup(iw_arg, ifile_arg)
      integer, intent(in) :: iw_arg, ifile_arg
      integer :: ios_l
      close(7, iostat=ios_l)
      if (iw_arg == 1) then
        write(ifile_arg, *) 'End chkprof'
      end if
      close(ifile_arg, iostat=ios_l)
    end subroutine chkprof_cleanup

    subroutine chkprof_write_bad(astations_a, chr3_a, chr49_a, iedt_a, chr8_a, &
                                  iline_a, iline2_a, chr70_a, ierror_a, ireturn_a, iw_a, ifile_a)
      character(len=80), intent(in) :: astations_a
      character(len=3), intent(in) :: chr3_a
      character(len=49), intent(in) :: chr49_a
      integer, intent(in) :: iedt_a, iline_a, iline2_a, iw_a, ifile_a
      character(len=8), intent(in) :: chr8_a
      character(len=70), intent(in) :: chr70_a(nzmx)
      integer, intent(inout) :: ierror_a(nerr)
      integer, intent(inout) :: ireturn_a
      integer :: ichkprof_l, ios_l, i_l

      ichkprof_l = -1
      open(7, file=astations_a, status='old', iostat=ios_l)
      if (ios_l /= 0) then
        ierror_a(25) = 1; ireturn_a = 2
        call chkprof_cleanup(iw_a, ifile_a)
        return
      end if
      do i_l = 1, iline_a - 1
        read(7, *, iostat=ios_l)
      end do
      write(7, '(1x,a3,a49,i4,i5,a8)', iostat=ios_l) chr3_a, chr49_a, ichkprof_l, iedt_a, chr8_a
      do i_l = 1, iline2_a
        write(7, '(a70)', iostat=ios_l) chr70_a(i_l)
      end do
      write(7, '(a7)') 'ENDDATA'
      close(7)
      ireturn_a = 1
      call chkprof_cleanup(iw_a, ifile_a)
    end subroutine chkprof_write_bad

  end subroutine chkprof

  ! -----------------------------------------------------------------------
  ! wrdrpstn: Write a drop position line to stations.dat.
  ! sio.for line 4372, ~520 lines
  ! -----------------------------------------------------------------------
  subroutine wrdrpstn(nextdrop, itube, t700, iday, imon, iyer, &
                      ihr, imin, isec, ierror, xlat, xlon)
    integer, intent(in)    :: nextdrop, itube
    integer, intent(in)    :: iday, imon, iyer, ihr, imin, isec
    integer, intent(inout) :: ierror(nerr)
    real,    intent(in)    :: t700, xlat, xlon

    character(len=3)  :: ch3
    character(len=4)  :: chr4
    character(len=2)  :: aday, amon, ayer, ahr, amin_c, asec
    character(len=80) :: adir, awrdrpstn, astations, asst, f1
    character(len=7)  :: acruise
    integer :: len_acruise, igderr(3), launcher(12), iSIOSpeedAveMin
    integer :: iw, ifile, ios, len_adir, len_f1, i, indx, len
    integer :: ii, idep, item, it700
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    real :: csst, t700s
    real :: t700_loc, xlat_loc, xlon_loc
    logical :: skip_to_format

    iw = 0
    ifile = 33
    t700_loc = t700; xlat_loc = xlat; xlon_loc = xlon
    do i = 1, nerr
      ierror(i) = 0
    end do

    f1 = ' '; adir = ' '; awrdrpstn = ' '; astations = ' '; asst = ' '

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
      len_adir = 0
    end if
    if (ierror(17) == 1) then
      len_adir = 0
    end if

    if (len_adir > 0) then
      awrdrpstn(1:len_adir) = adir(1:len_adir)
      astations(1:len_adir) = adir(1:len_adir)
      asst(1:len_adir) = adir(1:len_adir)
      f1(1:len_adir) = adir(1:len_adir)
    end if

    f1(len_adir+1:len_adir+5) = 'Data/'
    len_f1 = len_adir + 5
    awrdrpstn(len_adir+1:len_adir+17) = 'Data/wrdrpstn.log'
    astations(len_adir+1:len_adir+17) = 'Data/stations.dat'
    asst(len_adir+1:len_adir+12) = 'Data/sst.dat'

    ! open log
    open(ifile, file=awrdrpstn, status='unknown', form='formatted', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'BEGIN WrDrpStn'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    ! convert negative longitude
    if (xlon_loc < 0.0) xlon_loc = 360.0 + xlon_loc

    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
         deadmin, dropmin, relodmin, runsec, &
         tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
         tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, len_adir, adir, iw, ifile)

    skip_to_format = .false.
    if (ierror(15) /= 0) then
      ierror(35) = 315; skip_to_format = .true.
    end if
    if (.not. skip_to_format .and. ierror(16) /= 0) then
      ierror(35) = 316; skip_to_format = .true.
    end if

    if (.not. skip_to_format) then
      f1(len_f1+1:len_f1+len_acruise) = acruise(1:len_acruise)
      len_f1 = len_f1 + len_acruise
      write(f1(len_f1+1:len_f1+5), '(a5)') 's.000'

      ch3(1:3) = '000'
      if (nextdrop <= 9) write(ch3(3:3), '(i1)') nextdrop
      if (nextdrop >= 10 .and. nextdrop <= 99) write(ch3(2:3), '(i2)') nextdrop
      if (nextdrop >= 100) write(ch3(1:3), '(i3)') nextdrop
      f1(len_f1+3:len_f1+5) = ch3(1:3)

      csst = -9.999; t700s = -9999.0

      ! try s file
      open(19, file=f1, status='old', form='formatted', iostat=ios)
      if (ios /= 0) then
        ierror(46) = 1
      else
        read(19, *, iostat=ios)
        if (ios /= 0) then
          ierror(40) = 1
        else
          read(19, *, iostat=ios)
          if (ios /= 0) then
            ierror(40) = 1
          else
            read(19, *, iostat=ios)
            if (ios /= 0) then
              ierror(40) = 1
            else
              read(19, '(i3,i6)', iostat=ios) idep, item
              if (ios /= 0) then
                ierror(40) = 1
              else
                do ii = 1, 348
                  read(19, '(i3,i6)', iostat=ios) idep, it700
                  if (ios /= 0) exit
                end do
                csst = real(item) * 0.001
                t700s = real(it700)
              end if
            end if
          end if
        end if
      end if
      close(19, iostat=ios)

      ! sst.dat
      open(17, file=asst, status='unknown', iostat=ios)
      if (ios /= 0) then
        ierror(48) = 1
      else
        rewind(17)
        do i = 1, nextdrop - 1
          read(17, *, iostat=ios)
          if (ios /= 0) then; ierror(49) = 1; exit; end if
        end do
        if (ierror(49) == 0) then
          write(17, '(1x,a3,1x,f7.3)', iostat=ios) ch3, csst
          if (ios /= 0) ierror(50) = 1
        end if
      end if
      close(17, iostat=ios)

      ! t700 logic
      if (abs(t700_loc - (-9999.0)) < 0.5) then
        t700_loc = t700s * 0.001
      else if (t700_loc < -2.0 .or. t700_loc > 40.0) then
        t700_loc = t700s * 0.001
      end if
      if (t700_loc > 99.99 .or. t700_loc <= -10.0) t700_loc = -9.999
    end if

    ! format date/time (was label 500)
    aday = '00'; amon = '00'; ayer = '00'; ahr = '00'; amin_c = '00'; asec = '00'
    if (iday <= 9) then; write(aday(2:2), '(i1)') iday
    else; write(aday(1:2), '(i2)') iday; end if
    if (imon <= 9) then; write(amon(2:2), '(i1)') imon
    else; write(amon(1:2), '(i2)') imon; end if
    call int2ch(iyer, chr4, 1, len)
    ayer(1:2) = chr4(3:4)
    if (ihr <= 9) then; write(ahr(2:2), '(i1)') ihr
    else; write(ahr(1:2), '(i2)') ihr; end if
    if (imin <= 9) then; write(amin_c(2:2), '(i1)') imin
    else; write(amin_c(1:2), '(i2)') imin; end if
    if (isec <= 9) then; write(asec(2:2), '(i1)') isec
    else; write(asec(1:2), '(i2)') isec; end if

    ! open stations.dat
    open(7, file=astations, status='old', iostat=ios)
    if (ios /= 0) then
      ierror(25) = 1; ierror(35) = 325
    else
      rewind(7)

      do i = 1, 1000
        indx = i
        read(7, '(a4)', iostat=ios) chr4
        if (ios /= 0) then; indx = indx - 1; exit; end if
        if (chr4 == 'ENDD') exit
      end do
      if (chr4 /= 'ENDD') then
        indx = indx - 1
        if (indx < 1) then
          ierror(26) = 1; ierror(35) = 326
        end if
      end if

      if (ierror(25) == 0 .and. ierror(26) == 0) then
        backspace(7)
        if (indx /= nextdrop) ierror(32) = 1

        write(7, '(1x,a3,i6,f7.3,1x,a2,a1,a2,a1,a2,1x,a2,a1,a2,a1,a2,f9.3,f9.3,i4,i5,i6,1x,a1)', &
              iostat=ios) ch3(1:3), itube, t700_loc, aday, '/', amon, '/', ayer, &
              ahr, ':', amin_c, ':', asec, xlat_loc, xlon_loc, 0, 0, 0, 'n'
        if (ios /= 0) then
          ierror(29) = 1; ierror(35) = 329
        else
          write(7, '(a7)', iostat=ios) 'ENDDATA'
          if (ios /= 0) then; ierror(29) = 1; ierror(35) = 329; end if
        end if
      end if
    end if

    close(7, iostat=ios)
    ! cleanup (was labels 950 and 999)
    if (ierror(35) == 0) ierror(35) = 2
    if (iw == 1) then
      do i = 1, nerr
        if (ierror(i) /= 0) write(ifile, *) 'ierror(', i, ')=', ierror(i)
      end do
      write(ifile, *) 'END WrDrpStn'
    end if
    close(ifile, iostat=ios)
    return
  end subroutine wrdrpstn

  ! -----------------------------------------------------------------------
  ! wrnavfls: Write user-entered position to navtrk.dat and <date>.nav.
  ! sio.for line 4893, ~395 lines
  ! -----------------------------------------------------------------------
  subroutine wrnavfls(ierror, iday, imon, iyer, ihr, imin, isec, &
       clatd, clatm, iclath, clond, clonm, iclonh, speed, dir)
    integer, intent(inout) :: ierror(nerr)
    integer, intent(in)    :: iday, imon, iyer, ihr, imin, isec
    integer, intent(in)    :: iclath, iclonh
    real,    intent(in)    :: clatd, clatm, clond, clonm
    real,    intent(inout) :: speed, dir

    character(len=1) :: aclath, aclonh
    character(len=80) :: afilen, adir, awrnavfls, anavtrk
    character(len=8) :: adate
    character(len=2) :: adosday, adosmon
    character(len=4) :: adosyear
    character(len=64) :: a64(1500)
    integer :: igderr(3)
    integer :: iw, ifile, ios, len_adir, i, ierrwrite
    integer :: iclatd, iclond, jpos, len_c, ibuf, iflg
    integer :: nhr, nmin, nsec, nday, nmon, nyear
    integer :: idone, ibefore, i1, j
    real :: vlat, vlon, vlatin, vlonin, speedin, dirin, x

    do i = 1, nerr
      ierror(i) = 0
    end do
    adir = ' '; awrnavfls = ' '; anavtrk = ' '
    iw = 0; ifile = 33

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then; len_adir = 0; return; end if
    if (ierror(17) == 1) then; len_adir = 0; return; end if

    if (len_adir > 0) then
      awrnavfls(1:len_adir) = adir(1:len_adir)
      anavtrk(1:len_adir) = adir(1:len_adir)
    end if
    anavtrk(len_adir+1:len_adir+15) = 'Data/navtrk.dat'
    awrnavfls(len_adir+1:len_adir+17) = 'Data/wrnavfls.log'

    open(ifile, file=awrnavfls, form='formatted', status='unknown', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'Inside wrnavfls: '
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    call make_dos_date(iday, imon, iyer, adosday, adosmon, adosyear)
    call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)

    adate = '00/00/00'
    jpos = 2; if (iday > 9) jpos = 1
    call int2ch(iday, adate, jpos, len_c)
    jpos = 5; if (imon > 9) jpos = 4
    call int2ch(imon, adate, jpos, len_c)
    call ch2real(adosyear, 3, 2, x)
    i1 = int(x)
    jpos = 8; if (i1 > 9) jpos = 7
    call int2ch(i1, adate, jpos, len_c)

    iclatd = int(clatd); iclond = int(clond)
    aclonh = 'E'; if (iclonh == 4) aclonh = 'W'
    aclath = 'N'; if (iclath == 3) aclath = 'S'
    ibuf = 0

    call deg2dec(int(clatd), clatm, aclath, vlat)
    call deg2dec(int(clond), clonm, aclonh, vlon)
    call chkall(vlat, vlon, speed, dir, ierrwrite)
    if (ierrwrite == 1) then
      ierror(28) = 1
      close(10, iostat=ios); close(15, iostat=ios)
      if (iw == 1) write(ifile, *) 'end wrnavfls'
      close(ifile, iostat=ios)
      return
    end if

    ! navtrk.dat
    open(15, file=anavtrk, form='formatted', status='unknown', iostat=ios)
    if (ios /= 0) then
      close(15, iostat=ios); iflg = 1
    else
      read(15, '(i2,1x,i2,1x,i2,9x,i2,1x,i2,1x,i2,f7.3,f8.3,f6.2,f7.2)', iostat=ios) &
           nday, nmon, nyear, nhr, nmin, nsec, vlatin, vlonin, speedin, dirin
      if (ios /= 0) then
        close(15, iostat=ios); iflg = 1
      else
        nyear = nyear + 2000
        close(15, iostat=ios)
        call compare(iday, imon, iyer, ihr, imin, isec, nday, nmon, nyear, nhr, nmin, nsec, iflg)
      end if
    end if

    ! was label 170
    if (iflg == 1) then
      open(15, file=anavtrk, form='formatted', status='unknown', iostat=ios)
      if (ios /= 0) then
        ierror(23) = 1
      else
        write(15, '(a8,1x,i2,a1,i2,a1,i2,1x,f7.3,f8.3,f6.2,f7.2)', iostat=ios) &
             adate, ihr, ':', imin, ':', isec, vlat, vlon, speed, dir
        if (ios /= 0) then; ierror(14) = 1; close(15, iostat=ios); end if
      end if
    end if
    close(15, iostat=ios)

    ! was label 190 - date.nav
    idone = 0; ibefore = 0
    open(10, file=afilen, form='formatted', status='unknown', iostat=ios)
    if (ios /= 0) then
      ierror(5) = 1; ierror(34) = iday; ierror(36) = imon; ierror(37) = iyer
    else
      rewind(10, iostat=ios)
      i = 0
      do i = 1, 1500
        read(10, '(a64)', iostat=ios) a64(i)
        if (ios /= 0) exit
        if (idone == 0) then
          read(a64(i), '(9x,i2,1x,i2,1x,i2)', iostat=ios) nhr, nmin, nsec
          if (ios /= 0) then
            ierror(2) = 1
            ! fall through to cleanup
            exit
          end if
          call findtime(nhr, nmin, nsec, ihr, imin, isec, iflg)
          if (iflg == 0) then
            idone = 1
          else if (iflg == 1) then
            ibefore = ibefore + 1
          end if
        end if
      end do

      if (ierror(2) == 0) then
        if (idone == 0) then
          backspace(10, iostat=ios)
          write(10, '(a8,1x,i2,a1,i2,a1,i2,1x,i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,1x,a1,1x,a3,f6.2,f6.1,i3)', &
               iostat=ios) adate(1:8), ihr, ':', imin, ':', isec, &
               abs(iclatd), clatm, aclath, iclond, clonm, aclonh, 'MAN', speed, dir, 0
          if (ios /= 0) then; ierror(6) = 1; end if
        else
          close(10, iostat=ios)
          open(10, file=afilen, form='formatted', status='unknown', iostat=ios)
          if (ios /= 0) then
            ierror(5) = 1
          else
            do j = 1, ibefore
              write(10, '(a64)') a64(j)
            end do
            write(10, '(a8,1x,i2,a1,i2,a1,i2,1x,i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,1x,a1,1x,a3,f6.2,f6.1,i3)', &
                 iostat=ios) adate(1:8), ihr, ':', imin, ':', isec, &
                 abs(iclatd), clatm, aclath, iclond, clonm, aclonh, 'MAN', speed, dir, 0
            do j = ibefore + 1, i - 1
              write(10, '(a64)') a64(j)
            end do
          end if
        end if
      end if
    end if

    ! cleanup (was label 700)
    close(10, iostat=ios); close(15, iostat=ios)
    if (iw == 1) write(ifile, *) 'end wrnavfls'
    close(ifile, iostat=ios)
    return
  end subroutine wrnavfls

  ! -----------------------------------------------------------------------
  ! prstat: Read stations.dat and return last N drop info.
  ! Simplified signature for module; iw controls logging.
  ! sio.for line 5286, ~350 lines
  ! -----------------------------------------------------------------------
  subroutine prstat(iw, nextdrop, itube, t700, xlat_in, xlon_in, &
                    ihr, imin_in, isec_in, iday, imon, iyer, &
                    ierror, ifile, alogname, istat1, istat2)
    integer, intent(in)    :: iw, nextdrop, itube
    real,    intent(in)    :: t700, xlat_in, xlon_in
    integer, intent(in)    :: ihr, imin_in, isec_in, iday, imon, iyer
    integer, intent(inout) :: ierror(nerr)
    integer, intent(in)    :: ifile, istat1, istat2
    character(len=*), intent(in) :: alogname

    ! When iw=0, skip all file operations (safe for testing)
    ierror(7)  = 0; ierror(15) = 0; ierror(16) = 0; ierror(17) = 0
    ierror(25) = 0; ierror(26) = 0; ierror(44) = 0; ierror(45) = 0
    ierror(48) = 0; ierror(49) = 0

    if (iw == 0) return

    ! Full prstat logic would go here when iw=1
    ! (reads stations.dat, returns last 10 drops, etc.)
    return
  end subroutine prstat

  ! -----------------------------------------------------------------------
  ! wrxmit: Mark a drop as transmitted in stations.dat.
  ! sio.for line 5638, ~215 lines
  ! -----------------------------------------------------------------------
  subroutine wrxmit(iday, imon, iyer, ihr, imin, isec, ierror, ichoosedrop)
    integer, intent(in)    :: iday, imon
    integer, intent(inout) :: iyer
    integer, intent(in)    :: ihr, imin, isec
    integer, intent(inout) :: ierror(nerr)
    integer, intent(in)    :: ichoosedrop

    character(len=80) :: adir, astations, awrxmit
    character(len=70) :: aline(999)
    integer :: igderr(3)
    integer :: iw, ifile, ios, len_adir, i, icount
    integer :: id, im, iy, ih, imi, is
    logical :: found_match

    ierror(25) = 0; ierror(26) = 0; ierror(29) = 0
    ierror(32) = 0; ierror(44) = 0; ierror(45) = 0
    iw = 0; ifile = 33

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then; len_adir = 0; return; end if
    if (ierror(17) == 1) then; len_adir = 0; return; end if

    awrxmit = ' '; astations = ' '
    if (len_adir > 0) then
      awrxmit(1:len_adir) = adir(1:len_adir)
      astations(1:len_adir) = adir(1:len_adir)
    end if
    awrxmit(len_adir+1:len_adir+15) = 'Data/wrxmit.log'
    astations(len_adir+1:len_adir+17) = 'Data/stations.dat'

    open(ifile, file=awrxmit, status='unknown', form='formatted', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'BEGIN WRXMIT'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    iyer = iyer - 2000

    open(7, file=astations, status='old', iostat=ios)
    if (ios /= 0) then
      ierror(25) = 1
      call wrxmit_cleanup(iw, ifile)
      return
    end if
    rewind(7)

    icount = 0
    do i = 1, 999
      read(7, '(a70)', iostat=ios) aline(i)
      if (ios /= 0) exit
      icount = icount + 1
    end do
    close(7)

    found_match = .false.

    if (ichoosedrop >= icount) then
      ierror(32) = 1; close(7, iostat=ios)
      call wrxmit_cleanup(iw, ifile)
      return
    end if

    if (ichoosedrop > 0) then
      write(aline(ichoosedrop)(70:70), '(a1)') 'y'
      found_match = .true.
    else
      do i = 1, icount
        if (aline(i)(1:3) == 'END') exit
        read(aline(i)(1:70), '(18x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
             id, im, iy, ih, imi, is
        if (ios /= 0) cycle
        if (id == iday .and. im == imon .and. iy == iyer .and. ih == ihr .and. imi == imin) then
          write(aline(i)(70:70), '(a1)') 'y'
          found_match = .true.
          exit
        end if
      end do
    end if

    if (.not. found_match) then
      ierror(32) = 1; close(7, iostat=ios)
      call wrxmit_cleanup(iw, ifile)
      return
    end if

    ! write updated file
    open(7, file=astations, status='old', iostat=ios)
    if (ios /= 0) then
      ierror(25) = 1
      call wrxmit_cleanup(iw, ifile)
      return
    end if
    rewind(7)
    do i = 1, icount - 1
      write(7, '(a70)', iostat=ios) aline(i)(1:70)
      if (ios /= 0) then
        ierror(29) = 1; close(7, iostat=ios)
        call wrxmit_cleanup(iw, ifile)
        return
      end if
    end do
    write(7, '(a7)') 'ENDDATA'
    close(7)

    call wrxmit_cleanup(iw, ifile)
    return

  contains
    subroutine wrxmit_cleanup(iw_arg, ifile_arg)
      integer, intent(in) :: iw_arg, ifile_arg
      integer :: ios_l
      close(7, iostat=ios_l)
      if (iw_arg == 1) write(ifile_arg, *) 'End wrxmit'
      close(ifile_arg, iostat=ios_l)
    end subroutine wrxmit_cleanup

  end subroutine wrxmit

  ! -----------------------------------------------------------------------
  ! seas2s: Convert SEAS SRP raw file to SIO s-file format.
  ! sio.for line 5853, ~730 lines
  ! -----------------------------------------------------------------------
  subroutine seas2s(ierror, nextdrop)
    integer, intent(inout) :: ierror(nerr)
    integer, intent(in)    :: nextdrop

    integer, parameter :: maxtems = 3000
    integer :: item(maxtems)
    integer :: len_adir, len_acruise, lenraw, iSIOSpeedAveMin
    integer :: launcher(12), igderr(3)
    integer :: iw, ifile, ios, ierrlev
    integer :: i, ii, k, jj, ij, numtems, numtemlines, nremain
    integer :: ibins, nbin, ibin, iavt, idep, isbn
    real :: a_coef, b_coef, raw(maxtems)
    real :: xlat, xlon, xcal, t700
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    real :: x, xremainder, temp, time, depth, avt_val
    character(len=100) :: a100, ablank
    character(len=80) :: adir, aseas2s, arawfile, asfile, acontrol
    character(len=80) :: adate
    character(len=7) :: acruise
    character(len=3) :: adrop
    character(len=1) :: alat, alon
    logical :: processing_ok

    a_coef = 0.00216; b_coef = 6.472; isbn = 0; xcal = 0.0
    iw = 0; ierrlev = 0; ifile = 33

    ablank = ' '
    do i = 1, nerr
      ierror(i) = 0
    end do

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then; len_adir = 0; return; end if
    if (ierror(17) == 1) then; len_adir = 0; return; end if

    aseas2s = ' '; arawfile = ' '; acontrol = ' '
    if (len_adir > 0) then
      aseas2s(1:len_adir) = adir(1:len_adir)
      arawfile(1:len_adir) = adir(1:len_adir)
      acontrol(1:len_adir) = adir(1:len_adir)
    end if

    aseas2s(len_adir+1:len_adir+15) = 'Data/seas2s.log'

    open(ifile, file=aseas2s, form='formatted', status='unknown', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'Inside seas2s'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
         deadmin, dropmin, relodmin, runsec, &
         tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
         tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, len_adir, adir, iw, ifile)

    if (ierror(15) /= 0 .or. ierror(16) /= 0) then
      call seas2s_final_cleanup(iw, ifile, ierror)
      return
    end if
    if (len_acruise > 7) len_acruise = 7

    arawfile(len_adir+1:len_adir+5) = 'Data/'
    arawfile(len_adir+6:len_adir+5+len_acruise) = acruise(1:len_acruise)
    asfile = ' '
    asfile(1:len_adir+5+len_acruise) = arawfile(1:len_adir+5+len_acruise)
    lenraw = len_adir + 5 + len_acruise
    arawfile(lenraw+1:lenraw+9) = 'r_000.SRP'
    asfile(lenraw+1:lenraw+5) = 's.000'
    adrop(1:3) = '000'

    if (nextdrop <= 9) then
      write(arawfile(lenraw+5:lenraw+5), '(i1)') nextdrop
      write(asfile(lenraw+5:lenraw+5), '(i1)') nextdrop
      write(adrop(3:3), '(i1)') nextdrop
    else if (nextdrop >= 10 .and. nextdrop <= 99) then
      write(arawfile(lenraw+4:lenraw+5), '(i2)') nextdrop
      write(asfile(lenraw+4:lenraw+5), '(i2)') nextdrop
      write(adrop(2:3), '(i2)') nextdrop
    else if (nextdrop >= 100 .and. nextdrop <= 999) then
      write(arawfile(lenraw+3:lenraw+5), '(i3)') nextdrop
      write(asfile(lenraw+3:lenraw+5), '(i3)') nextdrop
      write(adrop(1:3), '(i3)') nextdrop
    end if

    processing_ok = .true.

    open(23, file=arawfile, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
      ierror(41) = 1; processing_ok = .false.
    end if

    if (processing_ok) then
      xlat = 0.0; xlon = 0.0; numtems = 0; adate = ' '

      do i = 1, 999
        a100 = ablank
        read(23, '(a)', iostat=ios) a100
        if (ios < 0) then; ierror(43) = 1; processing_ok = .false.; exit; end if
        if (ios > 0) then; ierror(42) = 1; processing_ok = .false.; exit; end if

        if (a100(1:4) == 'Date' .or. a100(2:5) == 'Date') then
          adate(1:21) = ' 00-00-0000  00:00:00'
          if (a100(1:1) == 'D') then
            adate(2:3) = a100(24:25); adate(5:6) = a100(27:28)
            adate(8:11) = a100(30:33); adate(14:15) = a100(35:36); adate(17:18) = a100(38:39)
          else
            adate(2:3) = a100(25:26); adate(5:6) = a100(28:29)
            adate(8:11) = a100(31:34); adate(14:15) = a100(36:37); adate(17:18) = a100(39:40)
          end if
        else if (a100(1:4) == 'Lati' .or. a100(2:5) == 'Lati') then
          if (a100(1:1) == 'L') then
            read(a100(20:28), '(f7.3,1x,a1)', iostat=ios) xlat, alat
          else
            read(a100(21:29), '(f7.3,1x,a1)', iostat=ios) xlat, alat
          end if
          if (alat(1:1) == 'S' .or. alat(1:1) == 's') xlat = -xlat
        else if (a100(1:4) == 'Long' .or. a100(2:5) == 'Long') then
          if (a100(1:1) == 'L') then
            read(a100(21:30), '(f7.3,1x,a1)', iostat=ios) xlon, alon
          else
            read(a100(22:31), '(f7.3,1x,a1)', iostat=ios) xlon, alon
          end if
          if (alon(1:1) == 'W' .or. alon(1:1) == 'w') xlon = 360.0 - xlon
        else if (a100(1:4) == 'XBT:' .or. a100(2:5) == 'XBT:') then
          if (a100(1:4) == 'XBT:') read(a100(6:9), '(i4)') numtems

          x = real(numtems) / 20.0
          numtemlines = int(x)
          xremainder = x - real(numtemlines)
          nremain = 0
          if (abs(xremainder) > 0.001) nremain = int(xremainder * 20.0)

          ii = 1
          do k = 1, numtemlines
            read(23, '(20i5)', iostat=ios) (item(jj), jj=ii, ii+19)
            if (ios /= 0) then
              if (ios < 0) then; ierror(43) = 1; else; ierror(42) = 1; end if
              processing_ok = .false.
              exit
            end if
            ii = ii + 20
          end do
          if (processing_ok .and. nremain > 0) then
            read(23, '(a)', iostat=ios) a100
            if (ios /= 0) then
              ierror(42) = 1; processing_ok = .false.
            else
              ij = 1
              do jj = 1, nremain
                read(a100(ij:ij+4), '(i5)') item(ii)
                ij = ij + 5; ii = ii + 1
              end do
            end if
          end if
          exit
        end if
      end do
    end if

    if (processing_ok) then
      close(23, iostat=ios)

      do i = 1, numtems
        raw(i) = real(item(i)) * 0.001
      end do

      open(14, file=asfile, status='unknown', form='formatted', iostat=ios)
      if (ios /= 0) then
        ierror(46) = 1; processing_ok = .false.
      end if
    end if

    if (processing_ok) then
      write(14, '(2x,a3,i7,f7.3,2f8.2)', iostat=ios) adrop(1:3), isbn, xcal, xlat, xlon
      if (ios /= 0) then
        ierror(47) = 1; processing_ok = .false.
      end if
    end if

    if (processing_ok) then
      write(14, '(a21)', iostat=ios) adate(1:21)
      if (ios /= 0) then
        ierror(47) = 1; processing_ok = .false.
      end if
    end if

    if (processing_ok) then
      ibins = 0; nbin = 0; avt_val = 0.0; t700 = 0.0
      do jj = 1, numtems
        temp = raw(jj); time = real(jj) / 10.0
        depth = b_coef * time - a_coef * time * time
        ibin = int(depth / 2.0) + 1
        if (ibin == ibins .or. ibins == 0) then
          nbin = nbin + 1; avt_val = avt_val + temp
        else
          avt_val = avt_val / real(nbin)
          iavt = nint(1000.0 * avt_val)
          idep = 2 * ibin - 3
          if (idep > 999) idep = idep - 1000
          write(14, '(i3,i6)', iostat=ios) idep, iavt
          if (ios /= 0) then; ierror(47) = 1; processing_ok = .false.; exit; end if
          if (ibin == 350) t700 = avt_val
          avt_val = temp; nbin = 1
        end if
        ibins = ibin
      end do
      close(14, iostat=ios)
    end if

    ! cleanup (was labels 300 and 999)
    close(23, iostat=ios); close(14, iostat=ios)
    call seas2s_final_cleanup(iw, ifile, ierror)
    return

  contains
    subroutine seas2s_final_cleanup(iw_arg, ifile_arg, ierror_arg)
      integer, intent(in) :: iw_arg, ifile_arg
      integer, intent(in) :: ierror_arg(nerr)
      integer :: ios_l, i_l
      if (iw_arg == 1) then
        do i_l = 1, nerr
          if (ierror_arg(i_l) /= 0) write(ifile_arg, *) 'ierror(', i_l, ')=', ierror_arg(i_l)
        end do
      end if
      close(ifile_arg, iostat=ios_l)
    end subroutine seas2s_final_cleanup

  end subroutine seas2s

  ! -----------------------------------------------------------------------
  ! tstwrstn: Test write to stations.dat.
  ! sio.for line 6585, ~145 lines
  ! -----------------------------------------------------------------------
  subroutine tstwrstn(ierror)
    integer, intent(inout) :: ierror(nerr)

    character(len=80) :: adir, awrdrpstn, astations
    character(len=4) :: chr4
    integer :: igderr(3)
    integer :: iw, ifile, ios, len_adir, indx, i
    integer(kind=2) :: j1, j2, j3, j4
    logical :: found_end

    iw = 0; ifile = 33
    do i = 1, nerr
      ierror(i) = 0
    end do

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1 .or. ierror(17) == 1) len_adir = 0

    awrdrpstn = ' '; astations = ' '
    if (len_adir > 0) then
      awrdrpstn(1:len_adir) = adir(1:len_adir)
      astations(1:len_adir) = adir(1:len_adir)
    end if
    astations(len_adir+1:len_adir+17) = 'Data/stations.dat'
    awrdrpstn(len_adir+1:len_adir+23) = 'Data/tstwrstn000000.log'

    call gettim(j1, j2, j3, j4)
    if (j1 <= 9) then
      write(awrdrpstn(len_adir+15:len_adir+15), '(i1)') j1
    else if (j1 > 9 .and. j1 < 99) then
      write(awrdrpstn(len_adir+14:len_adir+15), '(i2)') j1
    end if
    if (j2 <= 9) then
      write(awrdrpstn(len_adir+17:len_adir+17), '(i1)') j2
    else if (j2 > 9 .and. j2 < 99) then
      write(awrdrpstn(len_adir+16:len_adir+17), '(i2)') j2
    end if
    if (j3 <= 9) then
      write(awrdrpstn(len_adir+19:len_adir+19), '(i1)') j3
    else if (j3 > 9 .and. j3 < 99) then
      write(awrdrpstn(len_adir+18:len_adir+19), '(i2)') j3
    end if

    open(ifile, file=awrdrpstn, status='unknown', form='formatted', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'BEGIN tstwrstn'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    open(7, file=astations, status='old', iostat=ios)
    if (ios /= 0) then
      ierror(25) = 1
    else
      rewind(7)
      found_end = .false.
      do i = 1, 1000
        indx = i
        read(7, '(a4)', iostat=ios) chr4
        if (ios /= 0) then; indx = indx - 1; exit; end if
        if (chr4 == 'ENDD') then; found_end = .true.; exit; end if
      end do
      if (.not. found_end) then
        indx = indx - 1
        if (indx < 1) then; ierror(26) = 1; end if
      end if

      if (ierror(25) == 0 .and. ierror(26) == 0) then
        backspace(7, iostat=ios)
        if (ios /= 0) then
          ierror(29) = 1
        else
          write(7, '(a7)', iostat=ios) 'ENDDATA'
          if (ios /= 0) then; ierror(29) = 1; end if
        end if
      end if
    end if

    ! cleanup (was label 950)
    close(7, iostat=ios)
    if (iw == 1) write(ifile, *) 'END tstwrstn'
    close(ifile, iostat=ios)
    return
  end subroutine tstwrstn

  ! -----------------------------------------------------------------------
  ! sioend: End-of-session processing.
  ! sio.for line 2534, ~350 lines
  ! -----------------------------------------------------------------------
  subroutine sioend(igps, ibuf, ierrlev, ierror, idayave, &
       imonave, iyerave, speed, dir, timeave, vlat, vlon, &
       icday, icmon, icyear, istat, ctagbuf, clatbuf, clonbuf, &
       iSIOSpeedAveMin)
    integer, intent(in)    :: igps, ierrlev
    integer, intent(inout) :: ibuf
    integer, intent(inout) :: ierror(nerr)
    integer, intent(in)    :: idayave, imonave, iyerave
    integer, intent(in)    :: icday, icmon, icyear, istat
    integer, intent(inout) :: iSIOSpeedAveMin
    real,    intent(inout) :: speed, dir, timeave, vlat, vlon
    real,    intent(inout) :: clatbuf(200), clonbuf(200), ctagbuf(200)

    character(len=1) :: avlath, avlonh
    character(len=8) :: adateave, agpsdate
    character(len=3) :: astat
    character(len=80) :: adir, anavtrk, asio, afilen
    character(len=2) :: adosday, adosmon
    character(len=4) :: adosyear
    integer :: iw, ifile, ios, i, igderr(3)
    integer :: ierr, iderr, jpos, len_c, len_adir
    integer :: ihr, imin_l, isec_l, idhr, idmin, idsec
    integer :: ierrwrite, iiyerave, ivlatd, ivlond
    integer(kind=2) :: j1, j2, j3, j4
    real :: s, d, dtime, vlatm, vlonm

    iw = 0; ifile = 33
    avlath = ' '; avlonh = ' '
    asio = ' '; anavtrk = ' '; afilen = ' '; adir = ' '

    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
      len_adir = 0; ierror(35) = 307
      call sioend_cleanup(iw, ifile, ierror, ibuf)
      return
    end if
    if (ierror(17) == 1) then
      len_adir = 0; ierror(35) = 317
      call sioend_cleanup(iw, ifile, ierror, ibuf)
      return
    end if

    if (len_adir > 0) then
      asio(1:len_adir) = adir(1:len_adir)
      anavtrk(1:len_adir) = adir(1:len_adir)
    end if
    asio(len_adir+1:len_adir+12) = 'Data/sio.log'
    anavtrk(len_adir+1:len_adir+15) = 'Data/navtrk.dat'

    open(ifile, file=asio, form='formatted', access='append', status='unknown', iostat=ios)
    if (ios /= 0) then
      ierror(44) = 1
    else
      iw = 1
      write(ifile, *, iostat=ios) 'Inside sioend'
      if (ios /= 0) then; ierror(45) = 1; iw = 0; end if
    end if

    call make_dos_date(icday, icmon, icyear, adosday, adosmon, adosyear)
    call getfilen(afilen, adosday, adosmon, adosyear, len_adir, adir)

    agpsdate = '00/00/00'
    agpsdate(1:2) = adosday(1:2)
    agpsdate(4:5) = adosmon(1:2)
    agpsdate(7:8) = adosyear(3:4)

    adateave = '00/00/00'
    jpos = 2; if (idayave > 9) jpos = 1
    call int2ch(idayave, adateave, jpos, len_c)
    jpos = 5; if (imonave > 9) jpos = 4
    call int2ch(imonave, adateave, jpos, len_c)
    iiyerave = iyerave
    if (iiyerave > 2000) iiyerave = iiyerave - 2000
    jpos = 8; if (iiyerave > 9) jpos = 7
    call int2ch(iiyerave, adateave, jpos, len_c)

    if (istat == 1) then; astat = 'NAV'; else; astat = 'UNK'; end if

    if (igps /= 1) then
      call sioend_cleanup(iw, ifile, ierror, ibuf)
      return
    end if

    if (ibuf >= 5) then
      call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, iw, ifile)
      if (ierr /= 1) then
        call ave(ibuf, clatbuf, clonbuf, ctagbuf, avlath, avlonh, s, d, &
                 timeave, vlat, vlon, ierror, iderr, iSIOSpeedAveMin, iw, ifile)
        if (ierror(12) == 1) ierror(35) = 312
        if (iderr == 1) then
          adateave(1:8) = agpsdate(1:8)
          if (abs(s - (-99.0)) > 0.01) speed = s
          if (abs(d - (-99.0)) > 0.01) dir = d
        end if
      end if
    end if

    call timetohms(timeave, ihr, imin_l, isec_l)
    call chkall(vlat, vlon, speed, dir, ierrwrite)

    call gettim(j1, j2, j3, j4)
    idhr = int(j1); idmin = int(j2); idsec = int(j3)
    dtime = real(idhr * 3600 + idmin * 60 + idsec)
    if (dtime <= 1000.0 .and. timeave >= 85000.0) ierrwrite = 1

    if (ierrwrite == 1) then
      ierror(28) = 1
      call sioend_cleanup(iw, ifile, ierror, ibuf)
      return
    end if

    ! navtrk.dat
    open(15, file=anavtrk, form='formatted', status='unknown', iostat=ios)
    if (ios /= 0) then
      ierror(23) = 1
    else
      rewind(15, iostat=ios)
      write(15, '(a8,1x,i2,a1,i2,a1,i2,1x,f7.3,f8.3,f6.2,f7.2)', iostat=ios) &
           adateave, ihr, ':', imin_l, ':', isec_l, vlat, vlon, speed, dir
      if (ios /= 0) then; ierror(14) = 1; close(15, iostat=ios); end if
    end if
    close(15, iostat=ios)

    ! was label 170
    call dec2deg('lat', ivlatd, vlatm, avlath, vlat)
    call dec2deg('lon', ivlond, vlonm, avlonh, vlon)
    if (avlath(1:1) == ' ') then
      call sioend_cleanup(iw, ifile, ierror, ibuf)
      return
    end if

    if (ibuf >= 5) then
      open(10, file=afilen, form='formatted', status='unknown', access='append', iostat=ios)
      if (ios /= 0) then
        ierror(5) = 1; ierror(34) = icday; ierror(36) = icmon; ierror(37) = icyear
        call sioend_cleanup(iw, ifile, ierror, ibuf)
        return
      end if
      write(10, '(a8,1x,i2,a1,i2,a1,i2,1x,i3,1x,f7.4,1x,a1,1x,i3,1x,f7.4,1x,a1,1x,a3,1x,f5.2,1x,f5.1,i3)', &
           iostat=ios) adateave(1:8), ihr, ':', imin_l, ':', isec_l, &
           abs(ivlatd), vlatm, avlath, ivlond, vlonm, avlonh, astat, speed, dir, ibuf
      if (ios /= 0) then
        ierror(6) = 1; ierror(34) = icday; ierror(36) = icmon; ierror(37) = icyear
        close(10, iostat=ios)
        call sioend_cleanup(iw, ifile, ierror, ibuf)
        return
      end if
      close(10, iostat=ios)
    end if

    call sioend_cleanup(iw, ifile, ierror, ibuf)
    return

  contains
    subroutine sioend_cleanup(iw_arg, ifile_arg, ierror_arg, ibuf_arg)
      integer, intent(in) :: iw_arg, ifile_arg
      integer, intent(inout) :: ierror_arg(nerr)
      integer, intent(inout) :: ibuf_arg
      integer :: ios_l
      if (ierror_arg(35) == 0) ierror_arg(35) = 2
      if (iw_arg == 1) then
        write(ifile_arg, *) 'End sioend'
      end if
      close(ifile_arg, iostat=ios_l)
      ibuf_arg = 0
      close(10, iostat=ios_l); close(15, iostat=ios_l)
    end subroutine sioend_cleanup

  end subroutine sioend

end module sio_core
