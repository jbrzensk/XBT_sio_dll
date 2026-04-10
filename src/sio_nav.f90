! src/sio_nav.f90
module sio_nav
  use sio_math,    only: dpolft, dp1vlu
  use sio_time,    only: gettmtg
  use sio_convert, only: dec2deg, deg2dec
  implicit none
  private
  public :: ave, newpos, xbteta, interp, planinfo, chkall, chkbuf, chkwrite

  integer, parameter :: nerr = 50

contains

  ! Average GPS positions, compute speed and direction. siosub.for:17.
  ! ibuf       — number of GPS fixes in buffers (1..200)
  ! xlat(200)  — latitude buffer
  ! xlon(200)  — longitude buffer
  ! timetag(200) — GPS timetag buffer
  ! s10,d10    — output: averaged speed (kt), direction (deg true)
  ! timeave,vlat,vlon — in/out: last averaged position timetag/lat/lon
  ! ierror(38) — jptr counter; ierror(39) — icall counter
  ! iSIOSpeedAveMin — minutes of data to use for speed/dir averaging
  subroutine ave(ibuf, xlat, xlon, timetag, avlath, avlonh, &
                 s10, d10, timeave, vlat, vlon, ierror, &
                 ierr, iSIOSpeedAveMin, iw, ifile)
    integer,          intent(in)    :: ibuf, iw, ifile
    real,             intent(inout) :: xlat(200), xlon(200), timetag(200)
    real,             intent(inout) :: s10, d10, timeave, vlat, vlon
    character(len=1), intent(out)   :: avlath, avlonh
    integer,          intent(inout) :: ierror(nerr), iSIOSpeedAveMin
    integer,          intent(out)   :: ierr

    ! SAVE state for persistent averaging buffers
    real,    save :: tbuf(10), xltbuf(10), xlnbuf(10)
    real,    save :: ylatsav, ylonsav
    integer, save :: iSIOsave, iSIOset

    real    :: w(200), r(200), b(220)
    real    :: deg2rad, eps
    real    :: stime, svlat, svlon
    real    :: timeave1
    real    :: yfit, yp(1), yplat, yplon, yfitlat
    real    :: speed, dir
    real    :: x10, y10, t10, x
    real    :: ylatdif, ylondif
    real    :: prev_ylat, prev_ylon
    integer :: jptr, icall, ifirst, ndeg, iptr
    integer :: i, ideg
    real    :: xlatm, xlonm

    deg2rad = 3.141592654 / 180.0

    ! Guard against empty buffer (documented range is 1..200)
    if (ibuf < 1) then
      ierr = -1
      return
    end if

    ! Recover persistent counters from ierror
    jptr  = ierror(38)
    icall = ierror(39)
    ifirst = icall + 1

    ! Save incoming averaged position
    stime = timeave
    svlat = vlat
    svlon = vlon

    ! First call: zero out the averaging buffers
    if (ifirst == 1) then
      do i = 1, 10
        tbuf(i)   = 0.0
        xltbuf(i) = 0.0
        xlnbuf(i) = 0.0
      end do
    end if

    ! Debug output
    if (iw == 1 .and. ierror(33) /= 0) then
      write(ifile,*) 'in ave, iSIOSpeedAveMin=', iSIOSpeedAveMin
      write(ifile,*) 'jptr,icall,ifirst=', jptr, icall, ifirst
      write(ifile,*) 'stime,svlat,svlon=', stime, svlat, svlon
    end if

    w(1) = -1.0
    eps  = 0.0

    ! Saturday night rollover: GPS timetag wraps from ~604800 back to ~0
    if (timetag(1) > 600000.0 .and. timetag(ibuf) < 6000.0) then
      do i = 1, ibuf
        if (timetag(i) < 600.0) timetag(i) = timetag(i) + 604800.0
      end do
    end if

    if (iw == 1) then
      write(ifile,*) 'begin ave, ibuf=', ibuf, ' timeave=', timeave
    end if

    ! Compute mean timetag; subtract from each timetag for fitting
    timeave1 = 0.0
    do i = 1, ibuf
      timeave1 = timeave1 + timetag(i)
    end do
    timeave1 = timeave1 / real(ibuf)

    do i = 1, ibuf
      timetag(i) = timetag(i) - timeave1
    end do

    ! Handle 0/360 lon crossing in buffer
    if ( (xlon(1) > 359.0 .and. xlon(ibuf) < 1.0) .or. &
         (xlon(1) < 1.0   .and. xlon(ibuf) > 359.0) ) then
      do i = 1, ibuf
        if (xlon(i) < 1.0) xlon(i) = 360.0 + xlon(i)
      end do
    end if

    ! Polynomial fit for latitude
    call dpolft(ibuf, timetag, xlat, w, 1, ndeg, eps, r, ierr, b)
    if (ierr /= 1) return

    x = 0.0
    call dp1vlu(1, 1, x, yfit, yp, b)

    ! Compute lat speed (deg/sec → nm/hr)
    yp(1) = -99.0
    yplat = -99.0
    if ( (timeave1 - stime) < 150.0 .and. (timeave1 - stime) > 0.0 ) then
      yp(1) = (yfit - vlat) / (timeave1 - stime)
    end if
    if (yp(1) /= -99.0) yplat = 216000.0 * yp(1)
    yfitlat  = yfit
    prev_ylat = ylatsav    ! read before overwriting (C3: save prev for sanity check)
    ylatsav  = yfitlat
    vlat     = yfitlat
    call dec2deg('lat', ideg, xlatm, avlath, vlat)

    ! Polynomial fit for longitude
    w(1) = -1.0
    eps  = 0.0
    call dpolft(ibuf, timetag, xlon, w, 1, ndeg, eps, r, ierr, b)
    if (ierr /= 1) return

    ! Accept timeave1 as new timeave only after successful lon fit
    timeave = timeave1
    x = 0.0
    call dp1vlu(1, 1, x, yfit, yp, b)

    ! Fix up lon wrap artifact (subtract, not 360-yfit which would invert the sign)
    if (yfit > 360.0) yfit = yfit - 360.0

    yp(1) = -99.0
    yplon = -99.0
    if ( (timeave - stime) < 150.0 .and. (timeave - stime) > 0.0 ) then
      yp(1) = (yfit - vlon) / (timeave1 - stime)
    end if
    if (yp(1) /= -99.0) yplon = 216000.0 * cos(yfitlat * deg2rad) * yp(1)

    prev_ylon = ylonsav    ! read before overwriting (C3: save prev for sanity check)
    ylonsav   = yfit
    vlon      = yfit
    call dec2deg('lon', ideg, xlonm, avlonh, vlon)

    ! Compute instantaneous speed and direction from polynomial derivatives
    speed = -99.0
    dir   = -99.0
    if (yplat /= -99.0) then
      speed = sqrt(yplat**2 + yplon**2)
      if (speed == 0.0) then
        dir = acos(max(-1.0, min(1.0, yplon))) * (1.0 / deg2rad)
      else
        dir = acos(max(-1.0, min(1.0, yplon / speed))) * (1.0 / deg2rad)
      end if
      if (yplat < 0.0) dir = -dir
      dir = 90.0 - dir
      if (dir < 0.0) dir = dir + 360.0

      ! Adaptive averaging window: slow speed → shrink window
      if (speed < 8.0) then
        if (iSIOSpeedAveMin >= 5) then
          iSIOsave = iSIOSpeedAveMin
          iSIOset  = 2
          iSIOSpeedAveMin = 2
          jptr  = 0
          icall = 0
        end if
      else if (speed >= 8.0 .and. iSIOset == 2) then
        iSIOSpeedAveMin = iSIOsave
        iSIOset = 0
      end if
    end if

    if (iw == 1) write(ifile,*) 'current speed&dir=', speed, dir

    ! Check for large position jump (DR sanity) — compare with PREVIOUS fitted position
    if (ifirst /= 1) then
      ylatdif = prev_ylat - vlat
      ylondif = prev_ylon - vlon
      if ( abs(ylatdif) > 1.0 .or. &
           (abs(ylondif) > 1.0 .and. abs(ylondif) < 350.0) ) then
        ierror(12) = 1
      end if
    end if

    ! Reset if we crossed the 360 lon boundary
    if (jptr > 1 .and. abs(xlnbuf(1) - vlon) > 350.0) then
      ifirst = 1
      jptr   = 0
      icall  = 0
    end if

    ! Reset on Saturday night rollover
    if (jptr > 1 .and. abs(tbuf(1) - timeave) > 35000.0) then
      ifirst = 1
      jptr   = 0
      icall  = 0
      if (iw == 1) write(ifile,*) 'reset jptr in ave', jptr, ifirst, icall
    end if

    ! Clamp iSIOSpeedAveMin to valid range
    if (iSIOSpeedAveMin < 1 .or. iSIOSpeedAveMin > 10) iSIOSpeedAveMin = 10

    d10 = -99.0
    s10 = -99.0

    jptr  = jptr + 1
    icall = icall + 1

    ! Need at least 2 points in ring buffer before computing s10/d10
    if (jptr == 1) then
      tbuf(jptr)   = timeave
      xltbuf(jptr) = vlat
      xlnbuf(jptr) = vlon
      ierror(38)   = jptr
      ierror(39)   = icall
      return
    end if

    ! Wrap ring buffer pointer
    if (jptr >= iSIOSpeedAveMin + 1) jptr = 1
    iptr = 1
    if (icall >= iSIOSpeedAveMin + 1) iptr = jptr

    t10 = timeave - tbuf(iptr)

    if (t10 > 650.0 .or. t10 <= 0.0) then
      ! Gap too large or non-positive (rollover/reset) — fall back to instantaneous
      s10 = speed
      d10 = dir
    else
      x10 = 216000.0 * cos(vlat * deg2rad) * (vlon - xlnbuf(iptr)) / t10
      y10 = 216000.0 * (vlat - xltbuf(iptr)) / t10
      s10 = sqrt(x10*x10 + y10*y10)
      if (s10 == 0.0) then
        d10 = acos(max(-1.0, min(1.0, x10))) * (1.0 / deg2rad)
      else
        d10 = acos(max(-1.0, min(1.0, x10 / s10))) * (1.0 / deg2rad)
      end if
      if (y10 < 0.0) d10 = -d10
      d10 = 90.0 - d10
      if (d10 < 0.0) d10 = d10 + 360.0
    end if

    tbuf(jptr)   = timeave
    xltbuf(jptr) = vlat
    xlnbuf(jptr) = vlon

    ierror(38) = jptr
    ierror(39) = icall

    if (iw == 1) write(ifile,*) 's10,d10', s10, d10

  end subroutine ave

  ! Dead-reckon a new position from speed, direction, elapsed time. siosub.for:1955.
  subroutine newpos(speed, change, dir, vlat, vlat1, vlon1, aclath, &
                   ierrlev, ifile)
    real,             intent(in)    :: speed, change, dir, vlat
    real,             intent(inout) :: vlat1, vlon1
    character(len=1), intent(out)   :: aclath
    integer,          intent(in)    :: ierrlev, ifile

    real, parameter :: deg2rad = 3.141592654 / 180.0
    real :: speedsec, distnew, dxlatnm1, dxlonnm1, dxlat1, dxlon1, x

    aclath = 'N'
    if (vlat < 0.0) aclath = 'S'

    ! Convert knots → nm/sec
    speedsec = speed / 3600.0
    ! Distance travelled in 'change' seconds
    distnew  = change * speedsec

    if (ierrlev >= 6) write(ifile,*) ' newpos: distnew =', distnew

    ! Resolve into lat/lon components (nm)
    dxlatnm1 = distnew * cos(dir * deg2rad)
    dxlonnm1 = distnew * sin(dir * deg2rad)

    ! Convert nm to degrees
    dxlat1 = dxlatnm1 / 60.0
    x = cos(vlat * deg2rad)
    if (x == 0.0) then
      dxlon1 = dxlonnm1
    else
      dxlon1 = dxlonnm1 / (60.0 * x)
    end if

    ! Update latitude
    if (dir > 270.0 .or. dir < 90.0) then
      vlat1 = vlat1 + abs(dxlat1)
    else
      vlat1 = vlat1 - abs(dxlat1)
    end if

    ! Update longitude (heading E adds, heading W subtracts)
    if (dir >= 0.0 .and. dir < 180.0) then
      vlon1 = vlon1 + abs(dxlon1)
    else
      vlon1 = vlon1 - abs(dxlon1)
    end if

    ! Wrap longitude to [0, 360)
    if (vlon1 > 360.0) vlon1 = vlon1 - 360.0
    if (vlon1 < 0.0)   vlon1 = vlon1 + 360.0

    if (ierrlev >= 6) write(ifile,*) 'out newpos,vlat1,vlon1', vlat1, vlon1

  end subroutine newpos

  ! Compute ETA to next drop positions. siosub.for:2233.
  ! Note: simplified signature vs. original — ispec array and peta output
  ! are embedded; caller passes ispec(12) via ierror-style array.
  subroutine xbteta(xlatload, vlat1, vlon1, speed, &
                    ispec, iplandir, iw, ifile, ierror)
    real,    intent(in)    :: xlatload(12), vlat1, vlon1, speed
    integer, intent(in)    :: ispec(12), iplandir, iw, ifile
    integer, intent(inout) :: ierror(nerr)

    ! stub — xbteta signature in skeleton differs substantially from
    ! the original (which passes dir, ctime, nplan, xlat, xlon, ixhr,
    ! ixmin, ixsec, ierrlev, nlnchr, peta, ifile).  The skeleton
    ! interface is what tests/callers use; leave as no-op until
    ! a concrete caller is known.
    if (iw == 1) write(ifile,*) 'xbteta: iplandir=', iplandir, &
                                 ' vlat1=', vlat1, ' vlon1=', vlon1

  end subroutine xbteta

  ! Interpolate position at a given yearday. siosub.for:1843.
  ! Linear interpolation of lat/lon at time yrdrop between two known
  ! nav positions: (yrsav, zlat, zlon) [past] and (yrnav, xlat, xlon) [prev].
  subroutine interp(yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav, &
                    xlat, xlon)
    real, intent(in)  :: yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav
    real, intent(out) :: xlat, xlon

    real :: frac, yrdenom

    yrdenom = yrnav - yrsav
    if (yrdenom == 0.0) yrdenom = 0.001   ! avoid divide-by-zero

    frac = (yrdrop - yrsav) / yrdenom

    ! Interpolate latitude
    xlat = zlat + (ylat - zlat) * frac

    ! Handle 0/360 lon crossing before interpolating
    if (abs(ylon - zlon) > 300.0) then
      if (ylon > 300.0) then
        ! zlon is near 0, ylon is near 360 → adjust zlon up
        xlon = (zlon + 360.0) + (ylon - (zlon + 360.0)) * frac
      else
        ! zlon is near 360, ylon is near 0 → adjust ylon up
        xlon = zlon + ((ylon + 360.0) - zlon) * frac
      end if
    else
      xlon = zlon + (ylon - zlon) * frac
    end if

  end subroutine interp

  ! Extract plan position information. siosub.for:2019.
  ! Determines ispec (lat=1 or lon=0) and iplandir (N=1,E=2,S=3,W=4).
  subroutine planinfo(xlat, alath, xlat1, ahemi, aspec, ispec, &
                      iplandir, vlat1, vlon1)
    real,             intent(in)  :: xlat, xlat1, vlat1, vlon1
    character(len=1), intent(in)  :: alath, ahemi
    character(len=3), intent(out) :: aspec
    integer,          intent(out) :: ispec, iplandir

    ! vlat1, vlon1, ahemi: API-compatibility arguments retained for callers
    ! that pass ship position; not used in this implementation.
    ! The references below prevent unused-argument warnings from -Wextra.
    if (vlat1 + vlon1 < -1.0e30 .or. ahemi == char(0)) continue

    ! Determine whether plan is latitude-based or longitude-based
    if (alath == 'E' .or. alath == 'e' .or. &
        alath == 'W' .or. alath == 'w') then
      aspec = 'lon'
    else
      aspec = 'lat'
    end if

    ispec = 1
    if (aspec == 'lon') ispec = 0

    iplandir = 1   ! default: northward

    if (aspec == 'lat') then
      if (xlat > xlat1) then
        iplandir = 3   ! S
      else if (xlat < xlat1) then
        iplandir = 1   ! N
      end if
    else
      ! lon-based plan
      if (xlat > xlat1) then
        iplandir = 4   ! W
      else if (xlat < xlat1) then
        iplandir = 2   ! E
      end if
      ! Handle 0/360 wrap-around
      if (xlat > 350.0 .and. xlat1 < 10.0) iplandir = 4   ! W (crossing 0)
      if (xlat < 10.0  .and. xlat1 > 350.0) iplandir = 2  ! E (crossing 0)
    end if

  end subroutine planinfo

  ! Validate speed, direction, lat, lon are in physical range. siosub.for:455.
  subroutine chkall(xlat, xlon, speed, dir, ierr)
    real,    intent(in)  :: xlat, xlon, speed, dir
    integer, intent(out) :: ierr

    ierr = 0
    if (speed < 0.0  .or. speed > 99.99) ierr = 1
    if (dir   < 0.0  .or. dir   > 360.0) ierr = 1
    if (xlat  < -90.0 .or. xlat > 90.0)  ierr = 1
    if (xlon  < 0.0  .or. xlon  > 360.0) ierr = 1

  end subroutine chkall

  ! Validate GPS buffer values for outliers. siosub.for:482.
  subroutine chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, iw, ifile)
    integer, intent(in)    :: iw, ifile
    integer, intent(inout) :: ibuf
    real,    intent(inout) :: clatbuf(200), clonbuf(200), ctagbuf(200)
    integer, intent(out)   :: ierr

    real    :: clatbuf1(200), clonbuf1(200), ctagbuf1(200)
    integer :: imark(200)
    integer :: i, ibad, ibad2, ibufnew

    ierr   = 0
    ibad   = 0
    ibad2  = 0
    ibufnew = 0

    do i = 1, ibuf
      imark(i) = 0
    end do

    ! Saturday-night rollover in timetags
    if (ctagbuf(1) > 604500.0) then
      if (iw == 1) write(ifile,*) 'Sat night? DOS:, ibuf=', ibuf
      do i = 1, ibuf
        if (ctagbuf(i) < 1000.0) ctagbuf(i) = ctagbuf(i) + 604800.0
      end do
    end if

    ! Flag timetags that are out-of-order or too far from last entry
    do i = 1, ibuf - 1
      if ( ctagbuf(i) > ctagbuf(ibuf) .or. &
           (ctagbuf(ibuf) - ctagbuf(i)) > 400.0 ) then
        ibad = ibad + 1
        imark(i) = 1
      end if
    end do

    if (ibad >= (ibuf - 1) .or. ibad > 10) then
      ierr = 1
      return
    end if

    ! Flag lat/lon points too far from last entry
    do i = 1, ibuf - 1
      if ( abs(clatbuf(ibuf) - clatbuf(i)) > 0.5 .or. &
           abs(clonbuf(ibuf) - clonbuf(i)) > 0.5 ) then
        ibad2 = ibad2 + 1
        imark(i) = 2
      end if
    end do

    if (ibad2 >= (ibuf - 1) .or. ibad2 > 10) then
      ierr = 1
      return
    end if

    ! If no bad points, done
    if (ibad == 0 .and. ibad2 == 0) return

    ! Pack out the bad points
    do i = 1, ibuf
      if (imark(i) == 0) then
        ibufnew = ibufnew + 1
        ctagbuf1(ibufnew) = ctagbuf(i)
        clatbuf1(ibufnew) = clatbuf(i)
        clonbuf1(ibufnew) = clonbuf(i)
      end if
    end do

    ibuf = ibufnew
    do i = 1, ibuf
      ctagbuf(i) = ctagbuf1(i)
      clatbuf(i) = clatbuf1(i)
      clonbuf(i) = clonbuf1(i)
    end do

  end subroutine chkbuf

  ! Validate lat/lon before writing to nav file. siosub.for:810.
  subroutine chkwrite(ylat, ylon, ierr)
    real,    intent(in)  :: ylat, ylon
    integer, intent(out) :: ierr

    ierr = 0
    if (ylat < -90.0 .or. ylat > 90.0)  ierr = 1
    if (ylon < 0.0   .or. ylon > 360.0) ierr = 1

  end subroutine chkwrite

end module sio_nav
