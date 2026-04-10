! src/sio_io.f90
module sio_io
  use sio_convert, only: ch2real, dec2deg, deg2dec, findspace
  use sio_time,    only: compare, findtime
  implicit none
  private
  public :: getdir, navopen, chknav, getfilen, decodeplan, rdcntrl

  integer, parameter :: nerr = 50

contains

  ! ---------------------------------------------------------------------------
  ! getdir: Read siodir.txt to get XBT data directory path. siosub.for:1609.
  ! ierror(7)=1  error opening siodir.txt
  ! ierror(17)=1 error reading siodir.txt
  ! igderr(1..3) — iostat values for open/read/close
  ! adir     — output: directory path string
  ! len_adir — output: length of adir
  subroutine getdir(adir, len_adir, ierror, igderr)
    character(len=80), intent(out)   :: adir
    integer,           intent(out)   :: len_adir
    integer,           intent(inout) :: ierror(nerr)
    integer,           intent(out)   :: igderr(3)

    integer :: i, ios
    character(len=1) :: a

    ierror(7)  = 0
    ierror(17) = 0
    igderr(1)  = 0
    igderr(2)  = 0
    igderr(3)  = 0
    len_adir   = 0
    adir       = ' '

    open(31, file='c:\Users\Public\Documents\siodir.txt', &
         status='old', form='formatted', iostat=ios)
    if (ios /= 0) then
      igderr(1) = ios
      ierror(7) = 1
      return
    end if

    read(31, '(a)', iostat=ios) adir
    if (ios /= 0) then
      igderr(2) = ios
      ierror(17) = 1
      close(31, iostat=ios)
      if (ios /= 0) igderr(3) = ios
      return
    end if

    close(31, iostat=ios)
    if (ios /= 0) igderr(3) = ios

    ! Search for '?' which marks end of path
    do i = 1, 80
      a = adir(i:i)
      if (a == '?') then
        len_adir = i - 1
        exit
      end if
    end do

    ! If last char is not '\', add it
    if (len_adir > 0) then
      if (adir(len_adir:len_adir) /= '\') then
        len_adir = len_adir + 1
        adir(len_adir:len_adir) = '\'
      end if
    end if

  end subroutine getdir

  ! ---------------------------------------------------------------------------
  ! navopen: Open dated nav file (ddmmyy.nav). siosub.for:1913.
  ! iday,imo,iyr — date integers (iyr is 2-digit)
  ! ierr — output: iostat from open (0=ok,1=open err,2=empty,3=read err)
  ! fnav — output: filename used
  ! adir,len_adir — directory path
  subroutine navopen(iday, imo, iyr, ierr, fnav, adir, len_adir)
    integer,           intent(in)  :: iday, imo, iyr, len_adir
    integer,           intent(out) :: ierr
    character(len=80), intent(out) :: fnav
    character(len=80), intent(in)  :: adir

    character(len=10) :: ftemp
    character(len=2)  :: clats, clons
    integer :: kday, kmo, kyr, khr, kmin, ksec, klat, klon, nfix
    real    :: zltm, zlnm, spd, dir

    fnav = ' '
    if (len_adir > 0) fnav(1:len_adir) = adir(1:len_adir)

    ftemp = '000000.nav'
    ierr  = 0

    ! Encode day (positions 1-2)
    if (iday > 9) then
      write(ftemp(1:2), '(i2)') iday
    else
      write(ftemp(2:2), '(i1)') iday
    end if

    ! Encode month (positions 3-4)
    if (imo > 9) then
      write(ftemp(3:4), '(i2)') imo
    else
      write(ftemp(4:4), '(i1)') imo
    end if

    ! Encode year (positions 5-6)
    if (iyr > 9) then
      write(ftemp(5:6), '(i2)') iyr
    else
      write(ftemp(6:6), '(i1)') iyr
    end if

    fnav(len_adir+1:len_adir+5)  = 'Data\'
    fnav(len_adir+6:len_adir+15) = ftemp(1:10)

    open(8, file=fnav, status='old', iostat=ierr)
    if (ierr /= 0) then
      ierr = 1
      return
    end if

    ! Verify at least one readable data line
    read(8, 500, iostat=ierr) kday, kmo, kyr, khr, kmin, ksec, &
                               klat, zltm, clats, klon, zlnm, clons, &
                               spd, dir, nfix
500 format(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1,i3)

    if (ierr < 0) then
      ! end of file
      close(8)
      ierr = 2
      return
    else if (ierr > 0) then
      ! read error
      close(8)
      ierr = 3
      return
    end if

    rewind(8)
    ierr = 0

  end subroutine navopen

  ! ---------------------------------------------------------------------------
  ! chknav: Check/validate a nav file. siosub.for:585.
  ! Sets ierr=0 if file is usable, ierr=1 if not.
  ! Note: original comment says "DO NOT USE THIS ROUTINE, IT HAS NEVER WORKED
  ! PROPERLY!!!" — translated faithfully nonetheless.
  subroutine chknav(iday, imo, iyr, ierr, fnav, len_adir, adir, iw, ifile)
    integer,           intent(in)    :: iday, imo, iyr, len_adir, iw, ifile
    integer,           intent(out)   :: ierr
    character(len=80), intent(inout) :: fnav
    character(len=80), intent(in)    :: adir

    integer, parameter :: n = 1500
    integer, parameter :: maxchk = 60

    character(len=64) :: a64(n)
    character(len=1)  :: alth(n), alnh(n)
    character(len=6)  :: chkdnav(maxchk)
    character(len=10) :: ftemp

    integer :: ibad(n), ktimesec(n)
    integer :: klatd(n), klond(n)
    real    :: xlatm(n), xlonm(n)
    real    :: speed(n), dir(n)
    integer :: kdy(n), kmo(n), kyr(n), khr(n), kmn(n), ksc(n)

    integer :: i, j, nread, ibadcntr, icha642, ichlat, ichlon
    integer :: ios

    ! Static-like variables via save
    integer, save :: ichkdnav = 0
    data chkdnav / maxchk * '      ' /
    save chkdnav

    ! Put actual nav file name (minus path) into ftemp
    ftemp(1:10) = fnav(len_adir+1:len_adir+10)
    ierr    = 0
    ibadcntr = 0

    if (iw == 1) write(ifile, *) '  ichkdnav=', ichkdnav

    ! Don't recheck already-checked nav files
    do i = 1, ichkdnav
      if (iw == 1) write(ifile, *) '  chkdnav', i, '=', chkdnav(i)
      if (chkdnav(i)(1:6) == ftemp(1:6)) return
    end do

    ichkdnav = ichkdnav + 1
    if (ichkdnav <= maxchk) chkdnav(ichkdnav)(1:6) = ftemp(1:6)

    ! Read all records from unit 8
    nread = 0
    do i = 1, n
      a64(i) = ' '
      read(8, '(a64)', end=101, err=601, iostat=ios) a64(i)

      icha642 = ichar(a64(i)(2:2))
      if (icha642 /= 32) then
        ibad(i) = 0
        read(a64(i), 556, err=90) kdy(i), kmo(i), kyr(i), khr(i), &
                                   kmn(i), ksc(i), klatd(i), xlatm(i), &
                                   alth(i), klond(i), xlonm(i), alnh(i), &
                                   speed(i), dir(i)
556     format(6(i2,1x),i3,f8.4,a2,i4,f8.4,a2,4x,f6.2,f6.1)
        ! Check date matches filename date
        if ((kdy(i) /= iday) .or. (kmo(i) /= imo) .or. (kyr(i) /= iyr)) then
          ibad(i) = 2
          if (iw == 1) write(ifile, *) '  bad date'
          ibadcntr = ibadcntr + 1
        else
          ktimesec(i) = ksc(i) + 60*kmn(i) + 3600*khr(i)
          if (ktimesec(i) < 0 .or. ktimesec(i) > 86400) then
            ibad(i) = 4
            ibadcntr = ibadcntr + 1
          end if
        end if
      else
        ibad(i) = 1
        if (iw == 1) write(ifile, *) '  blank line'
        ibadcntr = ibadcntr + 1
      end if
      cycle
90    ibad(i) = 8
      ibadcntr = ibadcntr + 1
    end do

101 continue
    nread = i - 1
    if (iw == 1) write(ifile, *) '  ibadcntr=', ibadcntr, ' nread=', nread

    ! Check monotonically increasing times
    do i = 1, nread-1
      if (ibad(i) == 0) then
        do j = i+1, nread-1
          if (ibad(j) == 0) then
            if (ktimesec(i) < ktimesec(j)) then
              ! order ok
              exit
            else
              ibad(j) = 3
              ibadcntr = ibadcntr + 1
            end if
          end if
        end do
      end if
    end do

    ! Validate actual values
    do i = 1, nread
      if (ibad(i) == 0) then
        ibadcntr = ibadcntr + 1
        ichlat = ichar(alth(i))
        ichlon = ichar(alnh(i))
        if (klatd(i) < -90 .or. klatd(i) > 90) then
          ibad(i) = 5
        else if (xlatm(i) < 0.0 .or. xlatm(i) > 60.0) then
          ibad(i) = 5
        else if (ichlat /= 78 .and. ichlat /= 110 .and. &
                 ichlat /= 83 .and. ichlat /= 115) then
          ibad(i) = 5
        else if (klond(i) < 0 .or. klond(i) > 360) then
          ibad(i) = 6
        else if (xlonm(i) < 0.0 .or. xlonm(i) > 60.0) then
          ibad(i) = 6
        else if (ichlon /= 87 .and. ichlon /= 119 .and. &
                 ichlon /= 69 .and. ichlon /= 101) then
          ibad(i) = 6
        else if (speed(i) < -0.1 .or. speed(i) > 45.0) then
          ibad(i) = 7
        else if (dir(i) < 0.0 .or. dir(i) > 360.0) then
          ibad(i) = 8
        else
          ibadcntr = ibadcntr - 1
        end if
      end if
    end do

    if (ibadcntr == 0) then
      rewind(8)
      return
    else if (ibadcntr <= 4) then
      if (iw == 1) write(ifile, *) 'I found ', ibadcntr, ' errors in the nav file'
      close(8)
      if (iw == 1) write(ifile, *) ' Closing nav file to fix it'
      call navopen(iday, imo, iyr, ierr, fnav, adir, len_adir)
      if (iw == 1) write(ifile, 510) ibadcntr, ftemp(1:6)
510   format('Removed following ', i2, ' lines from ', a6, '.nav file:')
      do i = 1, nread
        if (ibad(i) == 0) then
          write(8, '(a64)') a64(i)
        else if (iw == 1) then
          select case (ibad(i))
          case (1)
            write(ifile, '(a64,a)') a64(i), ' blank line'
          case (2)
            write(ifile, '(a64,a)') a64(i), ' bad date'
          case (3)
            write(ifile, '(a64,a)') a64(i), ' tim out of ordr'
          case (4)
            write(ifile, '(a64,a)') a64(i), ' screwy time'
          case (5)
            write(ifile, '(a64,a)') a64(i), ' bad latitude'
          case (6)
            write(ifile, '(a64,a)') a64(i), ' bad longitude'
          case (7)
            write(ifile, '(a64,a)') a64(i), ' bad speed'
          case (8)
            write(ifile, '(a64,a)') a64(i), ' bad something!'
          end select
        end if
      end do
      rewind(8)
      return
    else
      ! 5 or more errors — bad file
      ierr = 1
      if (iw == 1) write(ifile, *) 'Help me!'
      if (iw == 1) write(ifile, *) &
        ' Here is the nav file and some of the errors I found'
      if (iw == 1) then
        do i = 1, nread
          write(ifile, '(a64)') a64(i)
        end do
      end if
    end if

    return

601 continue
    return

  end subroutine chknav

  ! ---------------------------------------------------------------------------
  ! getfilen: Construct dated filename (ddmmyy.nav format). siosub.for:1693.
  ! afilen   — output: full path filename
  ! adosday,adosmon,adosyear — 2-char day, 2-char month, 4-char year strings
  ! The task description signature omits idy,imn,iyr (used to fill adosday etc)
  ! and takes pre-built adosday/adosmon/adosyear strings instead.
  ! Produces: adir(1:len_adir) + 'Data\' + adosday + adosmon + yy + '.nav'
  subroutine getfilen(afilen, adosday, adosmon, adosyear, &
                      len_adir, adir)
    character(len=80), intent(out) :: afilen
    character(len=2),  intent(in)  :: adosday, adosmon
    character(len=4),  intent(in)  :: adosyear
    integer,           intent(in)  :: len_adir
    character(len=80), intent(in)  :: adir

    ! Extract last 2 digits of year (positions 3-4 of 4-char year)
    character(len=2) :: yy
    character(len=10) :: atemp

    afilen = ' '
    if (len_adir > 0) afilen(1:len_adir) = adir(1:len_adir)

    ! Build atemp = ddmmyy.nav  (10 chars)
    atemp = '000000.nav'
    yy = adosyear(3:4)

    atemp(1:2)  = adosday(1:2)
    atemp(3:4)  = adosmon(1:2)
    atemp(5:6)  = yy(1:2)
    ! positions 7-10 are '.nav' (already in atemp init)

    afilen(len_adir+1:len_adir+5)  = 'Data\'
    afilen(len_adir+6:len_adir+15) = atemp(1:10)

  end subroutine getfilen

  ! ---------------------------------------------------------------------------
  ! decodeplan: Parse one line from plan.dat into degrees/minutes/hemisphere.
  ! siosub.for:968.
  ! aplan    — input: one line from plan.dat
  ! latd     — output: integer degrees
  ! xlatm    — output: real minutes
  ! ahem     — output: hemisphere char (N/S/E/W)
  ! ierrplan — output: 0=good, 1=bad/unparseable line
  ! ispec1   — output: 0=lon-based plan, 1=lat-based plan
  subroutine decodeplan(aplan, latd, xlatm, ahem, ierrplan, ispec1)
    character(len=*),  intent(in)  :: aplan
    integer,           intent(out) :: latd, ierrplan, ispec1
    real,              intent(out) :: xlatm
    character(len=1),  intent(out) :: ahem

    integer :: i, ifounddeg, ifoundmin, ifoundhem
    integer :: ic
    real    :: xlat
    ! ch2real/findspace require intent(inout) string; copy to local buffer
    character(len=256) :: abuf

    ierrplan   = 1
    ifounddeg  = 0
    ifoundmin  = 0
    ifoundhem  = 0
    i          = 1
    latd       = 0
    xlatm      = 0.0
    ahem       = ' '
    ispec1     = 0

    abuf = ' '
    abuf(1:len(aplan)) = aplan

    ! Skip leading spaces, then read degrees
    do while (i <= len(aplan))
      if (abuf(i:i) == ' ') then
        i = i + 1
        cycle
      end if
      ! Found non-space: read degrees using findspace + ch2real
      call findspace(abuf, i, ic)
      call ch2real(abuf, i - ic, ic, xlat)
      latd = int(xlat)
      ifounddeg = 1
      i = i + 1   ! skip the space after degrees (i was advanced by findspace)
      exit
    end do

    if (ifounddeg == 0) return

    ! Skip spaces between degrees and minutes
    do while (i <= len(aplan))
      if (abuf(i:i) == ' ') then
        i = i + 1
        cycle
      end if
      call findspace(abuf, i, ic)
      call ch2real(abuf, i - ic, ic, xlatm)
      ifoundmin = 1
      i = i + 1   ! skip the space after minutes
      exit
    end do

    if (ifoundmin == 0) return

    ! Skip spaces between minutes and hemisphere
    do while (i <= len(aplan))
      if (abuf(i:i) == ' ') then
        i = i + 1
        cycle
      end if
      ahem(1:1) = abuf(i:i)
      ifoundhem = 1
      exit
    end do

    if (ifoundhem == 0) return

    ! Set ispec1
    if (ahem == 'N' .or. ahem == 'n' .or. &
        ahem == 'S' .or. ahem == 's') then
      ispec1 = 1
    else if (ahem == 'E' .or. ahem == 'e' .or. &
             ahem == 'W' .or. ahem == 'w') then
      ispec1 = 0
    else
      ! Not a valid hemisphere character
      return
    end if

    ierrplan = 0

  end subroutine decodeplan

  ! ---------------------------------------------------------------------------
  ! rdcntrl: Read control.dat and set run parameters. rdcntrl.for:21.
  ! ierror(15)=1 error opening control.dat
  ! ierror(16)=1 error reading control.dat
  ! ierror(33)=1 operator name = "debug"
  subroutine rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                     deadmin, dropmin, relodmin, runsec, &
                     tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                     tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                     len_adir, adir, iw, ifile)
    integer,           intent(inout) :: ierror(nerr)
    integer,           intent(out)   :: len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7),  intent(out)   :: acruise
    real,              intent(out)   :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real,              intent(out)   :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real,              intent(out)   :: tm_pl_mx, tm_pl_mn
    integer,           intent(in)    :: len_adir, iw, ifile
    character(len=80), intent(in)    :: adir

    integer, parameter :: nlines  = 18
    integer, parameter :: nlnchrs = 12

    character(len=34) :: a(nlines)
    character(len=72) :: a72, acut
    integer :: la(nlines)
    integer :: i, j, k, ifound, iblank, lacut, laj1, ios
    integer :: ichkprofdepth
    real    :: xminspd, xminspd1, xmaxspd1
    real    :: deadmin1, dropmin1, relodmin1
    real    :: tdzmx1, tdzrms1, dtdzmn1, dtdzth1, dtmx1, dtmx7001
    real    :: tm_pl_mn1, tm_pl_mx1
    integer :: ichkprofdepth1, ii
    character(len=80) :: acontrol

    ! Build path to control.dat: adir + 'control.dat'
    acontrol = ' '
    if (len_adir > 0) then
      acontrol(1:len_adir) = adir(1:len_adir)
      acontrol(len_adir+1:len_adir+11) = 'control.dat'
    else
      acontrol = 'control.dat'
    end if

    ! Keyword strings matching control.dat new format
    a(1)  = 'Ship Name ='
    a(2)  = 'Cruise Name ='
    a(3)  = 'Operator Name ='
    a(4)  = 'Max Ship Speed ='
    a(5)  = 'Auto Launcher Sequence ='
    a(6)  = 'Max Plot  Temp ='
    a(7)  = 'Min Plot Temp ='
    a(8)  = 'Max Minutes to Dead Reakon ='
    a(9)  = 'Max min duration between drops ='
    a(10) = 'Empty Launcher Reload Alarm Time ='
    a(11) = 'Max Displacement ='
    a(12) = 'Max Rms Displacement ='
    a(13) = 'Min dtdz ='
    a(14) = 'Min dtdz Displacement Test ='
    a(15) = 'Max Delta ='
    a(16) = 'Max 700m Delta ='
    a(17) = 'Check Profile Depth ='
    a(18) = 'Min Plot  Temp ='

    ! Default values
    xmaxspd1       = 30.0
    xminspd1       = 30.0
    deadmin1       = 60.0
    dropmin1       = -90.0
    relodmin1      = 15.0
    relodmin       = 15.0
    tdzmx1         = 450.0
    tdzrms1        = 110.0
    dtdzmn1        = -0.01100
    dtdzth1        = 0.00050
    dtmx1          = 4.6
    dtmx7001       = 0.8
    tm_pl_mn1      = 0.0
    tm_pl_mx1      = 30.0
    ichkprofdepth1 = 700
    runsec         = 0.0
    iSIOSpeedAveMin = 0

    ! Set launcher 1..12 = 1..12 as default
    do i = 1, nlnchrs
      launcher(i) = i
    end do

    ! Reset the errors this routine may set
    ierror(15) = 0
    ierror(16) = 0
    ierror(33) = 0

    ! Determine logging: if ierror(44) or (45) set, suppress writes
    ! (iw is passed in as a parameter; honour it directly)

    ! Get lengths of keyword strings
    do i = 1, nlines
      la(i) = len_trim(a(i))
    end do

    open(22, file=trim(acontrol), status='old', form='formatted', iostat=ios)
    if (ios /= 0) then
      ierror(15) = 1
      return
    end if

    ! Peek at first line to detect format
    read(22, '(a)', iostat=ios) a72
    if (ios /= 0) then
      if (i <= 3) ierror(16) = 1
      close(22, iostat=ios)
      return
    end if
    rewind(22)

    ! ---- New format (starts with "Ship Name =") ----
    if (a72(1:la(1)) == a(1)(1:la(1))) then

      if (iw == 1) write(ifile, *) 'In rdcntrl,rd cntrl.dat:'

      do i = 1, nlines
        read(22, '(a)', end=301, err=301, iostat=ios) a72
        if (iw == 1) write(ifile, *) i, a72

        do j = 1, nlines
          ifound = 0
          ! Special case for Reakon vs Reckon
          if (j == 8) then
            if (a72(1:19) == a(8)(1:19)) then
              lacut = 72 - la(j) + 1
              laj1  = la(j) + 1
              acut(1:lacut) = a72(laj1:72)
              ifound = 1
            end if
          else
            if (a72(1:la(j)) == a(j)(1:la(j))) then
              lacut = 72 - la(j) + 1
              laj1  = la(j) + 1
              acut(1:lacut) = a72(laj1:72)
              ifound = 1
            end if
          end if

          if (ifound == 1) then
            select case (j)
            case (1)
              ! skip ship name

            case (2)
              ! cruise name — find first blank
              iblank = lacut
              do k = 1, lacut
                if (acut(k:k) == ' ') then
                  iblank = k
                  exit
                end if
              end do
              len_acruise = iblank - 1
              if (len_acruise > 7) len_acruise = 7
              acruise(1:len_acruise) = acut(1:len_acruise)
              if (iw == 1) then
                write(ifile, *) 'len_acruise=', len_acruise
                write(ifile, *) 'acruise=', acruise(1:len_acruise)
              end if

            case (3)
              ! operator name — check for "debug"
              if (acut(1:5) == 'debug' .or. acut(1:5) == 'DEBUG') then
                ierror(33) = 6
              end if

            case (4)
              ! max ship speed
              read(acut, *, iostat=ios) xmaxspd
              if (ios /= 0) xmaxspd = xmaxspd1
              if (iw == 1) write(ifile, *) 'xmaxspd=', xmaxspd

            case (5)
              ! launcher sequence — skipped per legacy comment (01jul2014)

            case (6)
              ! plot temperature max
              read(acut, *, iostat=ios) tm_pl_mx
              if (ios /= 0) tm_pl_mx = tm_pl_mx1
              if (iw == 1) write(ifile, *) 'tm_pl_mx=', tm_pl_mx

            case (7)
              ! plot temperature min
              read(acut, *, iostat=ios) tm_pl_mn
              if (ios /= 0) tm_pl_mn = tm_pl_mn1
              if (iw == 1) write(ifile, *) 'tm_pl_mn=', tm_pl_mn

            case (8)
              ! max min to deadreckon
              read(acut, *, iostat=ios) deadmin
              if (ios /= 0) deadmin = deadmin1
              if (iw == 1) write(ifile, *) 'deadmin=', deadmin

            case (9)
              ! max min between drops
              read(acut, *, iostat=ios) dropmin
              if (ios /= 0) dropmin = dropmin1
              if (iw == 1) write(ifile, *) 'dropmin=', dropmin

            case (10)
              ! empty launcher reload time
              read(acut, *, iostat=ios) relodmin
              if (ios /= 0) relodmin = relodmin1
              if (iw == 1) write(ifile, *) 'relodmin=', relodmin

            case (11)
              ! tdzmx
              read(acut, *, iostat=ios) tdzmx
              if (ios /= 0) tdzmx = tdzmx1
              if (iw == 1) write(ifile, *) 'tdzmx=', tdzmx

            case (12)
              ! tdzrms
              read(acut, *, iostat=ios) tdzrms
              if (ios /= 0) tdzrms = tdzrms1
              if (iw == 1) write(ifile, *) 'tdzrms=', tdzrms

            case (13)
              ! dtdzmn
              read(acut, *, iostat=ios) dtdzmn
              if (ios /= 0) dtdzmn = dtdzmn1
              if (iw == 1) write(ifile, *) 'dtdzmn=', dtdzmn

            case (14)
              ! dtdzth
              read(acut, *, iostat=ios) dtdzth
              if (ios /= 0) dtdzth = dtdzth1
              if (iw == 1) write(ifile, *) 'dtdzth=', dtdzth

            case (15)
              ! dtmx
              read(acut, *, iostat=ios) dtmx
              if (ios /= 0) dtmx = dtmx1
              if (iw == 1) write(ifile, *) 'dtmx=', dtmx

            case (16)
              ! dtmx700
              read(acut, *, iostat=ios) dtmx700
              if (ios /= 0) dtmx700 = dtmx7001
              if (iw == 1) write(ifile, *) 'dtmx700=', dtmx700

            case (17)
              ! ichkprofdepth
              read(acut, *, iostat=ios) ichkprofdepth
              if (ios /= 0) ichkprofdepth = ichkprofdepth1
              if (iw == 1) write(ifile, *) 'ichkprofdepth=', ichkprofdepth

            case (18)
              ! plot temperature min (alternate keyword)
              read(acut, *, iostat=ios) tm_pl_mn
              if (ios /= 0) tm_pl_mn = tm_pl_mn1
              if (iw == 1) write(ifile, *) 'tm_pl_mn=', tm_pl_mn

            end select
          end if
        end do  ! j loop
      end do    ! i loop

      if (tm_pl_mn == tm_pl_mx) then
        tm_pl_mn = tm_pl_mn1
        tm_pl_mx = tm_pl_mx1
      end if

      go to 900

    ! ---- Old format ----
    else

      read(22, '(a)', iostat=ios) acruise
      len_acruise = len_trim(acruise)
      ! board addresses
      read(22, *, iostat=ios)
      ! drop time in seconds, 0, screen type
      read(22, *, iostat=ios)
      read(22, 551, iostat=ios) deadmin, dropmin, relodmin, runsec
551   format(4f5.0)
      if (ios /= 0) then
        deadmin  = deadmin1
        dropmin  = dropmin1
        relodmin = relodmin1
        runsec   = 0.0
      end if

      read(22, 520, iostat=ios) tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
520   format(2f6.0,2f9.5,2f5.1)
      if (ios /= 0) then
        tdzmx   = tdzmx1
        tdzrms  = tdzrms1
        dtdzmn  = dtdzmn1
        dtdzth  = dtdzth1
        dtmx    = dtmx1
        dtmx700 = dtmx7001
      end if

      read(22, *, iostat=ios) ii, tm_pl_mn, tm_pl_mx
      if (ios /= 0) then
        tm_pl_mn = tm_pl_mn1
        tm_pl_mx = tm_pl_mx1
      end if

      if (tm_pl_mn == tm_pl_mx) then
        tm_pl_mn = tm_pl_mn1
        tm_pl_mx = tm_pl_mx1
      end if

      ! ship name
      read(22, *, iostat=ios)
      ! operator name
      read(22, *, iostat=ios)
      ! gps type, com port
      read(22, *, iostat=ios)
      ! ship min/max speed
      read(22, 522, iostat=ios) xminspd, xmaxspd
522   format(2f5.1)
      if (ios /= 0) then
        xminspd = xminspd1
        xmaxspd = xmaxspd1
      end if

      ! data path
      read(22, *, iostat=ios)
      ! read version
      read(22, *, iostat=ios)

      ! launcher sequence (6 per old format)
      read(22, 550, iostat=ios) (launcher(i), i=1,6)
550   format(6i3)
      if (ios /= 0) then
        do i = 1, 6
          launcher(i) = -1*i
        end do
      end if

      ! fill in launcher(7..12)
      do i = 7, nlnchrs
        launcher(i) = -99
      end do

    end if  ! new vs old format

    go to 900

301 if (i <= 3) ierror(16) = 1
    go to 900

900 continue
    close(22, iostat=ios)
    if (iw == 1) then
      write(ifile, *) 'close22ios=', ios
      write(ifile, *) 'end rdcntrl:ierror(15)=', ierror(15)
      write(ifile, *) 'end rdcntrl:ierror(16)=', ierror(16)
      write(ifile, *) 'leaving rdcntrl'
    end if

  end subroutine rdcntrl

end module sio_io
