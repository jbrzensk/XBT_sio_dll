! tests/integration/test_integration_replay.f90
! Replays the 9/8/2026 field day (tests/fixtures/incident_0908, see its
! README) through the DLL the way Seas drove it: the GPS log's PC timestamps
! drive the test clock, each sentence is decoded as Seas's NMEA decoder does
! (checksum, field checks), and sioloop is called every simulated second
! (tests/seas_sim.f90). At "drop now" Seas calls sioend, launches (launch_sec),
! records the drop in stations.dat and calls siobegin for the next one.
!
! The plan.dat in the fixture is a stand-in (lat plan, a station every 3'),
! so the drop checks are against it:
!  - every drop happens with the true ship position past the target station;
!  - every station the ship crossed is dropped exactly once;
!  - no two drops closer than 10 minutes (ierror(30) never set);
!  - siobegin succeeds every time;
!  - the DLL position (drlat/drlon) stays within 0.5 nm of the GPS truth
!    while GPS is healthy (the tolerance fix 3 uses to distrust a position).
! A summary is written to tests\tmp\replay_0908\replay_summary.txt, and the
! seconds with the DLL position > 0.5 nm off to replay_trace.txt.
program test_integration_replay
  use test_support
  use seas_sim
  use sio_time, only: set_test_clock, advance_test_clock, use_real_clock
  implicit none

  character(len=*), parameter :: fixture = 'tests\fixtures\incident_0908'
  character(len=*), parameter :: logname = 'GpsData20260908072521.txt'
  integer, parameter :: launch_sec = 240          ! SioEnd -> SioBegin
  integer, parameter :: maxdrop = 100
  ! The DLL appends to Data\sio.log on every call (~15 MB and several minutes
  ! for this day). Off by default: a directory named sio.log makes the open
  ! fail, so the DLL skips logging. Set .true. to diagnose.
  logical, parameter :: keep_sio_log = .false.
  real,    parameter :: d2r = 3.141592654 / 180.0

  character(len=80)  :: sdir
  character(len=400) :: line
  type(gps_data) :: g
  integer :: ulog, ios, now, pc_next, launch_end, i, failures, usum
  logical :: have_next, launching, eof
  character(len=200) :: rmc_next
  ! GPS truth: last healthy fix
  real    :: tlat, tlon, tlat_max
  integer :: tpc
  ! drops
  integer :: ndrop, dtime(maxdrop)
  real    :: dxlat(maxdrop), ddlat(maxdrop), ddlon(maxdrop), dtlat(maxdrop), dtlon(maxdrop)
  ! position error while GPS healthy
  real    :: err, errmax
  integer :: errmax_t, n_err05, n_err10, n_checked
  ! stations crossed by the truth track
  real    :: plan_lat(100)
  integer :: nplan_lat, ncross
  integer :: nbegin_fail, utr

  failures = 0
  call scratch_dir('replay_0908', sdir, fixture)
  call point_getdir_at(sdir)
  if (.not. keep_sio_log) call execute_command_line('mkdir "' // trim(sdir) // 'Data\sio.log"')
  ! seconds where the DLL position is > 0.5 nm from the GPS truth
  open(newunit=utr, file=trim(sdir) // 'replay_trace.txt', status='replace')
  write(utr, '(a)') '  time     err_nm  DLL lat   DLL lon    true lat  true lon  truth_age' // &
                    '  vlat      vlon      timeave  speed   dir  iaveflg'
  call read_plan_lats(trim(sdir) // 'plan.dat', plan_lat, nplan_lat)

  open(newunit=ulog, file=trim(sdir) // logname, status='old', action='read', iostat=ios)
  if (ios /= 0) then
    print *, 'FAIL test_replay_0908: cannot open ', trim(sdir) // logname
    stop 1
  end if
  have_next = .false.; eof = .false.
  call next_line()
  now = pc_next
  call set_test_clock(2026, 9, 8, now / 3600, mod(now, 3600) / 60, mod(now, 60))

  tpc = -100000; tlat = 0; tlon = 0; tlat_max = -90
  ndrop = 0; errmax = 0; errmax_t = 0; n_err05 = 0; n_err10 = 0; n_checked = 0
  launching = .false.; launch_end = 0; nbegin_fail = 0
  g = gps_data()
  call sim_reset()

  do while (have_next .or. now <= tpc + 60)
    call apply_lines()
    if (launching .and. now >= launch_end) then
      call add_station()
      launching = .false.
    end if
    if (.not. launching) then
      ! SioBegin at the start and after each launch, once the GPS time is valid
      if (.not. st%begun) then
        call sim_begin(g, 1, 0)
        if (st%begun .and. st%ierror(35) /= 2) nbegin_fail = nbegin_fail + 1
      end if
      if (st%begun) then
        if (sim_loop(g)) then
          ndrop = ndrop + 1
          if (ndrop <= maxdrop) then
            dtime(ndrop) = now
            dxlat(ndrop) = st%xlat
            ddlat(ndrop) = st%drlat;  ddlon(ndrop) = st%drlon
            dtlat(ndrop) = tlat;      dtlon(ndrop) = tlon
          end if
          call sim_end()
          launching = .true.
          launch_end = now + launch_sec
        else if (now - tpc <= 30) then
          err = dist_nm(st%drlat, st%drlon, tlat, tlon)
          n_checked = n_checked + 1
          if (err > 0.5) n_err05 = n_err05 + 1
          if (err > 1.0) n_err10 = n_err10 + 1
          if (err > 0.5) write(utr, '(i2.2,a,i2.2,a,i2.2,f8.2,4f10.4,i6,2f10.4,f9.0,2f7.1,i4)') &
               now / 3600, ':', mod(now, 3600) / 60, ':', mod(now, 60), err, st%drlat, &
               st%drlon, tlat, tlon, now - tpc, st%vlat, st%vlon, st%timeave, st%speed, &
               st%dir, st%iaveflg
          if (err > errmax) then
            errmax = err; errmax_t = now
          end if
        end if
      end if
    end if
    now = now + 1
    call advance_test_clock(1)
  end do
  call use_real_clock()
  close(ulog)
  close(utr)

  ! stations the truth track crossed: between the first station after the
  ! last pre-log drop (33.001 N + 0.01) and the northernmost true position
  ncross = 0
  do i = 1, nplan_lat
    if (plan_lat(i) > 33.001 + 0.01 .and. plan_lat(i) <= tlat_max) ncross = ncross + 1
  end do

  call report()

  if (failures == 0) then
    print *, 'test_integration_replay: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_integration_replay: FAILURES =', failures
    stop 1
  end if

contains

  ! ---------------------------------------------------------------------------
  ! Log reading and Seas-style decoding
  ! ---------------------------------------------------------------------------

  ! Buffer the next "PC time: MM-DD-YYYY hh:mm:ss NMEA: <RMC...> Fix quality"
  ! line (other lines, e.g. Iridium, are skipped: not GPS data)
  subroutine next_line()
    integer :: k, k2, hh, mi, ss, ios2
    have_next = .false.
    do
      read(ulog, '(a)', iostat=ios2) line
      if (ios2 /= 0) then
        eof = .true.
        return
      end if
      if (line(1:9) /= 'PC time: ') cycle
      k = index(line, ' NMEA:')
      if (k == 0) cycle
      read(line(21:22), *, iostat=ios2) hh
      if (ios2 /= 0) cycle
      read(line(24:25), *, iostat=ios2) mi
      if (ios2 /= 0) cycle
      read(line(27:28), *, iostat=ios2) ss
      if (ios2 /= 0) cycle
      pc_next = hh * 3600 + mi * 60 + ss
      k2 = index(line, ' Fix quality')
      if (k2 == 0) k2 = len_trim(line) + 1
      rmc_next = adjustl(line(k + 6:k2 - 1))
      have_next = .true.
      return
    end do
  end subroutine next_line

  ! Apply every buffered line whose PC time has come: it becomes Seas's
  ! current GPS data
  subroutine apply_lines()
    logical :: healthy
    real    :: la, lo
    integer :: gsec
    do while (have_next)
      if (pc_next > now) exit
      call decode_rmc(trim(rmc_next), g, healthy, la, lo, gsec)
      if (healthy .and. abs(pc_next - gsec) <= 15) then
        tlat = la; tlon = lo; tpc = pc_next
        tlat_max = max(tlat_max, la)
      end if
      call next_line()
    end do
  end subroutine apply_lines

  ! CNmeaStreamDecoder: a bad checksum empties the RMC data (no valid time:
  ! Seas skips sioloop). Time hhmmss; date ddmmyy -> 20yy; latitude valid
  ! only as DD + MM MM digits with N/S, longitude DDD + MM MM with E/W.
  ! healthy: checksum, time, date and position all good (lat/lon out for truth)
  subroutine decode_rmc(rmc, gd, healthy, la, lo, gsec)
    character(len=*), intent(in) :: rmc
    type(gps_data), intent(out) :: gd
    logical, intent(out) :: healthy
    real,    intent(out) :: la, lo
    integer, intent(out) :: gsec
    character(len=64) :: f(20)
    integer :: nf, ios2
    logical :: tok, dok, latok, lonok
    gd = gps_data()
    healthy = .false.; la = 0; lo = 0; gsec = -100000
    if (len(rmc) < 4) return
    if (rmc(1:4) /= 'RMC,') return
    if (.not. checksum_ok('GP' // rmc)) return
    call split_commas(rmc, f, nf)
    tok = .false.; dok = .false.; latok = .false.; lonok = .false.
    ! time
    if (nf >= 2) then
      if (len_trim(f(2)) >= 6 .and. verify(f(2)(1:6), '0123456789') == 0) then
        read(f(2)(1:2), *) gd%ihr; read(f(2)(3:4), *) gd%imin; read(f(2)(5:6), *) gd%isec
        tok = gd%ihr <= 23 .and. gd%imin <= 59 .and. gd%isec <= 59
        if (.not. tok) then
          gd%ihr = 0; gd%imin = 0; gd%isec = 0
        end if
      end if
    end if
    ! date
    if (nf >= 10) then
      if (len_trim(f(10)) == 6 .and. verify(f(10)(1:6), '0123456789') == 0) then
        read(f(10)(1:2), *) gd%iday; read(f(10)(3:4), *) gd%imon
        read(f(10)(5:6), *, iostat=ios2) gd%iyr
        gd%iyr = 2000 + gd%iyr
        dok = gd%imon >= 1 .and. gd%imon <= 12 .and. gd%iday >= 1 .and. gd%iday <= 31
        if (.not. dok) then
          gd%iday = 0; gd%imon = 0; gd%iyr = 0
        end if
      end if
    end if
    ! latitude ddmm.mmmm + N/S
    if (nf >= 5) then
      latok = coord_ok(f(4), 2) .and. (f(5) == 'N' .or. f(5) == 'S')
      if (latok) then
        gd%latdd = f(4)(1:2); gd%latmm = f(4)(3:); gd%ns = f(5)(1:1)
      end if
    end if
    ! longitude dddmm.mmmm + E/W
    if (nf >= 7) then
      lonok = coord_ok(f(6), 3) .and. (f(7) == 'E' .or. f(7) == 'W')
      if (lonok) then
        gd%londdd = f(6)(1:3); gd%lonmm = f(6)(4:); gd%ew = f(7)(1:1)
      end if
    end if
    gd%valid = latok .and. lonok
    if (tok) gsec = gd%ihr * 3600 + gd%imin * 60 + gd%isec
    healthy = tok .and. dok .and. gd%valid
    if (healthy) then
      la = atof(gd%latdd) + atof(gd%latmm) / 60.0
      if (gd%ns == 'S') la = -la
      lo = atof(gd%londdd) + atof(gd%lonmm) / 60.0
      if (gd%ew == 'W') lo = -lo
    end if
  end subroutine decode_rmc

  ! CHelper::IsValidLatitude / IsValidLongitude on DD(D) + MM with the '.'
  ! made a space: nd digits, 2 digits, a space, 2 digits
  logical function coord_ok(s, nd)
    character(len=*), intent(in) :: s
    integer, intent(in) :: nd
    coord_ok = .false.
    if (len_trim(s) < nd + 5) return
    if (verify(s(1:nd + 2), '0123456789') /= 0) return
    if (s(nd + 3:nd + 3) /= '.') return
    if (verify(s(nd + 4:nd + 5), '0123456789') /= 0) return
    coord_ok = .true.
  end function coord_ok

  ! NMEA checksum: XOR of the characters before '*' == the 2 hex digits after
  logical function checksum_ok(s)
    character(len=*), intent(in) :: s
    integer :: k, i, x, v, ios2
    checksum_ok = .false.
    k = index(s, '*')
    if (k == 0 .or. k + 2 > len(s)) return
    x = 0
    do i = 1, k - 1
      x = ieor(x, ichar(s(i:i)))
    end do
    read(s(k + 1:k + 2), '(z2)', iostat=ios2) v
    if (ios2 /= 0) return
    checksum_ok = v == x
  end function checksum_ok

  subroutine split_commas(s, f, nf)
    character(len=*), intent(in)  :: s
    character(len=*), intent(out) :: f(:)
    integer, intent(out) :: nf
    integer :: i, k0
    f = ' '
    nf = 1
    k0 = 1
    do i = 1, len(s)
      if (s(i:i) == ',') then
        if (nf <= size(f)) f(nf) = s(k0:i - 1)
        nf = nf + 1
        k0 = i + 1
      end if
    end do
    if (nf <= size(f)) f(nf) = s(k0:)
    nf = min(nf, size(f))
  end subroutine split_commas

  ! ---------------------------------------------------------------------------
  ! Seas side of a drop
  ! ---------------------------------------------------------------------------

  ! Record the drop just made in Data\stations.dat as a good (test mode, -3)
  ! drop at the DLL position, before ENDDATA, as Seas's drop recording does
  subroutine add_station()
    character(len=120) :: lines(1000)
    integer :: u, n, k, id
    real :: lon360
    character(len=2) :: a(6)
    id = ndrop
    n = 0
    open(newunit=u, file=trim(sdir) // 'Data\stations.dat', status='old')
    do
      read(u, '(a)', iostat=k) lines(n + 1)
      if (k /= 0) exit
      if (lines(n + 1)(1:3) == 'END') exit
      n = n + 1
    end do
    close(u)
    lon360 = ddlon(id)
    if (lon360 < 0.0) lon360 = lon360 + 360.0
    write(a(1), '(i2.2)') 8; write(a(2), '(i2.2)') 9; write(a(3), '(i2.2)') 26
    write(a(4), '(i2.2)') dtime(id) / 3600
    write(a(5), '(i2.2)') mod(dtime(id), 3600) / 60
    write(a(6), '(i2.2)') mod(dtime(id), 60)
    n = n + 1
    write(lines(n), '(1x,i3.3,i6,f7.3,1x,a2,a1,a2,a1,a2,1x,a2,a1,a2,a1,a2,f9.3,f9.3,i4,i5,i6,1x,a1)') &
         n, mod(n - 1, 6) + 1, 1.520, a(1), '/', a(2), '/', a(3), a(4), ':', a(5), ':', a(6), &
         ddlat(id), lon360, -3, 0, -1, 'y'
    open(newunit=u, file=trim(sdir) // 'Data\stations.dat', status='replace')
    do k = 1, n
      write(u, '(a)') trim(lines(k))
    end do
    write(u, '(a)') 'ENDDATA'
    close(u)
  end subroutine add_station

  ! ---------------------------------------------------------------------------
  ! Checks and summary
  ! ---------------------------------------------------------------------------

  subroutine report()
    integer :: k, nbad_past, nclose
    real :: past_nm
    character(len=8) :: hms
    open(newunit=usum, file=trim(sdir) // 'replay_summary.txt', status='replace')
    call out('9/8/2026 replay (stand-in plan: lat, every 3''), launch cycle 240 s')
    write(line, '(a,i4,a,i4)') 'drops:', ndrop, '   stations crossed by the ship:', ncross
    call out(trim(line))
    call out(' drop  time     station   DLL lat   DLL lon    true lat  true lon  past by')
    nbad_past = 0
    nclose = 0
    do k = 1, min(ndrop, maxdrop)
      past_nm = (dtlat(k) - dxlat(k)) * 60.0
      if (dtlat(k) < dxlat(k) - 0.005) nbad_past = nbad_past + 1
      if (k > 1) then
        if (dtime(k) - dtime(k - 1) < 600) nclose = nclose + 1
      end if
      write(hms, '(i2.2,a,i2.2,a,i2.2)') dtime(k) / 3600, ':', mod(dtime(k), 3600) / 60, &
            ':', mod(dtime(k), 60)
      write(line, '(i4,2x,a8,f9.3,f10.4,f10.4,f10.4,f10.4,f7.2,a)') k, hms, dxlat(k), &
            ddlat(k), ddlon(k), dtlat(k), dtlon(k), past_nm, ' nm'
      call out(trim(line))
    end do
    write(line, '(a,f6.2,a,i6,a,i6,a,i6,a,i2.2,a,i2.2)') 'DLL position vs GPS truth (GPS healthy): max', &
         errmax, ' nm; >0.5 nm for', n_err05, ' s, >1 nm for', n_err10, ' s of', n_checked, &
         ' s; max at ', errmax_t / 3600, ':', mod(errmax_t, 3600) / 60
    call out(trim(line))
    close(usum)

    call check('siobegin succeeded every time (ierror(35)=2)', nbegin_fail == 0)
    call check('every drop with the ship truly past its station', nbad_past == 0)
    call check('every crossed station dropped exactly once', ndrop == ncross)
    call check('no two drops closer than 10 minutes', nclose == 0 .and. st%ierror(30) == 0)
    call check('DLL position within 0.5 nm of GPS truth while GPS healthy', n_err05 == 0)
  end subroutine report

  subroutine out(s)
    character(len=*), intent(in) :: s
    print '(1x,a)', s
    write(usum, '(a)') s
  end subroutine out

  subroutine check(name, ok)
    character(len=*), intent(in) :: name
    logical, intent(in) :: ok
    if (ok) then
      print *, 'PASS test_replay_0908: ', name
    else
      print *, 'FAIL test_replay_0908: ', name
      failures = failures + 1
    end if
  end subroutine check

  ! Station latitudes of a lat plan.dat (4 header lines, then "dd mm.mm N,")
  subroutine read_plan_lats(path, lats, n)
    character(len=*), intent(in) :: path
    real, intent(out) :: lats(:)
    integer, intent(out) :: n
    integer :: u, k, dd, ios2
    real :: mm
    character(len=80) :: s
    n = 0
    open(newunit=u, file=path, status='old', iostat=ios2)
    if (ios2 /= 0) return
    do k = 1, 4
      read(u, '(a)', iostat=ios2) s
    end do
    do
      read(u, '(a)', iostat=ios2) s
      if (ios2 /= 0) exit
      read(s, *, iostat=ios2) dd, mm
      if (ios2 /= 0) cycle
      n = n + 1
      lats(n) = real(dd) + mm / 60.0
    end do
    close(u)
  end subroutine read_plan_lats

  real function dist_nm(lat1, lon1, lat2, lon2)
    real, intent(in) :: lat1, lon1, lat2, lon2
    real :: dlon
    dlon = lon1 - lon2
    if (dlon > 180.0) dlon = dlon - 360.0
    if (dlon < -180.0) dlon = dlon + 360.0
    dist_nm = sqrt(((lat1 - lat2) * 60.0)**2 + (dlon * 60.0 * cos(lat2 * d2r))**2)
  end function dist_nm

end program test_integration_replay
