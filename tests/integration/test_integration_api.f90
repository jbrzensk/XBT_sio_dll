! tests/integration/test_integration_api.f90
! Integration tests for the DLL entry points in src/sio_api.f90, called the
! way Seas calls them (tests/seas_sim.f90). They find their files through
! getdir, so each test runs against a scratch copy of a fixture
! (tests/test_support.f90), never the installed Seas data. sioloop runs in
! simulated time (test clock in sio_time): one pass = one second.
program test_integration_api
  use test_support
  use seas_sim
  use sio_time, only: set_test_clock, advance_test_clock, use_real_clock
  implicit none
  integer :: failures = 0

  call test_siobegin_base_fixture(failures)
  call test_sioloop_one_nav_line_per_minute(failures)
  call test_sioloop_begin_holds_drop_until_average(failures)
  call test_sioloop_no_snap_back_on_repeated_sentences(failures)

  if (failures == 0) then
    print *, 'test_integration_api: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_integration_api: FAILURES =', failures
    stop 1
  end if

contains

  ! siobegin on a valid data directory (1 drop done, northbound lat plan):
  ! all well (ierror(35)=2), next station loaded, and no stray debug files
  ! written into the data directory
  subroutine test_siobegin_base_fixture(failures)
    integer, intent(inout) :: failures
    character(len=80) :: sdir
    logical :: probe

    call scratch_dir('siobegin_base', sdir, 'tests\fixtures\base')
    call point_getdir_at(sdir)
    call sim_reset()
    call set_test_clock(2024, 6, 1, 12, 5, 0)
    call sim_begin(gps_on_track(37.52, 200.5, 10.0, 5.0, 300), 1, 0)
    call use_real_clock()

    inquire(file=trim(sdir) // 'Data\sio_probe.txt', exist=probe)
    if (st%ierror(35) /= 2 .or. probe) then
      print *, 'FAIL test_siobegin_base_fixture: ierror(35)=', st%ierror(35), &
               ' nextdrop=', st%nextdrop, ' xlat=', st%xlat, ' sio_probe.txt written=', probe
      failures = failures + 1
    else
      print *, 'PASS test_siobegin_base_fixture: nextdrop=', st%nextdrop, ' xlat=', st%xlat
    end if
  end subroutine test_siobegin_base_fixture

  ! Three simulated minutes of good GPS, far short of the next station, with
  ! a 10 s stale sentence just after each minute starts (csec 55, then 06):
  ! the old GPS-seconds rule took that as a second minute boundary. Expect
  ! exactly one GPS average (.nav NAV line) per PC minute, no DED lines, no
  ! drop.
  subroutine test_sioloop_one_nav_line_per_minute(failures)
    integer, intent(inout) :: failures
    character(len=80) :: sdir
    type(gps_data) :: g
    integer :: t, ndrop, nnav, nded

    call scratch_dir('loop_nav_per_minute', sdir, 'tests\fixtures\base')
    call point_getdir_at(sdir)
    call start_position(sdir, 37.00, 200.5, 10.0, 5.0)
    call sim_reset()
    call set_test_clock(2024, 6, 1, 12, 1, 0)
    call sim_begin(gps_on_track(37.00, 200.5, 10.0, 5.0, 60), 1, 0)
    ndrop = 0
    do t = 60, 270                              ! 12:01:00 .. 12:04:30
      g = gps_on_track(37.00, 200.5, 10.0, 5.0, t)
      if (mod(t, 60) == 5) g = gps_on_track(37.00, 200.5, 10.0, 5.0, t - 10)
      if (sim_loop(g)) ndrop = ndrop + 1
      call advance_test_clock(1)
    end do
    call use_real_clock()

    nnav = count_status(sdir, 'NAV')
    nded = count_status(sdir, 'DED')
    if (nnav /= 3 .or. nded /= 0 .or. ndrop /= 0) then
      print *, 'FAIL test_sioloop_one_nav_line_per_minute: NAV lines=', nnav, &
               ' (expected 3) DED lines=', nded, ' drops=', ndrop
      failures = failures + 1
    else
      print *, 'PASS test_sioloop_one_nav_line_per_minute'
    end if
  end subroutine test_sioloop_one_nav_line_per_minute

  ! After siobegin the reloaded position (navtrk.dat, 12:00:00) is already
  ! past the next station (37.52 N vs 37.50 N, heading north). No drop until
  ! the first GPS average (PC minute 12:02:00) confirms it; then the drop
  ! countdown (5 s) fires at 12:02:05. With the old trust-at-begin it
  ! dropped at 12:01:05.
  subroutine test_sioloop_begin_holds_drop_until_average(failures)
    integer, intent(inout) :: failures
    character(len=80) :: sdir
    integer :: t, tdrop

    call scratch_dir('loop_begin_hold', sdir, 'tests\fixtures\base')
    call point_getdir_at(sdir)
    call start_position(sdir, 37.52, 200.5, 10.0, 5.0)
    call sim_reset()
    call set_test_clock(2024, 6, 1, 12, 1, 0)
    call sim_begin(gps_on_track(37.52, 200.5, 10.0, 5.0, 60), 1, 0)
    tdrop = -1
    do t = 60, 180                              ! 12:01:00 .. 12:03:00
      if (sim_loop(gps_on_track(37.52, 200.5, 10.0, 5.0, t))) then
        tdrop = t
        exit
      end if
      call advance_test_clock(1)
    end do
    call use_real_clock()

    if (tdrop /= 125) then
      print *, 'FAIL test_sioloop_begin_holds_drop_until_average: drop at 12:00:00 +', &
               tdrop, ' s (expected 125 = 12:02:05; -1 = none)'
      failures = failures + 1
    else
      print *, 'PASS test_sioloop_begin_holds_drop_until_average'
    end if
  end subroutine test_sioloop_begin_holds_drop_until_average

  ! A new sentence only every 5 s (as in the 9/8 log): in between, sioloop
  ! is called with the same sentence (same GPS second, iupdate=0). The
  ! reported position (drlat/drlon) must stay on the track - within 0.05 nm,
  ! i.e. at most the 4 s since the last sentence - and not fall back to the
  ! last GPS average (up to ~90 s behind: 0.25 nm at 10 kn).
  subroutine test_sioloop_no_snap_back_on_repeated_sentences(failures)
    integer, intent(inout) :: failures
    character(len=80) :: sdir
    type(gps_data) :: g
    integer :: t, tworst
    real :: err, errmax, tlat, tlon
    real, parameter :: d2r = 3.141592654 / 180.0

    call scratch_dir('loop_no_snap_back', sdir, 'tests\fixtures\base')
    call point_getdir_at(sdir)
    call start_position(sdir, 37.00, 200.5, 10.0, 5.0)
    call sim_reset()
    call set_test_clock(2024, 6, 1, 12, 1, 0)
    call sim_begin(gps_on_track(37.00, 200.5, 10.0, 5.0, 60), 1, 0)
    errmax = 0.0; tworst = -1
    do t = 60, 240                              ! 12:01:00 .. 12:04:00
      g = gps_on_track(37.00, 200.5, 10.0, 5.0, t - mod(t, 5))
      if (sim_loop(g)) exit
      if (t >= 125) then                        ! after the first average
        tlat = 37.00 + 10.0 * real(t) / 3600.0 * cos(5.0 * d2r) / 60.0
        tlon = 200.5 + 10.0 * real(t) / 3600.0 * sin(5.0 * d2r) / (60.0 * cos(37.0 * d2r)) - 360.0
        err = sqrt(((st%drlat - tlat) * 60.0)**2 + &
                   ((st%drlon - tlon) * 60.0 * cos(tlat * d2r))**2)
        if (err > errmax) then
          errmax = err; tworst = t
        end if
      end if
      call advance_test_clock(1)
    end do
    call use_real_clock()

    if (errmax > 0.05) then
      print *, 'FAIL test_sioloop_no_snap_back_on_repeated_sentences: max', errmax, &
               ' nm off the track at 12:00:00 +', tworst, ' s'
      failures = failures + 1
    else
      print *, 'PASS test_sioloop_no_snap_back_on_repeated_sentences'
    end if
  end subroutine test_sioloop_no_snap_back_on_repeated_sentences

  ! ---------------------------------------------------------------------------
  ! Helpers
  ! ---------------------------------------------------------------------------

  ! Last known position at 12:00:00 on 1 Jun 2024, as siobegin reloads it:
  ! Data\navtrk.dat and a MAN line in Data\010624.nav (lon in 0-360 E)
  subroutine start_position(sdir, lat, lone, spd, hdg)
    character(len=*), intent(in) :: sdir
    real, intent(in) :: lat, lone, spd, hdg
    integer :: u
    real :: lonw
    open(newunit=u, file=trim(sdir) // 'Data\navtrk.dat', status='replace')
    write(u, '(a2,a,a2,a,a2,a,i2,a,i2,a,i2,a,f7.3,f8.3,f6.2,f7.2)') &
         '01', '/', '06', '/', '24', ' ', 12, ':', 0, ':', 0, ' ', lat, lone, spd, hdg
    close(u)
    lonw = 360.0 - lone
    open(newunit=u, file=trim(sdir) // 'Data\010624.nav', status='replace')
    write(u, '(a2,a,a2,a,a2,a,i2,a,i2,a,i2,a,i3,a,f7.4,a,a1,a,i3,a,f7.4,a,a1,a,a3,a,f5.2,a,f5.1,i3)') &
         '01', '/', '06', '/', '24', ' ', 12, ':', 0, ':', 0, ' ', &
         int(lat), ' ', (lat - int(lat)) * 60.0, ' ', 'N', ' ', &
         int(lonw), ' ', (lonw - int(lonw)) * 60.0, ' ', 'W', ' ', 'MAN', ' ', spd, ' ', hdg, 0
    close(u)
  end subroutine start_position

  ! Good GPS fix t seconds after 12:00:00 on 1 Jun 2024, on the track from
  ! (lat0, lone0) at spd knots, heading hdg
  type(gps_data) function gps_on_track(lat0, lone0, spd, hdg, t) result(g)
    real,    intent(in) :: lat0, lone0, spd, hdg
    integer, intent(in) :: t
    real, parameter :: d2r = 3.141592654 / 180.0
    real :: nm, lat, lonw
    nm   = spd * real(t) / 3600.0
    lat  = lat0 + nm * cos(hdg * d2r) / 60.0
    lonw = 360.0 - (lone0 + nm * sin(hdg * d2r) / (60.0 * cos(lat0 * d2r)))
    g%ihr  = 12 + t / 3600
    g%imin = mod(t, 3600) / 60
    g%isec = mod(t, 60)
    g%iday = 1; g%imon = 6; g%iyr = 2024
    write(g%latdd,  '(i2)')   int(lat)
    write(g%latmm,  '(f7.4)') (lat - int(lat)) * 60.0
    write(g%londdd, '(i3)')   int(lonw)
    write(g%lonmm,  '(f7.4)') (lonw - int(lonw)) * 60.0
    g%ns = 'N'
    g%ew = 'W'
    g%valid = .true.
  end function gps_on_track

  ! Lines in Data\010624.nav with this status (NAV, DED, MAN)
  integer function count_status(sdir, astat)
    character(len=*), intent(in) :: sdir, astat
    character(len=120) :: line
    integer :: u, ios
    count_status = 0
    open(newunit=u, file=trim(sdir) // 'Data\010624.nav', status='old', iostat=ios)
    if (ios /= 0) return
    do
      read(u, '(a)', iostat=ios) line
      if (ios /= 0) exit
      if (index(line, ' ' // astat // ' ') > 0) count_status = count_status + 1
    end do
    close(u)
  end function count_status

end program test_integration_api
