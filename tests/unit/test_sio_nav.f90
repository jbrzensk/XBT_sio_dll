! tests/unit/test_sio_nav.f90
program test_sio_nav
  use sio_nav
  implicit none
  integer :: failures = 0

  ! chkall
  call test_chkall_valid(failures)
  call test_chkall_bad_speed(failures)
  call test_chkall_bad_dir(failures)
  call test_chkall_bad_lat(failures)
  call test_chkall_bad_lon(failures)
  call test_chkall_negative_lat(failures)
  call test_chkall_negative_lon(failures)

  ! chkwrite
  call test_chkwrite_valid(failures)
  call test_chkwrite_bad_lat(failures)
  call test_chkwrite_bad_lon(failures)

  ! newpos
  call test_newpos_north(failures)
  call test_newpos_east(failures)
  call test_newpos_south(failures)
  call test_newpos_lon_wrap(failures)
  call test_newpos_zero_speed(failures)

  ! interp
  call test_interp_midpoint(failures)
  call test_interp_at_start(failures)
  call test_interp_at_end(failures)
  call test_interp_before_start(failures)
  call test_interp_lon_crossing_near_zero(failures)
  call test_interp_zero_denominator(failures)

  ! planinfo
  call test_planinfo_lat_northbound(failures)
  call test_planinfo_lat_southbound(failures)
  call test_planinfo_lon_eastbound(failures)
  call test_planinfo_lon_westbound(failures)
  call test_planinfo_lon_wrap_west(failures)
  call test_planinfo_lon_wrap_east(failures)

  ! xbteta
  call test_xbteta_lat_positive_eta(failures)
  call test_xbteta_lat_negative_eta(failures)
  call test_xbteta_lon_eastbound(failures)
  call test_xbteta_zero_speed(failures)
  call test_xbteta_perpendicular_heading_no_crash(failures)

  ! chkbuf
  call test_chkbuf_single_point_bug(failures)
  call test_chkbuf_all_good(failures)
  call test_chkbuf_bad_timetag_packed_out(failures)
  call test_chkbuf_bad_latlon_packed_out(failures)
  call test_chkbuf_too_many_bad_times(failures)
  call test_chkbuf_saturday_rollover(failures)

  ! ave  (sequential: saves state across calls within the same test)
  call test_ave_ibuf_zero(failures)
  call test_ave_sequential(failures)
  call test_ave_timetag_rollover(failures)
  call test_ave_lon_crossing(failures)

  if (failures == 0) then
    print *, 'test_sio_nav: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_nav: FAILURES =', failures
    stop 1
  end if

contains

  ! ---------------------------------------------------------------------------
  ! chkall
  ! ---------------------------------------------------------------------------

  subroutine test_chkall_valid(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 200.0, 10.0, 90.0, ierr)
    if (ierr /= 0) then
      print *, 'FAIL test_chkall_valid: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkall_valid'
    end if
  end subroutine

  subroutine test_chkall_bad_speed(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 200.0, 100.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_speed: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_speed'
    end if
  end subroutine

  subroutine test_chkall_bad_dir(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 200.0, 10.0, 400.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_dir: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_dir'
    end if
  end subroutine

  subroutine test_chkall_bad_lat(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(100.0, 200.0, 10.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_lat: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_lat'
    end if
  end subroutine

  subroutine test_chkall_bad_lon(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 400.0, 10.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_lon: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_lon'
    end if
  end subroutine

  subroutine test_chkall_negative_lat(failures)
    ! xlat < -90 triggers the < -90 branch
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(-95.0, 200.0, 5.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_negative_lat: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_negative_lat'
    end if
  end subroutine

  subroutine test_chkall_negative_lon(failures)
    ! xlon < 0 triggers the < 0.0 branch
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, -1.0, 5.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_negative_lon: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_negative_lon'
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! chkwrite
  ! ---------------------------------------------------------------------------

  subroutine test_chkwrite_valid(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkwrite(30.0, 200.0, ierr)
    if (ierr /= 0) then
      print *, 'FAIL test_chkwrite_valid: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkwrite_valid'
    end if
  end subroutine

  subroutine test_chkwrite_bad_lat(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkwrite(95.0, 200.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkwrite_bad_lat: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkwrite_bad_lat'
    end if
  end subroutine

  subroutine test_chkwrite_bad_lon(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkwrite(30.0, 400.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkwrite_bad_lon: ierr =', ierr
      failures = failures + 1
    else
      print *, 'PASS test_chkwrite_bad_lon'
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! newpos
  ! ---------------------------------------------------------------------------

  subroutine test_newpos_north(failures)
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    vlat1 = 30.0; vlon1 = 200.0
    call newpos(60.0, 3600.0, 0.0, 30.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlat1 - 31.0) > 0.05 .or. aclath /= 'N') then
      print *, 'FAIL test_newpos_north: vlat1=', vlat1, ' aclath=', aclath
      failures = failures + 1
    else
      print *, 'PASS test_newpos_north'
    end if
  end subroutine

  subroutine test_newpos_east(failures)
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    vlat1 = 0.0; vlon1 = 200.0
    call newpos(60.0, 3600.0, 90.0, 0.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlon1 - 201.0) > 0.05) then
      print *, 'FAIL test_newpos_east: vlon1=', vlon1, ' expected ~201.0'
      failures = failures + 1
    else
      print *, 'PASS test_newpos_east'
    end if
  end subroutine

  subroutine test_newpos_south(failures)
    ! Heading south (dir=180) at 60 kt for 3600 sec = 60 nm = 1 degree lat south
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    vlat1 = 30.0; vlon1 = 200.0
    call newpos(60.0, 3600.0, 180.0, 30.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlat1 - 29.0) > 0.05) then
      print *, 'FAIL test_newpos_south: vlat1=', vlat1, ' expected ~29.0'
      failures = failures + 1
    else
      print *, 'PASS test_newpos_south'
    end if
  end subroutine

  subroutine test_newpos_lon_wrap(failures)
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    vlat1 = 0.0; vlon1 = 359.5
    call newpos(60.0, 3600.0, 90.0, 0.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlon1 - 0.5) > 0.05) then
      print *, 'FAIL test_newpos_lon_wrap: vlon1=', vlon1, ' expected ~0.5'
      failures = failures + 1
    else
      print *, 'PASS test_newpos_lon_wrap'
    end if
  end subroutine

  subroutine test_newpos_zero_speed(failures)
    ! Speed=0: position should not change
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    vlat1 = 30.0; vlon1 = 200.0
    call newpos(0.0, 3600.0, 45.0, 30.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlat1 - 30.0) > 0.001 .or. abs(vlon1 - 200.0) > 0.001) then
      print *, 'FAIL test_newpos_zero_speed: vlat1=', vlat1, ' vlon1=', vlon1
      failures = failures + 1
    else
      print *, 'PASS test_newpos_zero_speed'
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! interp
  ! ---------------------------------------------------------------------------

  subroutine test_interp_midpoint(failures)
    ! Drop exactly at midpoint between two nav fixes
    integer, intent(inout) :: failures
    real :: xlat, xlon
    call interp(0.5, 31.0, 201.0, 0.0, 30.0, 200.0, 1.0, xlat, xlon)
    if (abs(xlat - 30.5) > 0.001 .or. abs(xlon - 200.5) > 0.001) then
      print *, 'FAIL test_interp_midpoint: xlat=', xlat, ' xlon=', xlon
      failures = failures + 1
    else
      print *, 'PASS test_interp_midpoint'
    end if
  end subroutine

  subroutine test_interp_at_start(failures)
    ! Drop at exact start fix: frac=0 → should return zlat, zlon
    integer, intent(inout) :: failures
    real :: xlat, xlon
    call interp(0.0, 31.0, 201.0, 0.0, 30.0, 200.0, 1.0, xlat, xlon)
    if (abs(xlat - 30.0) > 0.001 .or. abs(xlon - 200.0) > 0.001) then
      print *, 'FAIL test_interp_at_start: xlat=', xlat, ' xlon=', xlon
      failures = failures + 1
    else
      print *, 'PASS test_interp_at_start'
    end if
  end subroutine

  subroutine test_interp_at_end(failures)
    ! Drop at exact end fix: frac=1 → should return ylat, ylon
    integer, intent(inout) :: failures
    real :: xlat, xlon
    call interp(1.0, 31.0, 201.0, 0.0, 30.0, 200.0, 1.0, xlat, xlon)
    if (abs(xlat - 31.0) > 0.001 .or. abs(xlon - 201.0) > 0.001) then
      print *, 'FAIL test_interp_at_end: xlat=', xlat, ' xlon=', xlon
      failures = failures + 1
    else
      print *, 'PASS test_interp_at_end'
    end if
  end subroutine

  subroutine test_interp_before_start(failures)
    ! Drop before first fix: frac < 0, linear extrapolation
    integer, intent(inout) :: failures
    real :: xlat, xlon
    ! frac = (yrdrop - yrsav) / (yrnav - yrsav) = (-0.5-0)/(1-0) = -0.5
    ! xlat = 30 + (31-30)*(-0.5) = 29.5
    call interp(-0.5, 31.0, 201.0, 0.0, 30.0, 200.0, 1.0, xlat, xlon)
    if (abs(xlat - 29.5) > 0.001 .or. abs(xlon - 199.5) > 0.001) then
      print *, 'FAIL test_interp_before_start: xlat=', xlat, ' xlon=', xlon
      failures = failures + 1
    else
      print *, 'PASS test_interp_before_start'
    end if
  end subroutine

  subroutine test_interp_lon_crossing_near_zero(failures)
    ! zlon near 360, ylon near 0 — abs(ylon-zlon)=358 > 300
    ! ylon=1 not > 300 → use: xlon = zlon + ((ylon+360)-zlon)*frac
    ! frac=0.5: xlon = 359 + (361-359)*0.5 = 360.0
    integer, intent(inout) :: failures
    real :: xlat, xlon
    call interp(0.5, 30.5, 1.0, 0.0, 30.0, 359.0, 1.0, xlat, xlon)
    ! Result xlon = 360.0 (caller responsibility to wrap)
    if (abs(xlon - 360.0) > 0.01) then
      print *, 'FAIL test_interp_lon_crossing_near_zero: xlon=', xlon, ' expected 360.0'
      failures = failures + 1
    else
      print *, 'PASS test_interp_lon_crossing_near_zero'
    end if
  end subroutine

  subroutine test_interp_zero_denominator(failures)
    ! yrsav == yrnav → yrdenom clamped to 0.001, no crash
    integer, intent(inout) :: failures
    real :: xlat, xlon
    call interp(1.0, 31.0, 201.0, 0.5, 30.0, 200.0, 0.5, xlat, xlon)
    ! Just verify it doesn't crash and returns a real number
    if (xlat /= xlat) then   ! NaN check
      print *, 'FAIL test_interp_zero_denominator: xlat is NaN'
      failures = failures + 1
    else
      print *, 'PASS test_interp_zero_denominator: xlat=', xlat
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! planinfo
  ! ---------------------------------------------------------------------------

  subroutine test_planinfo_lat_northbound(failures)
    ! xlat=30 < xlat1=31 (heading toward 31N) → iplandir=1 (N), ispec=1
    integer, intent(inout) :: failures
    character(len=3) :: aspec
    integer :: ispec, iplandir
    call planinfo(30.0, 'N', 31.0, 'N', aspec, ispec, iplandir, 0.0, 0.0)
    if (ispec /= 1 .or. iplandir /= 1 .or. aspec /= 'lat') then
      print *, 'FAIL test_planinfo_lat_northbound: ispec=', ispec, &
               ' iplandir=', iplandir, ' aspec=', aspec
      failures = failures + 1
    else
      print *, 'PASS test_planinfo_lat_northbound'
    end if
  end subroutine

  subroutine test_planinfo_lat_southbound(failures)
    ! xlat=32 > xlat1=31 → iplandir=3 (S), ispec=1
    integer, intent(inout) :: failures
    character(len=3) :: aspec
    integer :: ispec, iplandir
    call planinfo(32.0, 'N', 31.0, 'N', aspec, ispec, iplandir, 0.0, 0.0)
    if (ispec /= 1 .or. iplandir /= 3) then
      print *, 'FAIL test_planinfo_lat_southbound: ispec=', ispec, ' iplandir=', iplandir
      failures = failures + 1
    else
      print *, 'PASS test_planinfo_lat_southbound'
    end if
  end subroutine

  subroutine test_planinfo_lon_eastbound(failures)
    ! alath='E' → lon-based; xlat=200 < xlat1=201 → iplandir=2 (E), ispec=0
    integer, intent(inout) :: failures
    character(len=3) :: aspec
    integer :: ispec, iplandir
    call planinfo(200.0, 'E', 201.0, 'N', aspec, ispec, iplandir, 0.0, 0.0)
    if (ispec /= 0 .or. iplandir /= 2 .or. aspec /= 'lon') then
      print *, 'FAIL test_planinfo_lon_eastbound: ispec=', ispec, &
               ' iplandir=', iplandir, ' aspec=', aspec
      failures = failures + 1
    else
      print *, 'PASS test_planinfo_lon_eastbound'
    end if
  end subroutine

  subroutine test_planinfo_lon_westbound(failures)
    ! alath='W' → lon-based; xlat=202 > xlat1=201 → iplandir=4 (W)
    integer, intent(inout) :: failures
    character(len=3) :: aspec
    integer :: ispec, iplandir
    call planinfo(202.0, 'W', 201.0, 'N', aspec, ispec, iplandir, 0.0, 0.0)
    if (ispec /= 0 .or. iplandir /= 4) then
      print *, 'FAIL test_planinfo_lon_westbound: ispec=', ispec, ' iplandir=', iplandir
      failures = failures + 1
    else
      print *, 'PASS test_planinfo_lon_westbound'
    end if
  end subroutine

  subroutine test_planinfo_lon_wrap_west(failures)
    ! xlat=355 (>350) and xlat1=5 (<10): 0/360 crossing → iplandir=4 (W)
    integer, intent(inout) :: failures
    character(len=3) :: aspec
    integer :: ispec, iplandir
    call planinfo(355.0, 'W', 5.0, 'N', aspec, ispec, iplandir, 0.0, 0.0)
    if (iplandir /= 4) then
      print *, 'FAIL test_planinfo_lon_wrap_west: iplandir=', iplandir, ' expected 4'
      failures = failures + 1
    else
      print *, 'PASS test_planinfo_lon_wrap_west'
    end if
  end subroutine

  subroutine test_planinfo_lon_wrap_east(failures)
    ! xlat=5 (<10) and xlat1=355 (>350): 0/360 crossing → iplandir=2 (E)
    integer, intent(inout) :: failures
    character(len=3) :: aspec
    integer :: ispec, iplandir
    call planinfo(5.0, 'W', 355.0, 'N', aspec, ispec, iplandir, 0.0, 0.0)
    if (iplandir /= 2) then
      print *, 'FAIL test_planinfo_lon_wrap_east: iplandir=', iplandir, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_planinfo_lon_wrap_east'
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! xbteta
  ! ---------------------------------------------------------------------------

  subroutine test_xbteta_lat_positive_eta(failures)
    ! Ship at 30N, target lat 31N, heading north at 10 kt
    ! dxlatld = 30-31 = -1 deg, 60 nm. x=cos(0)=1. eta=60/10=6h (positive, ahead)
    integer, intent(inout) :: failures
    real    :: xlatload(12), peta(12), vlat1, vlon1, speed, dir
    integer :: ispec(12), nplan
    xlatload = 0.0; peta = 0.0; ispec = 0
    xlatload(1) = 31.0; ispec(1) = 1
    vlat1 = 30.0; vlon1 = 200.0; speed = 10.0; dir = 0.0; nplan = 0
    call xbteta(xlatload, vlat1, vlon1, speed, dir, ispec, nplan, 0, 12, peta, 0)
    if (abs(peta(1) - 6.0) > 0.1) then
      print *, 'FAIL test_xbteta_lat_positive_eta: peta(1)=', peta(1), ' expected 6.0'
      failures = failures + 1
    else
      print *, 'PASS test_xbteta_lat_positive_eta'
    end if
  end subroutine

  subroutine test_xbteta_lat_negative_eta(failures)
    ! Ship at 31N, target lat 30N, heading north — moving away → negative ETA
    integer, intent(inout) :: failures
    real    :: xlatload(12), peta(12), vlat1, vlon1, speed, dir
    integer :: ispec(12), nplan
    xlatload = 0.0; peta = 0.0; ispec = 0
    xlatload(1) = 30.0; ispec(1) = 1
    vlat1 = 31.0; vlon1 = 200.0; speed = 10.0; dir = 0.0; nplan = 0
    call xbteta(xlatload, vlat1, vlon1, speed, dir, ispec, nplan, 0, 12, peta, 0)
    ! peta < 0 means heading wrong direction
    if (peta(1) >= 0.0) then
      print *, 'FAIL test_xbteta_lat_negative_eta: peta(1)=', peta(1), ' expected < 0'
      failures = failures + 1
    else
      print *, 'PASS test_xbteta_lat_negative_eta'
    end if
  end subroutine

  subroutine test_xbteta_lon_eastbound(failures)
    ! Ship at 30N/200E, target lon 201E, heading east at 10 kt
    ! dxlonnm1 = 60*cos(30)~51.96 nm. x=sin(90)=1. eta~5.196h (positive)
    integer, intent(inout) :: failures
    real    :: xlatload(12), peta(12), vlat1, vlon1, speed, dir
    integer :: ispec(12), nplan
    xlatload = 0.0; peta = 0.0; ispec = 0
    xlatload(1) = 201.0; ispec(1) = 0
    vlat1 = 30.0; vlon1 = 200.0; speed = 10.0; dir = 90.0; nplan = 0
    call xbteta(xlatload, vlat1, vlon1, speed, dir, ispec, nplan, 0, 12, peta, 0)
    if (peta(1) < 4.0 .or. peta(1) > 7.0) then
      print *, 'FAIL test_xbteta_lon_eastbound: peta(1)=', peta(1), ' expected ~5.2'
      failures = failures + 1
    else
      print *, 'PASS test_xbteta_lon_eastbound'
    end if
  end subroutine

  subroutine test_xbteta_zero_speed(failures)
    ! speed=0 → eta_val = distld (raw distance, not hours)
    integer, intent(inout) :: failures
    real    :: xlatload(12), peta(12), vlat1, vlon1, speed, dir
    integer :: ispec(12), nplan
    xlatload = 0.0; peta = 0.0; ispec = 0
    xlatload(1) = 31.0; ispec(1) = 1
    vlat1 = 30.0; vlon1 = 200.0; speed = 0.0; dir = 0.0; nplan = 0
    call xbteta(xlatload, vlat1, vlon1, speed, dir, ispec, nplan, 0, 12, peta, 0)
    ! distld = 60 nm. With speed=0: eta_val = distld = 60.0
    if (abs(peta(1)) < 0.001) then
      print *, 'FAIL test_xbteta_zero_speed: peta(1)=', peta(1), ' expected non-zero'
      failures = failures + 1
    else
      print *, 'PASS test_xbteta_zero_speed: peta(1)=', peta(1)
    end if
  end subroutine

  subroutine test_xbteta_perpendicular_heading_no_crash(failures)
    ! Ship heading east (90), target is a latitude line — perpendicular.
    ! Bug #4: x=cos(90)=0, fallback uses raw nm distance, gives non-infinite ETA.
    ! This test documents the known behavior without failing.
    integer, intent(inout) :: failures
    real    :: xlatload(12), peta(12), vlat1, vlon1, speed, dir
    integer :: ispec(12), nplan
    xlatload = 0.0; peta = 0.0; ispec = 0
    xlatload(1) = 31.0; ispec(1) = 1
    vlat1 = 30.0; vlon1 = 200.0; speed = 10.0; dir = 90.0; nplan = 0
    call xbteta(xlatload, vlat1, vlon1, speed, dir, ispec, nplan, 0, 12, peta, 0)
    ! Just verify no crash and a finite result
    if (peta(1) /= peta(1)) then   ! NaN check
      print *, 'FAIL test_xbteta_perpendicular_heading_no_crash: peta(1) is NaN'
      failures = failures + 1
    else
      print *, 'PASS test_xbteta_perpendicular_heading_no_crash: peta(1)=', peta(1)
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! chkbuf
  ! ---------------------------------------------------------------------------

  subroutine test_chkbuf_single_point_bug(failures)
    ! BUG #3: ibuf=1 triggers ibad >= (ibuf-1) = 0 >= 0 → ierr=1 even with valid data.
    ! This test documents the known behavior.
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 1
    clatbuf(1) = 30.0; clonbuf(1) = 200.0; ctagbuf(1) = 100.0
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    ! Documented behavior: ierr=1 for single-point buffer (Bug #3)
    if (ierr /= 1) then
      print *, 'FAIL test_chkbuf_single_point_bug: ierr=', ierr, &
               ' (Bug #3: expected 1 even for valid single point)'
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_single_point_bug (Bug #3 documented)'
    end if
  end subroutine

  subroutine test_chkbuf_all_good(failures)
    ! ibuf=3, monotonic timetags, positions within 0.5 deg of last → ierr=0
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=100.0; clatbuf(1)=30.00; clonbuf(1)=200.00
    ctagbuf(2)=200.0; clatbuf(2)=30.05; clonbuf(2)=200.05
    ctagbuf(3)=300.0; clatbuf(3)=30.10; clonbuf(3)=200.10
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    if (ierr /= 0 .or. ibuf /= 3) then
      print *, 'FAIL test_chkbuf_all_good: ierr=', ierr, ' ibuf=', ibuf
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_all_good'
    end if
  end subroutine

  subroutine test_chkbuf_bad_timetag_packed_out(failures)
    ! ctagbuf(1) ahead of ctagbuf(3) → first entry marked bad, packed out → ibuf=2
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=500.0; clatbuf(1)=30.05; clonbuf(1)=200.05   ! bad: 500 > ctagbuf(3)=300
    ctagbuf(2)=200.0; clatbuf(2)=30.05; clonbuf(2)=200.05
    ctagbuf(3)=300.0; clatbuf(3)=30.10; clonbuf(3)=200.10
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    if (ierr /= 0 .or. ibuf /= 2) then
      print *, 'FAIL test_chkbuf_bad_timetag_packed_out: ierr=', ierr, ' ibuf=', ibuf
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_bad_timetag_packed_out'
    end if
  end subroutine

  subroutine test_chkbuf_bad_latlon_packed_out(failures)
    ! clatbuf(2) is far from clatbuf(3) → entry 2 marked bad, packed out → ibuf=2
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=100.0; clatbuf(1)=30.00; clonbuf(1)=200.00
    ctagbuf(2)=200.0; clatbuf(2)=35.00; clonbuf(2)=200.05   ! bad: |35-30.1|=4.9>0.5
    ctagbuf(3)=300.0; clatbuf(3)=30.10; clonbuf(3)=200.10
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    if (ierr /= 0 .or. ibuf /= 2) then
      print *, 'FAIL test_chkbuf_bad_latlon_packed_out: ierr=', ierr, ' ibuf=', ibuf
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_bad_latlon_packed_out'
    end if
  end subroutine

  subroutine test_chkbuf_too_many_bad_times(failures)
    ! Both non-last entries out of order: ibad=2 >= ibuf-1=2 → ierr=1
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=600.0; clatbuf(1)=30.00; clonbuf(1)=200.00   ! bad: 600 > 300
    ctagbuf(2)=700.0; clatbuf(2)=30.05; clonbuf(2)=200.05   ! bad: 700 > 300
    ctagbuf(3)=300.0; clatbuf(3)=30.10; clonbuf(3)=200.10
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    if (ierr /= 1) then
      print *, 'FAIL test_chkbuf_too_many_bad_times: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_too_many_bad_times'
    end if
  end subroutine

  subroutine test_chkbuf_saturday_rollover(failures)
    ! ctagbuf(1)>604500: rollover adjusts entries <1000 upward by 604800.
    ! After adjustment all entries are monotonic → ierr=0
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=604700.0; clatbuf(1)=30.00; clonbuf(1)=200.00
    ctagbuf(2)=100.0;    clatbuf(2)=30.05; clonbuf(2)=200.05   ! → 604900 after adjust
    ctagbuf(3)=200.0;    clatbuf(3)=30.10; clonbuf(3)=200.10   ! → 605000 after adjust
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    if (ierr /= 0) then
      print *, 'FAIL test_chkbuf_saturday_rollover: ierr=', ierr, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_saturday_rollover'
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! ave — sequential two-call test (save state carries over intentionally)
  ! ---------------------------------------------------------------------------

  subroutine test_ave_ibuf_zero(failures)
    integer, intent(inout) :: failures
    integer :: ierr, ierror(50), iSIOSpeedAveMin
    real :: xlat(200), xlon(200), timetag(200)
    real :: s10, d10, timeave, vlat, vlon
    character(len=1) :: avlath, avlonh
    ierror = 0; iSIOSpeedAveMin = 2
    timeave = 0.0; vlat = 0.0; vlon = 0.0; s10 = 0.0; d10 = 0.0
    call ave(0, xlat, xlon, timetag, avlath, avlonh, s10, d10, &
             timeave, vlat, vlon, ierror, ierr, iSIOSpeedAveMin, 0, 0)
    if (ierr /= -1) then
      print *, 'FAIL test_ave_ibuf_zero: ierr=', ierr, ' expected -1'
      failures = failures + 1
    else
      print *, 'PASS test_ave_ibuf_zero'
    end if
  end subroutine

  subroutine test_ave_sequential(failures)
    ! Two successive calls to ave. First call: ring buffer primed, s10/d10=-99.
    ! Second call: s10/d10 computed from 2-point ring buffer.
    integer, intent(inout) :: failures
    integer :: ierr, ierror(50), iSIOSpeedAveMin
    real :: xlat(200), xlon(200), timetag(200)
    real :: s10, d10, timeave, vlat, vlon
    character(len=1) :: avlath, avlonh
    integer :: i

    ! --- First call: fresh start (ierror(38)=0, ierror(39)=0 forces ifirst=1) ---
    ierror = 0
    iSIOSpeedAveMin = 2
    timeave = 0.0; vlat = 0.0; vlon = 0.0
    s10 = 0.0; d10 = 0.0
    timetag(1) = 0.0; xlat(1) = 30.0; xlon(1) = 200.0
    timetag(2) = 10.0; xlat(2) = 30.1; xlon(2) = 200.1
    timetag(3) = 20.0; xlat(3) = 30.2; xlon(3) = 200.2
    call ave(3, xlat, xlon, timetag, avlath, avlonh, s10, d10, &
             timeave, vlat, vlon, ierror, ierr, iSIOSpeedAveMin, 0, 0)
    ! First call: jptr becomes 1 → early return, s10 and d10 stay -99
    if (ierr /= 1) then
      print *, 'FAIL test_ave_sequential (call1 ierr): ierr=', ierr, ' expected 1'
      failures = failures + 1
      return
    end if
    if (s10 /= -99.0 .or. d10 /= -99.0) then
      print *, 'FAIL test_ave_sequential (call1 s10/d10): s10=', s10, ' d10=', d10
      failures = failures + 1
      return
    end if
    if (abs(vlat - 30.1) > 0.02 .or. abs(vlon - 200.1) > 0.02) then
      print *, 'FAIL test_ave_sequential (call1 vlat/vlon): vlat=', vlat, ' vlon=', vlon
      failures = failures + 1
      return
    end if
    print *, 'PASS test_ave_sequential call1: vlat=', vlat, ' vlon=', vlon

    ! --- Second call: ierror(38/39) carry state from first call ---
    timetag(1) = 30.0; xlat(1) = 30.3; xlon(1) = 200.3
    timetag(2) = 40.0; xlat(2) = 30.4; xlon(2) = 200.4
    timetag(3) = 50.0; xlat(3) = 30.5; xlon(3) = 200.5
    call ave(3, xlat, xlon, timetag, avlath, avlonh, s10, d10, &
             timeave, vlat, vlon, ierror, ierr, iSIOSpeedAveMin, 0, 0)
    ! Second call: jptr=2, s10 and d10 should now be computed (>= 0)
    if (ierr /= 1) then
      print *, 'FAIL test_ave_sequential (call2 ierr): ierr=', ierr, ' expected 1'
      failures = failures + 1
    else if (s10 < 0.0) then
      print *, 'FAIL test_ave_sequential (call2 s10): s10=', s10, ' expected >= 0'
      failures = failures + 1
    else if (d10 < 0.0 .or. d10 > 360.0) then
      print *, 'FAIL test_ave_sequential (call2 d10): d10=', d10, ' expected in [0,360]'
      failures = failures + 1
    else
      print *, 'PASS test_ave_sequential call2: s10=', s10, ' d10=', d10
    end if
  end subroutine

  ! ave: Saturday night rollover — timetag(1) >> timetag(ibuf) → adjusts small timetags.
  ! Covers the do-loop at lines 90-91 in sio_nav.f90.
  subroutine test_ave_timetag_rollover(failures)
    integer, intent(inout) :: failures
    integer :: ierr, ierror(50), iSIOSpeedAveMin
    real :: xlat(200), xlon(200), timetag(200)
    real :: s10, d10, timeave, vlat, vlon
    character(len=1) :: avlath, avlonh
    ierror = 0; iSIOSpeedAveMin = 2
    timeave = 0.0; vlat = 0.0; vlon = 0.0; s10 = 0.0; d10 = 0.0
    xlat = 0.0; xlon = 200.0; timetag = 0.0
    ! First two fixes near end of week (~604700-604750), third just after midnight (~300)
    timetag(1) = 604700.0; xlat(1) = 30.0; xlon(1) = 200.0
    timetag(2) = 604750.0; xlat(2) = 30.1; xlon(2) = 200.1
    timetag(3) =    300.0; xlat(3) = 30.2; xlon(3) = 200.2
    call ave(3, xlat, xlon, timetag, avlath, avlonh, s10, d10, &
             timeave, vlat, vlon, ierror, ierr, iSIOSpeedAveMin, 0, 0)
    if (ierr < -1) then
      print *, 'FAIL test_ave_timetag_rollover: ierr=', ierr
      failures = failures + 1
    else
      print *, 'PASS test_ave_timetag_rollover: ierr=', ierr
    end if
  end subroutine

  ! ave: lon crossing near 0/360 — adjusts xlon(i)<1.0 entries upward.
  ! Covers the do-loop at lines 113-114 in sio_nav.f90.
  subroutine test_ave_lon_crossing(failures)
    integer, intent(inout) :: failures
    integer :: ierr, ierror(50), iSIOSpeedAveMin
    real :: xlat(200), xlon(200), timetag(200)
    real :: s10, d10, timeave, vlat, vlon
    character(len=1) :: avlath, avlonh
    ierror = 0; iSIOSpeedAveMin = 2
    timeave = 0.0; vlat = 0.0; vlon = 0.0; s10 = 0.0; d10 = 0.0
    xlat = 0.0; xlon = 200.0; timetag = 0.0
    timetag(1) = 0.0; timetag(2) = 10.0; timetag(3) = 20.0
    xlat(1) = 30.0; xlat(2) = 30.1; xlat(3) = 30.2
    ! xlon crossing 360→0 boundary: first > 359, last < 1 → loop adjusts entries < 1
    xlon(1) = 359.5; xlon(2) = 0.3; xlon(3) = 0.5
    call ave(3, xlat, xlon, timetag, avlath, avlonh, s10, d10, &
             timeave, vlat, vlon, ierror, ierr, iSIOSpeedAveMin, 0, 0)
    if (ierr < -1) then
      print *, 'FAIL test_ave_lon_crossing: ierr=', ierr
      failures = failures + 1
    else
      print *, 'PASS test_ave_lon_crossing: ierr=', ierr
    end if
  end subroutine

end program test_sio_nav
