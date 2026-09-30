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
  call test_newpos_northwest_real(failures)
  call test_newpos_negative_change(failures)

  ! dr_elapsed
  call test_dr_elapsed_garbled_hour_behind(failures)
  call test_dr_elapsed_jump_ahead(failures)
  call test_dr_elapsed_outage_agrees(failures)
  call test_dr_elapsed_no_ref_negative(failures)
  call test_dr_elapsed_no_ref_positive(failures)
  call test_dr_elapsed_counter_reset(failures)

  ! past_station (geometry extracted from sioloop + trust gate)
  call test_past_station_north(failures)
  call test_past_station_south(failures)
  call test_past_station_east(failures)
  call test_past_station_east_too_far(failures)
  call test_past_station_west(failures)
  call test_past_station_west_across_zero(failures)
  call test_past_station_wrong_heading_lon(failures)
  call test_past_station_wrong_heading_lat(failures)
  call test_past_station_over_max_speed(failures)
  call test_past_station_bad_plandir(failures)
  call test_past_station_untrusted(failures)

  ! drop_countdown (runsec settle delay: arm once, re-check when it runs out)
  call test_countdown_zero_delay_fires_at_once(failures)
  call test_countdown_idle(failures)
  call test_countdown_arms_once_and_fires(failures)
  call test_countdown_single_glitch_cannot_drop(failures)
  call test_countdown_rearms_after_glitch(failures)
  call test_countdown_trust_lost_cancels(failures)
  call test_countdown_late_call_fires(failures)
  call test_begin_holds_drop_until_average(failures)
  call test_begin_drop_after_confirming_average(failures)

  ! ave_consistent (new average agrees with dead reckoning from the last one)
  call test_ave_consistent_on_track(failures)
  call test_ave_consistent_small_offset(failures)
  call test_ave_consistent_jump_2nm(failures)
  call test_ave_consistent_jump_0908(failures)
  call test_ave_consistent_lon_wrap(failures)
  call test_ave_consistent_stuck_same(failures)
  call test_ave_consistent_stuck_moved(failures)

  ! check_time (incoming GPS time vs PC clock)
  call test_check_time_no_reference(failures)
  call test_check_time_normal_advance(failures)
  call test_check_time_garbled_hour(failures)
  call test_check_time_stale_garmin(failures)
  call test_check_time_midnight(failures)
  call test_check_time_frozen_never_adopted(failures)
  call test_check_time_genuine_step_adopted(failures)

  ! check_fix (incoming GPS position plausibility)
  call test_check_fix_no_reference(failures)
  call test_check_fix_normal(failures)
  call test_check_fix_hemisphere_flip(failures)
  call test_check_fix_null_island(failures)
  call test_check_fix_zero_lat(failures)
  call test_check_fix_isolated_jump(failures)
  call test_check_fix_bad_reference_recovers(failures)
  call test_check_fix_after_outage(failures)

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
  call test_chkbuf_single_point_rejected(failures)
  call test_chkbuf_all_good(failures)
  call test_chkbuf_bad_timetag_packed_out(failures)
  call test_chkbuf_bad_latlon_packed_out(failures)
  call test_chkbuf_too_many_bad_times(failures)
  call test_chkbuf_saturday_rollover(failures)
  call test_chkbuf_stale_last_entry_keeps_good(failures)
  call test_chkbuf_flipped_last_entry_keeps_good(failures)
  call test_chkbuf_stale_block_at_end_packed_out(failures)
  call test_chkbuf_greenwich_crossing_kept(failures)
  call test_chkbuf_flipped_lon_across_dateline(failures)
  call test_chkbuf_no_majority_rejected(failures)
  call test_chkbuf_midnight_keeps_majority_side(failures)

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

  ! 9/8/26 case, forward in time: 10.62 kt on 351.8 for 3570 s from
  ! 33 37.8225N 118 10.0974W moves +10.43' lat and 1.80' further west.
  subroutine test_newpos_northwest_real(failures)
    integer, intent(inout) :: failures
    real :: vlat0, vlon0, vlat1, vlon1
    character(len=1) :: aclath
    vlat0 = 33.0 + 37.8225/60.0
    vlon0 = 360.0 - (118.0 + 10.0974/60.0)
    vlat1 = vlat0; vlon1 = vlon0
    call newpos(10.62, 3570.0, 351.8, vlat0, vlat1, vlon1, aclath, 0, 0)
    if (abs((vlat1 - vlat0)*60.0 - 10.43) > 0.05 .or. &
        abs((vlon0 - vlon1)*60.0 - 1.80) > 0.05) then
      print *, 'FAIL test_newpos_northwest_real: dlat min=', (vlat1-vlat0)*60.0, &
               ' dlon min west=', (vlon0-vlon1)*60.0
      failures = failures + 1
    else
      print *, 'PASS test_newpos_northwest_real'
    end if
  end subroutine

  ! 9/8/26 bug: a garbled GPS time an hour behind gave change = -3570 s and
  ! newpos moved the ship 10.4 nm FORWARD. Negative elapsed time must never
  ! move the dead-reckoned position.
  subroutine test_newpos_negative_change(failures)
    integer, intent(inout) :: failures
    real :: vlat0, vlon0, vlat1, vlon1
    character(len=1) :: aclath
    vlat0 = 33.0 + 37.8225/60.0
    vlon0 = 360.0 - (118.0 + 10.0974/60.0)
    vlat1 = vlat0; vlon1 = vlon0
    call newpos(10.62, -3570.0, 351.8, vlat0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlat1 - vlat0) > 1.0e-5 .or. abs(vlon1 - vlon0) > 1.0e-5) then
      print *, 'FAIL test_newpos_negative_change: moved dlat min=', (vlat1-vlat0)*60.0, &
               ' dlon min=', (vlon1-vlon0)*60.0
      failures = failures + 1
    else
      print *, 'PASS test_newpos_negative_change'
    end if
  end subroutine

  ! ---------------------------------------------------------------------------
  ! dr_elapsed(gps_change, itime, itimeave, iw, ifile)
  !   gps_change - DR seconds from GPS time (gpstime - timeave)
  !   itime      - PC-clock seconds counter now
  !   itimeave   - itime at the last accepted average (<0: unknown)
  ! ---------------------------------------------------------------------------

  subroutine check_elapsed(name, gps, itime, itimeave, expected, failures)
    character(len=*), intent(in) :: name
    real,    intent(in)    :: gps, expected
    integer, intent(in)    :: itime, itimeave
    integer, intent(inout) :: failures
    real :: got
    got = dr_elapsed(gps, itime, itimeave, 0, 0)
    if (abs(got - expected) > 0.001) then
      print *, 'FAIL ', name, ': got=', got, ' expected=', expected
      failures = failures + 1
    else
      print *, 'PASS ', name
    end if
  end subroutine

  ! 9/8/26 10:40:00 garbled time: GPS says -3570 s, PC clock says 30 s
  subroutine test_dr_elapsed_garbled_hour_behind(failures)
    integer, intent(inout) :: failures
    call check_elapsed('test_dr_elapsed_garbled_hour_behind', -3570.0, 1000, 970, 30.0, failures)
  end subroutine

  ! Garbled time ahead of truth: GPS says 900 s, PC clock says 30 s
  subroutine test_dr_elapsed_jump_ahead(failures)
    integer, intent(inout) :: failures
    call check_elapsed('test_dr_elapsed_jump_ahead', 900.0, 1000, 970, 30.0, failures)
  end subroutine

  ! Genuine 20 min outage: GPS and PC agree within tolerance -> keep GPS value
  subroutine test_dr_elapsed_outage_agrees(failures)
    integer, intent(inout) :: failures
    call check_elapsed('test_dr_elapsed_outage_agrees', 1200.0, 2195, 1000, 1200.0, failures)
  end subroutine

  ! No PC reference yet (first calls after siobegin): never go backwards
  subroutine test_dr_elapsed_no_ref_negative(failures)
    integer, intent(inout) :: failures
    call check_elapsed('test_dr_elapsed_no_ref_negative', -50.0, 1000, -1, 0.0, failures)
  end subroutine

  subroutine test_dr_elapsed_no_ref_positive(failures)
    integer, intent(inout) :: failures
    call check_elapsed('test_dr_elapsed_no_ref_positive', 600.0, 1000, -1, 600.0, failures)
  end subroutine

  ! siobegin resets itime to 0; a leftover itimeave larger than itime is not
  ! a valid reference and must not force a negative (frozen) DR time.
  subroutine test_dr_elapsed_counter_reset(failures)
    integer, intent(inout) :: failures
    call check_elapsed('test_dr_elapsed_counter_reset', 600.0, 10, 970, 600.0, failures)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! past_station(ispec1, iplandir, dir, speed, xmaxspd, vlat1, vlon1,
  !              xlat, xlon, trusted)
  !   ispec1   - 1 lat-based plan, 0 lon-based plan
  !   iplandir - plan direction N=1, E=2, S=3, W=4
  ! ---------------------------------------------------------------------------

  subroutine check_past(name, ispec1, iplandir, dir, speed, vlat1, vlon1, &
                        xlat, xlon, trusted, expected, failures)
    character(len=*), intent(in) :: name
    integer, intent(in)    :: ispec1, iplandir
    real,    intent(in)    :: dir, speed, vlat1, vlon1, xlat, xlon
    logical, intent(in)    :: trusted, expected
    integer, intent(inout) :: failures
    logical :: got
    got = past_station(ispec1, iplandir, dir, speed, 20.0, vlat1, vlon1, &
                       xlat, xlon, trusted)
    if (got .neqv. expected) then
      print *, 'FAIL ', name, ': got=', got, ' expected=', expected
      failures = failures + 1
    else
      print *, 'PASS ', name
    end if
  end subroutine

  subroutine test_past_station_north(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_north_past', 1, 1, 350.0, 9.0, &
                    33.70, 241.82, 33.69, 241.82, .true., .true., failures)
    call check_past('test_past_station_north_before', 1, 1, 350.0, 9.0, &
                    33.68, 241.82, 33.69, 241.82, .true., .false., failures)
  end subroutine

  subroutine test_past_station_south(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_south_past', 1, 3, 180.0, 9.0, &
                    33.68, 241.82, 33.69, 241.82, .true., .true., failures)
  end subroutine

  subroutine test_past_station_east(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_east_past', 0, 2, 90.0, 9.0, &
                    33.0, 200.2, 33.0, 200.1, .true., .true., failures)
  end subroutine

  ! More than 20 degrees of longitude beyond the station is not past it
  subroutine test_past_station_east_too_far(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_east_too_far', 0, 2, 90.0, 9.0, &
                    33.0, 225.0, 33.0, 200.0, .true., .false., failures)
  end subroutine

  subroutine test_past_station_west(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_west_past', 0, 4, 270.0, 9.0, &
                    33.0, 200.0, 33.0, 200.1, .true., .true., failures)
  end subroutine

  ! Westbound across 0/360: station 0.5E, ship at 359.9 has passed it
  subroutine test_past_station_west_across_zero(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_west_across_zero', 0, 4, 270.0, 9.0, &
                    0.0, 359.9, 0.0, 0.5, .true., .true., failures)
  end subroutine

  ! Lon plan westbound but ship heading east (circling): no trigger
  subroutine test_past_station_wrong_heading_lon(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_wrong_heading_lon', 0, 4, 90.0, 9.0, &
                    33.0, 200.0, 33.0, 200.1, .true., .false., failures)
  end subroutine

  ! Lat plan northbound but ship heading south: no trigger
  subroutine test_past_station_wrong_heading_lat(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_wrong_heading_lat', 1, 1, 180.0, 9.0, &
                    33.70, 241.82, 33.69, 241.82, .true., .false., failures)
  end subroutine

  subroutine test_past_station_over_max_speed(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_over_max_speed', 1, 1, 350.0, 25.0, &
                    33.70, 241.82, 33.69, 241.82, .true., .false., failures)
  end subroutine

  subroutine test_past_station_bad_plandir(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_bad_plandir', 1, 0, 350.0, 9.0, &
                    33.70, 241.82, 33.69, 241.82, .true., .false., failures)
  end subroutine

  ! Fix 3: geometrically past, but the position source is not trusted
  subroutine test_past_station_untrusted(failures)
    integer, intent(inout) :: failures
    call check_past('test_past_station_untrusted', 1, 1, 350.0, 9.0, &
                    33.70, 241.82, 33.69, 241.82, .false., .false., failures)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! drop_countdown(pastnow, trusted, itime, runsec, idsec2, stoptime, fire, event)
  ! Called once per sioloop call. event: 0 none, 1 armed, 2 cancelled (trust
  ! lost), 3 cancelled (not past when it ran out), 4 fire.
  ! ---------------------------------------------------------------------------

  subroutine test_countdown_zero_delay_fires_at_once(failures)
    ! runsec=0 (every deployment today): drop on the first trusted past fix,
    ! exactly as before
    integer, intent(inout) :: failures
    integer :: idsec2, event
    real    :: stoptime
    logical :: fire
    idsec2 = 0
    stoptime = 9.9e9
    call drop_countdown(.true., .true., 100, 0.0, idsec2, stoptime, fire, event)
    call report('test_countdown_zero_delay_fires_at_once', &
         fire .and. idsec2 == 1 .and. event == 4, failures)
  end subroutine

  subroutine test_countdown_idle(failures)
    integer, intent(inout) :: failures
    integer :: idsec2, event
    real    :: stoptime
    logical :: fire
    idsec2 = 0
    stoptime = 9.9e9
    call drop_countdown(.false., .true., 100, 10.0, idsec2, stoptime, fire, event)
    call report('test_countdown_idle', &
         .not. fire .and. idsec2 == 0 .and. event == 0, failures)
  end subroutine

  subroutine test_countdown_arms_once_and_fires(failures)
    ! The bug: stoptime = itime + runsec was re-set on every past call, so
    ! with runsec > 0 the countdown never ran out. Past from 100 on, runsec=10:
    ! armed at 100, fires at 110.
    integer, intent(inout) :: failures
    integer :: idsec2, event, it, ifire
    real    :: stoptime
    logical :: fire
    idsec2 = 0
    stoptime = 9.9e9
    ifire = -1
    do it = 100, 120
      call drop_countdown(.true., .true., it, 10.0, idsec2, stoptime, fire, event)
      if (fire .and. ifire < 0) ifire = it
    end do
    if (ifire /= 110) print *, '   first fire at itime', ifire, ' expected 110'
    call report('test_countdown_arms_once_and_fires', ifire == 110, failures)
  end subroutine

  subroutine test_countdown_single_glitch_cannot_drop(failures)
    ! One bad position past the station at 100, then back before it: the
    ! re-check at 110 cancels instead of dropping
    integer, intent(inout) :: failures
    integer :: idsec2, event, it, nfire, ev110
    real    :: stoptime
    logical :: fire
    idsec2 = 0
    stoptime = 9.9e9
    nfire = 0
    ev110 = -1
    do it = 100, 160
      call drop_countdown(it == 100, .true., it, 10.0, idsec2, stoptime, fire, event)
      if (fire) nfire = nfire + 1
      if (it == 110) ev110 = event
    end do
    if (nfire /= 0 .or. ev110 /= 3) print *, '   fires=', nfire, ' event@110=', ev110
    call report('test_countdown_single_glitch_cannot_drop', &
         nfire == 0 .and. ev110 == 3 .and. idsec2 == 0, failures)
  end subroutine

  subroutine test_countdown_rearms_after_glitch(failures)
    ! Glitch at 100 (cancelled at 110), real crossing at 115: fires at 125
    integer, intent(inout) :: failures
    integer :: idsec2, event, it, ifire
    real    :: stoptime
    logical :: fire
    idsec2 = 0
    stoptime = 9.9e9
    ifire = -1
    do it = 100, 140
      call drop_countdown(it == 100 .or. it >= 115, .true., it, 10.0, &
                          idsec2, stoptime, fire, event)
      if (fire .and. ifire < 0) ifire = it
    end do
    if (ifire /= 125) print *, '   first fire at itime', ifire, ' expected 125'
    call report('test_countdown_rearms_after_glitch', ifire == 125, failures)
  end subroutine

  subroutine test_countdown_trust_lost_cancels(failures)
    ! Armed at 100; position unconfirmed at 105 (past_station is false when
    ! untrusted) cancels; trusted and past again from 106: fires at 116
    integer, intent(inout) :: failures
    integer :: idsec2, event, it, ifire, ev105
    real    :: stoptime
    logical :: fire, trusted
    idsec2 = 0
    stoptime = 9.9e9
    ifire = -1
    ev105 = -1
    do it = 100, 130
      trusted = it /= 105
      call drop_countdown(trusted, trusted, it, 10.0, idsec2, stoptime, fire, event)
      if (fire .and. ifire < 0) ifire = it
      if (it == 105) ev105 = event
    end do
    if (ifire /= 116 .or. ev105 /= 2) print *, '   first fire', ifire, ' event@105=', ev105
    call report('test_countdown_trust_lost_cancels', &
         ifire == 116 .and. ev105 == 2, failures)
  end subroutine

  subroutine test_countdown_late_call_fires(failures)
    ! Calls stalled: armed at 100, next call at 130 and still past -> fire
    integer, intent(inout) :: failures
    integer :: idsec2, event
    real    :: stoptime
    logical :: fire
    idsec2 = 0
    stoptime = 9.9e9
    call drop_countdown(.true., .true., 100, 10.0, idsec2, stoptime, fire, event)
    call drop_countdown(.true., .true., 130, 10.0, idsec2, stoptime, fire, event)
    call report('test_countdown_late_call_fires', fire .and. event == 4, failures)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! After siobegin (Seas calls it after every launch) sioloop reloads the last
  ! position from navtrk.dat/.nav and starts dead reckoning at once, with the
  ! GPS time/fix references reset. Scenario: reloaded position already past
  ! the next station (northbound lat plan, station 37.75 N, ship 37.80 N).
  ! ---------------------------------------------------------------------------

  subroutine begin_scenario(postrust, it0, it1, idsec2, stoptime, ifire)
    logical, intent(in)    :: postrust
    integer, intent(in)    :: it0, it1
    integer, intent(inout) :: idsec2
    real,    intent(inout) :: stoptime
    integer, intent(out)   :: ifire
    integer :: it, event
    logical :: fire, pastnow
    ifire = -1
    do it = it0, it1
      pastnow = past_station(1, 1, 0.0, 10.0, 20.0, 37.80, 200.5, 37.75, 200.5, postrust)
      call drop_countdown(pastnow, postrust, it, 5.0, idsec2, stoptime, fire, event)
      if (fire .and. ifire < 0) ifire = it
    end do
  end subroutine

  subroutine test_begin_holds_drop_until_average(failures)
    ! No fresh GPS average yet: the reloaded position must not arm a drop
    integer, intent(inout) :: failures
    integer :: idsec2, ifire
    real    :: stoptime
    idsec2 = 0
    stoptime = 9.9e9
    call begin_scenario(postrust_at_begin, 1, 60, idsec2, stoptime, ifire)
    if (ifire >= 0) print *, '   dropped at itime', ifire, ' before any GPS average'
    call report('test_begin_holds_drop_until_average', ifire < 0, failures)
  end subroutine

  subroutine test_begin_drop_after_confirming_average(failures)
    ! First average (90 s after the reloaded fix, 10 kn north) agrees with
    ! dead reckoning from the reloaded position -> trusted -> drop 5 s later
    integer, intent(inout) :: failures
    integer :: idsec2, ifire
    real    :: stoptime
    logical :: postrust
    idsec2 = 0
    stoptime = 9.9e9
    call begin_scenario(postrust_at_begin, 1, 60, idsec2, stoptime, ifire)
    postrust = ave_consistent(37.80, 200.5, 43200.0, 10.0, 0.0, &
                              37.80 + 1.5 / 60.0 * 10.0 / 60.0, 200.5, 43290.0)
    call begin_scenario(postrust, 61, 80, idsec2, stoptime, ifire)
    if (ifire /= 66) print *, '   trusted=', postrust, ' first fire', ifire, ' expected 66'
    call report('test_begin_drop_after_confirming_average', ifire == 66, failures)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! ave_consistent(vlat_prev, vlon_prev, timeave_prev, speed, dir,
  !                vlat_new, vlon_new, timeave_new)
  !   true when the new GPS average lies within 0.5 nm of the position dead
  !   reckoned from the previous average with the previous speed/dir.
  ! ---------------------------------------------------------------------------

  subroutine check_consistent(name, vlat_new, vlon_new, dt, expected, failures)
    character(len=*), intent(in) :: name
    real,    intent(in)    :: vlat_new, vlon_new, dt
    logical, intent(in)    :: expected
    integer, intent(inout) :: failures
    logical :: got
    ! previous average: 33.6304N 241.8317E at 43170 s, 10 kt due north
    got = ave_consistent(33.6304, 241.8317, 43170.0, 10.0, 0.0, &
                         vlat_new, vlon_new, 43170.0 + dt)
    if (got .neqv. expected) then
      print *, 'FAIL ', name, ': got=', got, ' expected=', expected
      failures = failures + 1
    else
      print *, 'PASS ', name
    end if
  end subroutine

  ! 60 s at 10 kt north = 0.1667 nm = 0.002778 deg lat, exactly as predicted
  subroutine test_ave_consistent_on_track(failures)
    integer, intent(inout) :: failures
    call check_consistent('test_ave_consistent_on_track', 33.6304 + 0.002778, &
                          241.8317, 60.0, .true., failures)
  end subroutine

  ! 0.3 nm off the prediction (course change, current): still consistent
  subroutine test_ave_consistent_small_offset(failures)
    integer, intent(inout) :: failures
    call check_consistent('test_ave_consistent_small_offset', 33.6304 + 0.002778, &
                          241.8317 + 0.3/(60.0*cos(33.63*3.141592654/180.0)), &
                          60.0, .true., failures)
  end subroutine

  ! 2 nm ahead of the prediction: GPS glitch, not trusted
  subroutine test_ave_consistent_jump_2nm(failures)
    integer, intent(inout) :: failures
    call check_consistent('test_ave_consistent_jump_2nm', &
                          33.6304 + 0.002778 + 2.0/60.0, &
                          241.8317, 60.0, .false., failures)
  end subroutine

  ! 9/8/26-size jump (10.4 nm)
  subroutine test_ave_consistent_jump_0908(failures)
    integer, intent(inout) :: failures
    call check_consistent('test_ave_consistent_jump_0908', 33.6304 + 10.43/60.0, &
                          241.8317, 60.0, .false., failures)
  end subroutine

  ! Eastbound across 0/360 on the equator, on track
  subroutine test_ave_consistent_lon_wrap(failures)
    integer, intent(inout) :: failures
    logical :: got
    got = ave_consistent(0.0, 359.999, 1000.0, 10.0, 90.0, &
                         0.0, 0.001778, 1060.0)
    if (.not. got) then
      print *, 'FAIL test_ave_consistent_lon_wrap'
      failures = failures + 1
    else
      print *, 'PASS test_ave_consistent_lon_wrap'
    end if
  end subroutine

  ! Stuck average (same timeave repeated, 9/8 12:06:18) at the same position
  subroutine test_ave_consistent_stuck_same(failures)
    integer, intent(inout) :: failures
    call check_consistent('test_ave_consistent_stuck_same', 33.6304, 241.8317, &
                          0.0, .true., failures)
  end subroutine

  ! Same timeave but position moved 1 nm: inconsistent
  subroutine test_ave_consistent_stuck_moved(failures)
    integer, intent(inout) :: failures
    call check_consistent('test_ave_consistent_stuck_moved', 33.6304 + 1.0/60.0, &
                          241.8317, 0.0, .false., failures)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! check_time(ctag, itime, tref, itref, tcand, itcand, ncand, ok, tgood)
  !   ctag  - incoming GPS time (seconds of day); itime - PC seconds counter
  !   tref,itref - last good GPS time and the itime it arrived at (tref<0: none)
  !   tcand,itcand,ncand - run of self-consistent rejected times (re-sync)
  !   ok    - incoming time agrees with tref + PC elapsed (within 30 s)
  !   tgood - time to use downstream (ctag if ok, else predicted)
  ! ---------------------------------------------------------------------------

  subroutine report(name, cond, failures)
    character(len=*), intent(in) :: name
    logical, intent(in)    :: cond
    integer, intent(inout) :: failures
    if (.not. cond) then
      print *, 'FAIL ', name
      failures = failures + 1
    else
      print *, 'PASS ', name
    end if
  end subroutine

  subroutine test_check_time_no_reference(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand
    logical :: ok
    tref = -1.0; itref = 0; tcand = 0.0; itcand = 0; ncand = 0
    call check_time(43200.0, 100, tref, itref, tcand, itcand, ncand, ok, tgood)
    call report('test_check_time_no_reference', &
                ok .and. tgood == 43200.0 .and. tref == 43200.0 .and. itref == 100, failures)
  end subroutine

  subroutine test_check_time_normal_advance(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand
    logical :: ok
    tref = 43200.0; itref = 100; tcand = 0.0; itcand = 0; ncand = 0
    call check_time(43205.0, 105, tref, itref, tcand, itcand, ncand, ok, tgood)
    call report('test_check_time_normal_advance', &
                ok .and. tgood == 43205.0 .and. tref == 43205.0 .and. itref == 105, failures)
  end subroutine

  ! 9/8/26: 11:39:30 good, then "10:40:00" 30 s later -> use 11:40:00
  subroutine test_check_time_garbled_hour(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand
    logical :: ok
    tref = 41970.0; itref = 1000; tcand = 0.0; itcand = 0; ncand = 0
    call check_time(38400.0, 1030, tref, itref, tcand, itcand, ncand, ok, tgood)
    call report('test_check_time_garbled_hour', &
                (.not. ok) .and. abs(tgood - 42000.0) < 0.01 .and. tref == 41970.0, failures)
  end subroutine

  ! Last Furuno 07:45:47, Garmin re-selected 25 s later sends buffered 07:31:03
  subroutine test_check_time_stale_garmin(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand
    logical :: ok
    tref = 27947.0; itref = 500; tcand = 0.0; itcand = 0; ncand = 0
    call check_time(27063.0, 525, tref, itref, tcand, itcand, ncand, ok, tgood)
    call report('test_check_time_stale_garmin', &
                (.not. ok) .and. abs(tgood - 27972.0) < 0.01, failures)
  end subroutine

  ! 23:59:58 then 00:00:02 four seconds later is fine
  subroutine test_check_time_midnight(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand
    logical :: ok
    tref = 86398.0; itref = 10; tcand = 0.0; itcand = 0; ncand = 0
    call check_time(2.0, 14, tref, itref, tcand, itcand, ncand, ok, tgood)
    call report('test_check_time_midnight', ok .and. tgood == 2.0, failures)
  end subroutine

  ! A repeated stale time (Garmin buffer, 9/8 09:27-09:32) must never become
  ! the reference, however long it repeats
  subroutine test_check_time_frozen_never_adopted(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand, k
    logical :: ok, anyok
    tref = 34000.0; itref = 0; tcand = 0.0; itcand = 0; ncand = 0
    anyok = .false.
    do k = 60, 360
      call check_time(33388.0, k, tref, itref, tcand, itcand, ncand, ok, tgood)
      if (ok) anyok = .true.
    end do
    call report('test_check_time_frozen_never_adopted', &
                (.not. anyok) .and. abs(tgood - 34360.0) < 0.01, failures)
  end subroutine

  ! PC clock stepped 120 s: 5 consecutive self-consistent GPS times re-sync
  subroutine test_check_time_genuine_step_adopted(failures)
    integer, intent(inout) :: failures
    real :: tref, tcand, tgood
    integer :: itref, itcand, ncand, k
    logical :: ok, early
    tref = 50000.0; itref = 0; tcand = 0.0; itcand = 0; ncand = 0
    early = .false.
    do k = 1, 4
      call check_time(50000.0 + 120.0 + real(k), k, tref, itref, tcand, itcand, &
                      ncand, ok, tgood)
      if (ok) early = .true.
    end do
    call check_time(50125.0, 5, tref, itref, tcand, itcand, ncand, ok, tgood)
    call report('test_check_time_genuine_step_adopted', &
                (.not. early) .and. ok .and. tref == 50125.0 .and. tgood == 50125.0, failures)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! check_fix(clat, clon, itime, xmaxspd, alat, alon, ita, clatc, clonc, itc,
  !           nc, ok)
  !   clat,clon - incoming fix (decimal deg, lon 0-360 E); itime - PC seconds
  !   alat,alon,ita - last accepted fix (ita<0: none)
  !   clatc,clonc,itc,nc - run of self-consistent rejected fixes (re-sync)
  !   ok - fix is in range and within xmaxspd*elapsed + 0.1 nm of the last one
  ! ---------------------------------------------------------------------------

  subroutine test_check_fix_no_reference(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok
    alat = 0.0; alon = 0.0; ita = -1; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(33.63, 241.83, 100, 20.0, alat, alon, ita, clatc, clonc, itc, nc, ok)
    call report('test_check_fix_no_reference', &
                ok .and. alat == 33.63 .and. alon == 241.83 .and. ita == 100, failures)
  end subroutine

  ! 1 s at 10 kt north
  subroutine test_check_fix_normal(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok
    alat = 33.63; alon = 241.83; ita = 100; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(33.63 + 10.0/3600.0/60.0, 241.83, 101, 20.0, alat, alon, ita, &
                   clatc, clonc, itc, nc, ok)
    call report('test_check_fix_normal', ok .and. ita == 101, failures)
  end subroutine

  ! Seas sends S for any latitude cardinal that is not exactly "N", so a
  ! partial string arrives as 33.63 S
  subroutine test_check_fix_hemisphere_flip(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok
    alat = 33.63; alon = 241.83; ita = 100; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(-33.63, 241.83, 101, 20.0, alat, alon, ita, clatc, clonc, itc, nc, ok)
    call report('test_check_fix_hemisphere_flip', &
                (.not. ok) .and. alat == 33.63 .and. ita == 100, failures)
  end subroutine

  ! Empty fields parse to 0,0 (lon 0 or 360): never a fix, even with no reference
  subroutine test_check_fix_null_island(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok1, ok2
    alat = 0.0; alon = 0.0; ita = -1; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(0.0, 0.0, 100, 20.0, alat, alon, ita, clatc, clonc, itc, nc, ok1)
    call check_fix(0.0, 360.0, 101, 20.0, alat, alon, ita, clatc, clonc, itc, nc, ok2)
    call report('test_check_fix_null_island', &
                (.not. ok1) .and. (.not. ok2) .and. ita == -1, failures)
  end subroutine

  ! Latitude field empty (0) but longitude parsed
  subroutine test_check_fix_zero_lat(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok
    alat = 33.63; alon = 241.83; ita = 100; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(0.0, 241.83, 101, 20.0, alat, alon, ita, clatc, clonc, itc, nc, ok)
    call report('test_check_fix_zero_lat', .not. ok, failures)
  end subroutine

  ! One bad fix between good ones: bad rejected, the next good one accepted
  subroutine test_check_fix_isolated_jump(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok1, ok2
    alat = 33.63; alon = 241.83; ita = 100; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(33.80, 241.83, 101, 20.0, alat, alon, ita, clatc, clonc, itc, nc, ok1)
    call check_fix(33.63 + 2.0*10.0/3600.0/60.0, 241.83, 102, 20.0, alat, alon, ita, &
                   clatc, clonc, itc, nc, ok2)
    call report('test_check_fix_isolated_jump', (.not. ok1) .and. ok2 .and. ita == 102, &
                failures)
  end subroutine

  ! Reference itself was bad: 5 consecutive consistent real fixes re-sync
  subroutine test_check_fix_bad_reference_recovers(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc, k
    logical :: ok, early
    alat = -33.63; alon = 241.83; ita = 100; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    early = .false.
    do k = 1, 4
      call check_fix(33.63 + real(k)*10.0/3600.0/60.0, 241.83, 100 + k, 20.0, &
                     alat, alon, ita, clatc, clonc, itc, nc, ok)
      if (ok) early = .true.
    end do
    call check_fix(33.63 + 5.0*10.0/3600.0/60.0, 241.83, 105, 20.0, &
                   alat, alon, ita, clatc, clonc, itc, nc, ok)
    call report('test_check_fix_bad_reference_recovers', &
                (.not. early) .and. ok .and. alat > 0.0 .and. ita == 105, failures)
  end subroutine

  ! GPS lost 20 min, ship made 3 nm at 9 kt: accepted
  subroutine test_check_fix_after_outage(failures)
    integer, intent(inout) :: failures
    real :: alat, alon, clatc, clonc
    integer :: ita, itc, nc
    logical :: ok
    alat = 33.63; alon = 241.83; ita = 100; clatc = 0.0; clonc = 0.0; itc = 0; nc = 0
    call check_fix(33.63 + 3.0/60.0, 241.83, 1300, 20.0, alat, alon, ita, &
                   clatc, clonc, itc, nc, ok)
    call report('test_check_fix_after_outage', ok, failures)
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

  subroutine test_chkbuf_single_point_rejected(failures)
    ! Fewer than 3 points cannot form a median that outvotes a bad one → ierr=1
    ! (was Bug #3 under the last-entry reference; now intentional)
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 1
    clatbuf(1) = 30.0; clonbuf(1) = 200.0; ctagbuf(1) = 100.0
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    if (ierr /= 1) then
      print *, 'FAIL test_chkbuf_single_point_rejected: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkbuf_single_point_rejected'
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
    ! ctagbuf(1) >400 s from the median time → marked bad, packed out → ibuf=2
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=1500.0; clatbuf(1)=30.05; clonbuf(1)=200.05  ! bad: 1200 s from median 300
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
    ! No two timetags within 400 s: 2 of 3 flagged vs the median → no majority → ierr=1
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 3
    ctagbuf(1)=100.0;  clatbuf(1)=30.00; clonbuf(1)=200.00  ! bad: 1400 s from 1500
    ctagbuf(2)=1500.0; clatbuf(2)=30.05; clonbuf(2)=200.05
    ctagbuf(3)=3000.0; clatbuf(3)=30.10; clonbuf(3)=200.10  ! bad: 1500 s from 1500
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

  ! n good 1 Hz fixes from (t0, lat0, lon0), steaming NE at ~1 kn-ish
  subroutine fill_good(n, t0, lat0, lon0, ctagbuf, clatbuf, clonbuf)
    integer, intent(in) :: n
    real,    intent(in) :: t0, lat0, lon0
    real,    intent(inout) :: ctagbuf(200), clatbuf(200), clonbuf(200)
    integer :: i
    do i = 1, n
      ctagbuf(i) = t0 + real(i - 1)
      clatbuf(i) = lat0 + 0.0001 * real(i - 1)
      clonbuf(i) = modulo(lon0 + 0.0001 * real(i - 1), 360.0)
    end do
  end subroutine

  subroutine test_chkbuf_stale_last_entry_keeps_good(failures)
    ! Last entry is a stale Garmin sentence (16 min old). The last-entry
    ! reference flagged all 9 good fixes and threw the minute away.
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    call fill_good(9, 43000.0, 30.0, 200.0, ctagbuf, clatbuf, clonbuf)
    ibuf = 10
    ctagbuf(10) = 43000.0 - 960.0; clatbuf(10) = clatbuf(9); clonbuf(10) = clonbuf(9)
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_stale_last_entry_keeps_good', &
         ierr == 0 .and. ibuf == 9 .and. ctagbuf(9) == 43008.0, failures)
  end subroutine

  subroutine test_chkbuf_flipped_last_entry_keeps_good(failures)
    ! Last entry has its hemisphere flipped (Seas: cardinal not exactly "N")
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    call fill_good(9, 43000.0, 30.0, 200.0, ctagbuf, clatbuf, clonbuf)
    ibuf = 10
    ctagbuf(10) = 43009.0; clatbuf(10) = -clatbuf(9); clonbuf(10) = clonbuf(9)
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_flipped_last_entry_keeps_good', &
         ierr == 0 .and. ibuf == 9 .and. all(clatbuf(1:9) > 0.0), failures)
  end subroutine

  subroutine test_chkbuf_stale_block_at_end_packed_out(failures)
    ! 7 good fixes, then 3 stale ones. The last-entry reference kept the 3
    ! stale fixes and packed out the 7 good ones, so ave fitted stale data.
    integer, intent(inout) :: failures
    integer :: ibuf, ierr, i
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    call fill_good(7, 43000.0, 30.0, 200.0, ctagbuf, clatbuf, clonbuf)
    ibuf = 10
    do i = 8, 10
      ctagbuf(i) = 43000.0 - 960.0 + real(i - 8)
      clatbuf(i) = 29.99; clonbuf(i) = 199.99
    end do
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_stale_block_at_end_packed_out', &
         ierr == 0 .and. ibuf == 7 .and. all(ctagbuf(1:7) >= 43000.0), failures)
  end subroutine

  subroutine test_chkbuf_greenwich_crossing_kept(failures)
    ! Buffer straddles lon 0/360: all 5 fixes are within 0.04 deg of each other
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 5
    ctagbuf(1:5) = (/ 100.0, 101.0, 102.0, 103.0, 104.0 /)
    clatbuf(1:5) = 50.0
    clonbuf(1:5) = (/ 359.98, 359.99, 0.00, 0.01, 0.02 /)
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_greenwich_crossing_kept', &
         ierr == 0 .and. ibuf == 5, failures)
  end subroutine

  subroutine test_chkbuf_flipped_lon_across_dateline(failures)
    ! First entry has E/W flipped (20 vs 200): exactly 180 deg from the good
    ! fixes, which sit either side of it. Only the flipped entry is dropped.
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 6
    ctagbuf(1:6) = (/ 100.0, 101.0, 102.0, 103.0, 104.0, 105.0 /)
    clatbuf(1:6) = 30.0
    clonbuf(1:6) = (/ 20.0, 200.01, 199.99, 200.02, 199.98, 200.00 /)
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_flipped_lon_across_dateline', &
         ierr == 0 .and. ibuf == 5 .and. all(clonbuf(1:5) > 199.0), failures)
  end subroutine

  subroutine test_chkbuf_no_majority_rejected(failures)
    ! Two equal clusters 5 deg apart: no majority to trust → whole minute rejected
    integer, intent(inout) :: failures
    integer :: ibuf, ierr
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    ibuf = 6
    ctagbuf(1:6) = (/ 100.0, 101.0, 102.0, 103.0, 104.0, 105.0 /)
    clatbuf(1:6) = (/ 30.0, 35.0, 30.0, 35.0, 30.0, 35.0 /)
    clonbuf(1:6) = 200.0
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_no_majority_rejected', ierr == 1, failures)
  end subroutine

  subroutine test_chkbuf_midnight_keeps_majority_side(failures)
    ! Buffer spans GPS midnight: 30 fixes before, 6 after. The pre-midnight
    ! majority is kept (ave cannot fit across the 86400 -> 0 wrap); the last-
    ! entry reference flagged the 30 and lost the whole minute.
    integer, intent(inout) :: failures
    integer :: ibuf, ierr, i
    real :: clatbuf(200), clonbuf(200), ctagbuf(200)
    call fill_good(36, 86370.0, 30.0, 200.0, ctagbuf, clatbuf, clonbuf)
    do i = 31, 36
      ctagbuf(i) = ctagbuf(i) - 86400.0
    end do
    ibuf = 36
    call chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, 0, 0)
    call report('test_chkbuf_midnight_keeps_majority_side', &
         ierr == 0 .and. ibuf == 30 .and. ctagbuf(30) == 86399.0, failures)
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
