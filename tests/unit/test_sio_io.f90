! tests/unit/test_sio_io.f90
program test_sio_io
  use sio_io
  implicit none
  integer :: failures = 0

  call test_decodeplan_valid_lat(failures)
  call test_decodeplan_valid_lon(failures)
  call test_decodeplan_malformed(failures)
  call test_decodeplan_lowercase_n(failures)
  call test_decodeplan_lowercase_w(failures)
  call test_decodeplan_blank_line(failures)
  call test_decodeplan_invalid_hemisphere(failures)
  call test_getfilen_constructs_name(failures)
  call test_getfilen_with_adir(failures)
  call test_rdcntrl_new_format(failures)
  call test_rdcntrl_debug_operator(failures)
  call test_rdcntrl_malformed(failures)
  call test_rdcntrl_old_format(failures)
  call test_rdcntrl_missing_file(failures)
  call test_navopen_missing_file(failures)
  call test_navopen_valid_file(failures)
  call test_navopen_iday_gt9(failures)
  call test_navopen_imo_gt9(failures)
  call test_navopen_iyr_le9_empty(failures)
  call test_navopen_read_error(failures)
  call test_chknav_many_bad_lines(failures)
  call test_chknav_validation_errors(failures)
  call test_chknav_clean(failures)
  call test_getdir_success(failures)
  call test_decodeplan_extra_spaces_deg(failures)
  call test_decodeplan_extra_spaces_hemi(failures)
  call test_rdcntrl_new_eq_tmp(failures)
  call test_rdcntrl_old_trunc(failures)
  call test_rdcntrl_old_eq_tmp(failures)

  if (failures == 0) then
    print *, 'test_sio_io: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_io: FAILURES =', failures
    stop 1
  end if

contains

  subroutine test_decodeplan_valid_lat(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    ! typical plan.dat lat line: "  37 30.0 N"
    call decodeplan('  37 30.0 N', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 0 .or. latd /= 37 .or. abs(xlatm - 30.0) > 0.01 &
        .or. ahem /= 'N' .or. ispec1 /= 1) then
      print *, 'FAIL test_decodeplan_valid_lat: ierrplan=', ierrplan, &
               ' latd=', latd, ' xlatm=', xlatm, ' ahem=', ahem, ' ispec1=', ispec1
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_valid_lat'
    end if
  end subroutine

  subroutine test_decodeplan_valid_lon(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    ! typical plan.dat lon line: " 122 15.5 W"
    call decodeplan(' 122 15.5 W', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 0 .or. latd /= 122 .or. abs(xlatm - 15.5) > 0.01 &
        .or. ahem /= 'W' .or. ispec1 /= 0) then
      print *, 'FAIL test_decodeplan_valid_lon: ierrplan=', ierrplan, &
               ' latd=', latd, ' xlatm=', xlatm, ' ahem=', ahem, ' ispec1=', ispec1
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_valid_lon'
    end if
  end subroutine

  subroutine test_decodeplan_malformed(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    ! garbage line — expect ierrplan=1
    call decodeplan('   GARBAGE LINE HERE   ', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 1) then
      print *, 'FAIL test_decodeplan_malformed: ierrplan =', ierrplan, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_malformed'
    end if
  end subroutine

  ! Lowercase 'n': same as 'N' → ispec1=1, ierrplan=0
  subroutine test_decodeplan_lowercase_n(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    call decodeplan('  37 30.0 n', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 0 .or. ispec1 /= 1 .or. ahem /= 'n') then
      print *, 'FAIL test_decodeplan_lowercase_n: ierrplan=', ierrplan, &
               ' ispec1=', ispec1, ' ahem=', ahem
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_lowercase_n'
    end if
  end subroutine

  ! Lowercase 'w': same as 'W' → ispec1=0, ierrplan=0
  subroutine test_decodeplan_lowercase_w(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    call decodeplan(' 122 15.5 w', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 0 .or. ispec1 /= 0 .or. ahem /= 'w') then
      print *, 'FAIL test_decodeplan_lowercase_w: ierrplan=', ierrplan, &
               ' ispec1=', ispec1, ' ahem=', ahem
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_lowercase_w'
    end if
  end subroutine

  ! All-blank line: loop exhausts without finding degrees → ierrplan=1
  subroutine test_decodeplan_blank_line(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    call decodeplan('           ', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 1) then
      print *, 'FAIL test_decodeplan_blank_line: ierrplan=', ierrplan, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_blank_line'
    end if
  end subroutine

  ! Invalid hemisphere '?': falls through to else → return without setting ierrplan=0
  subroutine test_decodeplan_invalid_hemisphere(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    call decodeplan('  37 30.0 ?', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 1) then
      print *, 'FAIL test_decodeplan_invalid_hemisphere: ierrplan=', ierrplan, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_invalid_hemisphere'
    end if
  end subroutine

  subroutine test_getfilen_constructs_name(failures)
    integer, intent(inout) :: failures
    character(len=80) :: afilen
    ! day=01 mon=06 year=2024 -> should contain "01" and "06"
    call getfilen(afilen, '01', '06', '2024', 0, '                                                                                ')
    if (index(afilen, '01') == 0 .or. index(afilen, '06') == 0) then
      print *, 'FAIL test_getfilen_constructs_name: afilen="', trim(afilen), '"'
      failures = failures + 1
    else
      print *, 'PASS test_getfilen_constructs_name: afilen="', trim(afilen), '"'
    end if
  end subroutine

  ! getfilen with non-zero len_adir: adir prefix is prepended to filename
  subroutine test_getfilen_with_adir(failures)
    integer, intent(inout) :: failures
    character(len=80) :: afilen, adir
    adir = 'mydir/'
    call getfilen(afilen, '15', '12', '2023', 6, adir)
    ! expect result contains 'mydir/' and '15' and '12'
    if (index(afilen, 'mydir/') == 0 .or. index(afilen, '15') == 0 &
        .or. index(afilen, '12') == 0) then
      print *, 'FAIL test_getfilen_with_adir: afilen="', trim(afilen), '"'
      failures = failures + 1
    else
      print *, 'PASS test_getfilen_with_adir: afilen="', trim(afilen), '"'
    end if
  end subroutine

  ! rdcntrl: no file present (len_adir=0 → opens 'Data\control.dat') → ierror(15)=1
  subroutine test_rdcntrl_missing_file(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = ' '
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 0, adir, 0, 6)
    if (ierror(15) /= 1) then
      print *, 'FAIL test_rdcntrl_missing_file: ierror(15)=', ierror(15), ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_missing_file'
    end if
  end subroutine

  ! navopen: file does not exist → ierr=1
  subroutine test_navopen_missing_file(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    adir = 'tests/data/'
    call navopen(2, 1, 25, ierr, fnav, adir, 11)
    if (ierr /= 1) then
      print *, 'FAIL test_navopen_missing_file: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_navopen_missing_file'
    end if
  end subroutine

  ! navopen: tests/data/Data/010624.nav exists → ierr=0, fnav contains filename
  subroutine test_navopen_valid_file(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    adir = 'tests/data/'
    call navopen(1, 6, 24, ierr, fnav, adir, 11)
    if (ierr /= 0 .or. index(fnav, '010624') == 0) then
      print *, 'FAIL test_navopen_valid_file: ierr=', ierr, ' fnav="', trim(fnav), '"'
      failures = failures + 1
    else
      print *, 'PASS test_navopen_valid_file: fnav="', trim(fnav), '"'
      close(8)
    end if
  end subroutine

  ! rdcntrl: valid new-format control.dat → no errors, xmaxspd=20.0
  subroutine test_rdcntrl_new_format(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 11, adir, 0, 6)
    if (ierror(15) /= 0 .or. ierror(16) /= 0 .or. ierror(33) /= 0 &
        .or. abs(xmaxspd - 20.0) > 0.01) then
      print *, 'FAIL test_rdcntrl_new_format: ierror(15)=', ierror(15), &
               ' ierror(16)=', ierror(16), ' ierror(33)=', ierror(33), &
               ' xmaxspd=', xmaxspd
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_new_format'
    end if
  end subroutine

  ! rdcntrl: debug operator name → ierror(33)=1
  subroutine test_rdcntrl_debug_operator(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data_debug/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 17, adir, 0, 6)
    if (ierror(33) /= 1) then
      print *, 'FAIL test_rdcntrl_debug_operator: ierror(33)=', ierror(33), ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_debug_operator'
    end if
  end subroutine

  ! rdcntrl: one-line control.dat (EOF on 2nd line) → ierror(16)=1
  subroutine test_rdcntrl_malformed(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data_malformed_ctrl/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 26, adir, 0, 6)
    if (ierror(16) /= 1) then
      print *, 'FAIL test_rdcntrl_malformed: ierror(16)=', ierror(16), ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_malformed'
    end if
  end subroutine

  ! rdcntrl: old-format control.dat → xmaxspd=30.0, acruise='OLDFMT '
  subroutine test_rdcntrl_old_format(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data_old_format_ctrl/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 27, adir, 0, 6)
    if (abs(xmaxspd - 30.0) > 0.01 .or. acruise(1:6) /= 'OLDFMT') then
      print *, 'FAIL test_rdcntrl_old_format: xmaxspd=', xmaxspd, &
               ' acruise="', acruise, '"'
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_old_format'
    end if
  end subroutine

  ! navopen: iday > 9 encodes as 2-digit → covers line 100 in sio_io.f90
  subroutine test_navopen_iday_gt9(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    adir = 'tests/data/'
    ! File 150624.nav does not exist; ierr=1 but line 100 is exercised
    call navopen(15, 6, 24, ierr, fnav, adir, 11)
    if (ierr /= 1) then
      print *, 'FAIL test_navopen_iday_gt9: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_navopen_iday_gt9'
    end if
  end subroutine

  ! navopen: imo > 9 encodes as 2-digit → covers line 107 in sio_io.f90
  subroutine test_navopen_imo_gt9(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    adir = 'tests/data/'
    ! File 011124.nav does not exist; ierr=1 but line 107 is exercised
    call navopen(1, 11, 24, ierr, fnav, adir, 11)
    if (ierr /= 1) then
      print *, 'FAIL test_navopen_imo_gt9: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_navopen_imo_gt9'
    end if
  end subroutine

  ! navopen: iyr <= 9 encodes as 1-digit AND file is empty → covers lines 116, 136-138
  subroutine test_navopen_iyr_le9_empty(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    adir = 'tests/data/'
    ! tests/data/Data/010100.nav is an empty file (0 bytes); read gets EOF → ierr=2
    call navopen(1, 1, 0, ierr, fnav, adir, 11)
    if (ierr /= 2) then
      print *, 'FAIL test_navopen_iyr_le9_empty: ierr=', ierr, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_navopen_iyr_le9_empty'
    end if
  end subroutine

  ! navopen: file has unreadable content → read error → ierr=3; covers lines 141-143
  subroutine test_navopen_read_error(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    adir = 'tests/data/'
    ! tests/data/Data/010224.nav contains "BADDATA"; format-directed read fails → ierr=3
    call navopen(1, 2, 24, ierr, fnav, adir, 11)
    if (ierr /= 3) then
      print *, 'FAIL test_navopen_read_error: ierr=', ierr, ' expected 3'
      failures = failures + 1
    else
      print *, 'PASS test_navopen_read_error'
    end if
  end subroutine

  ! chknav: fixture with >4 bad lines → ierr=1 (no rewrite); covers lines 185-338
  subroutine test_chknav_many_bad_lines(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    integer :: len_adir
    adir = 'tests/data_chknav/'
    len_adir = 18
    ! Open the nav file via navopen (uses Data\ subpath)
    call navopen(1, 6, 24, ierr, fnav, adir, len_adir)
    if (ierr /= 0) then
      print *, 'FAIL test_chknav_many_bad_lines (navopen failed): ierr=', ierr, &
               ' fnav="', trim(fnav), '"'
      failures = failures + 1
      return
    end if
    ! chknav reads: 1 good, 1 out-of-order, 1 parse-error, 3 blank → ibadcntr=5 > 4
    ! → sets ierr=1 and returns without rewriting (non-destructive)
    call chknav(1, 6, 24, ierr, fnav, len_adir, adir, 0, 0)
    if (ierr /= 1) then
      print *, 'FAIL test_chknav_many_bad_lines: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chknav_many_bad_lines'
    end if
  end subroutine

  ! chknav: fixture with validation errors (ibad 5,6,7,8) → ierr=1
  subroutine test_chknav_validation_errors(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    integer :: len_adir
    adir = 'tests/data_chknav/'
    len_adir = 18
    ! iday=11 → cache key 'Data\1', distinct from iday=1 key 'Data\0'
    call navopen(11, 6, 24, ierr, fnav, adir, len_adir)
    if (ierr /= 0) then
      print *, 'FAIL test_chknav_validation_errors (navopen): ierr=', ierr
      failures = failures + 1
      return
    end if
    call chknav(11, 6, 24, ierr, fnav, len_adir, adir, 0, 0)
    if (ierr /= 1) then
      print *, 'FAIL test_chknav_validation_errors: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chknav_validation_errors'
    end if
  end subroutine

  ! chknav: all-clean fixture → ibadcntr==0 → rewind path → ierr=0
  subroutine test_chknav_clean(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir
    integer :: len_adir
    adir = 'tests/data_chknav/'
    len_adir = 18
    ! iday=21 → cache key 'Data\2', distinct from iday=1 and iday=11
    call navopen(21, 6, 24, ierr, fnav, adir, len_adir)
    if (ierr /= 0) then
      print *, 'FAIL test_chknav_clean (navopen): ierr=', ierr
      failures = failures + 1
      return
    end if
    call chknav(21, 6, 24, ierr, fnav, len_adir, adir, 0, 0)
    if (ierr /= 0) then
      print *, 'FAIL test_chknav_clean: ierr=', ierr, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_chknav_clean'
      close(8)
    end if
  end subroutine

  ! getdir: c:\Users\Public\Documents\siodir.txt exists → success
  subroutine test_getdir_success(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), igderr(3), len_adir
    character(len=80) :: adir
    ierror = 0; igderr = 0
    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) /= 0 .or. ierror(17) /= 0 .or. len_adir <= 0) then
      print *, 'FAIL test_getdir_success: ierror(7)=', ierror(7), &
               ' ierror(17)=', ierror(17), ' len_adir=', len_adir
      failures = failures + 1
    else
      print *, 'PASS test_getdir_success: len_adir=', len_adir
    end if
  end subroutine

  ! decodeplan: extra space between degrees and minutes → cycle at line 432
  subroutine test_decodeplan_extra_spaces_deg(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    call decodeplan('  37  30.0 N', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 0 .or. latd /= 37 .or. abs(xlatm - 30.0) > 0.01 &
        .or. ahem /= 'N' .or. ispec1 /= 1) then
      print *, 'FAIL test_decodeplan_extra_spaces_deg: ierrplan=', ierrplan, &
               ' latd=', latd, ' xlatm=', xlatm
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_extra_spaces_deg'
    end if
  end subroutine

  ! decodeplan: extra space before hemisphere → cycle at line 448
  subroutine test_decodeplan_extra_spaces_hemi(failures)
    integer, intent(inout) :: failures
    integer :: latd, ierrplan, ispec1
    real    :: xlatm
    character(len=1) :: ahem
    call decodeplan('  37 30.0  N', latd, xlatm, ahem, ierrplan, ispec1)
    if (ierrplan /= 0 .or. latd /= 37 .or. abs(xlatm - 30.0) > 0.01 &
        .or. ahem /= 'N' .or. ispec1 /= 1) then
      print *, 'FAIL test_decodeplan_extra_spaces_hemi: ierrplan=', ierrplan, &
               ' latd=', latd, ' xlatm=', xlatm
      failures = failures + 1
    else
      print *, 'PASS test_decodeplan_extra_spaces_hemi'
    end if
  end subroutine

  ! rdcntrl: new format with equal plot temps → reset to defaults (lines 744-747)
  subroutine test_rdcntrl_new_eq_tmp(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data_new_eq_tmp/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 22, adir, 0, 6)
    if (ierror(15) /= 0 .or. ierror(16) /= 0 .or. &
        abs(tm_pl_mn) > 0.01 .or. abs(tm_pl_mx - 30.0) > 0.01) then
      print *, 'FAIL test_rdcntrl_new_eq_tmp: ierror(15)=', ierror(15), &
               ' tm_pl_mn=', tm_pl_mn, ' tm_pl_mx=', tm_pl_mx
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_new_eq_tmp'
    end if
  end subroutine

  ! rdcntrl: 3-line old format → all reads after line 3 fail → error-path defaults
  subroutine test_rdcntrl_old_trunc(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data_old_3line/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 21, adir, 0, 6)
    if (ierror(15) /= 0) then
      print *, 'FAIL test_rdcntrl_old_trunc: ierror(15)=', ierror(15), ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_old_trunc'
    end if
  end subroutine

  ! rdcntrl: old format with "1 5.0 5.0" → tm_pl_mn==tm_pl_mx → reset (lines 784-787)
  subroutine test_rdcntrl_old_eq_tmp(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7) :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, tm_pl_mx, tm_pl_mn
    character(len=80) :: adir
    ierror = 0
    adir = 'tests/data_old_eq_tmp/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 22, adir, 0, 6)
    if (ierror(15) /= 0 .or. abs(tm_pl_mn) > 0.01 .or. abs(tm_pl_mx - 30.0) > 0.01) then
      print *, 'FAIL test_rdcntrl_old_eq_tmp: ierror(15)=', ierror(15), &
               ' tm_pl_mn=', tm_pl_mn, ' tm_pl_mx=', tm_pl_mx
      failures = failures + 1
    else
      print *, 'PASS test_rdcntrl_old_eq_tmp'
    end if
  end subroutine

end program test_sio_io
