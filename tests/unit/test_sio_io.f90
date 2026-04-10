! tests/unit/test_sio_io.f90
program test_sio_io
  use sio_io
  implicit none
  integer :: failures = 0

  call test_decodeplan_valid_lat(failures)
  call test_decodeplan_valid_lon(failures)
  call test_decodeplan_malformed(failures)
  call test_getfilen_constructs_name(failures)

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

end program test_sio_io
