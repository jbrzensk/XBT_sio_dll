program test_sio_convert
  use sio_convert
  implicit none
  integer :: failures = 0

  call test_dec2deg_lat_north(failures)
  call test_dec2deg_lat_south(failures)
  call test_dec2deg_lon_east(failures)
  call test_dec2deg_lon_west(failures)
  call test_deg2dec_roundtrip_lat(failures)
  call test_deg2dec_roundtrip_lon(failures)
  call test_int2ch_positive(failures)
  call test_int2ch_negative(failures)
  call test_int2ch_zero(failures)
  call test_ch2real_decimal(failures)
  call test_ch2real_integer(failures)
  call test_lev_debug(failures)
  call test_lev_normal(failures)
  call test_findspace_advances_i(failures)
  call test_real2ch_decimal(failures)
  call test_real2ch_integer(failures)

  if (failures == 0) then
    print *, 'test_sio_convert: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_convert: FAILURES =', failures
    stop 1
  end if

contains

  subroutine test_dec2deg_lat_north(failures)
    integer, intent(inout) :: failures
    integer :: ideg
    real    :: xmin
    character(len=1) :: ahem
    ! 37.5 degrees N = 37 deg 30.0 min N
    call dec2deg('lat', ideg, xmin, ahem, 37.5)
    if (ideg /= 37 .or. abs(xmin - 30.0) > 0.001 .or. ahem /= 'N') then
      print *, 'FAIL test_dec2deg_lat_north: ideg=', ideg, ' xmin=', xmin, ' ahem=', ahem
      failures = failures + 1
    else
      print *, 'PASS test_dec2deg_lat_north'
    end if
  end subroutine

  subroutine test_dec2deg_lat_south(failures)
    integer, intent(inout) :: failures
    integer :: ideg
    real    :: xmin
    character(len=1) :: ahem
    ! -20.25 = 20 deg 15.0 min S
    call dec2deg('lat', ideg, xmin, ahem, -20.25)
    if (ideg /= 20 .or. abs(xmin - 15.0) > 0.001 .or. ahem /= 'S') then
      print *, 'FAIL test_dec2deg_lat_south: ideg=', ideg, ' xmin=', xmin, ' ahem=', ahem
      failures = failures + 1
    else
      print *, 'PASS test_dec2deg_lat_south'
    end if
  end subroutine

  subroutine test_dec2deg_lon_east(failures)
    integer, intent(inout) :: failures
    integer :: ideg
    real    :: xmin
    character(len=1) :: ahem
    ! 120.5 (0-360 E convention) = 120 deg 30 min E
    call dec2deg('lon', ideg, xmin, ahem, 120.5)
    if (ideg /= 120 .or. abs(xmin - 30.0) > 0.001 .or. ahem /= 'E') then
      print *, 'FAIL test_dec2deg_lon_east: ideg=', ideg, ' xmin=', xmin, ' ahem=', ahem
      failures = failures + 1
    else
      print *, 'PASS test_dec2deg_lon_east'
    end if
  end subroutine

  subroutine test_dec2deg_lon_west(failures)
    integer, intent(inout) :: failures
    integer :: ideg
    real    :: xmin
    character(len=1) :: ahem
    ! 240.0 in 0-360 convention = 360-240=120 deg W
    call dec2deg('lon', ideg, xmin, ahem, 240.0)
    if (ideg /= 120 .or. abs(xmin) > 0.001 .or. ahem /= 'W') then
      print *, 'FAIL test_dec2deg_lon_west: ideg=', ideg, ' xmin=', xmin, ' ahem=', ahem
      failures = failures + 1
    else
      print *, 'PASS test_dec2deg_lon_west'
    end if
  end subroutine

  subroutine test_deg2dec_roundtrip_lat(failures)
    integer, intent(inout) :: failures
    integer :: ideg
    real    :: xmin, x_out
    character(len=1) :: ahem
    call dec2deg('lat', ideg, xmin, ahem, 37.5)
    call deg2dec(ideg, xmin, ahem, x_out)
    if (abs(x_out - 37.5) > 0.001) then
      print *, 'FAIL test_deg2dec_roundtrip_lat: x_out =', x_out, ' expected 37.5'
      failures = failures + 1
    else
      print *, 'PASS test_deg2dec_roundtrip_lat'
    end if
  end subroutine

  subroutine test_deg2dec_roundtrip_lon(failures)
    integer, intent(inout) :: failures
    integer :: ideg
    real    :: xmin, x_out
    character(len=1) :: ahem
    call dec2deg('lon', ideg, xmin, ahem, 120.5)
    call deg2dec(ideg, xmin, ahem, x_out)
    if (abs(x_out - 120.5) > 0.001) then
      print *, 'FAIL test_deg2dec_roundtrip_lon: x_out =', x_out, ' expected 120.5'
      failures = failures + 1
    else
      print *, 'PASS test_deg2dec_roundtrip_lon'
    end if
  end subroutine

  subroutine test_int2ch_positive(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    integer :: len
    a = '                    '
    call int2ch(42, a, 1, len)
    if (a(1:2) /= '42' .or. len /= 2) then
      print *, 'FAIL test_int2ch_positive: a="', a(1:5), '" len=', len
      failures = failures + 1
    else
      print *, 'PASS test_int2ch_positive'
    end if
  end subroutine

  subroutine test_int2ch_negative(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    integer :: len
    a = '                    '
    call int2ch(-7, a, 1, len)
    if (a(1:2) /= '-7' .or. len /= 2) then
      print *, 'FAIL test_int2ch_negative: a="', a(1:5), '" len=', len
      failures = failures + 1
    else
      print *, 'PASS test_int2ch_negative'
    end if
  end subroutine

  subroutine test_int2ch_zero(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    integer :: len
    a = '                    '
    call int2ch(0, a, 1, len)
    if (a(1:1) /= '0' .or. len /= 1) then
      print *, 'FAIL test_int2ch_zero: a="', a(1:3), '" len=', len
      failures = failures + 1
    else
      print *, 'PASS test_int2ch_zero'
    end if
  end subroutine

  subroutine test_ch2real_decimal(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    real :: x
    a = '  37.500  '
    call ch2real(a, 3, 6, x)
    if (abs(x - 37.5) > 0.001) then
      print *, 'FAIL test_ch2real_decimal: x =', x, ' expected 37.5'
      failures = failures + 1
    else
      print *, 'PASS test_ch2real_decimal'
    end if
  end subroutine

  subroutine test_ch2real_integer(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    real :: x
    a = '  42  '
    call ch2real(a, 3, 2, x)
    if (abs(x - 42.0) > 0.001) then
      print *, 'FAIL test_ch2real_integer: x =', x, ' expected 42.0'
      failures = failures + 1
    else
      print *, 'PASS test_ch2real_integer'
    end if
  end subroutine

  subroutine test_lev_debug(failures)
    integer, intent(inout) :: failures
    integer :: ierrlev
    call lev('debug  ', ierrlev)
    if (ierrlev /= 6) then
      print *, 'FAIL test_lev_debug: ierrlev =', ierrlev, ' expected 6'
      failures = failures + 1
    else
      print *, 'PASS test_lev_debug'
    end if
  end subroutine

  subroutine test_lev_normal(failures)
    integer, intent(inout) :: failures
    integer :: ierrlev
    call lev('johnson', ierrlev)
    if (ierrlev /= 0) then
      print *, 'FAIL test_lev_normal: ierrlev =', ierrlev, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_lev_normal'
    end if
  end subroutine

  ! findspace: 'abc def' starting at i=1 → ic=3 (3 non-space chars), i advances to 4 (space pos)
  subroutine test_findspace_advances_i(failures)
    integer, intent(inout) :: failures
    character(len=10) :: aplan
    integer :: i, ic
    aplan = 'abc def   '
    i = 1
    call findspace(aplan, i, ic)
    if (i /= 4 .or. ic /= 3) then
      print *, 'FAIL test_findspace_advances_i: i=', i, ' ic=', ic, ' expected i=4 ic=3'
      failures = failures + 1
    else
      print *, 'PASS test_findspace_advances_i'
    end if
  end subroutine

  ! real2ch with nrx=2: 3.14 written to string starting at pos 1 → '3.14'
  subroutine test_real2ch_decimal(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    integer :: len
    a = '                    '
    call real2ch(3.14, a, 1, 2, len)
    if (a(1:4) /= '3.14' .or. len /= 4) then
      print *, 'FAIL test_real2ch_decimal: a="', a(1:6), '" len=', len
      failures = failures + 1
    else
      print *, 'PASS test_real2ch_decimal'
    end if
  end subroutine

  ! real2ch with nrx=0: 42.0 → '42' (integer representation, no decimal point)
  subroutine test_real2ch_integer(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    integer :: len
    a = '                    '
    call real2ch(42.0, a, 1, 0, len)
    if (a(1:2) /= '42' .or. len /= 2) then
      print *, 'FAIL test_real2ch_integer: a="', a(1:5), '" len=', len
      failures = failures + 1
    else
      print *, 'PASS test_real2ch_integer'
    end if
  end subroutine

end program test_sio_convert
