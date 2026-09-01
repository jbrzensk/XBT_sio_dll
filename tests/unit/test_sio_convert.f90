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
  call test_ch2real_negative(failures)
  call test_lev_debug(failures)
  call test_lev_normal(failures)
  call test_findspace_advances_i(failures)
  call test_findspace_no_space(failures)
  call test_real2ch_decimal(failures)
  call test_real2ch_integer(failures)
  call test_real2ch_negative(failures)
  call test_deg2dec_lowercase_n(failures)
  call test_deg2dec_lowercase_s(failures)
  call test_deg2dec_lowercase_w(failures)
  call test_deg2dec_unknown_hemisphere(failures)

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

  ! Leading '-' sign triggers neg=1 branch → result is negated
  subroutine test_ch2real_negative(failures)
    integer, intent(inout) :: failures
    character(len=10) :: a
    real :: x
    a = '-37.5     '
    call ch2real(a, 1, 5, x)
    if (abs(x - (-37.5)) > 0.001) then
      print *, 'FAIL test_ch2real_negative: x =', x, ' expected -37.5'
      failures = failures + 1
    else
      print *, 'PASS test_ch2real_negative'
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

  ! findspace with no space in 15 chars: loop runs all 15 iterations → ic=15, i=start+15
  subroutine test_findspace_no_space(failures)
    integer, intent(inout) :: failures
    character(len=20) :: aplan
    integer :: i, ic
    aplan = 'ABCDEFGHIJKLMNO     '   ! 15 non-space chars then spaces
    i = 1
    call findspace(aplan, i, ic)
    if (ic /= 15 .or. i /= 16) then
      print *, 'FAIL test_findspace_no_space: i=', i, ' ic=', ic, ' expected i=16 ic=15'
      failures = failures + 1
    else
      print *, 'PASS test_findspace_no_space'
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

  ! real2ch with negative value: Fortran format includes '-' sign
  subroutine test_real2ch_negative(failures)
    integer, intent(inout) :: failures
    character(len=20) :: a
    integer :: len
    a = '                    '
    call real2ch(-3.14, a, 1, 2, len)
    if (a(1:5) /= '-3.14' .or. len /= 5) then
      print *, 'FAIL test_real2ch_negative: a="', a(1:7), '" len=', len
      failures = failures + 1
    else
      print *, 'PASS test_real2ch_negative'
    end if
  end subroutine

  ! deg2dec with lowercase 'n': same result as uppercase 'N'
  subroutine test_deg2dec_lowercase_n(failures)
    integer, intent(inout) :: failures
    real :: x_upper, x_lower
    call deg2dec(37, 30.0, 'N', x_upper)
    call deg2dec(37, 30.0, 'n', x_lower)
    if (abs(x_lower - x_upper) > 0.001) then
      print *, 'FAIL test_deg2dec_lowercase_n: x_lower=', x_lower, ' x_upper=', x_upper
      failures = failures + 1
    else
      print *, 'PASS test_deg2dec_lowercase_n'
    end if
  end subroutine

  ! deg2dec with lowercase 's': same result as uppercase 'S' (negative lat)
  subroutine test_deg2dec_lowercase_s(failures)
    integer, intent(inout) :: failures
    real :: x_upper, x_lower
    call deg2dec(20, 15.0, 'S', x_upper)
    call deg2dec(20, 15.0, 's', x_lower)
    if (abs(x_lower - x_upper) > 0.001) then
      print *, 'FAIL test_deg2dec_lowercase_s: x_lower=', x_lower, ' x_upper=', x_upper
      failures = failures + 1
    else
      print *, 'PASS test_deg2dec_lowercase_s'
    end if
  end subroutine

  ! deg2dec with lowercase 'w': same result as uppercase 'W' (0-360 E convention)
  subroutine test_deg2dec_lowercase_w(failures)
    integer, intent(inout) :: failures
    real :: x_upper, x_lower
    call deg2dec(120, 0.0, 'W', x_upper)   ! → 360-120 = 240.0
    call deg2dec(120, 0.0, 'w', x_lower)
    if (abs(x_lower - x_upper) > 0.001) then
      print *, 'FAIL test_deg2dec_lowercase_w: x_lower=', x_lower, ' x_upper=', x_upper
      failures = failures + 1
    else
      print *, 'PASS test_deg2dec_lowercase_w'
    end if
  end subroutine

  ! deg2dec with unknown hemisphere '?': defensive else branch returns x=0.0
  subroutine test_deg2dec_unknown_hemisphere(failures)
    integer, intent(inout) :: failures
    real :: x
    call deg2dec(30, 0.0, '?', x)
    if (x /= 0.0) then
      print *, 'FAIL test_deg2dec_unknown_hemisphere: x=', x, ' expected 0.0'
      failures = failures + 1
    else
      print *, 'PASS test_deg2dec_unknown_hemisphere'
    end if
  end subroutine

end program test_sio_convert
