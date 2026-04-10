! tests/unit/test_sio_math.f90
program test_sio_math
  use sio_math
  implicit none

  integer :: failures = 0

  call test_dpolft_linear(failures)
  call test_dpolft_bad_input(failures)
  call test_dp1vlu_constant(failures)

  if (failures == 0) then
    print *, 'test_sio_math: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_math: FAILURES =', failures
    stop 1
  end if

contains

  ! Fit y=2x+1 through 5 points, evaluate at x=0, expect yfit~1.0
  subroutine test_dpolft_linear(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 5
    ! a needs >= 3*n + 3*maxdeg + 3 = 15+6+3 = 24 elements
    real    :: x(np), y(np), w(np), r(np), a(24)
    real    :: eps
    integer :: ndeg, ierr, i
    do i = 1, np
      x(i) = real(i)
      y(i) = 2.0*x(i) + 1.0
      w(i) = 1.0
    end do
    eps = 0.0
    call dpolft(np, x, y, w, 2, ndeg, eps, r, ierr, a)
    if (ierr /= 1) then
      print *, 'FAIL test_dpolft_linear: ierr =', ierr, ' expected 1'
      failures = failures + 1
      return
    end if
    block
      real :: yfit, yp(1)
      call dp1vlu(ndeg, 0, 0.0, yfit, yp, a)
      if (abs(yfit - 1.0) > 0.01) then
        print *, 'FAIL test_dpolft_linear: yfit =', yfit, ' expected ~1.0'
        failures = failures + 1
      else
        print *, 'PASS test_dpolft_linear'
      end if
    end block
  end subroutine test_dpolft_linear

  ! n=0 is invalid input -- expect ierr=2
  subroutine test_dpolft_bad_input(failures)
    integer, intent(inout) :: failures
    real    :: x(1), y(1), w(1), r(1), a(6)
    real    :: eps
    integer :: ndeg, ierr
    x(1) = 1.0; y(1) = 1.0; w(1) = 1.0
    eps = 0.0
    call dpolft(0, x, y, w, 1, ndeg, eps, r, ierr, a)
    if (ierr /= 2) then
      print *, 'FAIL test_dpolft_bad_input: ierr =', ierr, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_bad_input'
    end if
  end subroutine test_dpolft_bad_input

  ! Fit constant y=5 through 3 points; dp1vlu should return yfit~5 at any x
  subroutine test_dp1vlu_constant(failures)
    integer, intent(inout) :: failures
    ! a needs >= 3*n + 3*maxdeg + 3 = 9+3+3 = 15 elements
    real    :: x(3), y(3), w(3), r(3), a(15)
    real    :: yfit, yp(1), eps
    integer :: ndeg, ierr, i
    do i = 1, 3
      x(i) = real(i); y(i) = 5.0; w(i) = 1.0
    end do
    eps = 0.0
    call dpolft(3, x, y, w, 1, ndeg, eps, r, ierr, a)
    if (ierr /= 1) then
      print *, 'FAIL test_dp1vlu_constant: dpolft ierr =', ierr
      failures = failures + 1
      return
    end if
    call dp1vlu(ndeg, 0, 2.5, yfit, yp, a)
    if (abs(yfit - 5.0) > 0.01) then
      print *, 'FAIL test_dp1vlu_constant: yfit =', yfit, ' expected ~5.0'
      failures = failures + 1
    else
      print *, 'PASS test_dp1vlu_constant'
    end if
  end subroutine test_dp1vlu_constant

end program test_sio_math
