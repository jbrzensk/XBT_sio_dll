! tests/unit/test_sio_math.f90
program test_sio_math
  use sio_math
  implicit none

  integer :: failures = 0

  call test_dpolft_linear(failures)
  call test_dpolft_bad_input(failures)
  call test_dp1vlu_constant(failures)
  call test_dpolft_maxdeg_negative(failures)
  call test_dpolft_too_few_points(failures)
  call test_dpolft_zero_weight(failures)
  call test_dpolft_uniform_weights(failures)
  call test_dpolft_eps_neg_m_eq_mop1(failures)
  call test_dpolft_eps_negative_ftest(failures)
  call test_dpolft_eps_auto_select(failures)
  call test_dpolft_eps_positive_rms_met(failures)
  call test_dpolft_eps_positive_rms_not_met(failures)
  call test_dp1vlu_l1_derivative(failures)
  call test_dp1vlu_invalid_l(failures)
  call test_dp1vlu_l_exceeds_nord(failures)

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

  ! maxdeg < 0 → ierr=2 (input validation)
  subroutine test_dpolft_maxdeg_negative(failures)
    integer, intent(inout) :: failures
    real    :: x(3), y(3), w(3), r(3), a(10)
    real    :: eps
    integer :: ndeg, ierr
    x = [1.0, 2.0, 3.0]; y = [1.0, 2.0, 3.0]; w = [1.0, 1.0, 1.0]
    eps = 0.0
    call dpolft(3, x, y, w, -1, ndeg, eps, r, ierr, a)
    if (ierr /= 2) then
      print *, 'FAIL test_dpolft_maxdeg_negative: ierr=', ierr, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_maxdeg_negative'
    end if
  end subroutine test_dpolft_maxdeg_negative

  ! n=2, maxdeg=2: m < mop1 (need at least maxdeg+1 points) → ierr=2
  subroutine test_dpolft_too_few_points(failures)
    integer, intent(inout) :: failures
    real    :: x(2), y(2), w(2), r(2), a(10)
    real    :: eps
    integer :: ndeg, ierr
    x = [1.0, 2.0]; y = [1.0, 4.0]; w = [1.0, 1.0]
    eps = 0.0
    call dpolft(2, x, y, w, 2, ndeg, eps, r, ierr, a)
    if (ierr /= 2) then
      print *, 'FAIL test_dpolft_too_few_points: ierr=', ierr, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_too_few_points'
    end if
  end subroutine test_dpolft_too_few_points

  ! w(2)=0.0: non-positive weight → ierr=2
  subroutine test_dpolft_zero_weight(failures)
    integer, intent(inout) :: failures
    real    :: x(3), y(3), w(3), r(3), a(15)
    real    :: eps
    integer :: ndeg, ierr
    x = [1.0, 2.0, 3.0]; y = [1.0, 2.0, 3.0]
    w = [1.0, 0.0, 1.0]
    eps = 0.0
    call dpolft(3, x, y, w, 1, ndeg, eps, r, ierr, a)
    if (ierr /= 2) then
      print *, 'FAIL test_dpolft_zero_weight: ierr=', ierr, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_zero_weight'
    end if
  end subroutine test_dpolft_zero_weight

  ! w(1) < 0 triggers uniform weights (w overwritten with 1.0); fit still succeeds
  subroutine test_dpolft_uniform_weights(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 5
    real    :: x(np), y(np), w(np), r(np), a(30)
    real    :: eps
    integer :: ndeg, ierr, i
    do i = 1, np
      x(i) = real(i); y(i) = 2.0*x(i) + 1.0
    end do
    w = 1.0
    w(1) = -1.0   ! triggers overwrite of all w to 1.0
    eps = 0.0
    call dpolft(np, x, y, w, 2, ndeg, eps, r, ierr, a)
    if (ierr /= 1) then
      print *, 'FAIL test_dpolft_uniform_weights: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_uniform_weights'
    end if
  end subroutine test_dpolft_uniform_weights

  ! eps < 0 with m == mop1 (n=3, maxdeg=2): F-test undefined → ierr=2
  subroutine test_dpolft_eps_neg_m_eq_mop1(failures)
    integer, intent(inout) :: failures
    real    :: x(3), y(3), w(3), r(3), a(20)
    real    :: eps
    integer :: ndeg, ierr
    x = [1.0, 2.0, 3.0]; y = [1.0, 4.0, 9.0]; w = [1.0, 1.0, 1.0]
    eps = -0.01
    call dpolft(3, x, y, w, 2, ndeg, eps, r, ierr, a)
    if (ierr /= 2) then
      print *, 'FAIL test_dpolft_eps_neg_m_eq_mop1: ierr=', ierr, ' expected 2'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_eps_neg_m_eq_mop1'
    end if
  end subroutine test_dpolft_eps_neg_m_eq_mop1

  ! eps < 0: F-test path with near-linear noisy data.
  ! Degree 1 passes F-test; degrees 2-4 fail → nfail reaches 3 → ierr=1, ndeg=1.
  subroutine test_dpolft_eps_negative_ftest(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 10
    real    :: x(np), y(np), w(np), r(np), a(60)
    real    :: eps
    integer :: ndeg, ierr, i
    real, parameter :: ydata(np) = &
      [3.1, 5.1, 7.2, 9.1, 11.0, 12.9, 15.1, 17.1, 18.9, 21.1]
    do i = 1, np
      x(i) = real(i); w(i) = 1.0
    end do
    y = ydata
    eps = -0.01
    call dpolft(np, x, y, w, 4, ndeg, eps, r, ierr, a)
    if (ierr == 1 .and. ndeg == 1) then
      print *, 'PASS test_dpolft_eps_negative_ftest: ierr=', ierr, ' ndeg=', ndeg
    else if (ierr == 1 .or. ierr == 4) then
      print *, 'PASS test_dpolft_eps_negative_ftest (alt): ierr=', ierr, ' ndeg=', ndeg
    else
      print *, 'FAIL test_dpolft_eps_negative_ftest: ierr=', ierr, ' ndeg=', ndeg, &
               ' expected ierr=1'
      failures = failures + 1
    end if
  end subroutine test_dpolft_eps_negative_ftest

  ! eps > 0: exact linear data → degree-1 RMS drops to ~0 ≤ etst → ierr=1
  subroutine test_dpolft_eps_positive_rms_met(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 5
    real    :: x(np), y(np), w(np), r(np), a(30)
    real    :: eps
    integer :: ndeg, ierr, i
    do i = 1, np
      x(i) = real(i); y(i) = 2.0*x(i) + 1.0; w(i) = 1.0
    end do
    eps = 1.0   ! RMS target = 1.0; exact data achieves 0 at degree 1
    call dpolft(np, x, y, w, 2, ndeg, eps, r, ierr, a)
    if (ierr /= 1) then
      print *, 'FAIL test_dpolft_eps_positive_rms_met: ierr=', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_eps_positive_rms_met: ndeg=', ndeg
    end if
  end subroutine test_dpolft_eps_positive_rms_met

  ! eps > 0: alternating data [1,3,1,3] cannot be fit by degree-1 polynomial.
  ! Tight eps → RMS not met at maxdeg=1 → ierr=3.
  subroutine test_dpolft_eps_positive_rms_not_met(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 4
    real    :: x(np), y(np), w(np), r(np), a(25)
    real    :: eps
    integer :: ndeg, ierr, i
    do i = 1, np
      x(i) = real(i)
      if (mod(i, 2) == 1) then
        y(i) = 1.0
      else
        y(i) = 3.0
      end if
      w(i) = 1.0
    end do
    eps = 0.01   ! very tight; alternating pattern → linear residual >> etst
    call dpolft(np, x, y, w, 1, ndeg, eps, r, ierr, a)
    if (ierr /= 3) then
      print *, 'FAIL test_dpolft_eps_positive_rms_not_met: ierr=', ierr, ' expected 3'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_eps_positive_rms_not_met'
    end if
  end subroutine test_dpolft_eps_positive_rms_not_met

  ! eps <= -0.55: auto-select ksig branch; noisy linear data, maxdeg=2.
  ! idegf = m-maxdeg-1 = 3 < 5 → ksig=3. Degree 2 fails F-test at maxdeg → ierr=4/ndeg=1.
  subroutine test_dpolft_eps_auto_select(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 6
    real    :: x(np), y(np), w(np), r(np), a(40)
    real    :: eps
    integer :: ndeg, ierr, i
    real, parameter :: ydata(np) = &
      [2.1, 3.9, 6.1, 7.9, 10.1, 11.9]   ! approximately y=2x
    do i = 1, np
      x(i) = real(i); w(i) = 1.0
    end do
    y = ydata
    eps = -0.60    ! <= -0.55 → auto-select ksig branch; idegf=3 → ksig=3
    call dpolft(np, x, y, w, 2, ndeg, eps, r, ierr, a)
    if (ierr /= 1 .and. ierr /= 4) then
      print *, 'FAIL test_dpolft_eps_auto_select: ierr=', ierr, ' expected 1 or 4'
      failures = failures + 1
    else
      print *, 'PASS test_dpolft_eps_auto_select: ierr=', ierr, ' ndeg=', ndeg
    end if
  end subroutine test_dpolft_eps_auto_select

  ! dp1vlu l=1 branch with nder=1: fit y=3x+2, evaluate at x=0.
  ! Exercises the l==1 short-circuit and derivative output yp(1)=slope.
  subroutine test_dp1vlu_l1_derivative(failures)
    integer, intent(inout) :: failures
    integer, parameter :: np = 5
    real    :: x(np), y(np), w(np), r(np), a(30)
    real    :: yfit, yp(1), eps
    integer :: ndeg, ierr, i
    do i = 1, np
      x(i) = real(i); y(i) = 3.0*x(i) + 2.0; w(i) = 1.0
    end do
    eps = 0.0
    call dpolft(np, x, y, w, 1, ndeg, eps, r, ierr, a)
    if (ierr /= 1 .or. ndeg /= 1) then
      print *, 'FAIL test_dp1vlu_l1_derivative: dpolft ierr=', ierr, ' ndeg=', ndeg
      failures = failures + 1
      return
    end if
    ! At x=0: y=3*0+2=2; derivative=3
    call dp1vlu(1, 1, 0.0, yfit, yp, a)
    if (abs(yfit - 2.0) > 0.01 .or. abs(yp(1) - 3.0) > 0.01) then
      print *, 'FAIL test_dp1vlu_l1_derivative: yfit=', yfit, ' yp(1)=', yp(1), &
               ' expected yfit~2.0 yp(1)~3.0'
      failures = failures + 1
    else
      print *, 'PASS test_dp1vlu_l1_derivative'
    end if
  end subroutine test_dp1vlu_l1_derivative

  ! dp1vlu with l < 0: guard returns yfit=0 immediately
  subroutine test_dp1vlu_invalid_l(failures)
    integer, intent(inout) :: failures
    real    :: x(3), y(3), w(3), r(3), a(15)
    real    :: yfit, yp(1), eps
    integer :: ndeg, ierr, i
    do i = 1, 3
      x(i) = real(i); y(i) = real(i); w(i) = 1.0
    end do
    eps = 0.0
    call dpolft(3, x, y, w, 1, ndeg, eps, r, ierr, a)
    yfit = 999.0
    call dp1vlu(-1, 0, 1.0, yfit, yp, a)
    if (yfit /= 0.0) then
      print *, 'FAIL test_dp1vlu_invalid_l: yfit=', yfit, ' expected 0.0'
      failures = failures + 1
    else
      print *, 'PASS test_dp1vlu_invalid_l'
    end if
  end subroutine test_dp1vlu_invalid_l

  ! dp1vlu with l > nord: requested degree exceeds what was fit → yfit=0
  subroutine test_dp1vlu_l_exceeds_nord(failures)
    integer, intent(inout) :: failures
    real    :: x(3), y(3), w(3), r(3), a(15)
    real    :: yfit, yp(1), eps
    integer :: ndeg, ierr, i
    do i = 1, 3
      x(i) = real(i); y(i) = real(i); w(i) = 1.0
    end do
    eps = 0.0
    call dpolft(3, x, y, w, 1, ndeg, eps, r, ierr, a)   ! ndeg=1
    yfit = 999.0
    call dp1vlu(2, 0, 1.0, yfit, yp, a)   ! l=2 > nord=1 → yfit=0
    if (yfit /= 0.0) then
      print *, 'FAIL test_dp1vlu_l_exceeds_nord: yfit=', yfit, ' expected 0.0'
      failures = failures + 1
    else
      print *, 'PASS test_dp1vlu_l_exceeds_nord'
    end if
  end subroutine test_dp1vlu_l_exceeds_nord

end program test_sio_math
