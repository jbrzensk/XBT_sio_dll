! src/sio_math.f90
! Polynomial fitting routines translated from siosub.for (Slatec library).
! DPOLFT: lines 1059-1406, DP1VLU: lines 1413-1550
module sio_math
  implicit none
  private
  public :: dpolft, dp1vlu

contains

  ! ---------------------------------------------------------------------------
  ! DPOLFT  -- Weighted least-squares polynomial fit of degrees 0..MAXDEG.
  !
  ! Translated from siosub.for lines 1059-1406 (Slatec POLFIT, REAL*4 version).
  !
  ! Arguments:
  !   n      - number of data points (>= 1)
  !   x(n)   - independent variable values
  !   y(n)   - dependent variable values
  !   w(n)   - weights (if w(1)<0, all weights set to 1.0)
  !   maxdeg - maximum polynomial degree to fit
  !   ndeg   - (out) degree of the highest fit computed
  !   eps    - (inout) degree-selection criterion (see original docs);
  !            on output contains the RMS error of the NDEG fit
  !   r(n)   - (out) fitted values at each x(i)
  !   ierr   - (out) error flag: 1=normal, 2=bad input, 3=eps not met,
  !            4=F-test not satisfied
  !   a(*)   - work/output array; must have >= 3*n + 3*maxdeg + 3 elements
  ! ---------------------------------------------------------------------------
  subroutine dpolft(n, x, y, w, maxdeg, ndeg, eps, r, ierr, a)
    integer, intent(in)    :: n, maxdeg
    real,    intent(in)    :: x(n), y(n)
    real,    intent(inout) :: w(n), eps  ! NOTE: if w(1)<0 on entry, w is overwritten with 1.0 (Slatec behavior)
    integer, intent(out)   :: ndeg, ierr
    real,    intent(out)   :: r(n)
    real,    intent(inout) :: a(*)

    ! Local variables
    integer :: i, idegf, j, jp1, jpas = 0, k1, k1pj, k2, k2pj, k3, k3pi
    integer :: k4, k4pi, k5, k5pi, ksig, m, mop1, nder, nfail = 0
    real    :: temd1, temd2
    real    :: degf, den, etst, f, fcrit, sig = 0.0, sigj, sigjm1, sigpas = 0.0
    real    :: temp, xm, w1, w11, yp_dummy(1)

    ! F-test critical-value coefficients (from original DATA statement)
    real, parameter :: co(4,3) = reshape( &
      [ -13.086850, -2.4648165, -3.3846535, -1.2973162, &
         -3.3381146, -1.7812271, -3.2578406, -1.6589279, &
         -1.6282703, -1.3152745, -3.2640179, -1.9829776 ], &
      shape=[4,3] )

    ! --- Input validation ---
    m = abs(n)
    if (m == 0) then
      ierr = 2
      return
    end if
    if (maxdeg < 0) then
      ierr = 2
      return
    end if
    a(1) = maxdeg
    mop1 = maxdeg + 1
    if (m < mop1) then
      ierr = 2
      return
    end if
    if (eps < 0.0 .and. m == mop1) then
      ierr = 2
      return
    end if

    xm   = real(m)
    etst = eps * eps * xm

    ! Set weights
    if (w(1) < 0.0) then
      w = 1.0
    else
      do i = 1, m
        if (w(i) <= 0.0) then
          ierr = 2
          return
        end if
      end do
    end if

    ! --- Determine significance level index (when eps < 0) ---
    ksig = 1   ! default; only used when eps < 0
    if (eps < 0.0) then
      if (eps <= -0.55) then
        ! auto-select based on degrees of freedom
        idegf = m - maxdeg - 1
        ksig = 1
        if (idegf < 10) ksig = 2
        if (idegf <  5) ksig = 3
      else
        ! user-specified significance level
        ksig = 1
        if (eps < -0.03) ksig = 2
        if (eps < -0.07) ksig = 3
      end if
    end if

    ! --- Initialize indexes and coefficients for fitting ---
    k1 = maxdeg + 1
    k2 = k1 + maxdeg
    k3 = k2 + maxdeg + 2
    k4 = k3 + m
    k5 = k4 + m

    do i = 2, k4
      a(i) = 0.0
    end do

    w11 = 0.0

    if (n >= 0) then
      ! Unconstrained case
      do i = 1, m
        k4pi = k4 + i
        a(k4pi) = 1.0
        w11 = w11 + w(i)
      end do
    else
      ! Constrained case
      do i = 1, m
        k4pi = k4 + i
        w11 = w11 + w(i) * a(k4pi)**2
      end do
    end if

    ! --- Compute fit of degree zero ---
    temd1 = 0.0
    do i = 1, m
      k4pi  = k4 + i
      temd1 = temd1 + w(i) * y(i) * a(k4pi)
    end do
    temd1   = temd1 / w11
    a(k2+1) = temd1
    sigj    = 0.0
    do i = 1, m
      k4pi     = k4 + i
      k5pi     = k5 + i
      temd2    = temd1 * a(k4pi)
      r(i)     = temd2
      a(k5pi)  = temd2 - r(i)
      sigj     = sigj + w(i) * ((y(i) - r(i)) - a(k5pi))**2
    end do
    j = 0

    ! --- Degree-selection loop ---
    ! Check degree-0 fit first, then increment degree
    degree_loop: do

      ! Branch on eps: negative=F-test, zero=all degrees, positive=RMS test
      if (eps < 0.0) then
        ! F-test path (label 23/24/25)
        if (j > 0) then   ! F-test only meaningful after degree 0
          if (sigj == 0.0) then
            ! Perfect fit — accept (go to 29 path with jpas/sigpas)
            ierr = 1
            ndeg = jpas
            sig  = sigpas
            a(k3) = ndeg
            exit degree_loop
          end if
          degf  = real(m - j - 1)
          den   = (co(4,ksig) * degf + 1.0) * degf
          fcrit = (((co(3,ksig) * degf) + co(2,ksig)) * degf + co(1,ksig)) / den
          fcrit = fcrit * fcrit
          f     = (sigjm1 - sigj) * degf / sigj
          if (f >= fcrit) then
            ! Degree j passes F-test (label 24)
            sigpas = sigj
            jpas   = j
            nfail  = 0
            if (maxdeg == j) then
              ierr  = 4
              ndeg  = jpas
              sig   = sigpas
              a(k3) = ndeg
              exit degree_loop
            end if
          else
            ! Degree j fails F-test (label 25)
            nfail = nfail + 1
            if (nfail >= 3) then
              ierr  = 1
              ndeg  = jpas
              sig   = sigpas
              a(k3) = ndeg
              exit degree_loop
            end if
            if (maxdeg == j) then
              ierr  = 4
              ndeg  = jpas
              sig   = sigpas
              a(k3) = ndeg
              exit degree_loop
            end if
          end if
        else
          ! j == 0: initialise jpas/sigpas/nfail; treat degree 0 as passing
          jpas   = 0
          sigpas = sigj
          nfail  = 0
          if (maxdeg == 0) then
            ierr  = 4
            ndeg  = jpas
            sig   = sigpas
            a(k3) = ndeg
            exit degree_loop
          end if
        end if

      else if (eps == 0.0) then
        ! All degrees up to maxdeg (label 26)
        if (maxdeg == j) then
          ierr  = 1
          ndeg  = j
          sig   = sigj
          a(k3) = ndeg
          exit degree_loop
        end if

      else
        ! RMS error test (label 27)
        if (sigj <= etst) then
          ierr  = 1
          ndeg  = j
          sig   = sigj
          a(k3) = ndeg
          exit degree_loop
        end if
        if (maxdeg == j) then
          ierr  = 3
          ndeg  = maxdeg
          sig   = sigj
          a(k3) = ndeg
          exit degree_loop
        end if
      end if

      ! --- Increment degree (label 16) ---
      j    = j + 1
      jp1  = j + 1
      k1pj = k1 + j
      k2pj = k2 + j
      sigjm1 = sigj

      ! New B coefficient (except when j==1)
      if (j > 1) a(k1pj) = w11 / w1

      ! New A coefficient
      temd1 = 0.0
      do i = 1, m
        k4pi  = k4 + i
        temd2 = a(k4pi)
        temd1 = temd1 + x(i) * w(i) * temd2 * temd2
      end do
      a(jp1) = temd1 / w11

      ! Evaluate orthogonal polynomial at data points
      w1  = w11
      w11 = 0.0
      do i = 1, m
        k3pi    = k3 + i
        k4pi    = k4 + i
        temp    = a(k3pi)
        a(k3pi) = a(k4pi)
        a(k4pi) = (x(i) - a(jp1)) * a(k3pi) - a(k1pj) * temp
        w11     = w11 + w(i) * a(k4pi)**2
      end do

      ! New orthogonal polynomial coefficient (partial double precision)
      temd1 = 0.0
      do i = 1, m
        k4pi  = k4 + i
        k5pi  = k5 + i
        temd2 = w(i) * ((y(i) - r(i)) - a(k5pi)) * a(k4pi)
        temd1 = temd1 + temd2
      end do
      temd1       = temd1 / w11
      a(k2pj + 1) = temd1

      ! Update polynomial evaluations and accumulate sum of squared errors
      sigj = 0.0
      do i = 1, m
        k4pi    = k4 + i
        k5pi    = k5 + i
        temd2   = r(i) + a(k5pi) + temd1 * a(k4pi)
        r(i)    = temd2
        a(k5pi) = temd2 - r(i)
        sigj    = sigj + w(i) * ((y(i) - r(i)) - a(k5pi))**2
      end do

    end do degree_loop

    ! --- Post-loop: evaluate best polynomial at all data points if needed ---
    if (eps < 0.0 .and. ndeg /= maxdeg) then
      nder = 0
      do i = 1, m
        call dp1vlu(ndeg, nder, x(i), r(i), yp_dummy, a)
      end do
    end if
    eps = sqrt(sig / xm)

  end subroutine dpolft

  ! ---------------------------------------------------------------------------
  ! DP1VLU  -- Evaluate the polynomial fit of degree L (and up to NDER
  !            derivatives) at point X, using coefficients from DPOLFT.
  !
  ! Translated from siosub.for lines 1413-1550.
  !
  ! Arguments:
  !   l     - degree of polynomial to evaluate (0 <= l <= ndeg)
  !   nder  - number of derivatives to evaluate (0 or more; <0 treated as 0)
  !   x     - evaluation point
  !   yfit  - (out) value of polynomial at x
  !   yp(*) - (out) array of first through nder derivatives
  !   a(*)  - work array from DPOLFT; NOTE: used as scratch space and modified,
  !           so polynomial cannot be evaluated at multiple points without
  !           calling dpolft again
  ! ---------------------------------------------------------------------------
  subroutine dp1vlu(l, nder, x, yfit, yp, a)
    integer, intent(in)  :: l, nder
    real,    intent(in)    :: x
    real,    intent(inout) :: a(*)
    real,    intent(out)   :: yfit, yp(*)

    ! Local variables
    integer :: i, ic, ilo, in, inp1, iup, k1, k1i, k2, k3, k3p1, k3pn
    integer :: k4, k4p1, k4pn, kc, lm1, lp1, maxord, n, ndo, ndp1, nord
    real    :: cc, dif, val = 0.0

    ! --- Guard against invalid L ---
    if (l < 0) then
      yfit = 0.0
      return
    end if

    ndo   = max(nder, 0)
    ndo   = min(ndo, l)
    maxord = int(a(1) + 0.5)
    k1    = maxord + 1
    k2    = k1 + maxord
    k3    = k2 + maxord + 2
    nord  = int(a(k3) + 0.5)

    if (l > nord) then
      ! Requested degree exceeds highest degree fit — return silently
      yfit = 0.0
      return
    end if

    k4 = k3 + l + 1

    ! Zero out derivative array
    if (nder >= 1) then
      do i = 1, nder
        yp(i) = 0.0
      end do
    end if

    ! Handle small degrees directly
    if (l == 0) then
      val  = a(k2 + 1)
      yfit = val
      return
    end if

    if (l == 1) then
      cc   = a(k2 + 2)
      val  = a(k2 + 1) + (x - a(2)) * cc
      if (nder >= 1) yp(1) = cc
      yfit = val
      return
    end if

    ! --- General case: l >= 2 ---
    ndp1 = ndo + 1
    k3p1 = k3 + 1
    k4p1 = k4 + 1
    lp1  = l + 1
    lm1  = l - 1
    ilo  = k3 + 3
    iup  = k4 + ndp1

    do i = ilo, iup
      a(i) = 0.0
    end do

    dif      = x - a(lp1)
    kc       = k2 + lp1
    a(k4p1)  = a(kc)
    a(k3p1)  = a(kc - 1) + dif * a(k4p1)
    a(k3 + 2) = a(k4p1)

    ! Recurrence relations for function value and derivatives
    do i = 1, lm1
      in   = l - i
      inp1 = in + 1
      k1i  = k1 + inp1
      ic   = k2 + in
      dif  = x - a(inp1)
      val  = a(ic) + dif * a(k3p1) - a(k1i) * a(k4p1)

      if (ndo > 0) then
        do n = 1, ndo
          k3pn  = k3p1 + n
          k4pn  = k4p1 + n
          yp(n) = dif * a(k3pn) + real(n) * a(k3pn - 1) - a(k1i) * a(k4pn)
        end do

        do n = 1, ndo
          k3pn    = k3p1 + n
          k4pn    = k4p1 + n
          a(k4pn) = a(k3pn)
          a(k3pn) = yp(n)
        end do
      end if

      a(k4p1) = a(k3p1)
      a(k3p1) = val
    end do

    yfit = val

  end subroutine dp1vlu

end module sio_math
