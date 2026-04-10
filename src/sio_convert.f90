module sio_convert
  implicit none
  private
  public :: ch2real, real2ch, int2ch, dec2deg, deg2dec, findspace, lev

contains

  ! ---------------------------------------------------------------------------
  ! ch2real: translate character substring to real
  ! IN:  acmsg  - character string (spaces replaced with '0' as side effect)
  !      lpos   - position of leftmost digit in acmsg
  !      length - number of characters to parse
  ! OUT: x      - translated real value
  ! ---------------------------------------------------------------------------
  subroutine ch2real(acmsg, lpos, length, x)
    character(len=*), intent(inout) :: acmsg
    integer,          intent(in)    :: lpos, length
    real,             intent(out)   :: x

    integer :: l, lp, neg, ik, k, i, nl, nr
    logical :: found_decimal

    l   = length
    lp  = lpos
    x   = 0.0
    neg = 0
    ik  = 0
    nl  = 0
    nr  = 0

    if (acmsg(lp:lp) == '-') then
      neg = 1
      lp  = lp + 1
      l   = l - 1
    end if

    k = lp

    ! First pass: scan for decimal point
    found_decimal = .false.
    ik = 0
    do i = k, k + l - 1
      ik = ik + 1
      if (acmsg(i:i) == '.') then
        nl            = ik - 1
        nr            = l - ik
        found_decimal = .true.
        exit
      end if
    end do

    if (.not. found_decimal) then
      ! No decimal: scan for spaces to determine integer digit count
      ik = 0
      do i = k, k + l - 1
        ik = ik + 1
        if (acmsg(i:i) == ' ') then
          nl = ik - 1
          if (nl == 0) then
            ! Leading space — keep scanning (matches original go to 11)
            ik = ik - 1
            nl = 0
            cycle
          end if
          nr = 0
          exit
        end if
        ! Reached last character without a space
        if (i == k + l - 1) then
          nl = l
          nr = 0
        end if
      end do
    end if

    ! Replace spaces with '0' in the substring
    do i = lp, lp + l - 1
      if (acmsg(i:i) == ' ') acmsg(i:i) = '0'
    end do

    ! Accumulate integer digits (left of decimal)
    if (nl >= 6) then
      x = real((ichar(acmsg(k:k)) - 48) * 100000)
      k = k + 1
    end if
    if (nl >= 5) then
      x = x + real((ichar(acmsg(k:k)) - 48) * 10000)
      k = k + 1
    end if
    if (nl >= 4) then
      x = x + real((ichar(acmsg(k:k)) - 48) * 1000)
      k = k + 1
    end if
    if (nl >= 3) then
      x = x + real((ichar(acmsg(k:k)) - 48) * 100)
      k = k + 1
    end if
    if (nl >= 2) then
      x = x + real((ichar(acmsg(k:k)) - 48) * 10)
      k = k + 1
    end if
    if (nl >= 1) then
      x = x + real(ichar(acmsg(k:k)) - 48)
      k = k + 1
    end if

    ! Accumulate fractional digits (right of decimal)
    if (nr == 0) then
      if (neg == 1) x = -x
      return
    end if
    ! k now points at the decimal point; skip it
    if (nr >= 1) then
      k = k + 1
      x = x + real(ichar(acmsg(k:k)) - 48) * 0.1
    end if
    k = k + 1
    if (nr >= 2) x = x + real(ichar(acmsg(k:k)) - 48) * 0.01
    k = k + 1
    if (nr >= 3) x = x + real(ichar(acmsg(k:k)) - 48) * 0.001
    k = k + 1
    if (nr >= 4) x = x + real(ichar(acmsg(k:k)) - 48) * 0.0001
    k = k + 1
    if (nr >= 5) x = x + real(ichar(acmsg(k:k)) - 48) * 0.00001
    k = k + 1
    if (nr >= 6) x = x + real(ichar(acmsg(k:k)) - 48) * 0.000001
    k = k + 1
    if (nr >= 7) x = x + real(ichar(acmsg(k:k)) - 48) * 0.0000001

    if (neg == 1) x = -x

  end subroutine ch2real

  ! ---------------------------------------------------------------------------
  ! real2ch: translate real to character string
  ! IN:  x    - real value to translate
  !      a    - character string to write into
  !      ipos - position in a to start writing
  !      nrx  - digits to the right of decimal point (0-6)
  ! OUT: a    - updated string
  !      len  - number of characters written
  ! ---------------------------------------------------------------------------
  subroutine real2ch(x, a, ipos, nrx, len)
    real,             intent(in)    :: x
    character(len=*), intent(inout) :: a
    integer,          intent(in)    :: ipos, nrx
    integer,          intent(out)   :: len

    character(len=80) :: a2
    integer :: i, nb

    a2 = ' '

    select case (nrx)
    case (0); write(a2(1:14), '(f14.0)') x
    case (1); write(a2(1:14), '(f14.1)') x
    case (2); write(a2(1:14), '(f14.2)') x
    case (3); write(a2(1:14), '(f14.3)') x
    case (4); write(a2(1:14), '(f14.4)') x
    case (5); write(a2(1:14), '(f14.5)') x
    case (6); write(a2(1:14), '(f14.6)') x
    end select

    nb  = 0
    len = 14

    ! Search for first non-blank character; insert '0' before leading decimal
    do i = 1, 14
      if (a2(i:i) == '.') then
        nb = nb - 1
        len = len + 1
        a2(i-1:i-1) = '0'
        exit
      end if
      if (a2(i:i) /= ' ') exit
      nb  = nb  + 1
      len = len - 1
    end do

    ! If nrx==0, don't write the decimal point
    if (nrx == 0) len = len - 1

    ! Copy the trimmed result into a at the requested position
    select case (len)
    case (1);  write(a(ipos:ipos),        '(a1)')  a2(1+nb:1+nb)
    case (2);  write(a(ipos:ipos+1),      '(a2)')  a2(1+nb:2+nb)
    case (3);  write(a(ipos:ipos+2),      '(a3)')  a2(1+nb:3+nb)
    case (4);  write(a(ipos:ipos+3),      '(a4)')  a2(1+nb:4+nb)
    case (5);  write(a(ipos:ipos+4),      '(a5)')  a2(1+nb:5+nb)
    case (6);  write(a(ipos:ipos+5),      '(a6)')  a2(1+nb:6+nb)
    case (7);  write(a(ipos:ipos+6),      '(a7)')  a2(1+nb:7+nb)
    case (8);  write(a(ipos:ipos+7),      '(a8)')  a2(1+nb:8+nb)
    case (9);  write(a(ipos:ipos+8),      '(a9)')  a2(1+nb:9+nb)
    case (10); write(a(ipos:ipos+9),      '(a10)') a2(1+nb:10+nb)
    case (11); write(a(ipos:ipos+10),     '(a11)') a2(1+nb:11+nb)
    case (12); write(a(ipos:ipos+11),     '(a12)') a2(1+nb:12+nb)
    case (13); write(a(ipos:ipos+12),     '(a13)') a2(1+nb:13+nb)
    case (14); write(a(ipos:ipos+13),     '(a14)') a2(1+nb:14+nb)
    end select

  end subroutine real2ch

  ! ---------------------------------------------------------------------------
  ! int2ch: convert integer to characters in string a starting at jpos
  ! IN:  ka   - integer to convert
  !      a    - character string to write into
  !      jpos - starting position in a (1-based)
  ! OUT: len  - number of characters written
  ! ---------------------------------------------------------------------------
  subroutine int2ch(ka, a, jpos, len)
    integer,          intent(in)    :: ka, jpos
    character(len=*), intent(inout) :: a
    integer,          intent(out)   :: len

    character(len=12) :: tmp
    integer :: i, j, kabs, ndig, idig

    kabs = abs(ka)

    ! Count digits needed (zero needs 1 digit)
    if (kabs == 0) then
      ndig = 1
    else
      ndig = 0
      j    = kabs
      do while (j > 0)
        ndig = ndig + 1
        j    = j / 10
      end do
    end if

    ! Total length includes sign for negatives
    len = ndig
    if (ka < 0) len = len + 1

    ! Build digit characters right-to-left in tmp
    tmp = ' '
    if (kabs == 0) then
      tmp(len:len) = char(48)
    else
      j = kabs
      do i = len, len - ndig + 1, -1
        idig = mod(j, 10)
        tmp(i:i) = char(idig + 48)
        j = j / 10
      end do
    end if

    ! Write sign if negative
    if (ka < 0) tmp(1:1) = char(45)

    ! Copy result into output string at requested position
    do i = 1, len
      a(jpos+i-1:jpos+i-1) = tmp(i:i)
    end do

  end subroutine int2ch

  ! ---------------------------------------------------------------------------
  ! dec2deg: translate decimal position to degrees, minutes, hemisphere
  ! IN:  typ - 'lat' or 'lon'
  !      x   - decimal position (lat: signed; lon: 0-360 E convention)
  ! OUT: ideg - integer degrees
  !      xmin - real minutes
  !      ahem - hemisphere character ('N','S','E','W')
  ! ---------------------------------------------------------------------------
  subroutine dec2deg(typ, ideg, xmin, ahem, x)
    character(len=3), intent(in)  :: typ
    real,             intent(in)  :: x
    integer,          intent(out) :: ideg
    real,             intent(out) :: xmin
    character(len=1), intent(out) :: ahem

    real :: xtemp

    if (typ == 'lat') then
      ideg = int(abs(x))
      xmin = (abs(x) - real(ideg)) * 60.0
      if (x >= 0.0) then
        ahem = 'N'
      else
        ahem = 'S'
      end if
    else
      ! Longitude: 0-360 E convention
      if (x <= 180.0) then
        ahem = 'E'
        ideg = int(abs(x))
        xmin = (abs(x) - real(ideg)) * 60.0
      else
        ahem  = 'W'
        xtemp = 360.0 - x
        ideg  = int(abs(xtemp))
        xmin  = (abs(xtemp) - real(ideg)) * 60.0
      end if
    end if

  end subroutine dec2deg

  ! ---------------------------------------------------------------------------
  ! deg2dec: translate degrees, minutes, hemisphere to decimal position
  ! IN:  ideg - integer degrees
  !      xmin - real minutes
  !      ahem - hemisphere character
  ! OUT: x    - decimal position (lon uses 0-360 E convention)
  ! ---------------------------------------------------------------------------
  subroutine deg2dec(ideg, xmin, ahem, x)
    integer,          intent(in)  :: ideg
    real,             intent(in)  :: xmin
    character(len=1), intent(in)  :: ahem
    real,             intent(out) :: x

    if (ahem == 'N' .or. ahem == 'n' .or. &
        ahem == 'E' .or. ahem == 'e') then
      x = real(ideg) + (xmin / 60.0)
    else if (ahem == 'S' .or. ahem == 's') then
      x = -1.0 * (real(abs(ideg)) + (abs(xmin) / 60.0))
    else if (ahem == 'W' .or. ahem == 'w') then
      x = 360.0 - (real(ideg) + (xmin / 60.0))
    end if

  end subroutine deg2dec

  ! ---------------------------------------------------------------------------
  ! findspace: scan aplan from position i to find next space; count characters
  ! IN:  aplan - character string to scan
  ! INOUT: i   - start position; updated to position of the space (or end)
  ! OUT: ic    - number of non-space characters found before the space
  ! ---------------------------------------------------------------------------
  subroutine findspace(aplan, i, ic)
    character(len=*), intent(in)    :: aplan
    integer,          intent(inout) :: i
    integer,          intent(out)   :: ic

    integer :: ii, j

    ii = i
    ic = 0

    do j = 1, 15
      if (aplan(ii:ii) /= ' ') then
        ic = ic + 1
        ii = ii + 1
      else
        exit
      end if
    end do
    i = ii

  end subroutine findspace

  ! ---------------------------------------------------------------------------
  ! lev: interpret operation string to set error level
  ! IN:  aop     - operation string
  ! OUT: ierrlev - error level (0 normal; 6 if aop starts with 'debug')
  ! ---------------------------------------------------------------------------
  subroutine lev(aop, ierrlev)
    character(len=*), intent(in)  :: aop
    integer,          intent(out) :: ierrlev

    ierrlev = 0
    if (len(aop) >= 5 .and. aop(1:5) == 'debug') then
      ierrlev = 6
    end if

  end subroutine lev

end module sio_convert
