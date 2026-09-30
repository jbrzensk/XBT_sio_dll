! tests/seas_sim.f90
! Plays Seas for integration tests. Keeps the Seas-side variables between
! calls (the m_* members of CSioDllInterface) and calls siobegin, sioloop
! and sioend the way Seas does (AmverSeas source: Sio/CSioDllInterface.cpp,
! XbtDataRecorder/CPositionDropPlan.cpp):
!  - Every second (sim_loop): the time comes from the latest GPS data; if it
!    is not a valid date/time Seas cannot build its CTime and sioloop is not
!    called. iupdate=1 only if the position differs from the one passed last
!    time. The position is copied only when the fix is valid: degrees and
!    minutes via atof (empty field -> 0), cardinal exactly "N" -> 1 else 3,
!    exactly "E" -> 2 else 4. Then igps=1, ierror(35)=0, dropmin=-1 and
!    sioloop; ierror(1)=1 means "drop now".
!  - At a drop Seas calls sioend (no sioloop calls until the next siobegin),
!    launches, then calls siobegin for the next drop.
! Pair with the test clock in sio_time for the PC time.
module seas_sim
  implicit none
  private
  public :: seas_state, gps_data, st, sim_reset, sim_begin, sim_loop, sim_end, atof

  ! GPS data as Seas holds it (last parsed sentence)
  type :: gps_data
    integer :: ihr = 0, imin = 0, isec = 0, iday = 0, imon = 0, iyr = 0
    character(len=16) :: latdd = ' ', latmm = ' ', londdd = ' ', lonmm = ' '
    character(len=1)  :: ns = ' ', ew = ' '
    logical :: valid = .false.
  end type gps_data

  ! Seas-side DLL arguments, kept between calls
  type :: seas_state
    real    :: deadmin = 0, dropmin = 0, relodmin = 0, runsec = 0, xmaxspd = 0
    real    :: xlat = 0, xlatload(12) = 0, alrmtime = 0, dtime = 0, yrday1 = 0
    real    :: speed = 0, dir = 0, timeave = 0, vlat = 0, vlon = 0
    real    :: gpssec = 0, chrsav = 0
    real    :: ctagbuf(200) = 0, clatbuf(200) = 0, clonbuf(200) = 0
    real    :: clatd = 0, clatm = 0, clond = 0, clonm = 0
    real    :: chr = 0, cmin = 0, csec = 0, cday = 0, cmon = 0, cyear = 0
    real    :: eta(12) = 0, drlat = 0, drlon = 0
    integer :: launcher(12) = 0, igps = 1, nplan = 0, ibuf = 0
    integer :: idsec2 = 0, ierrlev = 0, ifirst = 0, irollnav = 0, inav = 0
    integer :: ispec(12) = 0, ierror(50) = 0, iaveflg = 0, ispd = 0, itime = 0
    integer :: idayave = 0, imonave = 0, iyerave = 0, idosday = 0, iplandir = 0
    integer :: nlnchr = 0, nextdrop = 0, iplancnt = 0, iwait = 1, iskip = 0
    integer :: idaygps = 0, imongps = 0, iyergps = 0, istat = 0
    integer :: icsec1 = 0, iupdate = 0, iclath = 0, iclonh = 0
    integer :: iSIOSpeedAveMin = 10
    logical :: begun = .false.
  end type seas_state

  type(seas_state), save :: st
  type(gps_data),   save :: last_pos
  logical,          save :: have_last = .false.

  interface
    subroutine siobegin(deadmin, dropmin, relodmin, runsec, xmaxspd, &
         launcher, igps, xlat, xlatload, nplan, ibuf, &
         idsec2, ierrlev, alrmtime, ifirst, irollnav, &
         inav, ispec, dtime, yrday1, ierror, iaveflg, ispd, itime, &
         idayave, imonave, iyerave, icday1, iplandir, &
         speed, dir, timeave, vlat, vlon, &
         nlnchr, nextdrop, iplancnt, iwait, &
         chr, cmin, csec, cday, cmon, cyear, isio_skip_count)
      real,    intent(inout) :: deadmin, dropmin, relodmin, runsec, xmaxspd
      real,    intent(inout) :: xlat, xlatload(12), alrmtime, dtime, yrday1
      real,    intent(inout) :: speed, dir, timeave, vlat, vlon
      real,    intent(inout) :: chr, cmin, csec, cday, cmon, cyear
      integer, intent(inout) :: launcher(12), igps, nplan, ibuf
      integer, intent(inout) :: idsec2, ierrlev
      integer, intent(inout) :: ifirst, irollnav, inav, ispec(12)
      integer, intent(inout) :: ierror(50), iaveflg, ispd, itime
      integer, intent(inout) :: idayave, imonave, iyerave, icday1
      integer, intent(inout) :: iplandir, nlnchr, nextdrop, iplancnt, iwait
      integer, intent(inout) :: isio_skip_count
    end subroutine siobegin

    subroutine sioloop(deadmin, dropmin, relodmin, runsec, xmaxspd, &
         launcher, igps, xlat, xlatload, nplan, ibuf, &
         idsec2, ierrlev, alrmtime, ifirst, irollnav, &
         inav, ispec, dtime, yrday1, ierror, iaveflg, ispd, itime, &
         idayave, imonave, iyerave, icday1, iplandir, &
         speed, dir, timeave, vlat, vlon, &
         icday, icmon, icyear, istat, &
         gpssec, chrsav, icsec1, ctagbuf, clatbuf, clonbuf, &
         iupdate, clatd, clatm, iclath, clond, clonm, iclonh, &
         chr, cmin, csec, cday, cmon, cyear, nlnchr, eta, &
         drlat, drlon, iSIOSpeedAveMin)
      real,    intent(inout) :: deadmin, dropmin, relodmin, runsec, xmaxspd
      real,    intent(inout) :: xlat, xlatload(12), alrmtime, dtime, yrday1
      real,    intent(inout) :: speed, dir, timeave, vlat, vlon
      real,    intent(inout) :: gpssec, chrsav
      real,    intent(inout) :: ctagbuf(200), clatbuf(200), clonbuf(200)
      real,    intent(inout) :: clatd, clatm, clond, clonm
      real,    intent(inout) :: chr, cmin, csec, cday, cmon, cyear
      real,    intent(inout) :: eta(12)
      real,    intent(inout) :: drlat, drlon
      integer, intent(inout) :: launcher(12), igps, nplan, ibuf
      integer, intent(inout) :: idsec2, ierrlev
      integer, intent(inout) :: ifirst, irollnav, inav, ispec(12)
      integer, intent(inout) :: ierror(50), iaveflg, ispd, itime
      integer, intent(inout) :: idayave, imonave, iyerave, icday1
      integer, intent(inout) :: iplandir, nlnchr
      integer, intent(inout) :: icday, icmon, icyear, istat
      integer, intent(inout) :: icsec1, iupdate, iclath, iclonh
      integer, intent(inout) :: iSIOSpeedAveMin
    end subroutine sioloop

    subroutine sioend(igps, ibuf, ierrlev, ierror, idayave, &
                      imonave, iyerave, speed, dir, timeave, vlat, vlon, &
                      icday, icmon, icyear, istat, ctagbuf, clatbuf, clonbuf, &
                      iSIOSpeedAveMin)
      integer, intent(in)    :: igps, ierrlev
      integer, intent(inout) :: ibuf
      integer, intent(inout) :: ierror(50)
      integer, intent(in)    :: idayave, imonave, iyerave
      integer, intent(in)    :: icday, icmon, icyear, istat
      integer, intent(inout) :: iSIOSpeedAveMin
      real,    intent(inout) :: speed, dir, timeave, vlat, vlon
      real,    intent(inout) :: clatbuf(200), clonbuf(200), ctagbuf(200)
    end subroutine sioend
  end interface

contains

  ! Fresh Seas-side state (Seas started)
  subroutine sim_reset()
    st = seas_state()
    have_last = .false.
  end subroutine sim_reset

  ! CPositionDropPlan::CallSioBegin + CSioDllInterface::SioBegin: nothing if
  ! the GPS data has no valid date/time (Seas's CTime throws first); else
  ! time from the GPS data, igps=1, wait flag and skip count, ierror(35)=0,
  ! then siobegin. st%begun tells whether it was called.
  subroutine sim_begin(g, iwait, iskip)
    type(gps_data), intent(in) :: g
    integer,        intent(in) :: iwait, iskip
    if (.not. valid_ctime(g)) return
    call set_time(g)
    st%igps  = 1
    st%iwait = iwait
    st%iskip = iskip
    st%ierror(35) = 0
    call siobegin(st%deadmin, st%dropmin, st%relodmin, st%runsec, st%xmaxspd, &
         st%launcher, st%igps, st%xlat, st%xlatload, st%nplan, st%ibuf, &
         st%idsec2, st%ierrlev, st%alrmtime, st%ifirst, st%irollnav, &
         st%inav, st%ispec, st%dtime, st%yrday1, st%ierror, st%iaveflg, &
         st%ispd, st%itime, st%idayave, st%imonave, st%iyerave, st%idosday, &
         st%iplandir, st%speed, st%dir, st%timeave, st%vlat, st%vlon, &
         st%nlnchr, st%nextdrop, st%iplancnt, st%iwait, &
         st%chr, st%cmin, st%csec, st%cday, st%cmon, st%cyear, st%iskip)
    st%begun = .true.
  end subroutine sim_begin

  ! One second of CPositionDropPlan::IsTimeToDrop + CSioDllInterface::
  ! IsTimeToDrop. True when the DLL says drop now.
  logical function sim_loop(g)
    type(gps_data), intent(in) :: g
    integer :: iupd
    sim_loop = .false.
    if (.not. valid_ctime(g)) return        ! CTime throws: no sioloop call
    iupd = 1
    if (have_last) then
      if (same_position(g, last_pos)) iupd = 0
    end if
    last_pos = g
    have_last = .true.
    if (.not. st%begun) return              ! between sioend and siobegin
    if (g%valid) then                       ! SetPositionVariables
      st%clatd = atof(g%latdd)
      st%clatm = atof(g%latmm)
      st%clond = atof(g%londdd)
      st%clonm = atof(g%lonmm)
      st%iclath = 3
      if (g%ns == 'N') st%iclath = 1
      st%iclonh = 4
      if (g%ew == 'E') st%iclonh = 2
    end if
    st%igps = 1
    call set_time(g)
    st%iupdate = iupd
    st%ierror(35) = 0
    st%dropmin = -1.0
    call sioloop(st%deadmin, st%dropmin, st%relodmin, st%runsec, st%xmaxspd, &
         st%launcher, st%igps, st%xlat, st%xlatload, st%nplan, st%ibuf, &
         st%idsec2, st%ierrlev, st%alrmtime, st%ifirst, st%irollnav, &
         st%inav, st%ispec, st%dtime, st%yrday1, st%ierror, st%iaveflg, &
         st%ispd, st%itime, st%idayave, st%imonave, st%iyerave, st%idosday, &
         st%iplandir, st%speed, st%dir, st%timeave, st%vlat, st%vlon, &
         st%idaygps, st%imongps, st%iyergps, st%istat, &
         st%gpssec, st%chrsav, st%icsec1, st%ctagbuf, st%clatbuf, st%clonbuf, &
         st%iupdate, st%clatd, st%clatm, st%iclath, st%clond, st%clonm, st%iclonh, &
         st%chr, st%cmin, st%csec, st%cday, st%cmon, st%cyear, st%nlnchr, st%eta, &
         st%drlat, st%drlon, st%iSIOSpeedAveMin)
    sim_loop = st%ierror(1) == 1
  end function sim_loop

  ! CSioDllInterface::SioEnd (Seas calls it when told to drop)
  subroutine sim_end()
    if (.not. st%begun) return
    call sioend(st%igps, st%ibuf, st%ierrlev, st%ierror, st%idayave, &
         st%imonave, st%iyerave, st%speed, st%dir, st%timeave, st%vlat, st%vlon, &
         st%idaygps, st%imongps, st%iyergps, st%istat, &
         st%ctagbuf, st%clatbuf, st%clonbuf, st%iSIOSpeedAveMin)
    st%begun = .false.
  end subroutine sim_end

  ! SetTimeVariables (4-digit year)
  subroutine set_time(g)
    type(gps_data), intent(in) :: g
    st%chr  = real(g%ihr);  st%cmin = real(g%imin); st%csec  = real(g%isec)
    st%cday = real(g%iday); st%cmon = real(g%imon); st%cyear = real(g%iyr)
  end subroutine set_time

  ! Ranges MFC CTime accepts; outside them Seas skips the call
  logical function valid_ctime(g)
    type(gps_data), intent(in) :: g
    valid_ctime = g%iyr >= 1970 .and. g%imon >= 1 .and. g%imon <= 12 .and. &
                  g%iday >= 1 .and. g%iday <= 31 .and. g%ihr >= 0 .and. &
                  g%ihr <= 23 .and. g%imin >= 0 .and. g%imin <= 59 .and. &
                  g%isec >= 0 .and. g%isec <= 59
  end function valid_ctime

  logical function same_position(a, b)
    type(gps_data), intent(in) :: a, b
    same_position = a%latdd == b%latdd .and. a%latmm == b%latmm .and. &
                    a%ns == b%ns .and. a%londdd == b%londdd .and. &
                    a%lonmm == b%lonmm .and. a%ew == b%ew
  end function same_position

  ! C atof: leading number of the string, 0 if none
  real function atof(s)
    character(len=*), intent(in) :: s
    integer :: i, n, ios
    character(len=32) :: t
    t = adjustl(s)
    n = 0
    do i = 1, len_trim(t)
      if (index('+-.0123456789', t(i:i)) == 0) exit
      n = i
    end do
    atof = 0.0
    if (n == 0) return
    read(t(1:n), *, iostat=ios) atof
    if (ios /= 0) atof = 0.0
  end function atof

end module seas_sim
