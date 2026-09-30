! tests/integration/test_integration_api.f90
! Integration tests for the DLL entry points in src/sio_api.f90 (bare
! subroutines, called here as Seas calls them). They find their files
! through getdir, so each test runs against a scratch copy of a fixture
! (tests/test_support.f90), never the installed Seas data.
program test_integration_api
  use test_support
  implicit none
  integer :: failures = 0

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
  end interface

  call test_siobegin_base_fixture(failures)

  if (failures == 0) then
    print *, 'test_integration_api: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_integration_api: FAILURES =', failures
    stop 1
  end if

contains

  ! siobegin on a valid data directory (1 drop done, northbound lat plan):
  ! all well (ierror(35)=2), next station loaded, and no stray debug files
  ! written into the data directory
  subroutine test_siobegin_base_fixture(failures)
    integer, intent(inout) :: failures
    real    :: deadmin, dropmin, relodmin, runsec, xmaxspd
    real    :: xlat, xlatload(12), alrmtime, dtime, yrday1
    real    :: speed, dir, timeave, vlat, vlon
    real    :: chr, cmin, csec, cday, cmon, cyear
    integer :: launcher(12), igps, nplan, ibuf, idsec2, ierrlev
    integer :: ifirst, irollnav, inav, ispec(12), ierror(50)
    integer :: iaveflg, ispd, itime, idayave, imonave, iyerave, icday1
    integer :: iplandir, nlnchr, nextdrop, iplancnt, iwait, isio_skip_count
    character(len=80) :: sdir
    logical :: probe

    call scratch_dir('siobegin_base', sdir, 'tests\fixtures\base')
    call point_getdir_at(sdir)

    deadmin = 0.0; dropmin = 0.0; relodmin = 0.0; runsec = 0.0; xmaxspd = 0.0
    xlat = 0.0; xlatload = 0.0; alrmtime = 0.0; dtime = 0.0; yrday1 = 0.0
    speed = 0.0; dir = 0.0; timeave = 0.0; vlat = 0.0; vlon = 0.0
    launcher = 0; igps = 1; nplan = 0; ibuf = 0; idsec2 = 0; ierrlev = 0
    ifirst = 0; irollnav = 0; inav = 0; ispec = 0; ierror = 0
    iaveflg = 0; ispd = 0; itime = 0; idayave = 0; imonave = 0; iyerave = 0
    icday1 = 0; iplandir = 0; nlnchr = 0; nextdrop = 0; iplancnt = 0
    iwait = 1; isio_skip_count = 0
    ! GPS time as Seas passes it: 12:05:00 1 June 2024
    chr = 12.0; cmin = 5.0; csec = 0.0; cday = 1.0; cmon = 6.0; cyear = 2024.0

    call siobegin(deadmin, dropmin, relodmin, runsec, xmaxspd, &
         launcher, igps, xlat, xlatload, nplan, ibuf, &
         idsec2, ierrlev, alrmtime, ifirst, irollnav, &
         inav, ispec, dtime, yrday1, ierror, iaveflg, ispd, itime, &
         idayave, imonave, iyerave, icday1, iplandir, &
         speed, dir, timeave, vlat, vlon, &
         nlnchr, nextdrop, iplancnt, iwait, &
         chr, cmin, csec, cday, cmon, cyear, isio_skip_count)

    inquire(file=trim(sdir) // 'Data\sio_probe.txt', exist=probe)
    if (ierror(35) /= 2 .or. probe) then
      print *, 'FAIL test_siobegin_base_fixture: ierror(35)=', ierror(35), &
               ' nextdrop=', nextdrop, ' xlat=', xlat, ' sio_probe.txt written=', probe
      failures = failures + 1
    else
      print *, 'PASS test_siobegin_base_fixture: nextdrop=', nextdrop, ' xlat=', xlat
    end if
  end subroutine test_siobegin_base_fixture

end program test_integration_api
