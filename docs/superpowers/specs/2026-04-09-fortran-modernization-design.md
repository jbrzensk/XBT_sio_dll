# Fortran Modernization Design: sio.for / siosub.for / rdcntrl.for

**Date:** 2026-04-09  
**Target standard:** Fortran 95 (approach stays within F95; no submodules needed at this scale)  
**Compiler:** gfortran (Windows DLL via `!GCC$ ATTRIBUTES DLLEXPORT`)

---

## 1. Goals

Rewrite `sio.for`, `siosub.for`, and `rdcntrl.for` (~9,500 lines, 43 subroutines) in modern Fortran using modules, structured loops, explicit variable initialization, and `IMPLICIT NONE`. The public API (subroutine names and argument lists called by Seas2k) must not change.

---

## 2. Architecture — Approach C: Modules + Thin Public API Wrapper

Seven `.f90` source files, compiled bottom-up by dependency:

```
sio_math.f90       — polynomial fitting utilities (no deps)
sio_convert.f90    — unit/format conversion utilities (no deps)
sio_time.f90       — time and date utilities (no deps)
sio_io.f90         — file I/O: config, nav, plan, station files (USE sio_convert, sio_time)
sio_nav.f90        — navigation and position logic (USE sio_math, sio_time, sio_convert)
sio_core.f90       — core operational routines (USE all above)
sio_api.f90        — thin DLL wrapper: Seas2k entry points only (USE sio_core)
```

`sio_api.f90` is the only file containing `!GCC$ ATTRIBUTES DLLEXPORT`. All other modules are pure Fortran with no platform-specific attributes.

### Module contents

**`sio_math.f90`**
- `DPOLFT` — polynomial least-squares fit (Slatec)
- `DP1VLU` — polynomial evaluation

**`sio_convert.f90`**
- `ch2real` — character to real
- `real2ch` — real to character
- `int2ch` — integer to character
- `dec2deg` — decimal degrees to degrees/minutes
- `deg2dec` — degrees/minutes to decimal degrees
- `findspace` — find whitespace position in string
- `lev` — set logging level from operator name

**`sio_time.f90`**
- `compare` — compare two times
- `dayofw` — day of week
- `gettmtg` — compute GPS timetag
- `findtime` — find time match
- `YRDY` — year/month/day to yearday
- `timetohms` — timetag to hours/minutes/seconds
- `gettim` — get system time
- `getdat` — get system date

**`sio_io.f90`**
- `getdir` — read `siodir.txt` for working directory
- `navopen` — open dated `.nav` file
- `chknav` — check/validate nav file
- `getfilen` — construct dated filename
- `decodeplan` — parse a line from `plan.dat`
- `rdcntrl` — read `control.dat` (absorbed from `rdcntrl.for`)

**`sio_nav.f90`**
- `ave` — average GPS positions, compute speed/direction
- `newpos` — compute new dead-reckoned position
- `xbteta` — compute ETA to next drop position
- `interp` — interpolate between nav positions
- `planinfo` — extract info from plan position
- `chkall` — validate speed and direction values
- `chkbuf` — validate GPS buffer contents
- `chkwrite` — validate lat/lon before writing

**`sio_core.f90`**
- `gpspos` — process incoming GPS position
- `chkprof` — check XBT profile validity
- `wrdrpstn` — write drop to `stations.dat`
- `wrnavfls` — write navigation files
- `prstat` — print/log status
- `wrxmit` — write transmit record
- `seas2s` — process Seas2k secondary data
- `tstwrstn` — test write to stations
- `sioend` — end-of-run cleanup

**`sio_api.f90`** (DLL exports)
- `siobegin` — initialization, called once by Seas2k
- `sioloop` — main loop, called repeatedly by Seas2k
- `SioTimeBegin` — time-plan initialization variant

---

## 3. GOTO Replacement

Three patterns and their replacements:

### Pattern 1: Error-exit GOTO
```fortran
! old
ierror(35) = 307
go to 999
999 continue

! new
ierror(35) = 307
return
```

### Pattern 2: Labeled DO loops
```fortran
! old
do 5 i = 1, 12
  xlatload(i) = 999.0
5 continue

! new
do i = 1, 12
  xlatload(i) = 999.0
end do
```
Inner `GO TO` that skips to loop end → `CYCLE`; that breaks out → `EXIT`.

### Pattern 3: Conditional skip block
```fortran
! old
open(..., err=333)
go to 334
333 ierror(44) = 1
334 continue

! new
open(..., iostat=ios)
if (ios /= 0) then
  ierror(44) = 1
end if
```

---

## 4. Variable Initialization

- `IMPLICIT NONE` in every module — all variables must be declared
- `real*4` → `real`, `integer*4` → `integer`, `integer*2` → `integer` (or `integer(kind=2)` where 2-byte is intentional)
- Scalar initialization at declaration where value is constant:
  ```fortran
  real    :: speed = -0.00009
  real    :: dir   = 0.0
  integer :: ibuf  = 0
  ```
- Whole-array assignment replaces initialization loops:
  ```fortran
  ierror   = 0        ! replaces do 10 i=1,nerr loop
  xlatload = 999.0    ! replaces do 5 i=1,12 loop
  ```
- `SAVE` attributes made explicit where original code relied on implicit module-level `SAVE` (notably `tbuf`, `xltbuf`, `xlnbuf`, `iSIOsave`, `iSIOset` in `ave`)

---

## 5. Testing Strategy

### Directory structure

```
tests/
  unit/
    test_sio_math.f90
    test_sio_convert.f90
    test_sio_time.f90
    test_sio_nav.f90
    test_sio_io.f90
    test_sio_core.f90
  integration/
    test_integration_io.f90
    test_integration_nav.f90
    test_integration_core.f90
  data/
    siodir.txt                    — valid directory pointer
    control.dat                   — valid control file
    control_missing.dat           — (absent by design)
    control_malformed.dat         — bad field format
    plan.dat                      — valid drop plan
    plan_duplicate.dat            — first 2 positions equal
    plan_malformed.dat            — bad coordinate line
    navtrk.dat                    — valid nav track
    navtrk_malformed.dat          — bad nav data
    stations.dat                  — valid stations file
    stations_malformed.dat        — bad station line
    ddmmyy.nav                    — valid dated nav file
    sst.dat                       — valid SST file
```

### Unit tests

Each test is a standalone program. It calls one subroutine with known inputs and checks outputs, printing `PASS` or `FAIL` with the error code and expected vs actual values.

| Test file | Subroutines covered |
|-----------|-------------------|
| `test_sio_math.f90` | `DPOLFT`, `DP1VLU` — polynomial fit of simple dataset, evaluate at known x |
| `test_sio_convert.f90` | `dec2deg`/`deg2dec` round-trip; `ch2real`/`real2ch` round-trip; `int2ch`; `findspace` |
| `test_sio_time.f90` | `timetohms` with boundary values; `YRDY` leap year and year boundary; `compare` before/after/equal cases; day rollover in `gettmtg` |
| `test_sio_nav.f90` | `newpos` with known speed/dir; `chkall` valid and invalid speed/dir; `xbteta` ETA calculation; `chkwrite` valid/invalid coordinates |
| `test_sio_io.f90` | `decodeplan` valid line, malformed line, wrong hemisphere; `getfilen` filename construction for various dates |
| `test_sio_core.f90` | `wrdrpstn` error flag behavior; `prstat` with valid/invalid inputs |

### Integration tests — ierror code coverage

Every `ierror` code set anywhere in the source must be triggered by at least one integration test. Full mapping:

#### File I/O errors (`test_integration_io.f90`)

| ierror | Meaning | Test condition |
|--------|---------|----------------|
| `(2)` | error reading `.nav` file | malformed nav file |
| `(5)` | error opening `.nav` file | missing nav file |
| `(6)` | error writing `.nav` file | write-protected nav file |
| `(7)` | error opening `siodir.txt` | missing `siodir.txt` |
| `(14)` | error writing `navtrk.dat` | write-protected navtrk |
| `(15)` | error opening `control.dat` | missing `control.dat` |
| `(16)` | error reading `control.dat` | malformed `control.dat` |
| `(17)` | error reading `siodir.txt` | malformed `siodir.txt` |
| `(19)` | error opening `plan.dat` | missing `plan.dat` |
| `(20)` | error reading `plan.dat` | malformed `plan.dat` |
| `(21)` | reached end of `plan.dat` | all plan positions consumed |
| `(22)` | first 2 plan positions equal | duplicate positions in `plan.dat` |
| `(23)` | error opening `navtrk.dat` | missing `navtrk.dat` |
| `(24)` | error reading `navtrk.dat` | malformed `navtrk.dat` |
| `(25)` | error opening `stations.dat` | missing `stations.dat` |
| `(26)` | error reading `stations.dat` | malformed `stations.dat` |
| `(29)` | error writing `stations.dat` | write-protected `stations.dat` |
| `(40)` | error reading SST file | malformed SST (non-fatal) |
| `(41)` | error opening `seas2s` file | missing seas2s file |
| `(42)` | error reading `seas2s` file | malformed seas2s file |
| `(43)` | seas2s processing error | seas2s bad data |
| `(44)` | error opening log file | unwritable log path |
| `(45)` | error writing log file | write failure after open |
| `(46)` | seas2s error | seas2s processing |
| `(47)` | seas2s error | seas2s processing |
| `(48)` | error opening `sst.dat` | missing `sst.dat` (non-fatal) |
| `(49)` | error reading `sst.dat` | malformed `sst.dat` (non-fatal) |
| `(50)` | error writing `sst.dat` | write-protected `sst.dat` (non-fatal) |
| `(32)` | drop not found in `stations.dat` | wrong drop number passed |
| `(33)` | operator = "debug" → verbose log | `control.dat` with `operator=debug` |
| `(34)` | day (DD) of nav file | informational, verified alongside `(2)` or `(5)` |
| `(36)` | month (MM) of nav file | informational, verified alongside `(2)` or `(5)` |
| `(37)` | year (YY) of nav file | informational, verified alongside `(2)` or `(5)` |

#### Navigation/position errors (`test_integration_nav.f90`)

| ierror | Meaning | Test condition |
|--------|---------|----------------|
| `(3)` | end of `stations.dat`, no profile | stations exhausted |
| `(4)` | calculated nav values bad | corrupted nav position |
| `(11)` | calculated speed > `xmaxspd` | unrealistic speed in nav input |
| `(12)` | DR lat/lon change too large | bad dead-reckoning jump in `ave` |
| `(28)` | speed/direction values invalid | `chkall` failure path |
| `(30)` | 3rd probe drop in < 10 min | too-frequent drop sequence |
| `(38)` | `jptr` counter from `ave` | verified after `ave` calls |
| `(39)` | `icall` counter from `ave` | verified after `ave` calls |

#### Core control flow (`test_integration_core.f90`)

| ierror | Meaning | Test condition |
|--------|---------|----------------|
| `(1)` | drop now signal | position matches plan position |
| `(8)` | passed DR time limit | GPS alarm time exceeded |
| `(10)` | passed `dropmin` time limit | drop interval exceeded |
| `(13)` | incoming year from Seas bad | year < 2014 passed to `siobegin` |
| `(31)` | no previous profile for `chkprof` | first-run condition |
| `(35)` | watchdog: 2=ok, 3XX=error | verified: 2 on clean run, 307/317/etc on errors |

### Build

A `Makefile` compiles modules in dependency order then links each test program separately. No external framework required — each test program exits with code 0 on all-pass or non-zero on any failure.

---

## 6. Constraints

- Subroutine names and argument lists in `sio_api.f90` must exactly match current signatures (Seas2k calls these as DLL exports)
- `!GCC$ ATTRIBUTES DLLEXPORT` directives live only in `sio_api.f90`
- Windows file path separators (`Data\`) preserved in string literals inside the modules (behavior unchanged)
- `SAVE` semantics on module-level variables preserve existing behavior of `ave` across calls
