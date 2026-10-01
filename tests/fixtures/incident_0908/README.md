# 9/8/2026 incident replay fixture

Used by `tests/integration/test_integration_replay.f90`, which replays the
field day through the DLL the way Seas drove it (see `tests/seas_sim.f90`),
in simulated time (`set_test_clock` in `src/sio_time.f90`).

## Files

| File | Source | Notes |
|---|---|---|
| `GpsData20260908072521.txt` | Seas GPS log from the ship, unchanged | PC timestamp + RMC sentence about every 5 s, 07:25:31 to 18:19:15 |
| `field/080926NAV.txt`, `field/stations.txt` | the DLL's .nav and stations.dat from the ship, unchanged | reference only (what happened in the field); not read by the test |
| `Data/080926.nav` | first 61 lines (file order) of `field/080926NAV.txt` | the .nav as written up to 07:25:00, when the log starts |
| `Data/navtrk.dat` | the 07:25:00 DED line of the field .nav, in navtrk format | last position written before the log starts |
| `Data/stations.dat` | drops 001 and 002 of `field/stations.txt` | the drops made before the log starts |
| `Data/control.dat` | this PC's Seas `control.dat` (ship Kalamazoo), operator changed from `debug` to `replay` | **assumption**: the ship's own control.dat was not available |
| `plan.dat` | **STAND-IN**: latitude plan, northbound, a station every 3' from 32 57' N to 33 48' N | the 9/8 plan.dat was not available; drop checks are against this |

Replace `plan.dat` (and `Data/control.dat`) with the real 9/8 files to check
drops against the real plan. `read_plan_lats` in the test expects a
latitude plan; a longitude plan needs that and the "crossed" count adapted.

## How the replay drives the DLL

- The log's PC timestamps drive the test clock; `sioloop` is called every
  simulated second with the latest decoded sentence, as Seas does.
- Sentences are decoded as Seas's `CNmeaStreamDecoder` does: a bad NMEA
  checksum empties the RMC data (no valid time, so Seas skips `sioloop`);
  time, date (ddmmyy -> 20yy), latitude (DD MM.MM + N/S) and longitude
  (DDD MM.MM + E/W) are field-checked. 750 mashed sentences in this log fail
  the checksum and never reach the DLL; the stale sentences (valid checksum,
  GPS time 7-16 minutes old: 07:46, 08:41, 09:22) and the frozen time
  (09:26-09:32) do.
- Minutes are passed with their decimals. (In the Seas source in this repo,
  `SetPositionVariables` does `atof` on a minutes string like `"39 73"`,
  which would give whole minutes only; the field .nav averages have
  fractional minutes, so the ship's Seas evidently passed the decimals.)
- At "drop now" (`ierror(1)=1`) Seas calls `sioend`, launches (240 s here,
  from the gaps in the field .nav around drops: 3-8 min), records the drop in
  `stations.dat` (good, test mode `-3`, at the DLL position) and calls
  `siobegin` once the GPS time is valid.
- Fidelity limit: the log holds one sentence about every 5 s, while Seas saw
  one a second. Between logged sentences the DLL is called with the same
  sentence (same GPS second, `iupdate=0`), which Seas did far less often.
- `keep_sio_log` in the test turns on the DLL's own sio.log (off by default:
  it grows ~15 MB and slows the run from ~9 s to ~5 min).

## Current result (stand-in plan)

15 stations crossed, 15 drops, each with the ship truly past its station
(0.01-0.08 nm, except the first: already past at 07:25), no two drops within
10 minutes: no spurious or cascading drops. The DLL position stays within
0.16 nm of the GPS truth while GPS is healthy.

## Issues this replay found (fixed)

Before the fix the DLL position drifted up to 1.26 nm from the GPS truth
(08:41), and 0.5-0.8 nm for ~40 s after each siobegin:

1. `check_time` accepted a frozen GPS time (Seas repeats the last sentence
   when a receiver stops) as "within 30 s" and re-anchored its reference to
   it, so the prediction stopped advancing. At the 08:34 receiver switch the
   new receiver's times then looked 42 s ahead and were rejected; its
   5-in-a-row re-sync was reset by repeated times, so with 5 s sampling no
   GPS average was made from 08:34 to 08:41. Now a repeated time does not
   move the reference, and a repeated rejected time leaves the run alone.
2. On a call with no update and the same GPS second as the last call,
   `sioloop` skipped dead reckoning and reported the last GPS average
   (after a siobegin, the reloaded position from before the launch). Now it
   dead reckons every call.
