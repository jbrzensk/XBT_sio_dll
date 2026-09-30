@echo off
set PATH=C:\msys64\mingw32\bin;%PATH%
rem tests use paths relative to the repo root (tests\data, tests\tmp, ...)
cd /d "%~dp0"
echo --- test_sio_math ---
C:\Users\jbrze\github\XBT_sio_dll\test_sio_math.exe
if errorlevel 1 (echo FAILED test_sio_math)
echo --- test_sio_convert ---
C:\Users\jbrze\github\XBT_sio_dll\test_sio_convert.exe
if errorlevel 1 (echo FAILED test_sio_convert)
echo --- test_sio_time ---
C:\Users\jbrze\github\XBT_sio_dll\test_sio_time.exe
if errorlevel 1 (echo FAILED test_sio_time)
echo --- test_sio_nav ---
C:\Users\jbrze\github\XBT_sio_dll\test_sio_nav.exe
if errorlevel 1 (echo FAILED test_sio_nav)
echo --- test_sio_io ---
C:\Users\jbrze\github\XBT_sio_dll\test_sio_io.exe
if errorlevel 1 (echo FAILED test_sio_io)
echo --- test_sio_core ---
C:\Users\jbrze\github\XBT_sio_dll\test_sio_core.exe
if errorlevel 1 (echo FAILED test_sio_core)
for %%t in (test_integration_io test_integration_nav test_integration_core test_integration_api) do (
  echo --- %%t ---
  "%~dp0%%t.exe"
  if errorlevel 1 (echo FAILED %%t)
)
echo ALL DONE
