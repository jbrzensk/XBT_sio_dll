@echo off
set PATH=C:\msys64\mingw32\bin;%PATH%

del /Q *.gcda *.gcno *.o *.mod *.exe 2>nul

echo Building modules...
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_math.f90 -o sio_math.o
if errorlevel 1 (echo FAILED sio_math & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_convert.f90 -o sio_convert.o
if errorlevel 1 (echo FAILED sio_convert & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_time.f90 -o sio_time.o
if errorlevel 1 (echo FAILED sio_time & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_io.f90 -o sio_io.o
if errorlevel 1 (echo FAILED sio_io & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_nav.f90 -o sio_nav.o
if errorlevel 1 (echo FAILED sio_nav & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_core.f90 -o sio_core.o
if errorlevel 1 (echo FAILED sio_core & exit /b 1)

echo Building test executables...
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_math.f90 sio_math.o --coverage -o test_sio_math.exe
if errorlevel 1 (echo FAILED test_sio_math link & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_convert.f90 sio_convert.o --coverage -o test_sio_convert.exe
if errorlevel 1 (echo FAILED test_sio_convert link & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_time.f90 sio_time.o --coverage -o test_sio_time.exe
if errorlevel 1 (echo FAILED test_sio_time link & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_nav.f90 sio_nav.o sio_math.o sio_time.o sio_convert.o --coverage -o test_sio_nav.exe
if errorlevel 1 (echo FAILED test_sio_nav link & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_io.f90 sio_io.o sio_convert.o sio_time.o --coverage -o test_sio_io.exe
if errorlevel 1 (echo FAILED test_sio_io link & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_core.f90 sio_math.o sio_convert.o sio_time.o sio_io.o sio_nav.o sio_core.o --coverage -o test_sio_core.exe
if errorlevel 1 (echo FAILED test_sio_core link & exit /b 1)

echo BUILD OK
