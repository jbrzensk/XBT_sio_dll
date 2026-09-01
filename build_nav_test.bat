set PATH=C:\msys64\mingw32\bin;%PATH%
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_math.f90 -o sio_math.o 2>&1
if errorlevel 1 (echo FAILED sio_math & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_convert.f90 -o sio_convert.o 2>&1
if errorlevel 1 (echo FAILED sio_convert & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_time.f90 -o sio_time.o 2>&1
if errorlevel 1 (echo FAILED sio_time & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch -c src\sio_nav.f90 -o sio_nav.o 2>&1
if errorlevel 1 (echo FAILED sio_nav & exit /b 1)
gfortran --coverage -fno-underscoring -fallow-argument-mismatch tests\unit\test_sio_nav.f90 sio_nav.o sio_math.o sio_time.o sio_convert.o --coverage -o test_sio_nav.exe 2>&1
if errorlevel 1 (echo FAILED link & exit /b 1)
echo BUILD OK
