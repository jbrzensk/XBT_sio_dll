set PATH=C:\msys64\mingw32\bin;%PATH%
C:\msys64\mingw32\bin\gfortran.exe -shared -o sio.dll -fno-underscoring -fallow-argument-mismatch -static-libgfortran -static-libgcc -Wl,-Bstatic,-lwinpthread,-lquadmath,-Bdynamic src\sio_math.f90 src\sio_convert.f90 src\sio_time.f90 src\sio_io.f90 src\sio_nav.f90 src\sio_core.f90 src\sio_api.f90 > sio-win11-update-compile.log 2>&1
pause
