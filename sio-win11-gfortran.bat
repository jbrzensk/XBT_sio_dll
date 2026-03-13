set PATH=C:\msys64\mingw32\bin;%PATH%
C:\msys64\mingw32\bin\gfortran.exe -shared -o sio.dll -ffixed-form -fno-underscoring -fallow-argument-mismatch -static-libgfortran -static-libgcc -Wl,-Bstatic,-lwinpthread,-lquadmath,-Bdynamic sio.for siosub.for rdcntrl.for > sio-win11compile.log 2>&1
pause
