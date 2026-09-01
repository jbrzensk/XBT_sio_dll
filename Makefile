# Makefile for XBT SIO DLL modernization
FC      = gfortran
# Free-form source, no -ffixed-form. -fno-underscoring preserves symbol names for DLL.
FFLAGS  = -Wall -Wextra -fno-underscoring -fallow-argument-mismatch
MODDIR  = src
TESTDIR = tests

# Module objects in strict dependency order
MOD_OBJS = \
    sio_math.o \
    sio_convert.o \
    sio_time.o \
    sio_io.o \
    sio_nav.o \
    sio_core.o

# Unit test programs
UNIT_TESTS = \
    test_sio_math \
    test_sio_convert \
    test_sio_time \
    test_sio_nav \
    test_sio_io \
    test_sio_core

# Integration test programs
INT_TESTS = \
    test_integration_io \
    test_integration_nav \
    test_integration_core

.PHONY: all dll unit_tests integration_tests clean run_unit run_integration run_all

all: dll unit_tests integration_tests

# --- DLL ---
dll: sio.dll

sio.dll: $(MOD_OBJS) sio_api.o
	$(FC) $(FFLAGS) -shared -o sio.dll $(MOD_OBJS) sio_api.o \
	    -static-libgfortran -static-libgcc \
	    -Wl,-Bstatic,-lwinpthread,-lquadmath,-Bdynamic

# --- Module compilation (order matters) ---
sio_math.o: $(MODDIR)/sio_math.f90
	$(FC) $(FFLAGS) -c $< -o $@

sio_convert.o: $(MODDIR)/sio_convert.f90
	$(FC) $(FFLAGS) -c $< -o $@

sio_time.o: $(MODDIR)/sio_time.f90
	$(FC) $(FFLAGS) -c $< -o $@

sio_io.o: $(MODDIR)/sio_io.f90 sio_convert.o sio_time.o
	$(FC) $(FFLAGS) -c $< -o $@

sio_nav.o: $(MODDIR)/sio_nav.f90 sio_math.o sio_time.o sio_convert.o
	$(FC) $(FFLAGS) -c $< -o $@

sio_core.o: $(MODDIR)/sio_core.f90 $(MOD_OBJS:sio_core.o=)
	$(FC) $(FFLAGS) -c $< -o $@

sio_api.o: $(MODDIR)/sio_api.f90 sio_core.o
	$(FC) $(FFLAGS) -c $< -o $@

# --- Unit tests ---
unit_tests: $(UNIT_TESTS)

test_sio_math: $(TESTDIR)/unit/test_sio_math.f90 sio_math.o
	$(FC) $(FFLAGS) $^ -o $@

test_sio_convert: $(TESTDIR)/unit/test_sio_convert.f90 sio_convert.o
	$(FC) $(FFLAGS) $^ -o $@

test_sio_time: $(TESTDIR)/unit/test_sio_time.f90 sio_time.o
	$(FC) $(FFLAGS) $^ -o $@

test_sio_nav: $(TESTDIR)/unit/test_sio_nav.f90 sio_nav.o sio_math.o sio_time.o sio_convert.o
	$(FC) $(FFLAGS) $^ -o $@

test_sio_io: $(TESTDIR)/unit/test_sio_io.f90 sio_io.o sio_convert.o sio_time.o
	$(FC) $(FFLAGS) $^ -o $@

test_sio_core: $(TESTDIR)/unit/test_sio_core.f90 $(MOD_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

# --- Integration tests ---
integration_tests: $(INT_TESTS)

test_integration_io: $(TESTDIR)/integration/test_integration_io.f90 $(MOD_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

test_integration_nav: $(TESTDIR)/integration/test_integration_nav.f90 $(MOD_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

test_integration_core: $(TESTDIR)/integration/test_integration_core.f90 $(MOD_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

# --- Run all tests ---
run_unit: unit_tests
	@for t in $(UNIT_TESTS); do echo "--- $$t ---"; ./$$t || exit 1; done

run_integration: integration_tests
	@for t in $(INT_TESTS); do echo "--- $$t ---"; ./$$t || exit 1; done

run_all: run_unit run_integration

clean:
	rm -f *.o *.mod *.dll $(UNIT_TESTS) $(INT_TESTS)
