# This is a makefile for GNU make.
# This makefile builds the library NR_util.

# 1. Source files

sources = nrtype.F90 nr_util.f90 arth.f90 array_copy.f90 swap.f90 reallocate.f90 assert.f90 assert_eq.f90 geop.f90 cumsum.f90 poly.f90 poly_term.f90 outerprod.f90 outerdiff.f90 scatter_add.f90 scatter_max.f90 diagadd.f90 diagmult.f90 get_diag.f90 put_diag.f90 cumprod.f90 ifirstloc.f90 lower_triangle.f90 nrerror.f90 outerand.f90 outerdiv.f90 outersum.f90 unit_matrix.f90 upper_triangle.f90 vabs.f90 zroots_unity.f90

# 2. Objects and library

objects := $(addsuffix .o, $(basename ${sources}))
lib = libnr_util.a
ARFLAGS = rvU

# 3. Compiler-dependent part

FC = gfortran
CPPFLAGS = -DCPP_WP='kind(0.)'
FFLAGS = -O2

# 4. Rules

%.o: %.f90
	$(COMPILE.f) $(OUTPUT_OPTION) $<

%.o: %.F90
	$(COMPILE.F) $(OUTPUT_OPTION) $<

.PHONY: all clean depend
all: ${lib}
${lib}: ${lib}(${objects})

depend depend.mk:
	makedepf90 ${CPPFLAGS} -Wmissing -Wconfused -nosrc ${sources} >depend.mk

clean:
	rm -f ${lib} ${objects}

include depend.mk
