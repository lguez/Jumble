# This is a makefile for GNU make.
# This makefile builds the library NR_util.

# 1. Source files

sources = nrtype.F nr_util.f arth.f array_copy.f swap.f reallocate.f assert.f assert_eq.f geop.f cumsum.f poly.f poly_term.f outerprod.f outerdiff.f scatter_add.f scatter_max.f diagadd.f diagmult.f get_diag.f put_diag.f cumprod.f ifirstloc.f lower_triangle.f nrerror.f outerand.f outerdiv.f outersum.f unit_matrix.f upper_triangle.f vabs.f zroots_unity.f

# 2. Objects and library

objects := $(addsuffix .o, $(basename ${sources}))
lib = libnr_util.a

# 3. Compiler-dependent part

FC = gfortran
CPPFLAGS = -DCPP_WP='kind(0.)'
FFLAGS = -ffree-form -O2

# 4. Rules

.PHONY: all clean depend
all: ${lib}
${lib}: ${lib}(${objects})

depend depend.mk:
	makedepf90 ${CPPFLAGS} -free -Wmissing -Wconfused -nosrc ${sources} >depend.mk

clean:
	rm -f ${lib} ${objects}

include depend.mk
