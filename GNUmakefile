# This is a makefile for GNU make.
# This makefile builds the library Jumble.

# 1. Source files

VPATH = Numerical Numerical/Lin_2d_real NR_util

sources = avg_mag.f90 count_lines.f90 opt_merge.f90 point.f90 compare.f90 csvread.f90 new_unit.f90 read_column.f90 jumble.f90 averge.f90 divisors.f90 quadrat.f90 spherical.f90 prt_cmp.f90 uniq.f90 ediff1d.f90 pack_indices.f90 argwhere.f90 get_command_arg_dyn.f90 determin.f90 eigval.f90 eigvect.f90 inv_mat.f90 set2lin.f90 pr_matrix.f90 iso_varying_string.f90 differ_s.f90 greg2jd.f90 count_values.f90 nrtype.F90 arth.f90 array_copy.f90 swap.f90 reallocate.f90 assert.f90 assert_eq.f90 geop.f90 cumsum.f90 poly.f90 poly_term.f90 outerprod.f90 outerdiff.f90 scatter_add.f90 scatter_max.f90 diagadd.f90 diagmult.f90 get_diag.f90 put_diag.f90 cumprod.f90 ifirstloc.f90 lower_triangle.f90 nrerror.f90 outerand.f90 outerdiv.f90 outersum.f90 unit_matrix.f90 upper_triangle.f90 vabs.f90 zroots_unity.f90

# 2. Objects and libraries

objects := $(sources:.f90=.o)
lib = libjumble.a

# 3. Compiler-dependent part

nr_util_inc_dir = ...
FC = gfortran
FFLAGS = -O2 -I${nr_util_inc_dir}

# 4. Rules

%.o: %.f90
	$(COMPILE.f) $(OUTPUT_OPTION) $<

.PHONY: all clean depend
all: ${lib}
${lib}: ${lib}(${objects})

depend depend.mk:
	makedepf90 -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc -u nr_util ${sources} >depend.mk

clean:
	rm -f ${lib} ${objects}

include depend.mk
