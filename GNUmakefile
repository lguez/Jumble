# This is a makefile for GNU make.
# This makefile builds the library Jumble.

# 1. Source files

VPATH = Numerical Numerical/Lin_2d_real

sources = avg_mag.f90 count_lines.f90 opt_merge.f90 point.f90 compare.f90 csvread.f90 new_unit.f90 read_column.f90 jumble.f90 averge.f90 divisors.f90 quadrat.f90 spherical.f90 prep_file.f90 prt_cmp.f90 uniq.f90 ediff1d.f90 pack_indices.f90 argwhere.f90 get_command_arg_dyn.f90 determin.f90 eigval.f90 eigvect.f90 inv_mat.f90 set2lin.f90 pr_matrix.f90 iso_varying_string.f90 differ_s.f90 greg2jd.f90

# 2. Objects and libraries

objects := $(sources:.f90=.o)
lib = libjumble.a
ARFLAGS = rvU

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
