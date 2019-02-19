# This is a makefile for GNU make.
# This makefile builds the library Jumble.

# 1. Source files

VPATH = Numerical Numerical/Lin_2d_real

sources = avg_mag.f count_lines.f opt_merge.f point.f compare.f csvread.f new_unit.f read_column.f jumble.f averge.f divisors.f quadrat.f spherical.f prep_file.f prt_cmp.f uniq.f ediff1d.f pack_indices.f argwhere.f get_command_arg_dyn.f determin.f eigval.f eigvect.f inv_mat.f set2lin.f pr_matrix.f iso_varying_string.f differ_s.f greg2jd.f

# 2. Objects and libraries

objects := $(sources:.f=.o)
lib = libjumble.a

# 3. Compiler-dependent part

nr_util_dir = 
FC = gfortran
FFLAGS = -ffree-form -O2 -I${nr_util_dir}

# 4. Rules

.PHONY: all clean depend
all: ${lib}
${lib}: ${lib}(${objects})

depend depend.mk:
	makedepf90 -free -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc -u nr_util ${sources} >depend.mk

clean:
	rm -f ${lib} ${objects}

include depend.mk
