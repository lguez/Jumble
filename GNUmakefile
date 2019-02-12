# This is a makefile for GNU make.
# This makefile builds the library Jumble.

makefile_dir = .
nr_util_dir = ${HOME}/Compil_prod/NR_util_gfortran_debug
include ${general_compiler_options_dir}/settings.mk

# 1. Source files

VPATH += $(addprefix ${makefile_dir}/, Numerical Numerical/Lin_2d_real)

sources = avg_mag.f count_lines.f opt_merge.f point.f compare.f csvread.f new_unit.f read_column.f jumble.f averge.f divisors.f quadrat.f spherical.f prep_file.f prt_cmp.f uniq.f ediff1d.f pack_indices.f argwhere.f get_command_arg_dyn.f determin.f eigval.f eigvect.f inv_mat.f set2lin.f pr_matrix.f iso_varying_string.f differ_s.f greg2jd.f

# 2. Objects and libraries

objects := $(sources:.f=.o)
lib_dyn = libjumble.so
lib_stat = libjumble.a

# 3. Rules

all: ${lib_stat} log
##${lib_dyn}

${lib_dyn}: ${objects}
	$(FC) $(LDFLAGS) ${ldflags_lib_dyn} $^ -o $@

${lib_stat}: ${lib_stat}(${objects})

depend ${makefile_dir}/depend.mk:
	makedepf90 -free -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc $(addprefix -u , nr_util) ${sources} >${makefile_dir}/depend.mk

clean:
	rm -f ${lib_dyn} ${lib_stat} ${objects} log

ifneq ($(MAKECMDGOALS), clobber)
include ${makefile_dir}/depend.mk
endif
