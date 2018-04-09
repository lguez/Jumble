# This is a makefile for GNU make.
# This makefile builds the library Jumble.

# 1. Source files

makefile_dir = .
VPATH = ${makefile_dir} ${makefile_dir}/Numerical ${makefile_dir}/Numerical/Lin_2d_real

sources = avg_mag.f count_lines.f opt_merge.f point.f compare.f csvread.f new_unit.f read_column.f jumble.f averge.f get_divisors.f dtridgl.f quadrat.f spherical.f prep_file.f prt_cmp.f uniq.f ediff1d.f pack_indices.f argwhere.f get_command_arg_dyn.f determin.f eigval.f eigvect.f inv_mat.f set2lin.f

# 2. Objects and libraries

objects := $(sources:.f=.o)
lib_dyn = libjumble.so
lib_stat = libjumble.a

# 3. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk

# 4. Rules

SHELL = bash
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${lib_stat} log
##${lib_dyn}

${lib_dyn}: ${objects}
	$(FC) $(LDFLAGS) ${ldflags_lib_dyn} $^ -o $@

${lib_stat}: ${lib_stat}(${objects})

depend ${makefile_dir}/depend.mk:
	makedepf90 -free -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc $(addprefix -u , nr_util) ${sources} >${makefile_dir}/depend.mk

TAGS: ${sources}
	ctags -e --language-force=fortran $^

clean:
	rm -f ${lib_dyn} ${lib_stat} ${objects} log

clobber: clean
	rm -f *.mod ${makefile_dir}/depend.mk TAGS

log:
	hostname >$@
	${FC} ${version_flag} >>$@ 2>&1
	echo -e "\nFC = ${FC}\n\nFFLAGS = ${FFLAGS}\n\nLDFLAGS = ${LDFLAGS}\n\nldflags_lib_dyn = ${ldflags_lib_dyn}" >>$@

ifneq ($(MAKECMDGOALS), clobber)
include ${makefile_dir}/depend.mk
endif
