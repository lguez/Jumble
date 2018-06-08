# This is a makefile for GNU make.
# This makefile builds the library NR_util.

# 1. Source files

VPATH = .

sources = nrtype.F nr_util.f iminloc.f arth.f array_copy.f swap.f reallocate.f imaxloc.f assert.f assert_eq.f geop.f cumsum.f poly.f poly_term.f outerprod.f outerdiff.f scatter_add.f scatter_max.f diagadd.f diagmult.f get_diag.f put_diag.f cumprod.f ifirstloc.f lower_triangle.f nrerror.f outerand.f outerdiv.f outersum.f unit_matrix.f upper_triangle.f vabs.f zroots_unity.f

cpp_macros = CPP_WP='kind(0.)'

# 2. Objects and libraries

objects := $(addsuffix .o, $(basename ${sources}))
lib_dyn = libnr_util.so
lib_stat = libnr_util.a

# 3. Rules

all: ${lib_stat} log
##${lib_dyn}
include ${general_compiler_options_dir}/settings.mk

${lib_dyn}: ${objects}
	$(FC) $(LDFLAGS) ${ldflags_lib_dyn} $^ -o $@

${lib_stat}: ${lib_stat}(${objects})

depend ${VPATH}/depend.mk:
	makedepf90 $(addprefix -D, ${cpp_macros}) -free -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc ${sources} >${VPATH}/depend.mk

clean:
	rm -f ${lib_dyn} ${lib_stat} ${objects} log

clobber: clean
	rm -f *.mod ${VPATH}/depend.mk

ifneq ($(MAKECMDGOALS), clobber)
include ${VPATH}/depend.mk
endif
