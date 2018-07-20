# This is a makefile for GNU make.
# This makefile builds the library NR_util.

# 1. Objects and library

objects = nrtype.o nr_util.o arth.o array_copy.o swap.o reallocate.o assert.o assert_eq.o geop.o cumsum.o poly.o poly_term.o outerprod.o outerdiff.o scatter_add.o scatter_max.o diagadd.o diagmult.o get_diag.o put_diag.o cumprod.o ifirstloc.o lower_triangle.o nrerror.o outerand.o outerdiv.o outersum.o unit_matrix.o upper_triangle.o vabs.o zroots_unity.o

lib = libnr_util.a

# 2. Compiler-dependent part

FC = gfortran
CPPFLAGS = -DCPP_WP='kind(0.)'
FFLAGS = -ffree-form -O2

# 3. Rules

.PHONY: all clean depend
all: ${lib}
${lib}: ${lib}(${objects})

clean:
	rm -f ${lib} ${objects}

include depend.mk
