nr_util.o : zroots_unity.o vabs.o upper_triangle.o unit_matrix.o swap.o scatter_max.o scatter_add.o reallocate.o put_diag.o poly_term.o poly.o outersum.o outerprod.o outerdiv.o outerdiff.o outerand.o nrtype.o nrerror.o lower_triangle.o ifirstloc.o get_diag.o geop.o diagmult.o diagadd.o cumsum.o cumprod.o assert.o assert_eq.o arth.o array_copy.o 
swap.o : swap_array.h swap_scalar.h 
scatter_add.o : assert_eq.o 
scatter_max.o : assert_eq.o 
diagadd.o : assert_eq.o 
diagmult.o : assert_eq.o 
get_diag.o : assert_eq.o 
put_diag.o : assert_eq.o 
cumprod.o : nrtype.o 
lower_triangle.o : outerdiff.o arth.o 
outerdiv.o : nrtype.o 
outersum.o : nrtype.o 
unit_matrix.o : nrtype.o 
upper_triangle.o : outerdiff.o arth.o 
vabs.o : nrtype.o 
zroots_unity.o : nrtype.o 
