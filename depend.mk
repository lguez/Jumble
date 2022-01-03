point.o : assert.o 
compare.o : compare.h point.o prt_cmp.o opt_merge.o avg_mag.o assert.o 
csvread.o : csvread.h opt_merge.o new_unit.o count_values.o count_lines.o 
read_column.o : read_column.h opt_merge.o new_unit.o count_lines.o 
jumble.o : zroots_unity.o vabs.o upper_triangle.o unit_matrix.o uniq.o swap.o spherical.o set2lin.o scatter_max.o scatter_add.o reallocate.o read_column.o quadrat.o put_diag.o pr_matrix.o poly_term.o poly.o point.o pack_indices.o outersum.o outerprod.o outerdiv.o outerdiff.o outerand.o opt_merge.o nrtype.o nrerror.o new_unit.o lower_triangle.o iso_varying_string.o inv_mat.o ifirstloc.o greg2jd.o get_diag.o get_command_arg_dyn.o geop.o eigvect.o eigval.o ediff1d.o divisors.o differ_s.o diagmult.o diagadd.o determin.o cumsum.o cumprod.o csvread.o count_values.o count_lines.o compare.o avg_mag.o averge.o assert.o assert_eq.o arth.o array_copy.o argwhere.o 
quadrat.o : assert.o 
eigval.o : quadrat.o determin.o 
inv_mat.o : determin.o 
set2lin.o : determin.o 
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
upper_triangle.o : outerdiff.o arth.o 
vabs.o : nrtype.o 
zroots_unity.o : nrtype.o 
