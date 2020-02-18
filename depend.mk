compare.o : compare.h point.o prt_cmp.o opt_merge.o avg_mag.o 
csvread.o : csvread.h opt_merge.o new_unit.o count_values.o count_lines.o 
read_column.o : read_column.h opt_merge.o new_unit.o count_lines.o 
jumble.o : uniq.o spherical.o set2lin.o read_column.o quadrat.o point.o pr_matrix.o pack_indices.o opt_merge.o new_unit.o iso_varying_string.o inv_mat.o greg2jd.o get_command_arg_dyn.o eigvect.o eigval.o ediff1d.o divisors.o differ_s.o determin.o csvread.o count_values.o count_lines.o compare.o avg_mag.o averge.o argwhere.o 
eigval.o : quadrat.o determin.o 
inv_mat.o : determin.o 
set2lin.o : determin.o 
