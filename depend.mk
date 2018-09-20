compare.o : point.o avg_mag.o prt_cmp.o 
csvread.o : prep_file.o new_unit.o 
read_column.o : opt_merge.o count_lines.o new_unit.o 
jumble.o : uniq.o spherical.o set2lin.o read_column.o quadrat.o point.o pr_matrix.o pack_indices.o opt_merge.o new_unit.o iso_varying_string.o inv_mat.o get_command_arg_dyn.o eigvect.o eigval.o ediff1d.o dtridgl.o divisors.o differ_s.o determin.o csvread.o count_lines.o compare.o avg_mag.o averge.o argwhere.o 
prep_file.o : opt_merge.o count_lines.o 
eigval.o : quadrat.o determin.o 
inv_mat.o : determin.o 
set2lin.o : determin.o 
