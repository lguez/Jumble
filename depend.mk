compare.o : point.o avg_mag.o prt_cmp.o 
csvread.o : prep_file.o new_unit.o 
read_column.o : opt_merge.o new_unit.o 
jumble.o : uniq.o spherical.o read_column.o quadrat.o point.o pack_indices.o opt_merge.o new_unit.o get_divisors.o get_command_arg_dyn.o ediff1d.o dtridgl.o csvread.o count_lines.o compare.o avg_mag.o averge.o argwhere.o 
prep_file.o : opt_merge.o 
