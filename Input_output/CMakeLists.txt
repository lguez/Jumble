target_sources(jumble PRIVATE compare.f90 count_values.f90
  get_command_arg_dyn.f90 prt_cmp.f90 compare.h csvread.f90 new_unit.f90
  read_column.f90 count_lines.f90 csvread.h pr_matrix.f90 read_column.h
  read_opcol.f90 readline.f90)

add_executable(test_csvread EXCLUDE_FROM_ALL test_csvread.f90)
target_link_libraries(test_csvread PRIVATE jumble)

add_executable(test_readline EXCLUDE_FROM_ALL test_readline.f90)
target_link_libraries(test_readline PRIVATE jumble)

add_executable(test_read_column EXCLUDE_FROM_ALL test_read_column.f90)
target_link_libraries(test_read_column PRIVATE jumble)

add_dependencies(tests_jumble test_csvread test_readline test_read_column)
