  ! -*- mode: f90; -*-

  ! Body of all the specific procedures of generic interfaces
  ! "read_column" and "read_opcol".
  
  ! Determine first and last line which will actually be read, taking
  ! information from the file itself if necessary.
  
  skiprows_not_opt = opt_merge(skiprows, 0)

  if (present(nrows)) then
     last_not_opt = skiprows_not_opt + nrows
  else
     call count_lines(unit, last_not_opt)

     if (last_not_opt == 0) then
        write(ERROR_UNIT, fmt = *) &
             'Jumble:read_column or read_opcol: Empty file.'
        stop 1
     end if
     
     rewind(unit)
  end if

  ! Go to first line to read:
  do i = 1, skiprows_not_opt
     read(unit, fmt=*)
  end do
  
  my_lbound_not_opt = opt_merge(my_lbound, 1)
  allocate(a(my_lbound_not_opt:my_lbound_not_opt + last_not_opt &
       - skiprows_not_opt - 1))
  read(unit, fmt=*) a
