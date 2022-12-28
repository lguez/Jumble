  ! -*- mode: f90; -*-
  ! Body of all the specific procedures of generic interface "csvread".
  
  print *, 'jumble::csvread: Reading data from file "' // file // '"'
  call new_unit(unit)
  open(unit, file=file, status='old', action='read', position='rewind')

  ! Fill non-optional arguments: first and last row, first and last
  ! column which will actually be read, taking information from the
  ! file itself if necessary.

  f_r_loc = opt_merge(first_r, 1)
  f_c_loc = opt_merge(first_c, 1)
  l_r_loc = opt_merge(last_r, 0)
  l_c_loc = opt_merge(last_c, 0)

  if (l_r_loc == 0) then
     call count_lines(unit, l_r_loc)

     if (l_r_loc == 0) then
        print *, 'jumble::csvread: Empty file.'
        stop 1
     end if

     rewind(unit)
  end if

  ! Go to first row to read:
  do i = 1, f_r_loc - 1
     read(unit, fmt=*)
  end do

  if (l_c_loc == 0) then
     call count_values(unit, l_c_loc)
     backspace(unit)
  end if

  print *, 'jumble::csvread: Reading column(s) ', f_c_loc, ':', l_c_loc, &
       ', row(s) ', f_r_loc, ':', l_r_loc
  allocate(a(l_r_loc - f_r_loc + 1, l_c_loc - f_c_loc + 1))

  do i = 1, l_r_loc - f_r_loc + 1
     read(unit, fmt=*, iostat = iostat, iomsg = iomsg) &
          (trash, j = 1, f_c_loc - 1), a(i, :)
     if (iostat /= 0) then
        print *, "jumble::csvread:", trim(iomsg)
        print *, "i = ", i
        print *, "Is the data read numeric?"
        deallocate(a)
        exit
     end if
  end do

  close(unit)
