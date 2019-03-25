  ! -*- mode: f90; -*-
  ! Body of all the specific procedures of generic interface "compare".
  
  call assert(shape(data_old) == shape(data_new), &
       "compare: shapes differ -- " // tag)

  if (quiet) then
     tag_fmt = '(a, ":")'
  else
     tag_fmt = '(/, "' // dashes // '", /, a, ":")'
  end if

  if (size(data_old) == 0) then
     print tag_fmt, tag
     print *, "Zero-sized array"
  else
     if (all(data_old == data_new)) then
        if (report_id) then
           write(unit = *, fmt = tag_fmt, advance = "no") tag
           print *, "arrays are identical."
        end if
     else
        if (quiet) then
           write(unit = *, fmt = tag_fmt, advance = "no") tag
           print *, "arrays are different."
        else
           zero = data_old == 0._wp .or. data_new == 0._wp
           where (.not. zero) rel_diff = abs(data_new / data_old - 1._wp)
           abs_diff = abs(data_new - data_old)

           print tag_fmt, tag

           if (comp_mag) then
              print *
              print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                   avg_mag(data_old)
           end if

           if (any(.not. zero)) then
              print *
              print *, 'Relative difference for non-zero values:'
              location = maxloc(rel_diff, mask = .not. zero)
              call prt_cmp(point(data_old, location), &
                   point(data_new, location), point(rel_diff, location), &
                   location)
           end if

           if (any(zero)) then
              print *
              print *, 'Absolute difference when there is a zero:'
              location = maxloc(abs_diff, mask = zero)
              call prt_cmp(point(data_old, location), &
                   point(data_new, location), point(abs_diff, location), &
                   location)
           end if

           print *
           print *, 'Absolute difference:'
           location = maxloc(abs_diff)
           call prt_cmp(point(data_old, location), &
                point(data_new, location), point(abs_diff, location), &
                location)

           print *
           print "(a, 1pg8.1)", "||data_new - data_old||_infinite / " &
                // "||data_old||_infinite = ", &
                point(abs_diff, location) / maxval(abs(data_old))
        end if
     end if
  end if
