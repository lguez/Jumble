  if (present(mask)) then
     where (mask)
        dum=a
        a=b
        b=dum
     end where
  else
     dum=a
     a=b
     b=dum
  end if
