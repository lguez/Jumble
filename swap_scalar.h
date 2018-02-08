  if (present(mask)) then
     if (mask) then
        dum=a
        a=b
        b=dum
     end if
  else
     dum=a
     a=b
     b=dum
  end if
