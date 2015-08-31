module error
  implicit none
contains
  subroutine throw(msg, unit)
    character(*), intent(in) :: msg
    integer, optional, intent(in) :: unit
    if (present(unit)) then
       write(unit, *) "Error: " // trim(msg)
    else
       print *, "Error: " // trim(msg)
    end if
    stop
  end subroutine throw

  subroutine warn(msg, unit)
    character(*), intent(in) :: msg
    integer, optional, intent(in) :: unit
    if (present(unit)) then
       write(unit, *) "Warning: " // trim(msg)
    else
       print *, "Warning: " // trim(msg)
    end if
  end subroutine warn
  
    
end module error
