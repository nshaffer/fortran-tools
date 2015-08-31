module error
  implicit none
contains
  subroutine throw(msg, u)
    character(len=*), intent(in) :: msg
    integer, optional, intent(in) :: u
    if (present(u)) then 
       write(u, *) "Error: " // trim(msg)
    else
       print *, "Error: " // trim(msg)
    end if
    stop
  end subroutine throw

  subroutine warn(msg, u)
    character(len=*), intent(in) :: msg
    integer, optional, intent(in) :: u
    if (present(u)) then 
       write(u, *) "Warning: " // trim(msg)
    else
       print *, "Warning: " // trim(msg)
    end if
  end subroutine warn
  
    
end module error
