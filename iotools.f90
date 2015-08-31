module iotools
  use types, only: dp
  implicit none

contains
  subroutine loadtxt(file_name, ncol, raw_data, unit)
    character(len=*), intent(in) :: file_name
    integer, intent(in) :: ncol
    real(dp), dimension(:, :), allocatable, intent(out) ::  raw_data
    integer, optional, intent(in) :: unit

    integer :: u, i, length, io
    real(dp), dimension(ncol) :: dummy

    if (present(unit)) then
       u = unit
    else
       u = 100
    end if
    dummy = 0.d0
    length = 0
    io = 0

    open(unit=u, file=trim(file_name), status='old')
    do 
       read(u, *, iostat=io) dummy(:)
       if (io.ne.0) exit
       length = length + 1
    end do
    rewind(u)
    allocate(raw_data(length, ncol))
    do i = 1, length
       read (u, *) raw_data(i, :)
    end do
    close(u)
  end subroutine loadtxt

  subroutine savetxt(file_name, data_array, unit, safe)
    character(len=*), intent(in) :: file_name
    real(dp), dimension(:, :), intent(in) :: data_array
    integer, optional, intent(in) :: unit
    logical, optional, intent(in) :: safe
    
    character(len=8) :: s
    integer :: u, i
    
    if (present(unit)) then 
       u = unit
    else
       u = 100
    end if
    if (present(safe)) then
       s = 'old'
    else
       s = 'replace'
    end if

    open(unit=u, file=trim(file_name), status=trim(s))
    do i = 1, size(data_array, 1)
       write (u, *) data_array(i, :)
    end do
    close(u)
  end subroutine savetxt
  
    

end module iotools
