program test_loadtxt
  use types, only: dp
  use iotools, only: loadtxt
  implicit none 

  real(dp), dimension(:, :), allocatable :: my_data
  integer :: i

  call loadtxt("test.dat", 3, my_data)
  do i = 1, size(my_data, dim=1)
     print *, my_data(i, :)
  end do
  print *, 
  call loadtxt("test.dat", 3, my_data, unit=50)
  do i = 1, size(my_data, dim=1)
     print *, my_data(i, :)
  end do
end program test_loadtxt

  
  
