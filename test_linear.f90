program test_linear
  use types, only: dp
  use interp, only: linear_coeff, linear_eval
  implicit none
  
  integer, parameter :: n = 5
  real(dp), dimension(n) :: xx, yy
  real(dp), dimension(n-1) :: bb
  integer :: i

  do i = 1, n
     xx(i) = 2*3.14159d0/n*i - 3.14159d0
     yy(i) = sin(xx(i))
  end do
  
  call linear_coeff(xx, yy, bb, n)
  ! Should give   0.78195671360706542     
  print *, linear_eval(1.3d0, xx, yy, bb, n)
  ! Should warn and print 0.d0
  print *, linear_eval(4.d0, xx, yy, bb, n)
end program test_linear

