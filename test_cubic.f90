program test_cubic
  use types, only: dp
  use interp, only: cubic_coeff, cubic_eval
  implicit none
  
  integer, parameter :: n = 5
  real(dp), dimension(n) :: xx, yy
  real(dp), dimension(n) :: bb, cc, dd
  integer :: i
  real(dp) :: x, y
  real(dp), dimension(500) :: rr, ss

  xx(1) = 0.d0
  do i = 2, n
     xx(i) = xx(i-1) + 0.5d0
  end do
  yy = sin(xx)

  call cubic_coeff(xx, yy, bb, cc, dd, n)
  
  x = 1.2d0
  y = cubic_eval(x, xx, yy, bb, cc, dd, n) ! Should be 0.93148217019426938
  print *, y

  x = 4.d0
  y = cubic_eval(x, xx, yy, bb, cc, dd, n) ! Should warn and be 0.d0
  print *, y

end program test_cubic

