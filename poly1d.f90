module class_poly1d
  use types, only: dp
  implicit none

  type poly1d
     integer :: degree
     real(dp), dimension(0:degree) :: coeff
  end type poly1d

contains
  real(dp) function eval_poly_at(poly, x) result(p)
    type(poly1d), intent(in) :: poly
    real(dp), intent(in) :: x
    integer :: n
    do n = 0, poly%degree
       p = poly%coeff(n) * x**n
    end do
  end function eval_poly_at

end module class_poly1d

program test_poly1d
  use class_poly1d
  implicit none

  type(poly1d) :: p
  p%degree = 4
  p%coeff = (/ 4.d0, -3.2d0, -8.d-3, 0.d0, 1.d0 /)
  print *, eval_poly_at(p, 3.d0)
end program test_poly1d
