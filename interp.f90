module interp
  use error, only: throw, warn
  use types, only: dp
  implicit none
  
contains
  subroutine linear_coeff(xx, yy, bb, n)
    ! Computes the N-1 coefficients of the piecewise linear interpolant
    ! to N points (x, y).
    !   S_i(x) = y_i + b_i*(x-x_i)
    integer, intent(in) :: n
    real(dp), dimension(n), intent(in) :: xx, yy
    real(dp), dimension(n-1), intent(out) :: bb

    integer :: i

    if (n.lt.2) call throw("Linear interpolation requires at least two points.")
    do i = 1, n-1
       bb(i) = (yy(i+1) - yy(i))/(xx(i+1) - xx(i))
    end do
  end subroutine linear_coeff

  real(dp) function linear_eval(x, xx, yy, bb, n) result(s)
    ! Evaluates the piecewise linear interpolant
    !   S_i(x) = y_i + b_i*(x-x_i)
    ! at a point x.
    integer, intent(in) :: n
    real(dp), intent(in) :: x
    real(dp), dimension(n), intent(in) :: xx, yy
    real(dp), dimension(n-1), intent(in) :: bb

    integer :: i

    if ( (x.lt.xx(1)).or.(x.gt.xx(n)) ) then
       call warn("Requested abcissa lies outside the given range. Defaulting to 0.d0.")
       s = 0.d0
    else
       i = isearch(x, xx, n)
       s = yy(i) + bb(i)*(x - xx(i))
    end if
  end function linear_eval
    
  subroutine cubic_coeff(xx, yy, bb, cc, dd, n)
    ! Computes the N coefficients of the natural cubic spline fitting
    ! the set of N knots (xx, y). 
    !   S_i(xx) = y_i + b_i*(xx-xx_i) + c_i*(xx-xx_i)**2 + d_i*(xx-xx_i)**3
    !
    ! I don't really like that this returns one more coefficient than is
    ! necessary, but it doesn't seems like a big deal either.
    integer, intent(in) :: n
    real(dp), dimension(n), intent(in) :: xx, yy
    real(dp), dimension(n), intent(out) :: bb, cc, dd

    real(dp) :: h
    integer :: i, j, nm1
    h = 0.d0
    nm1 = n-1

    ! check input
    if (n.lt.3) call throw("Cubic spline requires at least three points.")
    !
    ! step 1: preparation
    !
    dd(1) = xx(2) - xx(1)
    cc(2) = (yy(2) - yy(1))/dd(1)
    do i = 2, nm1
       dd(i) = xx(i+1) - xx(i)
       bb(i) = 2.0*(dd(i-1) + dd(i))
       cc(i+1) = (yy(i+1) - yy(i))/dd(i)
       cc(i) = cc(i+1) - cc(i)
    end do
    !
    ! step 2: end conditions 
    !
    bb(1) = -dd(1)
    bb(n) = -dd(nm1)
    cc(1) = 0.d0
    cc(n) = 0.d0
    if (n.ne.3) then
       cc(1) = cc(3)/(xx(4)-xx(2)) - cc(2)/(xx(3)-xx(1))
       cc(n) = cc(n-1)/(xx(n)-xx(n-2)) - cc(n-2)/(xx(n-1)-xx(n-3))
       cc(1) = cc(1)*dd(1)**2/(xx(4)-xx(1))
       cc(n) = -cc(n)*dd(n-1)**2/(xx(n)-xx(n-3))
    end if
    !
    ! step 3: forward elimination 
    !
    do i = 2, n
       h = dd(i-1)/bb(i-1)
       bb(i) = bb(i) - h*dd(i-1)
       cc(i) = cc(i) - h*cc(i-1)
    end do
    !
    ! step 4: back substitution
    !
    cc(n) = cc(n)/bb(n)
    do j = 1, nm1
       i = n-j
       cc(i) = (cc(i) - dd(i)*cc(i+1))/bb(i)
    end do
    !
    ! step 5: compute spline coefficients
    !
    bb(n) = (yy(n) - yy(nm1))/dd(nm1) + dd(nm1)*(cc(nm1) + 2.d0*cc(n))
    do i = 1, nm1
       bb(i) = (yy(i+1) - yy(i))/dd(i) - dd(i)*(cc(i+1) + 2.d0*cc(i))
       dd(i) = (cc(i+1) - cc(i))/dd(i)
       cc(i) = 3.d0*cc(i)
    end do
    cc(n) = 3.d0*cc(n)
    dd(n) = dd(nm1)
  end subroutine cubic_coeff
  
  real(dp) function cubic_eval(x, xx, yy, bb, cc, dd, n) result(s)
    ! Evaluates the cubic spline
    !   S_i(x) = y_i + b_i*(x-x_i) + c_i*(x-x_i)**2 + d_i*(x-x_i)**3
    ! at a point x.
    integer, intent(in) :: n
    real(dp), intent(in) :: x
    real(dp), dimension(n), intent(in) :: xx, yy
    real(dp), dimension(n), intent(in) :: bb, cc, dd
    integer :: i
    if ( (x.lt.xx(1)).or.(x.gt.xx(n)) ) then
       s = 0.d0
       call warn("Requested abcissa lies outside the given range. Defaulting to 0.d0.")
    else
       i = isearch(x, xx, n)
       s = yy(i) + bb(i)*(x-xx(i)) + cc(i)*(x-xx(i))**2 + dd(i)*(x-xx(i))**3
    end if
  end function cubic_eval

  integer function isearch(x, xx, n) result(i)
    ! Returns 'i' such that xx lies between xx(i) and xx(i+1).
    ! Assumes that there is only one such 'i'.
    integer, intent(in) :: n
    real(dp), intent(in) :: x
    real(dp), dimension(n), intent(in) :: xx

    integer :: j, k

    i = 1
    j = n+1

    do while (j.gt.i+1)
       k = (i + j)/2
       if (x.lt.xx(k)) then
          j = k
       else
          i = k
       end if
    end do
  end function isearch
  
end module interp
