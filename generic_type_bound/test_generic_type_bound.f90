module line_mod

  type line_type
      real :: pt1(2)
      real :: pt2(2)
    contains
      procedure :: sc => scale
      procedure :: tr => translate
      generic :: modify => sc, tr
      procedure :: show => show_line
  end type line_type

  contains

  subroutine show_line(this)
      class(line_type), intent(in) :: this
      print *, 'Line ', this%pt1, ' -> ', this%pt2
  end subroutine show_line

  function scale(this, factor) result(res)
      class(line_type), intent(in) :: this
      real, intent(in) :: factor
      type(line_type) :: res
      res%pt1 = this%pt1 * factor
      res%pt2 = this%pt2 * factor
  end function scale

  function translate(this, disp) result(res)
    class(line_type), intent(in) :: this
    real, intent(in) :: disp(2)
    type(line_type) :: res
    res%pt1 = this%pt1 + disp
    res%pt2 = this%pt2 + disp
  end function translate

end module line_mod

program test
  use line_mod
  implicit none
  type(line_type) :: line, new_line, new_line2
  line%pt1 = [0.0, 1.0]
  line%pt2 = [2.0, 3.0]
  ! scale 
  new_line = line%modify(2.0)
  ! new
  new_line2 = new_line%modify([4.0, 5.0])
  call new_line%show()
  call new_line2%show()
  print *, 'SUCCESS'
end program test
