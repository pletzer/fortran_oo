module shapes_module
  implicit none

  ! Define a derived type for a rectangle
  type :: rectangle
    real :: length, width
  contains
    procedure :: set_dimensions
    procedure :: get_area
  end type rectangle

contains

  ! Type-bound procedure to set the dimensions of the rectangle
  subroutine set_dimensions(self, l, w)
    class(rectangle), intent(inout) :: self
    real, intent(in) :: l, w
    self%length = l
    self%width = w
  end subroutine set_dimensions

  ! Type-bound procedure to get the area of the rectangle
  function get_area(self) result(area)
    class(rectangle), intent(in) :: self
    real :: area
    area = self%length * self%width
  end function get_area

end module shapes_module

program test_type_bound_procedure
  use shapes_module
  implicit none

  ! Declare a variable of type rectangle
  type(rectangle) :: r

  ! Set the dimensions of the rectangle
  call r%set_dimensions(4.0, 6.0)

  ! Get and print the area of the rectangle
  print *, "Rectangle area: ", r%get_area()

end program test_type_bound_procedure
