module shapes_module
  implicit none

  ! Define an abstract base type
  type, abstract :: shape
  end type shape

  ! Define a derived type for a circle
  type, extends(shape) :: circle
    real :: radius
  end type circle

  ! Define a derived type for a rectangle
  type, extends(shape) :: rectangle
    real :: length, width
  end type rectangle

contains

  ! A subroutine to print the area of a shape using select type
  subroutine print_area(s)
    class(shape), intent(in) :: s
    real :: area

    select type(s)
    type is (circle)
      area = 3.14159 * s%radius**2
      print *, "Circle area: ", area
    type is (rectangle)
      area = s%length * s%width
      print *, "Rectangle area: ", area
    class default
      print *, "Unknown shape"
    end select
  end subroutine print_area

end module shapes_module

program test_select_type
  use shapes_module
  implicit none

  ! Declare polymorphic variables
  class(shape), allocatable :: s
  type(circle) :: c
  type(rectangle) :: r

  ! Initialize a circle
  c%radius = 5.0
  allocate(s, source=c)
  call print_area(s)
  deallocate(s)

  ! Initialize a rectangle
  r%length = 4.0
  r%width = 6.0
  allocate(s, source=r)
  call print_area(s)
  deallocate(s)

end program test_select_type
