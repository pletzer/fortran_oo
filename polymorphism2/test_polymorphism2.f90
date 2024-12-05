module shapes_module
  implicit none

  ! Define an abstract base type
  type, abstract :: shape
    contains
      procedure(area), deferred :: get_area
  end type shape

  ! Define the interface for the area procedure
  abstract interface
    function area(self) result(a)
      import :: shape
      class(shape), intent(in) :: self
      real :: a
    end function area
  end interface

  ! Define a derived type for a circle
  type, extends(shape) :: circle
    real :: radius
  contains
    procedure :: get_area => circle_area
  end type circle

  ! Define a derived type for a rectangle
  type, extends(shape) :: rectangle
    real :: length, width
  contains
    procedure :: get_area => rectangle_area
  end type rectangle

contains

  ! Implementation of the area function for a circle
  function circle_area(self) result(a)
    class(circle), intent(in) :: self
    real :: a
    a = 3.14159 * self%radius**2
  end function circle_area

  ! Implementation of the area function for a rectangle
  function rectangle_area(self) result(a)
    class(rectangle), intent(in) :: self
    real :: a
    a = self%length * self%width
  end function rectangle_area

end module shapes_module

program polymorphism_example
  use shapes_module
  implicit none

  ! Declare polymorphic variable, must be pointer or allocatable
  class(shape), allocatable :: s
  type(circle) :: c
  type(rectangle) :: r

  ! Initialize a circle
  c%radius = 5.0
  allocate(s, source=c)
  print *, "Circle area: ", s%get_area()
  deallocate(s)

  ! Initialize a rectangle
  r%length = 4.0
  r%width = 6.0
  allocate(s, source=r)
  print *, "Rectangle area: ", s%get_area()
  deallocate(s)

end program polymorphism_example
