module polymorphic_example
  implicit none

contains

  ! A generic subroutine that prints the type and value of any variable
  subroutine print_anything(x)
    class(*), intent(in) :: x

    select type(x)
    type is (integer)
      print *, "Integer value: ", x
    type is (real)
      print *, "Real value: ", x
    type is (character(len=*))
      print *, "Character value: ", x
    class default
      print *, "Unknown type"
    end select
  end subroutine print_anything

end module polymorphic_example

program test_polymorphism
  use polymorphic_example
  implicit none

  ! Declare variables of different types
  integer :: i
  real :: r
  character(len=20) :: c

  ! Assign values to the variables
  i = 42
  r = 3.14
  c = "Hello, Fortran!"

  ! Call the generic subroutine with different types
  call print_anything(i)
  call print_anything(r)
  call print_anything(c)

end program test_polymorphism
