program associate_example
  implicit none

  ! Declare variables
  real :: a, b, c
  real :: result

  ! Assign values to the variables
  a = 3.0
  b = 4.0
  c = 5.0

  ! Use the associate construct to create aliases
  associate (sum => a + b + c, product => a * b * c)
    ! Calculate the result using the aliases
    result = sum + product
    print *, "Sum: ", sum
    print *, "Product: ", product
    print *, "Result: ", result
  end associate

end program associate_example
