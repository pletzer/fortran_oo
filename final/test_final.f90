module final_mod
    implicit none

    type :: my_type
      integer :: value
    contains
      final :: finalize_my_type
    end type my_type
  
  contains
  
    subroutine finalize_my_type(this)
      type(my_type), intent(inout) :: this
      print *, "Finalizing my_type with value:", this%value
    end subroutine finalize_my_type
end module final_mod

subroutine test
    use final_mod
    implicit none

    type(my_type) :: obj
    obj%value = 10
    print *, "Value of obj:", obj%value
    ! obj goes out of scope, finalization will be called automatically
end subroutine test
  
program main
    call test
end program main
