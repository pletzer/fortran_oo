module inheritance_module
    implicit none
  
    type :: base_class
      integer :: base_value
    contains
      procedure :: base_init
      final :: finalize_base
    end type base_class
  
    type, extends(base_class) :: derived_class
      integer :: derived_value
    contains
      procedure :: derived_init
      final :: finalize_derived
    end type derived_class
  
  contains
  
    subroutine base_init(this, value)
      class(base_class), intent(inout) :: this
      integer, intent(in) :: value
      this%base_value = value
      print *, "Base class initialized with value:", this%base_value
    end subroutine base_init
  
    subroutine derived_init(this, base_value, derived_value)
      class(derived_class), intent(inout) :: this
      integer, intent(in) :: base_value, derived_value
      call this%base_init(base_value)
      this%derived_value = derived_value
      print *, "Derived class initialized with value:", this%derived_value
    end subroutine derived_init
  
    subroutine finalize_base(this)
      type(base_class), intent(inout) :: this
      print *, "Finalizing base class with value:", this%base_value
    end subroutine finalize_base
  
    subroutine finalize_derived(this)
      type(derived_class), intent(inout) :: this
      print *, "Finalizing derived class with value:", this%derived_value
      call finalize_base(this%base_class)  ! Correctly call the base class finalization
    end subroutine finalize_derived
  
  end module inheritance_module

subroutine test
    use inheritance_module
    implicit none
  
    type(derived_class) :: obj
  
    call obj%derived_init(10, 20)
    ! obj goes out of scope here, triggering finalization
  end subroutine test
  
program main
    call test
end program main
  
  