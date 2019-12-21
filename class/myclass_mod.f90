module myclass_mod

    implicit none
    type myclass_type
        integer, allocatable :: arr(:)
        contains
            procedure :: del => myclass_del
            procedure :: set => myclass_set
    end type myclass_type

    ! constructor
    interface myclass_type
        module procedure myclass_new
    end interface

contains

    function myclass_new(n) result(this)
        implicit none
        type(myclass_type)                  :: this
        integer, intent(in)                 :: n
        allocate(this%arr(n))
    end function myclass_new

    subroutine myclass_set(this, vals)
        implicit none
        class(myclass_type), intent(inout) :: this
        integer                            :: vals(:)
        this%arr = vals
    end subroutine myclass_set

    subroutine myclass_del(this)
        implicit none
        class(myclass_type), intent(inout) :: this
        deallocate(this%arr)
    end subroutine myclass_del

end module myclass_mod