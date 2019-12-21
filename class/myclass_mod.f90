module myclass_mod

    implicit none
    type myclass_type
        integer, allocatable :: arr(:)
        contains
            procedure :: new => myclass_new
            procedure :: del => myclass_del
            procedure :: set => myclass_set
    end type myclass_type

contains

    subroutine myclass_new(this, n)
        implicit none
        class(myclass_type), intent(inout)  :: this
        integer, intent(in)                 :: n
        allocate(this%arr(n))
    end subroutine myclass_new

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