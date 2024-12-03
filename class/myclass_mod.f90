module myclass_mod

    implicit none
    type myclass_type
    private
        ! members
        integer, allocatable :: arr(:)
        contains
        ! methods
        procedure :: set => myclass_set    
        final :: myclass_del
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
        print *, 'constructor was called'
    end function myclass_new

    subroutine myclass_set(this, vals)
        implicit none
        class(myclass_type), intent(inout) :: this
        integer                            :: vals(:)
        this%arr = vals
        print *, 'set method was called'
    end subroutine myclass_set

    subroutine myclass_del(this)
        implicit none
        type(myclass_type), intent(inout)  :: this
        integer :: ier
        ! some compilers have already deallocated this%arr
        deallocate(this%arr, stat=ier)
        print *, 'destructor was called'
    end subroutine myclass_del

end module myclass_mod