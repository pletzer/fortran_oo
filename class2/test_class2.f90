module myclass_mod

    implicit none
    type myclass_type
    private
        ! members
        integer, allocatable :: arr(:)
        contains
            ! methods
            procedure :: init => myclass_init
            procedure :: set => myclass_set    
            final :: myclass_del
    end type myclass_type

contains

    subroutine myclass_init(this, n)
        implicit none
        class(myclass_type), intent(inout)               :: this
        integer, intent(in)                 :: n
        allocate(this%arr(n))
        print *, 'constructor was called'
    end subroutine myclass_init

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

subroutine test()
    use myclass_mod
    implicit none
    type(myclass_type)   :: mc
    integer              :: n
    integer, allocatable :: vals(:)

    n = 10
    allocate(vals(n))
    vals = 1

    ! call constructor
    call mc%init(n)

    ! call method
    call mc%set(vals)

    ! mc will be destroyed when going out of scope
end subroutine

program test_myclass
    implicit none
    ! moved code to a subroutine as destructor might not be called 
    ! from program 
    call test
end program test_myclass