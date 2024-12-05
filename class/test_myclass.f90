module myclass_mod


    implicit none

    integer :: ref_count = 0

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
        type(myclass_type)    :: this
        integer, intent(in)    :: n
        allocate(this%arr(n))
        ref_count = ref_count + 1
        print *, 'constructor was called ', ref_count, %LOC(this)
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
        if (allocated(this%arr)) deallocate(this%arr)

        ref_count = ref_count - 1
        print *, 'destructor was called ', ref_count
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
    mc = myclass_type(n)
    print *, %LOC(mc)

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
