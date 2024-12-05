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
            procedure :: get => myclass_get
            procedure :: getf => myclass_getf
            procedure :: getf2 => myclass_getf2
            final :: myclass_del
    end type myclass_type

contains

    subroutine myclass_init(this, n)
        implicit none
        class(myclass_type), intent(inout)  :: this
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

    subroutine myclass_get(this, array)
        class(myclass_type) :: this
        integer, allocatable :: array(:)
        allocate(array, source=this%arr)
        print *, 'get method was called'
    end subroutine myclass_get

    function myclass_getf(this) result(array)
        class(myclass_type) :: this
        integer, allocatable :: array(:)
        ! allocate and copy
        allocate(array, source=this%arr)
        print *, 'getf method was called'
    end function myclass_getf

    function myclass_getf2(this) result(array)
        class(myclass_type) :: this
        integer, allocatable :: array(:)
        integer :: ier
        ! sliently deallocate array, even if it is already deallocated
        deallocate(array, stat=ier)
        ! this will automatically allocate array, and then copy
        array = this%arr
        print *, 'getf method was called'
    end function myclass_getf2

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
    integer, allocatable :: vals(:), vals2(:)

    n = 10
    allocate(vals(n))
    vals = 1

    ! call constructor
    call mc%init(n)

    ! call method
    call mc%set(vals)

    call mc%get(vals2)

    ! function
    vals2 = mc%getf()
    vals2 = mc%getf2()

    ! mc will be destroyed when going out of scope
end subroutine

program test_myclass
    implicit none
    ! moved code to a subroutine as destructor might not be called 
    ! from program 
    call test
end program test_myclass