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