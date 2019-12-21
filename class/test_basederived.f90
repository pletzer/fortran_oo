subroutine test()
    use basederived_mod
    implicit none
    type(base_type)      :: b
    integer              :: n
    integer, allocatable :: vals(:)

    n = 10
    allocate(vals(n))
    vals = 1

    ! call constructor
    b = base_type(n)

    ! call method
    call b%set(vals)

    ! b will be destroyed when going out of scope
end subroutine

program test_basederived
    implicit none
    ! moved code to a subroutine as destructor might not be called 
    ! from program 
    call test
end program test_basederived