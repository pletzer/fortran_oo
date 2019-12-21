module basederived_mod

    implicit none

    !
    ! base class
    !
    type :: base_type
        ! members
        integer, allocatable :: arr(:)
        contains
        ! methods
        procedure :: set => base_set
        procedure :: print => base_print  
        final :: base_del
    end type base_type

    ! constructor
    interface base_type
        module procedure base_new
    end interface

    !
    ! derived class
    !
    type, extends(base_type) :: derived_type
        integer :: n
        contains
        procedure :: set => derived_set        
    end type derived_type 

    ! constructor
    interface derived_type
        module procedure derived_new
    end interface

contains

    function base_new(n) result(this)
        implicit none
        type(base_type)                     :: this
        integer, intent(in)                 :: n
        allocate(this%arr(n))
        print *, 'base constructor was called'
    end function base_new

    function derived_new(n) result(this)
        implicit none
        type(derived_type)                  :: this
        integer, intent(in)                 :: n
        allocate(this%arr(n))
        this%n = 10
        print *, 'derived constructor was called'
    end function derived_new

    subroutine base_set(this, vals)
        implicit none
        class(base_type), intent(inout)    :: this
        integer, intent(in)                :: vals(:)
        this%arr = vals
        print *, 'base set method was called'
    end subroutine base_set

    subroutine derived_set(this, vals)
        implicit none
        class(derived_type), intent(inout) :: this
        integer, intent(in)                :: vals(:)
        this%arr = vals
        print *, 'derived set method was called'
    end subroutine derived_set

    subroutine base_print(this)
        implicit none
        class(base_type), intent(in)       :: this
        integer                            :: i
        do i = 1, size(this%arr)
            print *,'i = ', i, ' arr = ', this%arr(i)
        enddo
        print *, 'base print method was called'
    end subroutine base_print

    subroutine base_del(this)
        implicit none
        type(base_type), intent(inout)  :: this
        deallocate(this%arr)
        print *, 'base destructor was called'
    end subroutine base_del

end module basederived_mod
