module basederived_mod

    implicit none

    !
    ! base class
    !
    type, extensible :: base_type
    private
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
    private
        integer :: n
        contains
        procedure :: print => derived_print        
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

    subroutine base_set(this, vals)
        implicit none
        class(base_type), intent(inout)    :: this
        integer                            :: vals(:)
        this%arr = vals
        print *, 'base set method was called'
    end subroutine base_set

    subroutine base_del(this)
        implicit none
        type(myclass_type), intent(inout)  :: this
        deallocate(this%arr)
        print *, 'base destructor was called'
    end subroutine base_del

end module basederived_mod