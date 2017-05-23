module list_mod

  implicit none

  type list
    type(list), pointer :: first
    type(list), pointer :: next
    integer             :: id
    integer, pointer    :: datum
  end type

  contains

  subroutine list_newitem(this, id)
    type(list), pointer :: this
    integer, intent(in) :: id
    allocate(this)
    this%first => this
    this%next => null()
    this%id = id
    this%datum => null()
  end subroutine list_newitem

  subroutine list_new(this)
    type(list), pointer :: this
    call list_newitem(this, 0)
  end subroutine list_new

  subroutine list_del(this)
    type(list), pointer :: this
    deallocate(this)
  end subroutine list_del

end module list_mod
