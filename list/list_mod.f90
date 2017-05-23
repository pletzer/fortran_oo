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

  subroutine list_begin(this)
    type(list), pointer :: this
    this => this%first
  end subroutine list_begin

  subroutine list_next(this)
    type(list), pointer :: this
    if (associated(this%next)) then
      this => this%next
    endif
  end subroutine list_next

  subroutine list_add(this, element)
    type(list), pointer :: this
    integer, pointer    :: element
    type(list), pointer :: newnode
    do while (.not. associated(this%next))
      call list_next(this)
    enddo
    call list_newitem(newnode, this%id + 1)
    this%next => newnode
    newnode%first => this%first
  end subroutine list_add

end module list_mod
