module list_mod

  use iso_c_binding 

  implicit none

  type list
    type(list), pointer :: first
    type(list), pointer :: next
    integer             :: id
    integer, pointer    :: datum
  end type

  contains

  subroutine list_newitem(this, element, id)
    type(list), pointer :: this
    integer, target     :: element
    integer, intent(in) :: id
    allocate(this)
    this%first => this
    this%next => null()
    this%id = id
    this%datum => element
  end subroutine list_newitem

  subroutine list_new(this, element)
    ! Constructor
    type(list), pointer :: this
    ! initial element to add to list
    integer, target     :: element
    
    call list_newitem(this, element, 0)
  end subroutine list_new

  subroutine list_del(this)
    ! Destructor
    type(list), pointer :: this

    type(list), pointer :: next
    call list_begin(this)
    do while (associated(this%next))
      next => this%next
      next%first => next
      deallocate(this)
      this => next
    enddo
    deallocate(this)
  end subroutine list_del

  subroutine list_begin(this)
    ! Set the iterator to the beginning
    type(list), pointer :: this

    this => this%first
  end subroutine list_begin

  subroutine list_next(this)
    ! Increment the iterator
    type(list), pointer :: this

    this => this%next
  end subroutine list_next

  subroutine list_add(this, element)
    ! Add an element to the end of the list
    type(list), pointer :: this
    ! element to add
    integer, target    :: element

    type(list), pointer :: newnode
    call list_begin(this)
    do while (associated(this%next))
      call list_next(this)
    enddo
    call list_newitem(newnode, element, this%id + 1)
    this%next => newnode
    newnode%first => this%first
  end subroutine list_add

  subroutine list_print(this)
    ! Print the content of the linked list
    type(list), pointer :: this

    call list_begin(this)
    do while (associated(this%next))
      print *,'id: ', this%id, ' datum: ', this%datum, ' first: ', c_loc(this%first), ' next: ', c_loc(this%next)
      call list_next(this)
    enddo
    print *,'id: ', this%id, ' datum: ', this%datum, ' first: ', c_loc(this%first), ' next: ', c_loc(this%next)
  end subroutine list_print

end module list_mod
