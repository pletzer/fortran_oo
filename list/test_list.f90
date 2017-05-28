program test_list
  ! Shows how to create a linked list in Fortran 95
  use list_mod
  implicit none
  type(list), pointer :: lst
  integer, target     :: d0
  integer, target     :: d1
  
  d0 = 10
  d1 = 20
  call list_new(lst, d0)
  call list_add(lst, d1)
  call list_print(lst)
  call list_del(lst)

end program test_list
