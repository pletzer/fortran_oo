program test_list
  use list_mod
  implicit none
  type(list), pointer :: lst
  integer, target     :: d0
  integer, target     :: d1
  
  call list_new(lst)
  d0 = 1
  call list_add(lst, d0)
  call list_del(lst)

end program test_list
