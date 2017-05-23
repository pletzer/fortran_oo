program test_list
  use list_mod
  implicit none
  type(list), pointer :: lst
  
  call list_new(lst)
  call list_del(lst)

end program test_list
