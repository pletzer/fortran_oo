program test_list
  use list_poly_mod
  implicit none
  type(list), pointer :: lst
  integer, target       :: d0
  real, target          :: d1
  character(10), target :: d3
  complex, target       :: d4
  
  d0 = 10
  d1 = 20
  d3 = "Hi there"
  d4 = cmplx(-1.0,3.0)
  call list_new(lst, d0)
  call list_add(lst, d1)
  call list_add(lst, d3)
  call list_add(lst, d4)
  call list_print(lst)
  call list_del(lst)

end program test_list
