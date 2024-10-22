module private_mod

  implicit none

  private

  integer, parameter :: secret = 123
  integer, public, parameter :: rumor = 456

end module private_mod
