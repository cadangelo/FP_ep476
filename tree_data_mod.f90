module tree_data_mod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This module defines a type, node.
! Each node represents a different volume in the tree structure.
! Each node may have all or a few of the following attributes:
! parent, first child, last child, right sibling, left sibling, ID
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  type node
     integer :: id  !volume ID number
     type(node), pointer :: head   !points to head of tree
     type(node), pointer :: parent !points to parent 
     type(node), pointer :: fchild !points to first child
     type(node), pointer :: lchild !points to last child
     type(node), pointer :: rsib   !points to right sibling
     type(node), pointer :: lsib   !points to left sibling
     type(node), pointer :: cn     !points to current node to test
  end type node

  integer, parameter :: rknd = selected_real_kind(16, 10)
  integer, parameter :: iknd = selected_int_kind(16)

end module tree_data_mod
