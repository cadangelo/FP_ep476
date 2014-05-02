module tree_data_mod
! type to define the nodes of the tree

  implicit none

  type node
     integer :: id  !volume ID number
     type(node), pointer :: head   !points to head of tree
     type(node), pointer :: parent !points to parent 
     type(node), pointer :: fchild !points to first child
     type(node), pointer :: lchild !points to last child
     type(node), pointer :: rsib   !points to right sibling
     type(node), pointer :: lsib   !points to left sibling
  end type node

end module tree_data_mod
