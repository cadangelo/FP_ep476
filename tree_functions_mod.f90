module tree_functions_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a module that contains subroutines for inserting          !
! new nodes into the tree                                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use tree_data_mod
  
  contains
    subroutine part_in_cn(part,head,insertion)
    implicit none
    type(node), pointer :: part ! new node to put in tree
    type(node), pointer :: head ! head of the tree
    logical :: insertion        ! if the part was succesfully inserted

    end subroutine part_in_cn
  

     
end module tree_functions_mod
