program tree_driver

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a program to build a hierarchical tree!
! based on toplogical information & arrangement !
! of 3D entities                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Author: C.A.D'Angelo                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Date: 03/31/2014                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use tree_data_mod
use tree_functions_mod

implicit none

type(node), pointer :: new_node ! next node to be inserted 
type(node), pointer :: head     ! head of the tree


write(*,*) "this is what i like"


end program tree_driver
