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

character(len=80) :: filename

write(*,*) "this is what i like"

filename="nested_vol.h5m"

CALL dagmcinit(filename//char(0),len_trim(filename)) ! setup DAG problem

end program tree_driver
