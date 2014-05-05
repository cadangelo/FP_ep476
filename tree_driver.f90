module tree_functions_mod
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! This is a module that contains subroutines for inserting          !
  ! new nodes into the tree                                           !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use tree_data_mod
  
  contains  
    subroutine create_node(id, new_node)
    
    use tree_data_mod
    implicit none

    integer :: id
    type(node), pointer, intent(inout)  :: new_node
  
    ALLOCATE(new_node) 
    nullify(new_node%head)
    nullify(new_node%parent)
    nullify(new_node%fchild)
    nullify(new_node%lchild)
    nullify(new_node%rsib)
    nullify(new_node%lsib)
    nullify(new_node%cn)

    new_node%id=id
  
    return  
    end subroutine create_node
    
    subroutine part_in_cn(cn, part, insertion)
  
    use tree_data_mod

    type(node), pointer, intent(inout)  :: part 
    type(node), pointer, intent(inout) :: cn
  
    logical :: insertion

    write(*,*) 'Calling part_in_cn ...'

    if (cn%fchild%id .gt. 0) then
       cn => cn%fchild
       insertion = .false.
    else
       part%parent => cn
       cn%fchild => part
       cn%lchild => part
       nullify(part%rsib)
       nullify(part%fchild)
       nullify(part%lchild)
       nullify(part%lsib)
       insertion = .true.
    endif

    write(*,*) 'insertion is ...', insertion
    end subroutine part_in_cn


    subroutine cn_in_part(cn, part, insertion)
  
    use tree_data_mod

    type(node), pointer, intent(inout)  :: part 
    type(node), pointer, intent(inout) :: cn
  
    logical :: insertion

    write(*,*) 'Calling cn_in_part ...'

    if (cn%parent%fchild%id .eq. cn%id) then

      ! cn is only child
      if (cn%parent%lchild%id .eq. cn%id) then
        cn%parent%fchild => part
        cn%parent%lchild => part
        part%parent => cn%parent
        part%fchild => cn
        part%lchild => cn
        cn%parent => part
        nullify(part%lsib)
        nullify(part%rsib)
        insertion = .true.

      ! cn is first child
      else
        cn%parent%fchild => cn%rsib
        part%parent => cn%parent
        cn%parent%lchild%rsib => part
        part%lsib => cn%parent%lchild
        part%fchild => cn
        part%lchild => cn
        cn%parent => part
        cn => cn%rsib
        nullify(cn%lsib%rsib)
        nullify(cn%lsib)
      endif

    ! cn is last child
    elseif (cn%parent%lchild%id .eq. cn%id) then 
      cn%parent%lchild => part
      part%parent => cn%parent
      cn%parent => part
      cn%lsib%rsib => part
      part%lsib => cn%lsib
      part%fchild => cn
      part%lchild => cn
      nullify(cn%lsib)
      nullify(cn%rsib)
      insertion = .true.
    
    ! cn is a middle child
    else
      cn%rsib%lsib => cn%lsib
      cn%lsib%rsib => cn%rsib
      cn%parent%lchild%rsib => part
      part%lsib => cn%parent%lchild !check this
      cn%parent%lchild => part
      part%parent => cn%parent
      part%fchild => cn
      part%lchild => cn
      cn%parent => part
      nullify(part%rsib) 
      insertion = .true.

    endif


    end subroutine cn_in_part


    subroutine cn_part_siblings(cn, part, insertion)
  
    use tree_data_mod

    type(node), pointer, intent(inout)  :: part 
    type(node), pointer, intent(inout) :: cn
  
    logical :: insertion
    
    write(*,*) 'Calling cn_part_siblings ...'
    write (*,*) 'cn rsib id',cn%rsib%id
    
    if (cn%rsib%id .gt. 0) then 
      cn => cn%rsib
    else
      cn%rsib => part
      part%lsib => cn
      nullify(part%rsib)
      part%parent => cn%parent !check this
      nullify(part%fchild)
      nullify(part%lchild)
      insertion = .true.
    endif

    end subroutine cn_part_siblings
    
end module tree_functions_mod

module tree_insertion_mod
contains
  subroutine insert_in_tree(part, head)
    use tree_data_mod
    use tree_functions_mod
    use volume_functions_mod
    
    implicit none
    
    type(node), pointer, intent(inout) :: head
    type(node), pointer, intent(inout) :: part 
    type(node), pointer :: cn
    
    logical :: insertion ! True if new part inserted into tree 
    logical :: inside    ! T/F result from A in B query
    !  logical :: is_A_in_B 
    
    
    cn => head
    
    write(*,*) cn%id
    write(*,*) part%id
    !write(*,*) 'part and head', part%id, head%id 
    insertion = .false.
    
    do while (insertion .eqv. .false.)
       
       if ( is_A_in_B(part, cn) .eqv. .true. ) then
         CALL part_in_cn(cn, part, insertion)
       else 
         if ( is_A_in_B(cn, part) .eqv. .true. ) then 
           CALL cn_in_part(cn, part, insertion)
         else
           CALL cn_part_siblings(cn, part, insertion)
           endif
       endif
       
    end do
    
  end subroutine insert_in_tree

end module tree_insertion_mod

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
use tree_insertion_mod
use tree_data_mod
use tree_functions_mod
use volume_data_mod

implicit none

type(node), pointer :: head     ! head of the tree
type(node), target  :: new_node ! next node to be inserted 
type(node), pointer :: tmp_node ! temporary node

integer :: vols, dagmc_num_vol ! number of volumes in geometry 
integer :: i
integer :: ios ! input/output status

character(len=80) :: filename   ! this is the geometry file

write(*,*) "this is what i like"

filename="nested_vol.h5m"

CALL dagmcinit(filename//char(0),len_trim(filename)) ! setup DAG problem

ALLOCATE(head)
head%id=0 ! I am the top of the tree

vols=dagmc_num_vol()
write (*,*) 'number of volumes is', vols

ALLOCATE (volume_centroids(vols,3))

OPEN (UNIT=20,FILE='vol_centroids.txt', STATUS='OLD', &
!      ACCESS='DIRECT', FORM='FORMATTED',&
      ERR=20, IOSTAT=ios)

do i=1, vols-1
   read(20,*) volume_centroids(i,1), volume_centroids(i,2), &
              volume_centroids(i,3)
   write(*,*) volume_centroids(i,1), volume_centroids(i,2), &
              volume_centroids(i,3)
end do
close (20)

!ALLOCATE(tmp_node)
!id=0


DO i=1, vols-1

 ALLOCATE(tmp_node)
 CALL create_node(i,tmp_node)
 write(*,*) tmp_node%id,head%id
 CALL insert_in_tree(tmp_node, head)
END DO
stop
20 write(*,*) 'could not open vol_centroids.txt'

end program tree_driver







