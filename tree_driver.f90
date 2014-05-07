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
  
    allocate(new_node) 
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
      if (associated(cn%fchild)) then
       write(*,*) 'cn fchild id', cn%fchild%id
       cn => cn%fchild
       insertion = .false.
       write(*,*) 'cn is now..', cn%id
    else
       part%parent => cn
       cn%fchild => part
       cn%lchild => part
       nullify(part%rsib)
       nullify(part%lsib)
       nullify(part%fchild)
       nullify(part%lchild)
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
    write(*,*) 'part, cn', part%id, cn%id
    write(*,*) 'insertion of cn in part is ...', insertion

    end subroutine cn_in_part


    subroutine cn_part_siblings(cn, part, insertion)
  
    use tree_data_mod

    type(node), pointer, intent(inout)  :: part 
    type(node), pointer, intent(inout) :: cn
  
    logical :: insertion
    
    write(*,*) 'Calling cn_part_siblings ...'
    
    if (associated(cn%rsib)) then 
      write (*,*) 'cn rsib id',cn%rsib%id
      cn => cn%rsib
    else
      cn%rsib => part
      part%lsib => cn
      nullify(part%rsib)
      part%parent => cn%parent !check this
      part%parent%lchild => part
      nullify(part%fchild)
      nullify(part%lchild)
      insertion = .true.
    endif

    
    write(*,*) 'part, cn', part%id, cn%id
    write(*,*) 'insertion of cn, part sibs is ...', insertion

    end subroutine cn_part_siblings

    subroutine print_tree(node_p)
    use tree_data_mod

    type(node), pointer, intent(inout) :: node_p
    type(node), pointer :: node_orig

    node_orig => node_p

    do while( associated(node_p%fchild))
      write(*,*) 'node ', node_p%id, ' has 1st child ', node_p%fchild%id
      write(*,*) 'node ', node_p%id, ' has last child ', node_p%lchild%id
      node_p => node_p%fchild
    enddo

    node_p => node_orig

    end subroutine
    
    subroutine write_tree(head,num_vol)
!      use tree_data_mod
!      implicit none
      type(node), pointer, intent(inout) :: head
      type(node), pointer                :: orig_head
      integer :: i,num_vol 
      write(*,*) 'digraph geometry {' 
      write(*,*) 'size="6,4"; ratio = fill;'
      write(*,*) 'node[style=filled];'
      
      orig_head => head

      do while (associated(head%fchild))
            write(*,*) head%id,'->',head%fchild%id,';'
            head => head%fchild
      enddo

      head => orig_head

      do while (associated(head%lchild))
            write(*,*) head%id,'->',head%lchild%id,';'
            head => head%lchild
      enddo
!      write(*,*) ';'

      do i = 1,num_vol
            write(*,*) i,';'
      enddo

      write(*,*) '}'

    end subroutine write_tree

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
    
    
    cn => head
   
    write(*,*) 'cn_id = ', cn%id 
 
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

    call print_tree(head)
       
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

character(len=80) :: filename   ! name geometry file

write(*,*) "this is what i like"

filename="nested_vol.h5m"

call dagmcinit(filename//char(0),len_trim(filename)) ! setup DAG problem

! create the head node;
! it is an imaginary node with id=0
! it is at the top of the tree and
! all other nodes are inside it
!allocate(head)
call create_node(0, head)

! find the total number of volumes in the geometry
vols=dagmc_num_vol()
write (*,*) 'The number of volumes is', vols

! allocate an array that contains the x,y,z
! coordinates of the centroids of the volumes
allocate (volume_surfpoints(vols,3))

! open the file containing the centroids
open (unit=20,file='vol_surf_points.txt', status='old', &
      err=20, iostat=ios)

do i=1, vols-1
   read(20,*) volume_surfpoints(i,1), volume_surfpoints(i,2), &
              volume_surfpoints(i,3)
   write(*,*) i, volume_surfpoints(i,1), volume_surfpoints(i,2), &
              volume_surfpoints(i,3)
end do
close (20)



do i=1, vols-1

 allocate(tmp_node)
 call create_node(i,tmp_node)
 write(*,*) 'new node, head', tmp_node%id,head%id
 call insert_in_tree(tmp_node, head)

end do

call print_tree(head)

call write_tree(head,vols-1)

stop

20 write(*,*) 'could not open vol_surf_points.txt'

end program tree_driver







