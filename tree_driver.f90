module tree_functions_mod
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! This is a module that contains subroutines for inserting          !
  ! new nodes into the tree                                           !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use tree_data_mod
  
  contains  

    subroutine create_node(id, new_node)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! This subroutine allocates space for the new node being 
    ! inserted into the tree.  It also nullifies all of the 
    ! new node's associations
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use tree_data_mod
    
    implicit none

    integer :: id   ! ID of the new node
    type(node), pointer, intent(inout)  :: new_node 
  
    allocate(new_node) 

    nullify(new_node%head)
    nullify(new_node%parent)
    nullify(new_node%fchild)
    nullify(new_node%lchild)
    nullify(new_node%rsib)
    nullify(new_node%lsib)
    nullify(new_node%cn)

    ! Set the new node's ID 
    new_node%id=id
  
    return  
    end subroutine create_node
    

    subroutine part_in_cn(cn, part, insertion)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! This subroutine is called when the new node being inserted 
    ! into the tree, part, is found to be INSIDE of the
    ! current node being tested against, cn. 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use tree_data_mod

    type(node), pointer, intent(inout) :: part ! node being inserted
    type(node), pointer, intent(inout) :: cn ! node part tested against
  

    logical :: insertion ! if insertion is true, the new node (part)
                         ! has successfully been inserted in the tree
    
    write(*,*) 'Calling part_in_cn ...'
    
    ! If the current node already has a child, we need to find part's 
    ! relation to the child node. So- that child node becomes the new
    ! cn to test against. 
    if (associated(cn%fchild)) then
    
      write(*,*) 'cn fchild id', cn%fchild%id
      cn => cn%fchild
    
      insertion = .false.
      write(*,*) 'cn is now..', cn%id

    ! If the current node does not have any children yet, part becomes
    ! cn's child. Part is now successfully inserted into the tree.
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

    write(*,*) 'insertion of part in cn is ...', insertion
    
    end subroutine part_in_cn

    subroutine cn_in_part(cn, part, insertion)
  
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! This subroutine is called when the new node being inserted 
    ! into the tree, part, is found to be OUTSIDE of the
    ! current node being tested against, cn. 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
    use tree_data_mod
    
    type(node), pointer, intent(inout) :: part ! node being inserted
    type(node), pointer, intent(inout) :: cn ! node part tested against
  
    logical :: insertion ! if insertion is true, the new node (part)
    logical :: check_sibs=.false.
    write(*,*) 'Calling cn_in_part ...'
    write(*,*) 'part, cn', part%id, cn%id
    if (associated(part%fchild)) then
     write(*,*) 'part, fchild', part%id, part%fchild%id
     write(*,*) 'part, lchild', part%id, part%lchild%id
    end if
    !!!!!!!!!!!!!! parent changes !!!!!!!!!!!!!!!!!!!!!!!!
    part%parent=>cn%parent
    if(cn%parent%fchild%id .eq. cn%id) then
      !if (associated (cn%rsib)) then
        cn%parent%fchild=>cn%rsib
        !check_sibs=.true.
      !end if  
    end if
    if (cn%parent%lchild%id .eq. cn%id) then
     ! if (associated (cn%lsib)) then
        cn%parent%lchild => cn%lsib
        !check_sibs=.false.
     ! end if
    end if 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    if (associated(cn%rsib)) then
      !if (associated(cn%lsib)) then
      cn%rsib%lsib => cn%lsib
      !else
      !nullify(cn%lsib%rsib)
      check_sibs=.true.
    end if
    if (associated(cn%lsib)) then
      cn%lsib%rsib=>cn%rsib
    end if
    nullify(cn%rsib)
    nullify(cn%lsib)

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   cn%parent => part
   cn%lsib=> part%lchild

   if (associated(part%lchild)) then
     part%lchild%rsib=>cn
   else
     part%fchild=>cn
   end if  

   part%lchild=>cn
   
   
   insertion = .true.   
  
   if (check_sibs .eqv. .true.) then
     cn=>part%parent%fchild
     insertion=.false.  
   end if
  
   write(*,*)    
   write(*,*) 'insertion', insertion
   write(*,*) 'cn is now', cn%id
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

    end subroutine print_tree
    
    subroutine write_tree(head,num_vol)

      type(node), pointer, intent(inout) :: head
      type(node), pointer                :: orig_head
      integer :: i,num_vol, num_parents
      logical :: p_sib=.false.

      open(unit=10, file='tree_graph.dot', status='replace')
      write(10, fmt=*)'digraph geometry {' 
      write(10, fmt=*)'size="6,4"; ratio = fill;'
      write(10, fmt=*)'node[style=filled];'
      num_parents=0
      orig_head => head
      write(*,*) 'i am printing stuff'

      do while (associated(head%fchild))
            write(*,*) 'blah'
            write(10, fmt=*) head%id,'->',head%fchild%id, &
                              '[color="blue4"];'
            write(10, fmt=*) head%id,'->',head%lchild%id, &
                             '[color="deepskyblue"];'
            write(10, fmt=*) '{ rank=same;', head%fchild%id, &
                                             head%lchild%id, '}'
            if (associated(head%parent)) then
            write(10, fmt=*) head%id,'->',head%parent%id, &
                       '[color="crimson"];'
            end if
            head => head%fchild
      enddo
      
      write(10, fmt=*) head%id,'->',head%parent%id, &
                       '[color="crimson"];'

      head=> orig_head%fchild 
      
      do while (p_sib .eqv. .false.)
        if (.not. associated(head%rsib)) then
          !p_sib=.false.
          if (associated (head%fchild)) then
            head=>head%fchild
          else
            p_sib=.true.
          end if

        else
          do while (associated(head%rsib))
            write(*,*)'head,rsib', head%id,head%rsib%id
            write(10, fmt=*) head%id,'->',head%rsib%id, &
                         '[color="darkorchid4"];'
            write(10, fmt=*) head%rsib%id,'->',head%parent%id, &
                         '[color="crimson"];'
            write(10, fmt=*) head%rsib%id,'->',head%id, &
                          '[color="darkorchid1"];'
            !write(*,*)'head,lsib', head%id,head%lsib%id
            if (associated(head%fchild)) then
              write(10, fmt=*) head%id,'->',head%fchild%id, &
                          '[color="deepskyblue"];'
            end if
                  
            head=>head%rsib
          end do
          
          if(associated(head%parent%fchild%fchild)) then
            head=>head%parent%fchild%fchild
          else
            p_sib=.true.
          end if

        end if
      end do

      do i = 1,num_vol
            write(10, fmt=*) i,';'
      enddo

      write(10, fmt=*) '}'
      
      close(10)
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

filename="nested_vol_2.h5m"

call dagmcinit(filename//char(0),len_trim(filename)) ! setup DAG problem

! create the head node;
! it is an imaginary node with id=0
! it is at the top of the tree and
! all other nodes are inside it
!allocate(head)
call create_node(100, head)

! find the total number of volumes in the geometry
vols=dagmc_num_vol()
write (*,*) 'The number of volumes is', vols

! allocate an array that contains the x,y,z
! coordinates of the centroids of the volumes
allocate (volume_surfpoints(vols,3))

! open the file containing the centroids
open (unit=20,file='vol_surf_points_2.txt', status='old', &
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

!call write_tree(head,vols-1)

stop

20 write(*,*) 'could not open vol_surf_points.txt'

end program tree_driver







