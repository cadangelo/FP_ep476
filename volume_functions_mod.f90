module volume_functions_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a module that contains a function for checking if volume !
! one volume, A, is contained inside another volume, B.             !                                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  use tree_data_mod
  use volume_data_mod

  implicit none
  contains
    function is_A_in_B(A, B)

    type(node), pointer :: A
    type(node), pointer :: B
    double precision  :: x, y, z
    integer(iknd) :: inside
    logical :: is_A_in_B
    integer(iknd)::ijk
    write(*,*) 'A = ', A%id
    write(*,*) 'B = ', B%id


    ! special case then inserting part into top of tree
    if(B%id .eq. 100 )then
       inside=0
    else  
      CALL find_point(A%id, x, y, z)
      CALL dagmcchkcel(x, y, z, B%id, inside)
!           write(*,*) 'A, point', A%id,B%id, ijk, x, y, z,inside
    endif

    if (inside .eq. 0) then
      is_A_in_B=.true.
    else
      is_A_in_B=.false.
    endif
   
    
    end function is_A_in_B
  
    subroutine find_point(A_id, x, y, z)
   
    integer, intent(in) :: A_id
    double precision, intent(out) :: x, y, z

    x=volume_surfpoints(A_id,1)
    y=volume_surfpoints(A_id,2)
    z=volume_surfpoints(A_id,3)
    
   
    end subroutine find_point

end module volume_functions_mod
