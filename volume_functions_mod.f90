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
    real(rknd) :: x, y, z
    integer(iknd) :: inside = 0
    logical :: is_A_in_B

!    A=>a_tmp
    write(*,*) 'A_id=', A%id
    write(*,*) 'B = ', B%id


    ! special case then inserting part into top of tree
    if(B%id .eq. 0 )then
       is_A_in_B=.true.
    else  
      CALL find_point(A%id, x, y, z)
      CALL dagmcchkcel(x, y, z, B%id, inside)
    endif

    if (inside .eq. 0) then
      is_A_in_B=.true.
    else
      is_A_in_B=.false.
    endif
   
    
    end function is_A_in_B
  
    subroutine find_point(A_id, x, y, z)
   
    integer :: A_id
    real(rknd) :: x, y, z

    x=volume_centroids(A_id,1)
    y=volume_centroids(A_id,2)
    z=volume_centroids(A_id,3)
       
    end subroutine find_point

end module volume_functions_mod
