module volume_functions_mod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a module that contains a function for checking if 
! one volume, A, is contained inside another volume, B.            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use tree_data_mod
  use volume_data_mod

  implicit none
  contains
    function is_A_in_B(A, B)

    type(node), pointer :: A
    type(node), pointer :: B
    double precision  :: x, y, z ! coordinated of point on surface of A
    integer(iknd) :: inside ! either 1 or 0 is returned from dagmcchkcel
                            ! 0= point is inside; 1=point is outside
    logical :: is_A_in_B    ! T/F depending on result of inside

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
    ! Special case when inserting part into top of tree
    ! All nodes are inside the head node (-1) so inside always =0
    ! Otherwise, find a point on surface A and call dagmcchkcel
    ! to find out if it is inside B.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if(B%id .eq. -1 )then
       inside=0
    else  
      CALL find_point(A%id, x, y, z)
      CALL dagmcchkcel(x, y, z, B%id, inside)
    endif
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! If inside is 0, volume A is inside volume B
    ! Otherwise, volume A is outside of volume B
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (inside .eq. 0) then
      is_A_in_B=.true.
    else
      is_A_in_B=.false.
    endif
   
    
    end function is_A_in_B
  


end module volume_functions_mod
