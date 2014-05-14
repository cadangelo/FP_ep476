module volume_data_mod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This module contains a subroutine that selects the correct point
! on A's surface according to it's ID number.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

implicit none

double precision, allocatable :: volume_surfpoints(:,:)

contains
    subroutine find_point(A_id, x, y, z)

    integer, intent(in) :: A_id ! ID of volume A
    double precision, intent(out) :: x, y, z ! point on surf. of A

    x=volume_surfpoints(A_id,1)
    y=volume_surfpoints(A_id,2)
    z=volume_surfpoints(A_id,3)
    
    end subroutine find_point

end module volume_data_mod
