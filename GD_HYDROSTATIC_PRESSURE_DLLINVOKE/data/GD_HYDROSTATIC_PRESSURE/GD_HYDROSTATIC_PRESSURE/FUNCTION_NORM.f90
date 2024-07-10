
    !ÏòÁ¿2·¶Êý
    real(kind=8) FUNCTION NORM(A,M)
    implicit none
    
    integer :: M
    real*8 :: A(M)
    integer :: i
    
    NORM = 0.0
    do i=1,M
        NORM = NORM+A(i)**2
    end do
    NORM = sqrt(NORM)
    
    end function