
    !计算多边形面积
    subroutine polyarea(n,x,y,s)
    implicit none
    
    integer :: n
    real*8 :: x(n),y(n)
    real*8 :: s
    
    real*8 :: temp1(n),temp2(n)
    integer :: i
    
    !do i=1,n
    !    if(isnan(x(i)).or.isnan(y(i))) then
    !        s = 0.0
    !        goto 10
    !    end if
    !end do
    
    if(n>0) then
        temp1(1:n-1) = x(2:n)
        temp1(n) = x(1)
        temp2(1:n-1) = y(2:n)
        temp2(n) = y(1)
        s = abs(sum((temp1-x)*(temp2+y))/2.0)
    else
        s = 0
    end if
    
    end subroutine