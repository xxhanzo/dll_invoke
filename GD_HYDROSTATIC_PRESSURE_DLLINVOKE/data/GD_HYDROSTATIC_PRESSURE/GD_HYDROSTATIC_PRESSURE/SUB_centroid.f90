
    !计算多边形形心(需要首尾闭合)
    subroutine centroid(n,x,y,cx,cy,whirl)
    implicit none
    
    integer :: n
    real*8 :: x(n),y(n)
    real*8 :: s
    real*8 :: cx,cy
    integer :: whirl    !1为顺时针，2为逆时针
    
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
    
    !求形心
    cx = 0
    cy = 0
    do i=1,n-1
        cx = cx+(x(i)+x(i+1))*(x(i)*y(i+1)-x(i+1)*y(i))
        cy = cy+(y(i)+y(i+1))*(x(i)*y(i+1)-x(i+1)*y(i))
    end do
    
    if(whirl==1) then
        cx = -cx/(6.0*s)
        cy = -cy/(6.0*s)
    else
        cx = cx/(6.0*s)
        cy = cy/(6.0*s)
    end if

    end subroutine