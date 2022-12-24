function wrap(a, b)
    ! essentially a 1-indexed modulo
    integer(kind=8) :: a, b
    integer(kind=8) :: wrap
    if (a.gt.0) then
        wrap = modulo(a,b-1)
    else
        wrap = modulo(a-1,b-1)+1
    end if
        
end function

subroutine day20b(w, num_lines)
    implicit none
    integer(kind=8) :: num_lines
    integer(kind=8), dimension(0:num_lines-1) :: w, idx
    integer(kind=8) :: i,ii,iii,xi
    integer(kind=8) :: old_idx,new_idx
    integer(kind=8) :: wrap
    integer(kind=8), dimension(0:num_lines-1) :: x, y

    do i=0,num_lines-1
        idx(i)=i
        x(i) = w(i)
        x(i) = x(i) * 811589153
    end do

    do iii=1,10
        do i=0,num_lines-1
            xi = x(i)
            if (xi.eq.0) cycle
            old_idx = idx(i)
            new_idx = wrap(xi+idx(i),num_lines)

            do ii=0,num_lines-1
                if (old_idx.lt.new_idx) then
                    if (idx(ii).gt.old_idx.and.idx(ii).le.new_idx) idx(ii) = idx(ii)-1
                else
                    if (idx(ii).ge.new_idx.and.idx(ii).lt.old_idx) idx(ii) = idx(ii)+1
                end if
            end do

            idx(i) = new_idx
        end do
     
        do ii=0,num_lines-1
            y(idx(ii)) = x(ii)
        end do



    end do
    do i=0,num_lines-1
        if (y(i).eq.0) exit
    end do

    print *,y(modulo(1000+i,num_lines))+y(modulo(2000+i,num_lines))+y(modulo(3000+i,num_lines))

    
        
end subroutine

program day20
    implicit none
    integer(kind=8), dimension(:), allocatable :: x
    integer(kind=8) :: num_lines, i
    integer(kind=8) :: wrap
    
    open(unit=1,file="input.txt")

    num_lines = 0
    do
        read(1,*,end=101)
        num_lines = num_lines+1
    end do
    101 continue

    allocate(x(0:num_lines-1))
    rewind(1)

    do i=1,num_lines
        read(1,*)x(i-1)
    end do
    
    close(1)

    call day20b(x, num_lines)
    
end program 
