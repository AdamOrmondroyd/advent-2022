function wrap(a, b)
    ! essentially a 1-indexed modulo
    integer :: a, b
    integer :: wrap
    if (a.gt.0) then
        wrap = modulo(a,b-1)
    else
        wrap = modulo(a-1,b-1)+1
    end if
        
end function

subroutine day20a(x, num_lines)
    implicit none
    integer :: num_lines
    integer, dimension(0:num_lines-1) :: x, idx, y
    integer :: i,ii,xi,delta
    integer :: old_idx,new_idx
    integer :: wrap

    do i=0,num_lines-1
        idx(i)=i
    end do

    do i=0,num_lines-1
        print *,"here"
        xi = x(i)
        print *,xi
        if (xi.eq.0) cycle
        old_idx = idx(i)
        new_idx = wrap(xi+idx(i),num_lines)
        print *,new_idx

        do ii=0,num_lines-1
            if (old_idx.lt.new_idx) then
                if (idx(ii).gt.old_idx.and.idx(ii).le.new_idx) idx(ii) = idx(ii)-1
            else
                if (idx(ii).ge.new_idx.and.idx(ii).lt.old_idx) idx(ii) = idx(ii)+1
            end if
        end do

        idx(i) = new_idx
 
        do ii=0,num_lines-1
            y(idx(ii)) = x(ii)
        end do

        ! print *,idx
        print *,y


    end do
    do i=0,num_lines-1
        if (y(i).eq.0) exit
    end do
    print *,y(modulo(1000+i,num_lines))+y(modulo(2000+i,num_lines))+y(modulo(3000+i,num_lines))

    
        
end subroutine

program day20
    implicit none
    integer, dimension(:), allocatable :: x
    integer :: num_lines, i
    integer :: wrap
    
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

    print *,x
    do i=0,num_lines-1
        print *,wrap(x(i)+i, num_lines)
    end do

    call day20a(x, num_lines)
    
end program 
