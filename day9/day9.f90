subroutine space_to_comma(str)
    implicit none
    character(len=*) :: str
    integer :: i
    do i=1,len(str)
        if (str(i:i).eq.'-') then
            str(i:i)=','
        end if
    end do
end subroutine

subroutine move_tail(head, tail)
    implicit none
    integer, dimension(2) :: head, tail
    integer :: i
    if (any(abs(head-tail).ge.2)) then
        ! I think this case is contained within how I've done diagonal??
        ! if any(head==tail) then 
            ! tail = (head + tail) / 2
        ! else 
        do i = 1, 2
            if (head(i).gt.tail(i)) then
                tail(i) = tail(i) + 1
            else if (head(i).lt.tail(i)) then
                tail(i) = tail(i) - 1
            end if
        end do 
        ! end if

    end if
end subroutine

subroutine move_head(head, direction)
    implicit none
    integer, dimension(2) :: head
    character :: direction

    if (direction=="D") then
        head(2) = head(2) - 1
    else if (direction=="U") then
        head(2) = head(2) + 1
    else if (direction=="L") then
        head(1) = head(1) - 1
    else ! direction=="R"
        head(1) = head(1) + 1
    end if
end subroutine


program day9

    implicit none
    integer, dimension(2, 10) :: knots
    logical, dimension(1024, 1024) :: visited
    character :: direction
    integer :: distance
    integer :: i
    integer :: ii

    

    visited = .false.
    knots= 512

    visited(knots(1,10), knots(2,10)) = .true.
    

    open(unit=1,file="input.txt")
    do
        read(1, *, end=101)direction, distance
        do ii=1, distance
            call move_head(knots(:, 1), direction)
            do i=2, size(knots, 2)
                call move_tail(knots(:,i-1),knots(:,i))
            end do 
            visited(knots(1,10),knots(2,10)) = .true.
        end do
    end do
    101 continue
    close(1)

    print *,count(visited)
    
end program day9
