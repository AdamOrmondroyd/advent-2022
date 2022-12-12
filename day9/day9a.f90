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

logical function move_tail(head, tail)
    implicit none
    integer, dimension(2), intent(in) :: head, tail
    move_tail = .false.
    if (any(abs(head-tail).ge.2)) move_tail = .true.
end function

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


program day8a

    implicit none
    integer, dimension(2) :: head
    integer, dimension(2) :: tail
    integer, dimension(2) :: old_head
    logical, dimension(1024, 1024) :: visited
    character :: direction
    integer :: distance
    integer :: i
    logical move_tail

    visited = .false.
    head = 512
    tail = head

    visited(tail(1), tail(2)) = .true.
    

    open(unit=1,file="input.txt")
    do
        read(1, *, end=101)direction, distance
        print *, direction, distance
        do i=1, distance
            old_head = head
            call move_head(head, direction)
            print *, "moved head"
            if(move_tail(head,tail)) then
                tail = old_head
                print *,"about to log visited"
                visited(tail(1),tail(2)) = .true.
                print *,"logged visited"
            end if
        end do
        
    end do
    101 continue
    close(1)

    print *,count(visited)

    

end program day8a
