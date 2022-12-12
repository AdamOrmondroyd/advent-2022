program day8a
    implicit none
    integer, dimension(99, 99) :: map
    logical, dimension(99, 99) :: visible
    integer :: highest
    integer :: i, ii

    open(unit=1,file="input.txt")
    do i=1, 99
        read(1,'(99I1)')map(:,i)
    end do
    close(unit=1)

    visible = .false.

    ! check from left
    do ii = 1, size(map, 2)
        visible(1, ii) = .true.
        highest = map(1, ii)
        do i = 1, size(map, 1)
            if (map(i, ii).gt.highest) then
                visible(i, ii) = .true.
                highest = map(i, ii)
            end if
        end do 
    end do 

    ! check from right
    do ii = 1, size(map, 2)
        visible(size(map, 1), ii) = .true.
        highest = map(size(map, 1), ii)
        do i = size(map, 1), 1, -1
            if (map(i, ii).gt.highest) then
                visible(i, ii) = .true.
                highest = map(i, ii)
            end if
        end do 
    end do   

    ! check from above
    do ii = 1, size(map, 1)
        visible(ii, 1) = .true.
        highest = map(ii, 1)
        do i = 1, size(map, 2)
            if (map(ii, i).gt.highest) then
                visible(ii, i) = .true.
                highest = map(ii, i)
            end if
        end do 
    end do 

    ! check from below
    do ii = 1, size(map, 1)
        visible(ii, size(map, 2)) = .true.
        highest = map(ii, size(map, 2))
        do i = size(map, 2), 1, -1
            if (map(ii, i).gt.highest) then
                visible(ii, i) = .true.
                highest = map(ii, i)
            end if
        end do 
    end do

    print *,count(visible)
    do i=1, size(map, 2)
        print *, (visible(ii, i), ii=1,size(map, 1))
    end do
end program day8a
