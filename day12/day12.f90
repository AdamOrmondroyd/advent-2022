program day12
    implicit none
    character, dimension(163, 41) :: landscape
    logical, dimension(163, 41) :: unvisited
    integer, dimension(163, 41) :: distances
    integer, dimension(2) :: x
    integer, dimension(2) :: finish
    integer :: i, ii

    unvisited = .true.
    ! set distances to a large number
    distances = 2**10
    open(unit=1, file="input.txt")
    do i=1,41
        read(1, '(163A1)') landscape(:,i)
    end do
    close(1)
    
    do ii = 1, size(landscape, 1)
        do i = 1, size(landscape, 2)
            if (landscape(ii, i)=='S') then
                x(1) = ii
                x(2) = i
                ! start has height a
                landscape(ii, i) = 'a'
            else if (landscape(ii, i)=='E') then
                finish(1) = ii
                finish(2) = i
                ! finish has height z
                landscape(ii, i) = 'z'
            end if
        end do
    end do

    distances(x(1),x(2)) = 0

    do while (x(1).ne.finish(1).or.x(2).ne.finish(2))
        unvisited(x(1),x(2)) = .false.
        ! left
        if (x(1).gt.1) then
            if (distances(x(1)-1, x(2)).gt.distances(x(1), x(2))+1.and.&
            & iachar(landscape(x(1)-1, x(2))).le.iachar(landscape(x(1),x(2)))+1) then
                distances(x(1)-1, x(2)) = distances(x(1), x(2)) + 1
            end if
        end if

        ! right
        if (x(1).lt.size(landscape, 1)) then
            if (distances(x(1)+1, x(2)).gt.distances(x(1), x(2))+1) then !.and.&
                if (iachar(landscape(x(1)+1, x(2))).le.iachar(landscape(x(1),x(2)))+1) then
                    distances(x(1)+1, x(2)) = distances(x(1), x(2)) + 1
                end if
            end if
        end if

        ! up
        if (x(2).gt.1) then
            if (distances(x(1), x(2)-1).gt.distances(x(1), x(2))+1.and.&
            & iachar(landscape(x(1), x(2)-1)).le.iachar(landscape(x(1),x(2)))+1) then
                distances(x(1), x(2)-1) = distances(x(1), x(2)) + 1
            end if
        end if

        ! down
        if (x(2).lt.size(landscape, 2)) then
            if (distances(x(1), x(2)+1).gt.distances(x(1), x(2))+1.and.&
            & iachar(landscape(x(1), x(2)+1)).le.iachar(landscape(x(1),x(2)))+1) then
                distances(x(1), x(2)+1) = distances(x(1), x(2)) + 1
            end if
        end if

        x = minloc(distances, mask=unvisited)
    end do

    print *,distances(x(1), x(2))



end program day12
