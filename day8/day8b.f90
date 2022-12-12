program day8b
    implicit none
    integer, dimension(99, 99) :: map
    integer, dimension(99, 99) :: scenic_score
    integer :: highest
    integer :: i, ii, iii

    ! open(unit=1,file="example_input.txt")
    open(unit=1,file="input.txt")
    do i=1, 99
        read(1,'(99I1)')map(:,i)
    end do
    close(unit=1)

    do iii = 1, size(map, 2)
        print *, (map(ii, iii), ii=1, size(map, 1))
    end do 
    print *, ""
    scenic_score = 0

    do iii = 2, size(map, 2)-1
        do ii = 2, size(map, 1)-1
            print *, ii, iii

            ! up
            i = 1
            !do while (iii-i.le.1.and.map(ii, iii-i).lt.map(ii, iii))
            do while (map(ii, iii-i).lt.map(ii, iii))
                if (iii-i.eq.1) exit
                i = i+1
            end do
            scenic_score(ii, iii) = i

            ! left
            i = 1
            ! do while (ii-i.ge.1.and.map(ii-i, iii).lt.map(ii, iii))
            do while (map(ii-i, iii).lt.map(ii, iii))
                if (ii-i.eq.1) exit
                i = i+1
            end do
            scenic_score(ii, iii) = scenic_score(ii, iii) * i

            ! down
            i = 1
            ! do while (iii+i.le.size(map,2).and.map(ii, iii+i).lt.map(ii, iii))
            do while (map(ii, iii+i).lt.map(ii, iii))
                if (iii+i.eq.size(map,2)) exit
                i = i+1
            end do
            scenic_score(ii, iii) = scenic_score(ii, iii) * i

            ! right
            i = 1
            ! do while (ii+i.le.size(map,1).and.map(ii+i, iii).lt.map(ii, iii))
            do while (map(ii+i, iii).lt.map(ii, iii))
                if (ii+i.eq.size(map,1)) exit
                i = i+1
            end do
            scenic_score(ii, iii) = scenic_score(ii, iii) * i

        end do
    end do
    do iii = 1, size(map, 2)
        print *, (scenic_score(ii, iii), ii=1, size(map, 1))
    end do 
    print *, "" 
    print *, maxval(scenic_score)

end program day8b
