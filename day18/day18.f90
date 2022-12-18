function num_neighbours(cube, cubes, num_cubes)
    implicit none
    integer, intent(in) :: num_cubes
    integer, dimension(3) :: cube
    integer, dimension(3,num_cubes) :: cubes
    integer, dimension(3,num_cubes) :: diffs
    integer :: num_neighbours, i
    num_neighbours = 0

    do i=1,num_cubes
        diffs(:,i) = abs(cubes(:,i)-cube)
        if(count(diffs(:,i).eq.0).eq.2.and.count(diffs(:,i).eq.1).eq.1) then
            num_neighbours = num_neighbours + 1
        end if
    end do
end function

recursive subroutine dfs(x, visited, bottom_corner, top_corner, directions)
    implicit none
    integer, dimension(3) :: x, y
    integer, dimension(3), intent(in) :: bottom_corner, top_corner
    logical, dimension(bottom_corner(1):top_corner(1),bottom_corner(2):top_corner(2),bottom_corner(3):top_corner(3)) :: visited
    integer, dimension(3,6) :: directions
    integer :: i

    visited(x(1),x(2),x(3)) = .true.
    
    do i=1,6
        y = x+directions(:,i)
        if(.not.visited(y(1),y(2),y(3)).and.all(y.ge.bottom_corner).and.all(y.le.top_corner)) then
            call dfs(y,visited,bottom_corner,top_corner,directions)
        end if
    end do
end subroutine

subroutine day18b(cubes, num_cubes)
    implicit none
    integer, intent(in) :: num_cubes
    integer, dimension(3,num_cubes) :: cubes
    logical, dimension(:,:,:), allocatable :: visited, original_cubes
    integer, dimension(3) :: bottom_corner, top_corner
    integer, dimension(3,6) :: directions
    integer, dimension(3) :: x
    integer :: i, ii, iii, iv
    integer :: num_cubes_and_bubbles
    integer, dimension(:,:), allocatable :: cubes_and_bubbles

    directions(:,1) = (/1,0,0/)
    directions(:,2) = (/0,1,0/)
    directions(:,3) = (/0,0,1/)
    directions(:,4) = -directions(:,1)
    directions(:,5) = -directions(:,2)
    directions(:,6) = -directions(:,3)

    do i=1,3
        bottom_corner(i) = minval(cubes(i,:))-1
        top_corner(i) = maxval(cubes(i,:))+1
    end do

    allocate(visited(bottom_corner(1):top_corner(1),bottom_corner(2):top_corner(2),bottom_corner(3):top_corner(3)))
    allocate(original_cubes(bottom_corner(1):top_corner(1),bottom_corner(2):top_corner(2),bottom_corner(3):top_corner(3)))
    
    visited = .false.

    do i=1, num_cubes
        visited(cubes(1,i),cubes(2,i),cubes(3,i)) = .true.
    end do

    original_cubes = visited

    x = bottom_corner
    call dfs(x, visited, bottom_corner, top_corner, directions)
    visited = visited.neqv.original_cubes

    num_cubes_and_bubbles = 0
    do iii = bottom_corner(1), top_corner(1)
        do ii = bottom_corner(2), top_corner(2)
            do i = bottom_corner(3), top_corner(3)
                if (.not.visited(iii, ii, i)) then
                    num_cubes_and_bubbles = num_cubes_and_bubbles + 1
                end if
            end do
        end do
    end do
    allocate(cubes_and_bubbles(3,num_cubes_and_bubbles))
    iv = 1
    do iii = bottom_corner(1), top_corner(1)
        do ii = bottom_corner(2), top_corner(2)
            do i = bottom_corner(3), top_corner(3)
                if (.not.visited(iii, ii, i)) then
                    cubes_and_bubbles(:,iv) = (/iii,ii,i/)
                    iv = iv+1
                end if
            end do
        end do
    end do

    call day18a(cubes_and_bubbles, num_cubes_and_bubbles)

end subroutine

subroutine day18a(cubes, num_cubes)
    implicit none
    integer, intent(in) :: num_cubes
    integer, dimension(3,num_cubes) :: cubes
    integer :: num_neighbours
    integer :: num_sides
    integer :: i

    num_sides = 6*num_cubes
    do i=1, num_cubes-1
        num_sides = num_sides-2*num_neighbours(cubes(:,i),cubes(:,i+1:),num_cubes-i)
    end do

    print *, num_sides
end subroutine
    

program day18
    implicit none
    integer :: num_cubes
    integer, dimension(:,:), allocatable :: cubes
    integer :: i
    open(unit=1,file="input.txt")
    num_cubes = 0
    do
        read(1,'(A)',end=101)
        num_cubes = num_cubes+1
    end do
    101 continue
    rewind(unit=1)

    allocate(cubes(3,num_cubes))

    do i=1,num_cubes
        read(1,*)cubes(:,i)
    end do
    close(1)
    
    call day18a(cubes, num_cubes)
    print *,"----------------------------"
    call day18b(cubes, num_cubes)
    
end program day18
