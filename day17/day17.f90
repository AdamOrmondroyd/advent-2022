subroutine print_chamber(chamber,x,y,shapes,i_shape)
    implicit none
    logical, dimension(10000,11) :: chamber
    integer :: x, y, i_shape
    logical, dimension(4,4,5) :: shapes
    character(len=11) :: letters
    logical, dimension(10000,11) :: temp
    integer :: i, ii
    temp = chamber
    temp(y:y+3,x:x+3) = temp(y:y+3,x:x+3).or.shapes(:,:,i_shape)
    do ii=10,1,-1
        do i=1,11
            if (temp(ii,i)) then
                letters(i:i)="#"
            else
                letters(i:i)="."
            end if
        end do
        print *,letters
    end do
end subroutine

subroutine make_shapes(shapes,shape_heights)
    implicit none
    logical, dimension(4,4,5) :: shapes
    ! 0-indexed heights are actually more useful lmao
    ! now not so sure
    integer, dimension(5) :: shape_heights

    shape_heights = (/0,2,2,3,1/)
    shapes = .false.

    ! ----
    shapes(1,:,1) = .true.
    ! +
    shapes(2,1:3,2) = .true.
    shapes(1:3,2,2) = .true.
    ! L
    shapes(1:3,3,3) = .true.
    shapes(1,1:3,3) = .true.
    ! |
    shapes(:,1,4) = .true.
    ! square
    shapes(1:2,1:2,5) = .true.
end subroutine

subroutine check_for_full_rows(chamber, height_of_bottom)
    implicit none
    logical, dimension(10000, 11) :: chamber
    integer :: height_of_bottom
    integer :: i

    do i=1, size(chamber, 1)
        if (all(chamber(i,:))) then
            ! might need to double-check this
            chamber(:size(chamber, 1)-i+1,:) = chamber(i:,:)
        end if
    end do
end subroutine
    

program day17
    implicit none
    logical, dimension(10000, 11) :: chamber

    logical, dimension(4,4,5) :: shapes
    integer, dimension(5) :: shape_heights
    integer :: x,y! position of bottom left of rock
    integer :: highest
    integer :: height_of_bottom ! as the recorded chamber moves up if there is a full
    ! row, need to keep track of what this actually is. Starts as 0 as the floor is initially
    ! filled in
    integer :: top_of_rock
    
    integer :: i_gas, i_shape, i_rock
    character(len=11000) :: buffer
    character(len=:), allocatable :: gas
    open(unit=1,file="input.txt")
    read(1,'(A)')buffer
    close(1)
    gas = buffer(:len_trim(buffer))

    ! initialise chamber floor and walls
    chamber = .false.
    ! walls
    chamber(:,1) = .true.
    chamber(:,9) = .true.
    ! floor
    chamber(1,:) = .true.
    height_of_bottom = 0
    highest = 0
    i_shape = 1
    i_gas = 1
    i_rock = 1

    call make_shapes(shapes,shape_heights)
    x = 4
    y = 5

    do while(i_rock.le.2022)
        ! move left/right
        ! call print_chamber(chamber,x,y,shapes,i_shape)
        if(gas(i_gas:i_gas)=='<'.and..not.any(shapes(:,:,i_shape).and.chamber(y:y+3,x-1:x+2))) then
            x = x-1
        else if(gas(i_gas:i_gas)=='>'.and..not.any(shapes(:,:,i_shape).and.chamber(y:y+3,x+1:x+4))) then
            x = x+1
        end if
        i_gas = modulo(i_gas, len(gas))+1
        ! call print_chamber(chamber,x,y,shapes,i_shape)
        ! down
        if (any(shapes(:,:,i_shape).and.chamber(y-1:y+2,x:x+3))) then
            chamber(y:y+3,x:x+3) = chamber(y:y+3,x:x+3).or.shapes(:,:,i_shape)
            if(y+shape_heights(i_shape).gt.highest) highest = y+shape_heights(i_shape)
            i_shape = modulo(i_shape,5)+1
            x = 4
            y = highest+4
            i_rock = i_rock+1
        else
            y = y-1
        end if


    end do
    print *,"-------------------------------------------"
    print *, highest-1

end program day17
