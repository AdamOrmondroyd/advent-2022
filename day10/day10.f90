!! addition occurs at the end of a cycle
subroutine cycle(x, counter, output)
    implicit none
    integer :: x, counter, output
    if (modulo(counter-20,40).eq.0) output = output + x * counter
    counter = counter + 1
end subroutine

subroutine crt(x, counter, screen)
    implicit none
    integer :: x, counter
    character, dimension(6,40) :: screen
    integer :: screen_position
    integer :: row

    screen_position = modulo(counter-1,40) + 1
    row = (counter-1)/40+1

    ! this is a mess, because the problem uses 0-indexing to relate x to the screen position
    if ((x-1.eq.screen_position-1).or.(x.eq.screen_position-1).or.(x+1.eq.screen_position-1)) then
        screen(row, screen_position) = '#'
    end if
        
end subroutine


program day10a

    implicit none
    integer :: x = 1
    integer :: v
    character(len=256) :: buffer
    character(len=4) :: opcode
    integer :: counter
    integer :: output
    character, dimension(6, 40) :: screen
    integer :: i

    counter = 1
    output = 0
    screen = '.'

    
    open(unit=1,file="input.txt")
    do
    read(1,'(A)',end=101) buffer
        if (len_trim(buffer).gt.4) then
            read(buffer,*)opcode,v

            call crt(x, counter, screen)
            call cycle(x, counter, output)
            call crt(x, counter, screen)
            call cycle(x, counter, output)
            
            x = x + v

        else
            call crt(x, counter, screen)
            call cycle(x, counter, output)
        end if
    end do
    101 continue
    close(1)

    print *,output
    do i = 1, 6
        print *, screen(i,:)
    end do
end program
