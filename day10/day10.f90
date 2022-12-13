!! addition occurs at the end of a cycle
subroutine cycle(x, counter, output)
    implicit none
    integer :: x, counter, output
    if (modulo(counter-20,40).eq.0) output = output + x * counter
    counter = counter + 1
end subroutine



program day10a

    implicit none
    integer :: x = 1
    integer :: v
    character(len=256) :: buffer
    character(len=4) :: opcode
    integer :: counter
    integer :: output

    counter = 1
    output = 0

    
    open(unit=1,file="input.txt")
    do
    read(1,'(A)',end=101) buffer
        if (len_trim(buffer).gt.4) then
            read(buffer,*)opcode,v
            print *,opcode, v

            call cycle(x, counter, output)
            call cycle(x, counter, output)
            
            x = x + v

        else
            print *,buffer
            call cycle(x, counter, output)
        end if
    end do
    101 continue
    close(1)

    print *,output
end program
