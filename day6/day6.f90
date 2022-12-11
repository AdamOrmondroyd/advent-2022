logical function repeats(str) result (bar)

    implicit none
    character(len=*), intent(in) :: str
    integer :: i,ii

    bar = .false.

    do ii=1, len(str)-1
        do i=ii+1, len(str)
            if (str(i:i) == str(ii:ii)) then
                bar = .true.
                exit
            end if
            if(bar) exit 
        end do
    end do
end function repeats


program day6a

    implicit none
    character(len=8192) :: buffer
    integer :: i
    logical repeats
    integer packet_length

    packet_length = 4

    open(unit=1,file="input.txt")
    read(1, *) buffer
    close(unit=1)

    do i=packet_length, len(buffer)
        if (.not.repeats(buffer((i-packet_length+1):i))) exit
    end do 

    print *,"no repeats once the ", i, "th signal received"

    

end program day6a
