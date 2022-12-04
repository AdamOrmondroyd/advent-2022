program day1a
    implicit none
    integer :: largest
    integer :: running_total
    integer :: buffer
    character(len=20) :: buffer_string

    running_total=0
    largest=0

    open(unit=1,file="test_input.txt")
    ! open(unit=1,file="input.txt")

    do
        read (1,'(a)',end=101) buffer_string
        if (buffer_string.eq."") then
            if (running_total.gt.largest) then
                largest=running_total
            end if
            running_total=0
        else
            read(buffer_string,*) buffer
            running_total = running_total + buffer
        end if
    end do
    101 continue
    if (running_total.gt.largest) then
        largest=running_total
    end if

    print *,largest
    close(1)


end program day1a
