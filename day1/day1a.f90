program day1
    implicit none
    integer :: largest
    integer :: running_total
    integer :: buffer
    character(len=20) :: buffer_string
    integer :: i

    largest=0

    print *, "Hello day1!"
    ! open(unit=1,file="test_input.txt")
    open(unit=1,file="input.txt")

    do i=1, 2244
    read (1,'(a)',end=101) buffer_string
        print *,buffer_string
        if (buffer_string.eq."") then
            if (running_total.gt.largest) then
                largest=running_total
            end if
            running_total=0
        else
            read(buffer_string,*) buffer
            print *,buffer
            running_total = running_total + buffer
        end if
        print *, running_total
    end do
    101 continue
    print *, largest
    close(1)


end program day1
