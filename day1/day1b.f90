subroutine insert_if_larger(list, value_to_insert)
    implicit none
    integer, dimension(3) :: list
    integer :: value_to_insert
    integer :: temp
    integer :: i

    if (value_to_insert.gt.list(1)) then
        list(1) = value_to_insert
        print *,"hello"
        do i = 1, size(list)-1
            if (list(i).gt.list(i+1)) then 
                temp = list(i)
                list(i) = list(i+1)
                list(i+1) = temp
            else
               exit
            end if 
        end do
    end if

end subroutine


program day1b
    implicit none
    integer, dimension(3) :: top_3
    integer :: running_total
    integer :: buffer
    character(len=20) :: buffer_string
    integer :: i

    top_3 = 0
    running_total = 0

    print *, "Hello day1!"
    ! open(unit=1,file="test_input.txt")
    open(unit=1,file="input.txt")

    do
        read (1,'(a)',end=101) buffer_string
        print *,buffer_string
        if (buffer_string.eq."") then
            print *,running_total
            call insert_if_larger(top_3, running_total)
            print *, top_3
            running_total=0
        else
            read(buffer_string,*) buffer
            print *,buffer
            running_total = running_total + buffer
        end if
    end do
    101 continue
    call insert_if_larger(top_3, running_total)
    print *, top_3
    print *, sum(top_3)
    close(1)


end program day1b
