function priority(item)
    implicit none
    character :: item
    integer :: priority


    priority = ichar(item)
    if (priority.gt.96) then
        priority = priority - 96
    else
        priority = priority - 38
    end if
end function

function appears_thrice(a, b, c)
    implicit none
    character(len=*),intent(in) :: a
    character(len=*),intent(in) :: b
    character(len=*),intent(in) :: c
    character :: appears_twice
    character :: appears_thrice
    integer :: i, ii
    logical :: found

    found = .false.

    do ii = 1, len(a)
        do i = 1, len(b)
            if (a(ii:ii).eq.b(i:i)) then 
                appears_twice = a(ii:ii)
                found = .true.
                exit
            end if
        end do 
        if (found) then
            found = .false.

            do i = 1, len(c)
                if (appears_twice.eq.c(i:i)) then 
                    appears_thrice = appears_twice
                    found = .true.
                    exit
                end if
            end do 
        end if
        if (found) exit
    end do

end function

program day3b

    implicit none
    integer::priority
    integer::total_priority
    character::appears_thrice
    character(len=256) :: buffer
    character(len=:),allocatable :: a
    character(len=:),allocatable :: b
    character(len=:),allocatable :: c 

    total_priority = 0

    open(unit=1,file="input.txt")
    do
        read(1,*,end=101)buffer
        a = trim(buffer)
        read(1,*,end=101)buffer
        b = trim(buffer)
        read(1,*,end=101)buffer
        c = trim(buffer)
        total_priority = total_priority + priority(appears_thrice(a,b,c))
    end do
    
    101 continue
    close(unit=1)

    print *,total_priority

end program day3b
