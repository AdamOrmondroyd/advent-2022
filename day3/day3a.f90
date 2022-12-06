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

function appears_twice(left, right)
    implicit none
    character(len=*),intent(in) :: left
    character(len=*),intent(in) :: right
    character :: appears_twice
    integer :: i, ii
    logical :: found

    found = .false.

    do ii = 1, len(left)
        do i = 1, len(right)
            if (left(ii:ii).eq.right(i:i)) then 
                appears_twice = left(ii:ii)
                found = .true.
                exit
            end if
        end do 
        if (found) exit
    end do

end function

program day3a

    implicit none
    integer::priority
    integer::total_priority
    character::appears_twice
    character(len=256) :: buffer
    character(len=:),allocatable :: rucksack
    character(len=:),allocatable :: left
    character(len=:),allocatable :: right 

    total_priority = 0

    open(unit=1,file="input.txt")
    do
        read(1,*,end=101)buffer
        rucksack = trim(buffer)
        left = rucksack(1:len(rucksack)/2)
        right = rucksack(len(rucksack)/2+1:len(rucksack))
        total_priority = total_priority + priority(appears_twice(left,right))
    end do
    
    101 continue
    close(unit=1)

    print *,total_priority


end program day3a
