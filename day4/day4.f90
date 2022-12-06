subroutine hyphen_to_comma(str)
    implicit none
    character(len=*) :: str
    integer :: i
    do i=1,len(str)
        if (str(i:i).eq.'-') then
            str(i:i)=','
        end if
    end do
end subroutine

function fully(a, b, c, d)
    implicit none
    integer, intent(in) :: a, b, c, d
    logical :: fully

    fully = ((a.ge.c).and.(b.le.d)).or.((a.le.c).and.(b.ge.d))

end function

function partially(a, b, c, d)
    implicit none
    integer, intent(in) :: a, b, c, d
    logical :: partially

    partially = .not.((b.lt.c).or.(a.gt.d))

end function


program day4
    implicit none
    integer :: a, b, c, d
    character :: temp
    character(len=256) :: buffer
    character(len=256) :: buffer2
    logical :: fully, partially
    integer :: fully_counter, partially_counter

    ! open(unit=1,file="example_input.txt")
    open(unit=1,file="input.txt")
    do
        read(1,*,end=101)buffer,buffer2
        call hyphen_to_comma(buffer)
        read(buffer,*)a,b
        call hyphen_to_comma(buffer2)
        read(buffer2,*)c,d
        print *, partially(a,b,c,d)
        if (fully(a,b,c,d)) fully_counter = fully_counter+1
        if (partially(a,b,c,d)) partially_counter = partially_counter+1
    end do

    101 continue
    close(unit=1)
    print *, "fully overlap: ", fully_counter
    print *, "partially overlap: ", partially_counter

end program day4
