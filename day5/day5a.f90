function reverse(string) result(reverse_string)
    character(len=256), intent(in) :: string
    character(len=len(string)) :: reverse_string
    integer :: i, n

    n = len(string)
    do i = 1, n
        reverse_string(n-i+1:n-i+1) = string(i:i)
    end do

end function reverse


function read_nth_stack(line,n)
    implicit none

    character(len=100) :: line
    integer :: n
    integer :: pos
    character :: read_nth_stack
    pos = 4 * n - 2
    read_nth_stack = line(pos:pos)
end function

subroutine get_three_ints_from_string(str,a,b,c)
    character(len=256) :: str
    integer :: a,b,c
    character(len=5) :: move
    character(len=6) :: from
    character(len=4) :: to

    read(str,*)move,a,from,b,to,c
end subroutine


program day5a
    implicit none
    character(len=256) :: reverse
    character :: read_nth_stack
    character(len=256), dimension(9) :: stacks = ''
    character(len=256) :: buffer
    integer :: i, ii
    integer :: a,b,c
    ! open(unit=1,file="example_input.txt")
    open(unit=1,file="input.txt")
    ii = 1
    do
       read(1,'(A)')buffer 
       if (buffer(2:2)=='1') exit
       print *,buffer
       do i=1, size(stacks)
            stacks(i)(ii:ii) = read_nth_stack(buffer,i)
       end do
       ii=ii+1
    end do
    print *,"finished reading file"
    do i=1, size(stacks)
        ! stacks(i) = reverse(stacks(i))
        print *,stacks(i)
        print *,len(stacks(i))
        stacks(i)=adjustl(stacks(i))
    end do 

    print *,len(stacks(2))
    print *,len_trim(stacks(2))
    print *, len_trim(trim(stacks(1))//trim(stacks(2)))
    stacks(1) = trim(stacks(1))//trim(stacks(2))
    print *,len(stacks(1))

    ! get rid of blank line
    read(1,'(A)')buffer
    print *,buffer

    do
        read(1,'(A)',end=101)buffer
        call get_three_ints_from_string(buffer,a,b,c)
        print *,a,b,c
        ! stacks(c) = trim(stacks(b)(1:a))//trim(stacks(c))
        ! stacks(b) = trim(stacks(b)(a+1:256))
        ! would have to reverse - might as well move one at a time
        do i=1,a
            stacks(c) = trim(stacks(b)(1:1))//stacks(c)
            stacks(b) = stacks(b)(2:256)
        end do

        do i=1, size(stacks)
            print *,stacks(i)
        end do 
    end do


    101 continue


    close(unit=1)
end program day5a
