subroutine falling(minx, maxx, miny, maxy)
    implicit none
    integer :: minx, maxx, miny, maxy
end subroutine

subroutine day14a(minx, maxx, miny, maxy)
    implicit none
    integer :: minx, maxx, miny, maxy
    integer :: leftx, lefty, rightx, righty
    integer :: x, y
    integer :: i
    logical, allocatable, dimension(:,:) :: sand
    integer :: result_a
    character(len=1024) :: buffer

    ! have to make it 1 wider to allow spillage to the side
    allocate(sand(minx-1:maxx+1, miny:maxy))
    sand = .false.

    open(unit=1,file="input.txt")
    do
        read(1,'(A)',end=101)buffer
        print *,trim(buffer)
        read(buffer, *, end=102)leftx, lefty
        call cut(buffer)
        do
            read(buffer, *, end=102)rightx,righty
            call cut(buffer)
            if(lefty.eq.righty) then ! horizontal
                ! have to check slice direction
                if (leftx.le.rightx) then
                    sand(leftx:rightx, lefty) = .true.
                else
                    sand(rightx:leftx, lefty) = .true.
                end if
            else ! vertical
                ! have to check slice direction
                if(lefty.le.righty) then
                    sand(leftx,lefty:righty) = .true.
                else
                    sand(leftx,righty:lefty) = .true.
                end if
            end if

            leftx = rightx
            lefty = righty
        end do
        102 continue
    end do
    101 continue
    close(1)

    do i=miny, maxy
        print *,sand(:,i)
    end do

    result_a = 0
    x = 500
    y = 0
    do
        if (y.eq.maxy) exit

        if(.not.sand(x,y+1)) then
            y = y+1
        else if(.not.sand(x-1,y+1)) then
            x = x-1
            y = y+1
        else if(.not.sand(x+1,y+1)) then
            x = x+1
            y = y+1
        else
            sand(x,y) = .true. 
            result_a = result_a+1
            x = 500
            y = 0
        end if
    end do
    print *,result_a

end subroutine

subroutine cut(str)
    implicit none
    character(len=1024) :: str
    integer :: i
    integer :: original_len

    original_len = len_trim(str)
    print *,"len trim", len_trim(str)
    do i=1,len_trim(str)
        if (str(i:i)=='>') then
            str = str(i+2:)
            exit
        end if
    end do
    if (original_len.eq.len_trim(str)) str = ''
end subroutine

program day14
    implicit none
    integer :: x, y, minx, maxx, miny, maxy
    character(len=1024) :: buffer

    minx = 500
    maxx = minx
    miny = 0
    maxy = miny


    open(unit=1,file="input.txt")
    do
        read(1,'(A)',end=103)buffer
        print *,trim(buffer)
        do
            read(buffer, *, end=104)x,y
            call cut(buffer)
            if (x<minx) minx = x
            if (x>maxx) maxx = x
            if (y<miny) miny = y
            if (y>maxy) maxy = y
        end do
        104 continue
    end do
    103 continue
    close(1)
    print *,minx, maxx, miny, maxy
    call day14a(minx,maxx,miny,maxy)
end program day14
