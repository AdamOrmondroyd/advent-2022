function score(a, x)
    implicit none
    character :: a
    character :: x
    integer :: score

    score = 0

    if (x.eq."X") then
        score = score + 1
        if (a.eq."A") then
            score = score + 3
        else if (a.eq."C") then
            score = score + 6
        end if

    else if(x.eq."Y")  then
        score = score + 2
        if (a.eq."B") then
            score = score + 3
        else if (a.eq."A") then
            score = score + 6
        end if

    else if (x.eq."Z") then
        score = score + 3
        if (a.eq."C") then
            score = score + 3
        else if (a.eq."B") then
            score = score + 6
        end if
    end if

end function

function scoreb(a, x)
    implicit none
    character :: a
    character :: x
    integer :: score
    integer :: scoreb

    score = 0

    ! lose
    if (x.eq."X") then
        if (a.eq."A") then
            score = score + 3
        else if (a.eq."B") then
            score = score + 1
        else if (a.eq."C") then
            score = score + 2
        end if

    ! draw
    else if(x.eq."Y")  then
        score = score + 3
        if (a.eq."A") then
            score = score + 1
        else if (a.eq."B") then
            score = score + 2
        else if (a.eq."C") then
            score = score + 3
        end if

    ! win
    else if (x.eq."Z") then
        score = score + 6
        if (a.eq."A") then
            score = score + 2
        else if (a.eq."B") then
            score = score + 3
        else if (a.eq."C") then
            score = score + 1
        end if

    end if

    scoreb = score

end function



program day2a
    implicit none
    integer :: score
    integer :: scoreb
    integer :: total_score = 0
    integer :: total_scoreb = 0
    character :: a, x
    open(unit=1,file="input.txt")
    do

        read(1,*,end=101)a,x
        print *,a,x
        total_score = total_score + score(a, x)
        total_scoreb = total_scoreb + scoreb(a, x)
    end do

    101 continue
    print *,total_score
    print *,total_scoreb
    
    close(1)
end program day2a
