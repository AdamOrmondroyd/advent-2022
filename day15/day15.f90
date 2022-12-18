function manhattan(a, b)
    implicit none
    integer :: manhattan
    integer, dimension(2), intent(in) :: a, b
    manhattan = abs(a(1)-b(1)) + abs(a(2)-b(2))
end function 

function is_beacon(sensor_and_beacon, x, num_sensors)
    implicit none
    integer, intent(in) :: num_sensors
    integer, dimension(num_sensors,4), intent(in) :: sensor_and_beacon
    integer, dimension(2), intent(in) :: x
    logical :: is_beacon
    integer :: i

    is_beacon = .false.

    do i=1,num_sensors
        if (sensor_and_beacon(i,3).eq.x(1).and.sensor_and_beacon(i,4).eq.x(2)) then
            is_beacon = .true.
            exit
        end if
    end do
end function


subroutine read_line(str, sensor_and_beacon)
    implicit none
    character(len=256) :: str
    character(len=256) :: buffer
    integer, dimension(4) :: sensor_and_beacon
    integer :: i, ii

    ii = 1
    do while(len_trim(str).gt.0)
        if (str(1:1)=='=') then
            str = str(2:)
            read(str,*)i
            sensor_and_beacon(ii) = i
            ii = ii + 1
        else
            str = str(2:)
        end if
    end do

end subroutine

program day15
    implicit none
    character(len=256) :: buffer
    integer :: num_sensors
    integer :: i, ii, iii, num_cannot
    logical :: cannot
    integer, dimension(:,:), allocatable :: sensor_and_beacon
    integer, dimension(:), allocatable :: distances
    integer, dimension(:,:), allocatable :: sensor

    integer, dimension(2) :: x
    integer :: r
    integer :: manhattan
    logical :: is_beacon
    logical :: is_distress
    open(unit=1,file="input.txt")
    num_sensors = 0
    do
        read(1,*,end=101)buffer
        num_sensors = num_sensors + 1
    end do
    101 continue
    rewind(1)

    allocate(sensor_and_beacon(num_sensors,4))
    allocate(distances(num_sensors))
    do i = 1, num_sensors
        read(1,'(A)')buffer
        call read_line(buffer, sensor_and_beacon(i,:))
    end do
    close(1)

    do i=1, num_sensors
        distances(i) = manhattan(sensor_and_beacon(i,1:2),sensor_and_beacon(i,3:4))
    end do

    x(2) = 10
    do ii=minval(sensor_and_beacon(:,1))-maxval(distances),maxval(sensor_and_beacon(:,1))+maxval(distances)
        x(1) = ii
        cannot = .false.
        if (is_beacon(sensor_and_beacon, x, num_sensors)) cycle
        do i=1, num_sensors
            if(manhattan(sensor_and_beacon(i,1:2),x).le.distances(i)) then
                cannot = .true.
                exit
            end if
        end do
        if (cannot) num_cannot = num_cannot + 1
    end do

    print *,"-------------------------------------------------------------------------"
    print *,num_cannot

    allocate(sensor(num_sensors,2))
    sensor = sensor_and_beacon(:,1:2)
    do iii=1,num_sensors
        r = distances(iii)+1
        x(1) = sensor(iii,1)
        x(2) = sensor(iii,2)+r
        do ii = 1, 4*r
            if (ii.le.r.or.ii.gt.3*r) then
                x(1) = x(1) + 1
            else
                x(1) = x(1) - 1
            end if
            if (ii.le.2*r) then
                x(2) = x(2) - 1
            else
                x(2) = x(2) + 1
            end if

            is_distress = .true.
            if (any(x.lt.0).or.any(x.gt.4000000)) then
                is_distress=.false.
                cycle
            end if

            do i = 1,num_sensors
                if (i.eq.iii) cycle
                if (manhattan(sensor(i,:),x).le.distances(i)) then
                    is_distress = .false.
                    exit
                end if
            end do
            if (is_distress) exit
        end do
        if (is_distress) exit
    end do
    print *,"-------------------------------------------------------------------------"
    print *,x
    print *,"I can't bring myself to change the integer types to allow this calculation"

end program day15
