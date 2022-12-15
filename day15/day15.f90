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
    integer :: i, ii, num_cannot
    logical :: cannot
    integer, dimension(:,:), allocatable :: sensor_and_beacon
    integer, dimension(:), allocatable :: distances

    integer, dimension(2) :: x
    integer :: manhattan
    logical :: is_beacon
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

    do i=1, num_sensors
        distances(i) = manhattan(sensor_and_beacon(i,1:2),sensor_and_beacon(i,3:4))
    end do

    x(2) = 2000000
    print *,minval(sensor_and_beacon(:,1))-maxval(distances)
    print *,maxval(sensor_and_beacon(:,1))+maxval(distances)
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


    close(1)
end program day15
