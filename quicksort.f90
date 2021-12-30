program quicksort
    implicit none
    interface
        recursive subroutine sort(i, j, n, a)
            integer, intent(in) :: i, j, n
            integer, dimension(n), intent(inout) :: a
        end subroutine sort
    end interface
    integer, parameter :: n = 7
    integer, dimension(n) :: a
    a = (/8, 2, 6, 3, 7, 1, 5/)
    write(*,*) a
    call sort(1, n, n, a)
    write(*,*) a
end program quicksort

recursive subroutine sort(i, j, n, a)
    integer, intent(in) :: i, j, n
    integer, dimension(n), intent(inout) :: a
    integer :: t, p
    integer :: partition
    if (i.lt.j) then
        p = partition(i, j, n, a)
        call sort(i, p-1, n, a)
        call sort(p+1, j, n, a)
    end if
end subroutine

function partition(l, h, n, a)
    integer partition
    integer :: l, h, n, pivot, i, j
    integer, dimension(n) :: a
    pivot = a(h)
    i = l-1
    do j=l,h,1
        if (a(j).lt.pivot) then
            i = i+1
            call swap(i, j, n, a)
        end if
    end do
    call swap(i+1, h, n, a)
    partition = i+1
end function partition

subroutine swap(i, j, n, a)
    integer :: i, j, n, t
    integer, dimension(n) :: a
    t = a(i)
    a(i) = a(j)
    a(j) = t
end subroutine swap