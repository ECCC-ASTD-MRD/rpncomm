
program format_comma
    use rpn_comm
    implicit none

    integer, parameter :: num_pes = 100
    integer, parameter :: num_pex = 10
    integer, parameter :: num_pey = 10
    integer :: status
    integer, dimension(num_pes) :: pex, pey
    integer :: i

    do i = 1, num_pes
        pex(i) = mod(i, num_pex)
        pey(i) = mod(i+1, num_pey)
    end do


    ! This should not crash
    status = RPN_COMM_check_ioset(1, pex, pey, num_pes, num_pex, num_pey, 0, .true.)
end program format_comma
