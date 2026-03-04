


subroutine rpncomm_test_015()
    use rpn_comm
    implicit none

    integer :: ierr
    integer :: i0
    logical :: l0

    call rpn_comm_bcast(l0, 1, 'MPI_LOGICAL', 0, 'GRID', ierr)
    call rpn_comm_bcast(i0, 1, 'MPI_INTEGER', 0, 'GRID', ierr)

end subroutine rpncomm_test_015
