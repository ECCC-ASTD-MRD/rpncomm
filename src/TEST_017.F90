
subroutine rpncomm_test_017
    use mpi_f08
    use rpn_comm, only: rpn_comm_mype, RPN_COMM_xch_halo_8, RPN_COMM_adj_halo4, RPN_COMM_adj_halo8
    implicit none

    print *, MPI_COMM_NULL
end subroutine rpncomm_test_017
