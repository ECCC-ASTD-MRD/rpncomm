
subroutine rpncomm_test_017
    use mpi_f08
    use rpn_comm, only: rpn_comm_mype, RPN_COMM_xch_halo_8, RPN_COMM_adj_halo4, RPN_COMM_adj_halo8, &
                        RPN_COMM_WAITALL_NOSTAT, RPN_COMM_XCH_HALOX, RPN_COMM_PROPAGATE_PILOT_CIRCULAR
    implicit none

    print *, MPI_COMM_NULL
end subroutine rpncomm_test_017
