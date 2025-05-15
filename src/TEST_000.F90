	subroutine rpncomm_test_000
	use rpn_comm
	use rpn_comm_mpi
	implicit none
	integer ierr, myrank,totpes
	call mpi_init(ierr)
	call mpi_comm_size(MPI_COMM_WORLD,totpes,ierr)
        call mpi_comm_rank(MPI_COMM_WORLD,myrank,ierr)
	print *,'I am PE ',myrank+1,' of ',totpes
	call mpi_finalize(ierr)
	stop
	end
