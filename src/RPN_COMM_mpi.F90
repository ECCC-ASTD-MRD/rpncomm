
module rpn_comm_mpi
    use iso_c_binding
    use mpi
    implicit none

#ifdef NEED_EXTRA_MPI_ITF
    interface
        subroutine MPI_BCAST(BUFFER, COUNT, DATATYPE, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank BUFFER
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUFFER(*)
        INTEGER COUNT, DATATYPE, ROOT, COMM, IERROR
        end subroutine
    end interface
#endif
end module rpn_comm_mpi
