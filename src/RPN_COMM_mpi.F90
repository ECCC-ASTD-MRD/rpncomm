
module rpn_comm_mpi
    use iso_c_binding
    use mpi

    implicit none

#ifdef NEED_EXTRA_MPI_ITF
    interface

    ! --------------------------- SEND ----------------------------
    subroutine MPI_SEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: IERROR
    end subroutine MPI_SEND

    subroutine MPI_ISEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: REQUEST, IERROR
    end subroutine MPI_ISEND

    subroutine MPI_BSEND(BUF, COUNT,DATATYPE, DEST, TAG, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: IERROR
    end subroutine MPI_BSEND

    subroutine MPI_IBSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: REQUEST, IERROR
    end subroutine MPI_IBSEND

    subroutine MPI_RSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: IERROR
    end subroutine MPI_RSEND

    subroutine MPI_IRSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: REQUEST, IERROR
    end subroutine MPI_IRSEND

    subroutine MPI_SSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: IERROR
    end subroutine MPI_SSEND

    subroutine MPI_ISSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, DEST, TAG, COMM
        INTEGER, intent(out) :: REQUEST, IERROR
    end subroutine MPI_ISSEND

    ! --------------------------- RECEIVE ----------------------------

    subroutine MPI_RECV(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)
        import :: MPI_STATUS_SIZE
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, SOURCE, TAG, COMM
        INTEGER              :: STATUS(MPI_STATUS_SIZE)
        INTEGER, intent(out) :: IERROR
    end subroutine MPI_RECV

    subroutine MPI_IRECV(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, SOURCE, TAG, COMM
        INTEGER, intent(out) :: REQUEST, IERROR
    end subroutine MPI_IRECV

    subroutine MPI_MRECV(BUF, COUNT, DATATYPE, MESSAGE, STATUS, IERROR)
        import :: MPI_STATUS_SIZE
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER COUNT, DATATYPE, MESSAGE
        INTEGER STATUS(MPI_STATUS_SIZE)
        INTEGER, intent(out) :: IERROR
    end subroutine MPI_MRECV

    subroutine MPI_IMRECV(BUF, COUNT, DATATYPE, MESSAGE, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER COUNT, DATATYPE, MESSAGE, REQUEST, IERROR
    end subroutine MPI_IMRECV

    ! --------------------------- SEND-RECEIVE ----------------------------
    subroutine MPI_SENDRECV(SENDBUF, SENDCOUNT, SENDTYPE, DEST, SENDTAG,            &
            RECVBUF, RECVCOUNT, RECVTYPE, SOURCE, RECVTAG, COMM, STATUS, IERROR)
        import :: MPI_STATUS_SIZE
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, DEST, SENDTAG
        INTEGER RECVCOUNT, RECVTYPE, SOURCE, RECVTAG, COMM
        INTEGER STATUS(MPI_STATUS_SIZE), IERROR
    end subroutine MPI_SENDRECV

    subroutine MPI_ISENDRECV(SENDBUF, SENDCOUNT, SENDTYPE, DEST, SENDTAG,           &
            RECVBUF, RECVCOUNT, RECVTYPE, SOURCE, RECVTAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>    SENDBUF(*), RECVBUF(*)
        INTEGER    SENDCOUNT, SENDTYPE, DEST, SENDTAG
        INTEGER    RECVCOUNT, RECVTYPE, SOURCE, RECVTAG, COMM
        INTEGER    REQUEST, IERROR
    end subroutine MPI_ISENDRECV

    subroutine MPI_SENDRECV_REPLACE(BUF, COUNT, DATATYPE, DEST, SENDTAG, SOURCE,    &
            RECVTAG, COMM, STATUS, IERROR)
        import :: MPI_STATUS_SIZE
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUF(*)
        INTEGER COUNT, DATATYPE, DEST, SENDTAG
        INTEGER SOURCE, RECVTAG, COMM
        INTEGER STATUS(MPI_STATUS_SIZE), IERROR
    end subroutine MPI_SENDRECV_REPLACE

    subroutine MPI_ISENDRECV_REPLACE(BUF, COUNT, DATATYPE, DEST, SENDTAG, SOURCE,   &
            RECVTAG, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUF
#include "IgnoreTypeKindRank.hf"
        ! <type>    BUF(*)
        INTEGER    COUNT, DATATYPE, DEST, SENDTAG
        INTEGER    SOURCE, RECVTAG, COMM
        INTEGER    REQUEST, IERROR
    end subroutine MPI_ISENDRECV_REPLACE

    ! --------------------------- BROADCAST ----------------------------
    subroutine MPI_BCAST(BUFFER, COUNT, DATATYPE, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank BUFFER
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUFFER(*)
        INTEGER, intent(in)  :: COUNT, DATATYPE, ROOT, COMM
        INTEGER, intent(out) :: IERROR
    end subroutine

    subroutine MPI_IBCAST(BUFFER, COUNT, DATATYPE, ROOT, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank BUFFER
#include "IgnoreTypeKindRank.hf"
        ! <type>  BUFFER(*)
        INTEGER COUNT, DATATYPE, ROOT, COMM, REQUEST, IERROR
    end subroutine MPI_IBCAST
        
    ! --------------------------- SCATTER ----------------------------
    subroutine MPI_SCATTER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,        &
             RECVTYPE, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
        INTEGER COMM, IERROR
    end subroutine MPI_SCATTER

    subroutine MPI_ISCATTER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,       &
            RECVTYPE, ROOT, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_ISCATTER

    subroutine MPI_SCATTER_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,   &
             RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_SCATTER_INIT

    subroutine MPI_SCATTERV(SENDBUF, SENDCOUNTS, DISPLS, SENDTYPE, RECVBUF,         &
            RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), DISPLS(*), SENDTYPE
        INTEGER RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR
    end subroutine MPI_SCATTERV

    subroutine MPI_ISCATTERV(SENDBUF, SENDCOUNTS, DISPLS, SENDTYPE, RECVBUF,        &
            RECVCOUNT, RECVTYPE, ROOT, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), DISPLS(*), SENDTYPE
        INTEGER RECVCOUNT, RECVTYPE, ROOT, COMM, REQUEST, IERROR
    end subroutine MPI_ISCATTERV

    subroutine MPI_SCATTERV_INIT(SENDBUF, SENDCOUNTS, DISPLS, SENDTYPE, RECVBUF,    &
            RECVCOUNT, RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), DISPLS(*), SENDTYPE
        INTEGER RECVCOUNT, RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR
    end subroutine MPI_SCATTERV_INIT

    ! --------------------------- GATHER ----------------------------

    subroutine MPI_GATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,     &
            RECVTYPE, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
        INTEGER COMM, IERROR
    end subroutine MPI_GATHER

    subroutine MPI_IGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,    &
            RECVTYPE, ROOT, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_IGATHER

    subroutine MPI_GATHER_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,    &
            RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_GATHER_INIT

    subroutine MPI_GATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNTS,       &
            DISPLS, RECVTYPE, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNTS(*), DISPLS(*)
        INTEGER RECVTYPE, ROOT, COMM, IERROR
    end subroutine MPI_GATHERV

    subroutine MPI_IGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNTS,      &
            DISPLS, RECVTYPE, ROOT, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNTS(*), DISPLS(*)
        INTEGER RECVTYPE, ROOT, COMM, REQUEST, IERROR
    end subroutine MPI_IGATHERV

    subroutine MPI_GATHERV_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNTS,  &
            DISPLS, RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNTS(*), DISPLS(*)
        INTEGER RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR
    end subroutine MPI_GATHERV_INIT

    subroutine MPI_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,      &
            RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF (*), RECVBUF (*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
        INTEGER IERROR
    end subroutine MPI_ALLGATHER

    subroutine MPI_IALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,     &
            RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF (*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
        INTEGER REQUEST, IERROR
    end subroutine MPI_IALLGATHER

    subroutine MPI_ALLGATHER_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, &
            RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF (*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
        INTEGER INFO, REQUEST, IERROR
    end subroutine MPI_ALLGATHER_INIT

    subroutine MPI_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,        &
            RECVCOUNT, DISPLS, RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT(*)
        INTEGER DISPLS(*), RECVTYPE, COMM, IERROR
    end subroutine MPI_ALLGATHERV

    subroutine MPI_IALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,       &
            RECVCOUNT, DISPLS, RECVTYPE, COMM,  REQUEST,  IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT(*)
        INTEGER DISPLS(*), RECVTYPE, COMM, REQUEST, IERROR
    end subroutine MPI_IALLGATHERV

    subroutine MPI_ALLGATHERV_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,   &
            RECVCOUNT, DISPLS, RECVTYPE, COMM,  INFO,  REQUEST,  IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT(*)
        INTEGER DISPLS(*), RECVTYPE, COMM, INFO, REQUEST, IERROR
    end subroutine MPI_ALLGATHERV_INIT

    ! --------------------------- REDUCE ----------------------------
    subroutine MPI_REDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, ROOT, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER COUNT, DATATYPE, OP, ROOT, COMM, IERROR
    end subroutine MPI_REDUCE

    subroutine MPI_IREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, ROOT, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER COUNT, DATATYPE, OP, ROOT, COMM, REQUEST, IERROR
    end subroutine MPI_IREDUCE

    subroutine MPI_REDUCE_INIT(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, ROOT, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER COUNT, DATATYPE, OP, ROOT, COMM, INFO, REQUEST, IERROR
    end subroutine MPI_REDUCE_INIT

    subroutine MPI_ALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER COUNT, DATATYPE, OP, COMM, IERROR
    end subroutine MPI_ALLREDUCE

    subroutine MPI_IALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER COUNT, DATATYPE, OP, COMM, REQUEST, IERROR
    end subroutine MPI_IALLREDUCE

    subroutine MPI_ALLREDUCE_INIT(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER COUNT, DATATYPE, OP, COMM, INFO, REQUEST, IERROR
    end subroutine MPI_ALLREDUCE_INIT
    
    subroutine MPI_REDUCE_SCATTER(SENDBUF, RECVBUF, RECVCOUNTS, DATATYPE, OP, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER RECVCOUNTS(*), DATATYPE, OP, COMM, IERROR
    end subroutine MPI_REDUCE_SCATTER

    subroutine MPI_IREDUCE_SCATTER(SENDBUF, RECVBUF, RECVCOUNTS, DATATYPE, OP, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER RECVCOUNTS(*), DATATYPE, OP, COMM, REQUEST, IERROR
    end subroutine MPI_IREDUCE_SCATTER

    subroutine MPI_REDUCE_SCATTER_INIT(SENDBUF, RECVBUF, RECVCOUNTS, DATATYPE, OP, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER RECVCOUNTS(*), DATATYPE, OP, COMM, INFO, REQUEST, IERROR
    end subroutine MPI_REDUCE_SCATTER_INIT
    
    ! --------------------------- ALL-TO-ALL ----------------------------

    subroutine MPI_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
        INTEGER COMM, IERROR
    end subroutine MPI_ALLTOALL

    subroutine MPI_IALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_IALLTOALL

    subroutine MPI_ALLTOALL_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_ALLTOALL_INIT

    subroutine MPI_ALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,        &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPE
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPE
        INTEGER COMM, IERROR
    end subroutine MPI_ALLTOALLV

    subroutine MPI_IALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,       &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPE
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPE
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_IALLTOALLV

    subroutine MPI_ALLTOALLV_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,           &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPE
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPE
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_ALLTOALLV_INIT

    subroutine MPI_ALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,           &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
        INTEGER COMM, IERROR
    end subroutine MPI_ALLTOALLW

    subroutine MPI_IALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,          &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_IALLTOALLW

    subroutine MPI_ALLTOALLW_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,      &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_ALLTOALLW_INIT

    ! --------------------------- NEIGHBORS ----------------------------

    subroutine MPI_NEIGHBOR_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,     &
            RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF (*), RECVBUF (*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
        INTEGER IERROR
    end subroutine MPI_NEIGHBOR_ALLGATHER

    subroutine MPI_INEIGHBOR_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,    &
            RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF (*), RECVBUF (*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
        INTEGER REQUEST, IERROR
    end subroutine MPI_INEIGHBOR_ALLGATHER

    subroutine MPI_NEIGHBOR_ALLGATHER_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,&
            RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF (*), RECVBUF (*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
        INTEGER INFO, REQUEST, IERROR
    end subroutine MPI_NEIGHBOR_ALLGATHER_INIT

    subroutine MPI_NEIGHBOR_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,       &
            RECVCOUNT, DISPLS, RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT(*)
        INTEGER DISPLS(*), RECVTYPE, COMM, IERROR
    end subroutine MPI_NEIGHBOR_ALLGATHERV

    subroutine MPI_INEIGHBOR_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,      &
            RECVCOUNT, DISPLS, RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT(*)
        INTEGER DISPLS(*), RECVTYPE, COMM,REQUEST, IERROR
    end subroutine MPI_INEIGHBOR_ALLGATHERV

    subroutine MPI_NEIGHBOR_ALLGATHERV_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,  &
            RECVCOUNT, DISPLS, RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT(*)
        INTEGER DISPLS(*), RECVTYPE, COMM,INFO,REQUEST, IERROR
    end subroutine MPI_NEIGHBOR_ALLGATHERV_INIT

    subroutine MPI_NEIGHBOR_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,      &
            RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
        INTEGER COMM, IERROR
    end subroutine MPI_NEIGHBOR_ALLTOALL

    subroutine MPI_INEIGHBOR_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,     &
            RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_INEIGHBOR_ALLTOALL

    subroutine MPI_NEIGHBOR_ALLTOALL_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, &
            RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_NEIGHBOR_ALLTOALL_INIT

    subroutine MPI_NEIGHBOR_ALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,       &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPE
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPE
        INTEGER COMM, IERROR
    end subroutine MPI_NEIGHBOR_ALLTOALLV

    subroutine MPI_INEIGHBOR_ALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,      &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPE
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPE
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_INEIGHBOR_ALLTOALLV

    subroutine MPI_NEIGHBOR_ALLTOALLV_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,  &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, INFO, REQUEST, IERROR)
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SDISPLS(*), SENDTYPE
        INTEGER RECVCOUNTS(*), RDISPLS(*), RECVTYPE
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_NEIGHBOR_ALLTOALLV_INIT

    subroutine MPI_NEIGHBOR_ALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,      &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SENDTYPES(*)
        INTEGER RECVCOUNTS(*), RECVTYPES(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) SDISPLS(*), RDISPLS(*)
        INTEGER COMM, IERROR
    end subroutine MPI_NEIGHBOR_ALLTOALLW

    subroutine MPI_INEIGHBOR_ALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,     &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, REQUEST, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SENDTYPES(*)
        INTEGER RECVCOUNTS(*), RECVTYPES(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) SDISPLS(*), RDISPLS(*)
        INTEGER COMM, REQUEST, IERROR
    end subroutine MPI_INEIGHBOR_ALLTOALLW

    subroutine MPI_NEIGHBOR_ALLTOALLW_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,     &
            RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, INFO, REQUEST, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank SENDBUF
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RECVBUF
#include "IgnoreTypeKindRank.hf"
        ! <type>  SENDBUF(*), RECVBUF(*)
        INTEGER SENDCOUNTS(*), SENDTYPES(*)
        INTEGER RECVCOUNTS(*), RECVTYPES(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) SDISPLS(*), RDISPLS(*)
        INTEGER COMM, INFO, REQUEST, IERROR
    end subroutine MPI_NEIGHBOR_ALLTOALLW_INIT

    ! --------------------------- WINDOW STUFF ----------------------------

    subroutine MPI_GET(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,     &
            TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK
        INTEGER TARGET_COUNT, TARGET_DATATYPE, WIN, IERROR
    end subroutine MPI_GET

    subroutine MPI_RGET(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,    &
            TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, REQUEST, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK
        INTEGER TARGET_COUNT, TARGET_DATATYPE, WIN, REQUEST, IERROR
    end subroutine MPI_RGET

    subroutine MPI_GET_ACCUMULATE(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, RESULT_ADDR,  &
            RESULT_COUNT, RESULT_DATATYPE, TARGET_RANK, TARGET_DISP, TARGET_COUNT,          &
            TARGET_DATATYPE, OP, WIN, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RESULT_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR, RESULT_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_COUNT, TARGET_DATATYPE
        INTEGER TARGET_RANK, RESULT_COUNT, RESULT_DATATYPE, OP, WIN, IERROR
    end subroutine MPI_GET_ACCUMULATE

    subroutine MPI_RGET_ACCUMULATE(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, RESULT_ADDR, &
            RESULT_COUNT, RESULT_DATATYPE, TARGET_RANK, TARGET_DISP, TARGET_COUNT,          &
            TARGET_DATATYPE, OP, WIN, REQUEST, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
#define IgnoreTypeKindRank RESULT_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR, RESULT_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_COUNT, TARGET_DATATYPE
        INTEGER TARGET_RANK, RESULT_COUNT, RESULT_DATATYPE, OP, WIN, REQUEST, IERROR
    end subroutine MPI_RGET_ACCUMULATE

    subroutine MPI_ACCUMULATE(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,  &
            TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, OP, WIN, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT
        INTEGER TARGET_DATATYPE, OP, WIN, IERROR
    end subroutine MPI_ACCUMULATE

    subroutine MPI_RACCUMULATE(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, &
            TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, OP, WIN, REQUEST, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT
        INTEGER TARGET_DATATYPE, OP, WIN, REQUEST, IERROR
    end subroutine MPI_RACCUMULATE

    subroutine MPI_PUT(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,     &
            TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT
        INTEGER TARGET_DATATYPE, WIN, IERROR
    end subroutine MPI_PUT

    subroutine MPI_RPUT(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,    &
            TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, REQUEST, IERROR)
        import :: MPI_ADDRESS_KIND
        implicit none
#define IgnoreTypeKindRank ORIGIN_ADDR
#include "IgnoreTypeKindRank.hf"
        ! <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT
        INTEGER TARGET_DATATYPE, WIN, REQUEST, IERROR
    end subroutine MPI_RPUT

    end interface
#endif
end module rpn_comm_mpi
