! This is a test of the callback functionaliy (from RPN_COMM_io_pes)

subroutine rpncomm_test_018
  use rpn_comm
  implicit none

  integer, external :: RPN_COMM_io_pe_test_callback   ! the callback function returns an integer value
  integer setno,nio,me_io,setno2,setno3,status
  integer, dimension(1), target :: argv               ! the argument list for the callback function
  integer, dimension(:,:), pointer :: iopelist        ! will receive a coordinate list
  integer, dimension(1) :: tbcst                      ! this will be broadcast in the test
!
! IGNORE
  if(pe_me == 0)  print *,'DEBUG: pe_nx,pe_ny',pe_nx,pe_ny
  nio = min(pe_nx,pe_ny)
  print 100,'RPN_COMM_io_pe test program, pe_nx,pe_ny,pe_me,pe_mex,pe_mey,nio=',pe_nx,pe_ny,pe_me,pe_mex,pe_mey,nio
! EXAMPLE
  setno = RPN_COMM_create_io_set(nio+2,0)                   ! create a set of IO PEs containing nio+2 members
  me_io = RPN_COMM_is_io_pe(setno)                          ! is this PE a member of this IO PE set ?
  if(me_io .ne. -1) then
    print *,"I am a proud IO pe !"                          ! YES it is
  else
    print *,"I am a lazy  NON-IO pe !"                      ! NO it is not
  endif
  print *,"set number, size of set='",setno,RPN_COMM_io_pe_size(setno)         ! get size of this IO PE set
  setno2 = RPN_COMM_create_io_set(nio,0)                                       ! crete another set containing nio PEs
  print *,"set number, size of set='",setno2,RPN_COMM_io_pe_size(setno2)       ! get size of this other IO PE set (should be nio+2)
  setno = RPN_COMM_free_io_set(setno)                                          ! delete IO set
  print *,'DEBUG: freed IO set ',setno
  print *,"set number, size of set='",setno,RPN_COMM_io_pe_size(setno)         ! this should return -1
  setno = RPN_COMM_create_io_set(nio,0)                                        ! re create IO PE set, this time with nio PEs
  print *,"set number, size of set='",setno,RPN_COMM_io_pe_size(setno)         ! this should return nio now
  setno3 = RPN_COMM_create_io_set(nio-1,0)                                     ! crete another set containing nio-1 PEs
  print *,"set number, size of set='",setno3,RPN_COMM_io_pe_size(setno3)       ! this should return nio-1
  argv(1) = pe_me                                                              ! argument array for callback function
  status = RPN_COMM_io_pe_callback(setno3,RPN_COMM_io_pe_test_callback,C_LOC(argv(1)))    ! call to callback function(see example below)
  print *,"after callback, status,argv=",status,argv(1)                        ! status 0 from non members of IO set, return of callback function on members
  iopelist => RPN_COMM_io_pe_coord(setno3)                                     ! get grid coordinates of PEs in IO set setno3
  print *,"PE list x=",iopelist(:,1)                                           ! row 1, X coordinates in "GRID"
  print *,"PE list y=",iopelist(:,2)                                           ! row 2, Y coordinates in "GRID"
  tbcst = pe_me
  if(RPN_COMM_is_io_pe(setno3) .ne. -1)then  ! part of the set only, bcst pe_me of highest rank in set
    call RPN_COMM_io_pe_bcast(tbcst,1,'MPI_INTEGER',RPN_COMM_io_pe_size(setno3)-1,setno3,status) 
!   root for broadcast is RPN_COMM_io_pe_size(setno3)-1, get "GRID" ordinal of PE with highest rank (last PE) in IO set
!   root for broadcast 0 would have returned the "GRID" ordinal of first (lowest rank) PE in set
    print *,'tbcst after broadcast',tbcst                                      ! and the answer is ...
  endif
!******
100 format(A,10I5)
  return
end subroutine rpncomm_test_018
