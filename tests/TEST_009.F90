subroutine RPN_COMM_haloflip_test(halox, haloy, ni, nj, nk)   ! test routine for RPN_COMM_haloflip
  use rpn_comm
  use rpn_comm_mpi
  implicit none
  integer, intent(IN) :: halox, haloy, ni, nj, nk
  integer :: ierr, errors, errorn, terrs, terrn
!  integer, parameter :: halox=3, haloy=3, ni=55, nj=27, nk=80
!  integer, parameter :: halox=1, haloy=2, ni=5, nj=6, nk=1
  integer :: minx
  integer :: maxx
  integer :: miny
  integer :: maxy
  integer, dimension(:,:,:), allocatable :: g
  integer :: gni, i, j, k, ipe, gmin, gmax
  integer, dimension(0:100) :: count, depl
  integer :: iexpect, jexpect
  real *8 :: t0,t1,t2,t3,t4
  integer :: tflg=-1

  if(command_argument_count()>0) tflg=1
  print *,'argument count =',command_argument_count()
  minx=1-halox-1
  maxx=ni+halox+1
  miny=1-haloy-1
  maxy=nj+haloy+1
  pe_defcomm = MPI_COMM_WORLD   ! value for module rpn_comm
  call mpi_comm_rank(MPI_COMM_WORLD,pe_mex,ierr)
  pe_me = pe_mex   ! value for module rpn_comm
  pe_mey = 0       ! value for module rpn_comm
  call mpi_comm_size(MPI_COMM_WORLD,pe_nx,ierr)
  pe_ny = 1        ! value for module rpn_comm
  allocate(pe_id(-1:pe_nx,0:0))     ! value for module rpn_comm
  do i = 0,pe_nx-1
    pe_id(i,0) = i                  ! value for module rpn_comm
  enddo
  pe_id(-1,0) = pe_nx-1             ! value for module rpn_comm
  pe_id(pe_nx,0) = 0                ! value for module rpn_comm
  allocate(g(minx:maxx,miny:maxy,nk))
  gni = ni*pe_nx
  gni = gni -  mod(gni,2)   ! must be even
  ierr = RPN_COMM_limit(pe_mex,pe_nx,1,gni,gmin,gmax,count,depl)
  if(pe_mex == 0) then
    write(6,*)'gni=',gni,' nj=',nj,' nk=',nk
    write(6,1)'count =',count(0:pe_nx-1)
    write(6,1)'depl  =',depl(0:pe_nx-1)
    call flush(6)
  endif
  g = 0
  do k=1,nk
  do j=1,nj
  do i=1,count(pe_mex)
!    g(i,j,k) = 10000*(i+pe_mex*ni) + 100*j + k
    g(i,j,k) = 10000*(i+gmin-1) + 100*j + k
  enddo
  enddo
  enddo
  bnd_south = .false.
  bnd_north = .true.
  call RPN_COMM_haloflip(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,tflg*gni)
!  goto 2
  bnd_south = .true.
  bnd_north = .false.
!  do k=nk,1,-1
!  do j=maxy,miny,-1
!    print 1,g(:,j,k)
!  enddo
!  enddo
!  print *,"========================================================="
  call RPN_COMM_haloflip(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,tflg*gni)
2 if(gni > 25) goto 3
  do ipe = 0,pe_nx
    if(ipe == pe_mex) then
      write(6,*)'============',pe_mex,'============'
      do k=nk,1,-1
      do j=maxy,miny,-1
        write(6,1) "",g(:,j,k)
      enddo
      enddo
    endif
    call flush(6)
    call mpi_barrier(MPI_COMM_WORLD,ierr)
  enddo
3 continue
  errors = 0
  errorn = 0
  do k=1,nk
  do j=1,haloy
  do i=minx,maxx
    iexpect = gmin - 1 + i + gni/2
    if(iexpect > gni) iexpect = iexpect - gni
    jexpect = nj + 1 - j       ! north side
    if(g(i,nj+j,k) .ne. iexpect*10000+jexpect*100+k) errorn = errorn + 1
    if(errorn == 1) write(6,*)'expected',iexpect*10000+jexpect*100+k,' got',g(i,nj+j,k),i,nj+j,k
    jexpect = j            ! south side
    if(g(i,1-j,k) .ne. iexpect*10000+jexpect*100+k) errors = errors + 1
    if(errors == 1) write(6,*)'expected',iexpect*10000+jexpect*100+k,' got',g(i,1-j,k),i,1-j,k
  enddo
  enddo
  enddo
  if(errors+errorn > 0) write(6,*) 'errors=',errors,' errorn=',errorn
  call mpi_allreduce(errors,terrs,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
  call mpi_allreduce(errorn,terrn,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
  if(pe_mex == 0) write(6,*)'total erors (south,north) =',terrs,terrn
  deallocate(g)
  deallocate(pe_id)
1 format(A,(20I7.6))
  return
end subroutine RPN_COMM_haloflip_test


subroutine rpncomm_test_009
! call RPN_COMM_haloflip_test(halox, haloy, ni, nj, nk)
  real*8 :: t1,t2,mpi_wtime
  integer :: msec
  external :: mpi_wtime
  call mpi_init(ierr)
  call RPN_COMM_haloflip_test(1,     2,      5,  6,  1)
  t1 = mpi_wtime()
  call RPN_COMM_haloflip_test(3,     3,     75, 27, 80)
  t2 = mpi_wtime()
  msec=(t2-t1)*1000000
  print *,'time=',msec,' microseconds'
  t1 = mpi_wtime()
  call RPN_COMM_haloflip_test(3,     3,     75, 27, 80)
  t2 = mpi_wtime()
  msec=(t2-t1)*1000000
  print *,'time=',msec,' microseconds'
  call mpi_finalize(ierr)
  stop
end
