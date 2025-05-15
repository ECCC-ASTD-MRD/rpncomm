!InTf!
      SUBROUTINE RPN_COMM_exchange_halo(pattern,array,periodx,periody,periodp,npol_row)  !InTf!
      use rpn_comm_globals
      use rpn_comm, only: RPN_COMM_xch_halo
!!    import ::  rpncomm_pattern, c_ptr                                                  !InTf!
      implicit none                                                                      !InTf!
      type(rpncomm_pattern), intent(IN) :: pattern                                       !InTf!
      type(c_ptr), intent(IN) :: array                                                   !InTf!
      logical, intent(IN), OPTIONAL  :: periodx,periody, periodp                         !InTf!
      integer, intent(IN), OPTIONAL :: npol_row                                          !InTf!
      integer, dimension(:), pointer :: g
      integer :: minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,gni
      type(rpncomm_field), pointer :: f

      call c_f_pointer(pattern%p,f)
      minx = f%x%lo
      maxx = f%x%hi
      miny = f%y%lo
      maxy = f%y%hi
      ni = f%x%lnp
      nj = f%y%lnp
      nk = f%z%lnp
      halox = f%hx
      haloy = f%hy
      gni = f%x%gnp
      call c_f_pointer(array,g,[(maxx-minx+1)*(maxy-miny+1)*nk])

      if(npol_row > 0) then  !  semi lag exchange, call old code
        call RPN_COMM_xch_halo(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,periodx,periody,gni,npol_row)
      else
        call RPN_COMM_exchange_ew(g,minx,maxx,miny,maxy,ni,nj,nk,halox,periodx,f%ew3d)
        call RPN_COMM_exchange_ns(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,periody,f%ns3d)
        if(periodp) call RPN_COMM_haloflip(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,gni)
      endif

      end SUBROUTINE RPN_COMM_exchange_halo                                              !InTf!

      SUBROUTINE RPN_COMM_exchange_ew(g,minx,maxx,miny,maxy,ni,nj,nk,halox,periodx,ew3d)
      use rpn_comm
      use rpn_comm_mpi
      implicit none
      integer, intent(IN) :: minx,maxx,miny,maxy,ni,nj,nk,halox,ew3d
      logical, intent(IN) :: periodx
      integer, dimension(minx:maxx,miny:maxy,nk), intent(INOUT) :: g
      logical :: east, west
      integer :: eastpe, westpe
      integer :: ierr
      integer, dimension(MPI_STATUS_SIZE) :: status

      east=(bnd_east) .and. (.not.periodx)
      eastpe=pe_id(pe_mex+1,pe_mey)
      west=(bnd_west) .and. (.not.periodx)
      westpe=pe_id(pe_mex-1,pe_mey)

      !   message tag is 1000 + pe_id of sender
      !   halo to east starts at g(ni-halox+1,1,1)
      !   halo from west starts at g(1-halox,1,1)
      !   halo to west starts at g(1,1,1)
      !   halo from east starts at g(ni+1,1,1)
      if(west) then                !   send to east_neighbor
        if(.not.east)then
          call MPI_SEND(g(ni-halox+1,1,1),1,ew3d,eastpe,1000+pe_medomm,PE_DEFCOMM,ierr)
        endif
      else if(east) then           !   receive from west_neighbor
        call MPI_RECV(g(1-halox,1,1),1,ew3d,westpe,1000+westpe,PE_DEFCOMM,status,ierr)
      else                         !   send to east_neighbor and receive from west_neighbor
        call MPI_SENDRECV(                                    &
               g(ni-halox+1,1,1),1,ew3d,eastpe,1000+pe_medomm,  &
               g(1-halox,1,1),1,ew3d,westpe,1000+westpe, &
               PE_DEFCOMM,status,ierr)
      endif
      if(east) then                !   send to west_neighbor
        if(.not.west) then
          call MPI_SEND(g(1,1,1),1,ew3d,westpe,1000+pe_medomm,PE_DEFCOMM,ierr)
        endif
      else if(west) then           !   receive from east_neighbor
        call MPI_RECV(g(ni+1,1,1),1,ew3d,eastpe,1000+eastpe,PE_DEFCOMM,status,ierr)
      else                         !   send to west_neighbor and receive from east_neighbor
        call MPI_SENDRECV(                                    &
               g(1,1,1),1,ew3d,westpe,1000+pe_medomm,  &
               g(ni+1,1,1),1,ew3d,eastpe,1000+eastpe, &
               PE_DEFCOMM,status,ierr)
      endif
      end SUBROUTINE RPN_COMM_exchange_ew

      SUBROUTINE RPN_COMM_exchange_ns(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,periody,ns3d)
      use rpn_comm, only: pe_id, bnd_north, bnd_south, pe_mex, pe_mey
      implicit none
      integer, intent(IN) :: minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,ns3d
      logical, intent(IN) :: periody
      integer, dimension(minx:maxx,miny:maxy,nk), intent(INOUT) :: g
      logical :: north, south
      integer :: northpe, southpe

      north=(bnd_north) .and. (.not.periody)
      northpe=pe_id(pe_mex,pe_mey+1)
      south=(bnd_south) .and. (.not.periody)
      southpe=pe_id(pe_mex,pe_mey-1)
      end SUBROUTINE RPN_COMM_exchange_ns
