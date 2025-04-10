!/* RPN_COMM - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2015  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!InTfout!
!**S/R RPN_COMM_haloflip handle across the pole halo exchange for global grids
subroutine RPN_COMM_haloflip(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,ggni)  !InTfout!
  use rpn_comm_globals
  use rpn_comm, only: RPN_COMM_limit
  implicit none                                                                !InTfout!
!ARGUMENTS
  integer, intent(IN) :: minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,ggni         !InTfout!
!!#define IgnoreTypeKindRank g                                        !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                    !InTfout!
  integer, intent(INOUT) :: g(minx:maxx,miny:maxy,nk)
! minx,maxx,miny,maxy,nk     dimensions of local portion of the global grid
! ni,nj                      number of useful points along x and y in local portion of grid
! halox,haloy                halo sizes
! ggni                       dimension along x of the global grid (  abs(ggni) really )
!                            ggni < 0 used for tests
  integer :: i,j,k,ierr,gmin,gmax,gni
!  integer :: i0,in
  integer :: j_src,j_dest
  integer, dimension(minx:maxx,haloy,nk,0:1) :: local
  integer, dimension(0:pe_nx) :: count, depl
  integer :: start, finish, start2, finish2, low, high, ihave
  integer :: send, recv, send_to, send_tag, recv_from, recv_tag, nwds
  integer :: ipe
  integer :: gis, gie, l_offset, g_offset
  integer, dimension(MPI_STATUS_SIZE) :: status
!  integer, external :: RPN_COMM_limit
  real *8 :: t0,t1,t2,t3,t4,t5
!NOTES
! each PE starts by collecting its polar halo area
! this area is then circulated ring fashion in the x direction between PEs
! each PE will pick on the fly from this circulated data what it needs for its transpolar area

  if(.not. bnd_south .and. .not. bnd_north) return ! nothing to do if not containing north or south pole
  gni = abs(ggni)
  t0 = mpi_wtime()
  t1 = 0
  t2 = 0
  t3 = 0
  t4 = 0
  t5 = 0
  if(bnd_south) then                               ! south pole
    j_src = 1             ! first source row
    j_dest = 1-haloy      ! first destination row
  endif
  if(bnd_north) then                               ! north pole
    j_src = nj - haloy + 1
    j_dest = nj + 1
  endif
  ierr = RPN_COMM_limit(pe_mex,pe_nx,1,gni,gmin,gmax,count,depl)  ! get distribution along x

  start = gmin + minx -1 + gni/2  ! index of first point in global space of region across pole
  finish = start + (maxx - minx)  ! index of last point in global space of region across pole
  if(start > gni) then            ! the whole region is wrapped around end of grid
    start = start - gni           ! wrap modulo gni
    finish = finish - gni
  endif
  if(finish > gni) then           ! 2 pieces, start thru gni + 1 thru finish2
    start2 = 1
    finish2 = finish - gni
    finish = gni
  else                            ! one piece only 
    start2 = 0
    finish2 = 0
  endif
!
  send = 0;                       ! this PE will send local(:,:,:,send)
  recv = 1;                       ! this PE will receive local(:,:,:,recv)
  ihave = pe_mex                  ! incoming buffer contains halo along y of tile "ihave" in local(:,:,:,send)
  do k=1,nk
  do j=1,haloy
  do i=minx,maxx
    local(i,j,k,0) = g(i,j_src+haloy-j,k)   ! get halo along y, reverse rows along y (j)
  enddo
  enddo
  enddo
  nwds = nk * haloy * (maxx-minx+1)  ! number of elements of said halo
  g_offset = minx - start
!
  t0 = mpi_wtime() - t0
  do ipe = 1, pe_nx
    t1 = mpi_wtime()
    low = depl(ihave) + 1            ! the incoming data goes from low to high in global index space
    high = low + count(ihave) -1
    l_offset = 1 - low
    gis = max(low,start)             ! this PE looks for start thru finish in global index space
    gie = min(high,finish)
    if(ggni < 0)print *,'low=',low,' high=',high
    if (gis <= gie) then
      if(ggni < 0)print *,'%gis=',gis,gis+g_offset,' gie=',gie,gie+g_offset
      do k=1,nk
      do j=1,haloy
      do i=gis,gie
        g(i+g_offset,j_dest+j-1,k) = local(i+l_offset,j,k,send)
      enddo
      enddo
      enddo
    endif
    if(low <= finish2) then          ! there is an overlap for piece no 2, collect points for gis thru gie
!      print *,'low=',low,' finish2=',finish2,' high=',high
      gis = low
      gie = min(high,finish2)
      if(ggni < 0)print *,'+gis=',gis,gis+minx+finish-start,' +gie=',gie,gie+minx+finish-start
      do k=1,nk
      do j=1,haloy
      do i=gis,gie
        g(i+minx+finish-start,j_dest+j-1,k) = local(i+l_offset,j,k,send)
      enddo
      enddo
      enddo
    endif
    t2 = mpi_wtime()
    send_to = pe_id(pe_mex+1,pe_mey)      ! send to east neighbor
    send_tag = pe_me
    recv_from = pe_id(pe_mex-1,pe_mey)     ! recv from west neighbor
    recv_tag = recv_from
!    print *,pe_me,' sending to ',send_to,' receiving from',recv_from
!    print 1,local(:,1,1,send)
    if(ipe<pe_nx) &
    call mpi_sendrecv(local(minx,1,1,send) , nwds, MPI_INTEGER, send_to,   send_tag,   &
                      local(minx,1,1,recv) , nwds, MPI_INTEGER, recv_from, recv_tag,   &
                      pe_defcomm,status,ierr)
    send = 1 - send   ! swap send and receive buffers
    recv = 1 - recv
    ihave = ihave - 1                    ! update ihave by shifting on poition towards west
    if(ihave<0) ihave = ihave + pe_nx    ! wrap around
    t3 = mpi_wtime()
    t4 = t4 + t2-t1
    t5 = t5 + t3-t2
  enddo
  if(pe_me==0) print *,'TIMES=',nint(t0*1000000),nint(t4*1000000),nint(t5*1000000)

!  i0 = 1
!  in = count(pe_mex)
!  do k=1,nk
!  do j=1,haloy
!  do i=i0,in
!    g(i,j_dest+j-1,k) = local(i,j,k,0)
!  enddo
!  enddo
!  enddo
1 format(20I7.6)
  return
end subroutine RPN_COMM_haloflip                           !InTfout!
