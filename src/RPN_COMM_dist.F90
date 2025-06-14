!/! RPN_COMM - Library of useful routines for C and FORTRAN programming
! ! Copyright (C) 1975-2015  Division de Recherche en Prevision Numerique
! !                          Environnement Canada
! !
! ! This library is free software; you can redistribute it and/or
! ! modify it under the terms of the GNU Lesser General Public
! ! License as published by the Free Software Foundation,
! ! version 2.1 of the License.
! !
! ! This library is distributed in the hope that it will be useful,
! ! but WITHOUT ANY WARRANTY; without even the implied warranty of
! ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! ! Lesser General Public License for more details.
! !
! ! You should have received a copy of the GNU Lesser General Public
! ! License along with this library; if not, write to the
! ! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! ! Boston, MA 02111-1307, USA.
! !/
        subroutine RPN_COMM_fast_dist(garr,gmini,gmaxi,gminj,&    !InTfout!
     &          gmaxj,nig,njg,nk,ghalox,ghaloy,size,         &    !InTfout!
     &          larr,mini,maxi,minj,maxj,halox,haloy,        &    !InTfout!
     &          periodx,periody,status)                           !InTfout!
!
!arguments
!  I    garr    array containing data to distribute, USED ONLY on PE 0
!               but should exist on all the other PEs ( garr(1,1,1,1) OK)
!  I    nig,njg global dimensions of data in garr 
!               (same value must be passed on ALL PEs)
!  I    nk      number of levels for both garr and larr
!  I    size    size of data elements (the size of an integer is 1)
!  O    larr    local array that will receive the part of the global
!               domain that belongs to the local PE
!  I    mini:maxi,minj:maxj
!               horizontal dimensions of larr
!  I    gmini:gmaxi,gminj:gmaxj
!               horizontal dimensions of garr (same value must be passed on ALL PEs)
!  I    ghalox,ghaloy,halox,haloy
!               size of halos of garr and larr
!  I    periodx,periody
!               logical, periodicity along x and y axis.
!  O    status  status code upon exit (RPN_COMM_OK or RPN_COMM_ERROR)
!
        use rpn_comm_globals
		use rpn_comm_mpi
        use rpn_comm, only: RPN_COMM_limit
        implicit none                                                    !InTfout!
#define IN_RPN_COMM_dist
        integer, intent(IN) :: ghalox,ghaloy,gmini,gmaxi,gmaxj,gminj     !InTfout!
        integer, intent(IN) :: halox,haloy                               !InTfout!
        integer, intent(IN) :: nig,njg,size,mini,maxi,minj,maxj,nk       !InTfout!
        integer, intent(OUT)::status                                     !InTfout!
!!#define IgnoreTypeKindRank garr                                        !InTfout!
!!#define ExtraAttributes ,target                                        !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                       !InTfout!
        integer, intent(IN), target :: garr(size,gmini:gmaxi,gminj:gmaxj,nk)
!!#define IgnoreTypeKindRank larr                                        !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                       !InTfout!
        integer, intent(OUT):: larr(size,mini:maxi,minj:maxj,nk)
        logical, intent(IN) :: periodx,periody                           !InTfout!

        integer, dimension(0:pe_nx) :: count_x, depl_x
        integer, dimension(0:pe_ny) :: count_y, depl_y
        integer :: base_x, base_y
        integer :: ierr, nil, njl, i, j, k
        integer :: lmini, lmaxi, lminj, lmaxj
        integer, dimension(:,:,:), pointer :: fullrow, partrow
        integer :: rowsize

        status = RPN_COMM_ERROR
!
!       check that global halos are large enough
!
        if(periodx) then
          if(gmini > 1-halox .or. gmaxi < nig+halox .or. ghalox < halox) return   ! OOPS along x
        endif
        if(periody) then
          if(gminj > 1-haloy .or. gmaxj < njg+haloy .or. ghaloy < haloy) return   ! OOPS along y
        endif
!
!       compute scatter counts and displacements along x
!
        count_x = -1
        depl_x = -1
        ierr =  RPN_COMM_limit(pe_mex, pe_nx, 1, nig , lmini, lmaxi, count_x, depl_x)
        nil=lmaxi-lmini+1
        if(mini > 1-halox .or. maxi < nil+halox) return   ! ERROR, cannot accomodate halo
!        do i = 1 , pe_nx - 2
!          count_x(i) = count_x(i) + 2*halox
!          depl_x(i)  = depl_x(i) -halox
!        enddo
        count_x = count_x + 2*halox
        depl_x  = depl_x - halox
        if ( .not. periodx ) then
          count_x(0) = count_x(0) - halox
          count_x(pe_nx-1) = count_x(pe_nx-1) - halox
          depl_x(0)  = depl_x(0)  + halox
        endif
        count_x = count_x * size
        depl_x  = depl_x  * size
        base_x = 1 - halox
        if(pe_mex == 0 .and. (.not. periodx)) base_x = 1
!
!       compute scatter counts and displacements along y
!
        count_y = -1
        depl_y = -1
        ierr =  RPN_COMM_limit(pe_mey, pe_ny, 1, njg , lminj, lmaxj, count_y, depl_y)
        njl=lmaxj-lminj+1
        if(minj > 1-haloy .or. maxj < njl+haloy) return  ! ERROR, cannot accomodate halo
        count_y = count_y + 2*haloy
        depl_y = depl_y - haloy
        if ( .not. periody ) then
          count_y(0) = count_y(0) - haloy
          count_y(pe_ny-1) = count_y(pe_ny-1) - haloy
          depl_y(0)  = depl_y(0)  + haloy
        endif
        rowsize = size * (gmaxi-gmini+1)
        count_y = count_y * rowsize
        depl_y  = depl_y  * rowsize
        base_y = 1 - haloy
        if(pe_mey == 0 .and. (.not. periody)) base_y = 1
!
!       allocate full row buffer for PEs on column 0
!
        if(pe_mex == 0) then
!          if(pe_ny > 1) allocate(fullrow(size,gmini:gmaxi,1-haloy:njl+haloy))
          allocate(fullrow(size,gmini:gmaxi,1-haloy:njl+haloy))
        else
          allocate(fullrow(1,1,1))
        endif
#if defined(DEBUG)
        print *,'DEBUG: base_x count_x=',base_x,count_x
        print *,' depl_x=',depl_x
        print *,'DEBUG: base_y rowsize count_y=',base_y,rowsize,count_y/rowsize
        print *,' depl_y=',depl_y/rowsize
#endif
        do k = 1 , nk   ! one level at a time

          fullrow = 88088088
          if(pe_mex == 0) then  ! data distribution over column 0
            if(pe_ny > 1) then
              call MPI_scatterv(garr(1,gmini,1,1),count_y,depl_y,MPI_INTEGER,fullrow(1,gmini,base_y),count_y(pe_mey),MPI_INTEGER,0,pe_mycol,ierr)
            else
              fullrow(1:size,gmini:gmaxi,1-haloy:njl+haloy) = garr(:,:,1-haloy:njl+haloy,k)
            endif
#if defined(DEBUG)
            print *,'DEBUG: fullrow'
            do j = njl+haloy,1-haloy,-1
              print 101,j,fullrow(1,gmini:gmaxi,j)
            enddo
#endif
101         format(I3,15I9)
          endif
          call RPN_COMM_barrier(RPN_COMM_GRID,ierr)   ! PEs on column 0 are now ready for the distribute along x phase
!         as an alternative to the following loop
!         allocate partrow(size,mini:maxi,minj:maxj,pe_nx)
!         scatterv partrow (one call instead of njl+2*halox)
          do j = 1-haloy , njl+haloy  ! distribution along x one row at a time
            if(.not. periody .and. j < 1   .and. pe_mey == 0      )           continue
            if(.not. periody .and. j > njl .and. pe_mey == pe_ny-1)           continue
            if(pe_nx > 1) then
              call MPI_scatterv(fullrow(1,1,j),count_x,depl_x,MPI_INTEGER,larr(1,base_x,j,k),count_x(pe_mex),MPI_INTEGER,0,pe_myrow,ierr)
            else
              larr(:,base_x:nil+1-base_x,j,k) = fullrow(:,base_x:nil+1-base_x,j)
            endif
          enddo
#if defined(DEBUG)
          print *,'DEBUG: local row'
            do j = njl+haloy,1-haloy,-1
              print 101,j,larr(1,mini:maxi,j,1)
            enddo
#endif

        enddo
        if(pe_mex == 0 .and. pe_ny > 1) deallocate(fullrow)     ! column 0 only

        status = RPN_COMM_OK
        return
        end subroutine RPN_COMM_fast_dist   !InTfout!


        subroutine RPN_COMM_dist(garr,gmini,gmaxi,gminj,&				!InTfout!
     &          gmaxj,nig,njg,nk,ghalox,ghaloy,size,&    				!InTfout!
     &          larr,mini,maxi,minj,maxj,halox,haloy,&   				!InTfout!
     &          periodx,periody,status)                  				!InTfout!
        use rpn_comm_globals
        implicit none                                    				!InTfout!
!
	integer ghalox,ghaloy,gmini,gmaxi,gmaxj,gminj           			!InTfout!
	integer nig,njg,size,mini,maxi,minj,maxj,nk,status      			!InTfout!
	integer halox, haloy												!InTfout!
!!#define IgnoreTypeKindRank garr                                       !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
	integer garr(size,gmini:gmaxi,gminj:gmaxj,nk)
	real reel,lreel                                         			!InTfout!
!!#define IgnoreTypeKindRank larr                                       !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
	integer larr(size,mini:maxi,minj:maxj,nk)
	logical periodx,periody                                 			!InTfout!

	integer dimtemp(2),dt1,dt2,ierr
	
	dimtemp(1)=maxi-mini+1
	dimtemp(2)=maxj-minj+1
! should be pe_id(x,x)
	if(pe_tot.gt.1) then
	 call RPN_COMM_bcast(dimtemp,2,"MPI_INTEGER",0,"GRID",ierr)
	endif
	dt1=dimtemp(1)
	dt2=dimtemp(2)
	call RPN_COMM_dist2(garr,gmini,gmaxi,gminj,&
     &          gmaxj,nig,njg,nk,ghalox,ghaloy,size,&
     &          larr,mini,maxi,minj,maxj,halox,haloy,&
     &          periodx,periody,status,dt1,dt2)
	return
	end																	!InTfout!

!**S/R RPN_COMM_dist  Global distribution of data
	subroutine RPN_COMM_dist2(garr,gmini,gmaxi,gminj,&					!InTfout!
     &          gmaxj,nig,njg,nk,ghalox,ghaloy,size,&					!InTfout!
     &          larr,mini,maxi,minj,maxj,halox,haloy,&					!InTfout!
     &          periodx,periody,status,dt1,dt2)    						!InTfout!
	use rpn_comm_globals
	use rpn_comm_mpi
	use rpn_comm, only: RPN_COMM_limit
	implicit none                                     					!InTfout!
!
	integer ghalox,ghaloy,gmini,gmaxi,gmaxj,gminj     					!InTfout!
	integer nig,njg,size,mini,maxi,minj,maxj,nk,status					!InTfout!
	integer halox, haloy												!InTfout!
!!#define IgnoreTypeKindRank garr                                       !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
	integer garr(size,gmini:gmaxi,gminj:gmaxj,nk)
	real reel,lreel                                   					!InTfout!
!!#define IgnoreTypeKindRank larr                                       !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
	integer larr(size,mini:maxi,minj:maxj,nk)
	logical periodx,periody                           					!InTfout!
	integer dt1,dt2                                   					!InTfout!
!
!arguments
!  I	garr	array containing data to distribute, USED ONLY by PE 0
!  I	nig,njg,nk
!		dimensions of garr
!  I	size	size of data elements (the size of an integer is 1)
!  O	larr	local array that will receive the part of the global
!		domain that belongs to the local PE
!  I	mini:maxi,minj:maxj
!		dimensions of larr
!  I	gmini:gmaxi,gminj:gmaxj
!		dimensions of garr
!  I    ghalox,ghaloy,halox,haloy
!               size of halos of garr and larr
!  I    periodx,periody
!               logical, periodicity along x and y axis.
!  O	status	status code upon exit
!*
	integer MAX_PENDING
	parameter (MAX_PENDING=4)
	logical distribute,slot_free(0:MAX_PENDING-1)
	integer i,j,k,ierr,nslots,nwords,i0,j0,level,nil,njl,islot
	integer clientpe,isz,j1,client,jlocal,islot0
	integer ipe, jpe, istatus(MPI_STATUS_SIZE),target
	integer PEND_STAT(MPI_STATUS_SIZE)
	integer PEND_REQ(0:MAX_PENDING-1)
	integer, dimension(min(mini,gmini):max(gmaxi,maxi)) :: ilst
	integer, dimension(size,dt1,dt2,nk,0:max_pending-1) :: temp
	logical east,west,north,south,flag
	integer bxmin, bxmax, bymin,bymax
        integer ixmin,ixmax,iymin,iymax
	integer lmini,lmaxi,lminj,lmaxj
	integer count(pe_nx+pe_ny)
	integer depl(pe_nx+pe_ny)
	logical alongx

	distribute = .true.
1	status=MPI_ERROR
	do  i=0,MAX_PENDING-1
	   slot_free(i)= .true.
	enddo

	alongx = .true.
	ierr =  RPN_COMM_limit(pe_mex, pe_nx, 1, nig , lmini, lmaxi,count, depl)

	nil=lmaxi-lmini+1

	alongx = .false.
	ierr =  RPN_COMM_limit(pe_mey, pe_ny, 1, njg , lminj,&
     &    lmaxj,count, depl)

	njl=lmaxj-lminj+1

	if(pe_medomm .eq.0) then
!
!	PE # 0, get own stuff, distribute rest to others
!
	 j0=0
	 islot = 0

	 do jpe=0,pe_ny-1
	  i0=0
	  do ipe=0,pe_nx-1
!
!	get own part of global array
!
!       Computation of local bounds
	     bxmin=1-halox
	     bxmax=nil+halox
	     bymin=1-haloy
	     bymax=njl+haloy

	     east=(pe_xtab(pe_id(ipe,jpe)).eq.(pe_nx-1))&
     &              .and.(.not.periodx)      
	     west=(pe_xtab(pe_id(ipe,jpe)).eq.0)&
     &              .and. (.not.periodx)
	     north=(pe_ytab(pe_id(ipe,jpe)).eq.(pe_ny-1))&
     &              .and.(.not.periody)      
	     south=(pe_ytab(pe_id(ipe,jpe)).eq.0)&
     &             .and. (.not.periody)
	     if(north) bymax=njg-(pe_ny-1)*njl+haloy
	     if(east) bxmax=nig-(pe_nx-1)*nil+halox
	     if(east.and.(ghalox.lt.halox)) then
		bxmax=nig-(pe_nx-1)*nil+ghalox
	     endif
	     if(west.and.(ghalox.lt.halox)) then
		bxmin=1-ghalox
	     endif
	     if(north.and.(ghaloy.lt.haloy)) then
		bymax=njg-(pe_ny-1)*njl+ghaloy
	     endif
	     if(south.and.(ghaloy.lt.haloy)) then
		bymin=1-ghaloy
	     endif
	     if(pe_id(ipe,jpe) .eq. 0)then
	  	do j=bymin,bymax
	          do i=bxmin,bxmax
	            ilst(i)=i0+i
		    if(periodx) then
		       if(ilst(i).gt.nig) ilst(i)=ilst(i)-nig		
		       if(ilst(i).lt.1)   ilst(i)=ilst(i)+nig
		    endif
	          enddo
	          jlocal=j0+j
		  if(periody) then
	            if(jlocal.gt.njg) jlocal=jlocal-njg
		    if(jlocal.lt.1)   jlocal=jlocal+njg
		  endif
	          do isz=1,size
	          do k=1,nk
	          do i=bxmin,bxmax
	            larr(isz,i,j,k)=garr(isz,ilst(i),jlocal,k)
	          enddo
	          enddo
	          enddo
	        enddo		
!
!	distribute to others using a pipelined method
!

	      else
	        islot0=mod(islot,MAX_PENDING)
!		write(rpn_u,*) slot_free(islot0)
		if(.not.slot_free(islot0)) then 
!		   write(rpn_u,*) 'on attend',PEND_REQ(islot0)
	           call MPI_WAIT(&
     &	              PEND_REQ(islot0),&
     &	              PEND_STAT,ierr)
!		   write(rpn_u,*) 'ok',PEND_REQ(islot0)
        	endif
		slot_free(islot0)=.false.
	        do j=bymin,bymax
	          do i=bxmin,bxmax
	            ilst(i)=i0+i
		    if(periodx) then
	              if(ilst(i).gt.nig) ilst(i)=ilst(i)-nig
	              if(ilst(i).lt.1)   ilst(i)=ilst(i)+nig
		   endif
	          enddo
	          jlocal=j0+j
		  if(periody) then
	            if(jlocal.gt.njg) jlocal=jlocal-njg
	            if(jlocal.lt.1)   jlocal=jlocal+njg
	          endif
		  do isz=1,size
	          do k=1,nk
	          do i=bxmin,bxmax
	            temp(isz,i-bxmin+1,j-bymin+1,k,islot0)=&
     &                      garr(isz,ilst(i),jlocal,k)
	          enddo
	          enddo
	          enddo
	        enddo
	   
	        call MPI_ISEND(temp(1,1,1,1,islot0),&
     &	               size*nk*dt1*dt2,&
     &	               MPI_INTEGER,pe_id(ipe,jpe),pe_id(ipe,jpe),&
     &	               PE_DEFCOMM,&
     &	               PEND_REQ(islot0),ierr)

                islot=islot+1
	      endif
	      i0=i0+nil
	    enddo
	    j0=j0+njl
	  enddo
!       On attend la fin de toutes les transmissions
	  do i=max(0,islot-MAX_PENDING-1),islot-1
	    call MPI_WAIT(PEND_REQ(mod(i,MAX_PENDING)),&
     &	         PEND_STAT,ierr)
	  enddo
	else
!
!	NOT pe # 0, passive receive
!
	       east=(pe_mex.eq.(pe_nx-1)).and.(.not.periodx)      
	       west=(pe_mex.eq.0) .and. (.not.periodx)
	       north=(pe_mey.eq.(pe_ny-1)).and.(.not.periody)      
	       south=(pe_mey.eq.0) .and. (.not.periody)

	       bxmin=1-halox
	       bxmax=nil+halox
	       bymin=1-haloy
	       bymax=njl+haloy
!	       write(rpn_u,*) 'bornes',bxmin,bxmax,bymin,bymax
	       if(east.and.(ghalox.lt.halox)) then
		  bxmax=nil+ghalox
	       endif
	       if(west.and.(ghalox.lt.halox)) then
		  bxmin=1-ghalox
	       endif
	       if(north.and.(ghaloy.lt.haloy)) then
		  bymax=njl+ghaloy
	       endif
	       if(south.and.(ghaloy.lt.haloy)) then
		  bymin=1-ghaloy
	       endif
	  call MPI_RECV(temp(1,1,1,1,0)&
     &                 ,size*dt1*dt2*nk,&
     &	                MPI_INTEGER,0,&
     &	                pe_medomm,PE_DEFCOMM,istatus,ierr)
	  do isz=1,size
	     do k=1,nk
	     do j=bymin,bymax
	     do i=bxmin,bxmax
		larr(isz,i,j,k)=temp(isz,i-bxmin+1,j-bymin+1,k,0)
	     enddo
	     enddo
	     enddo
	  enddo
	endif
	status=MPI_SUCCESS
	return
!
1111	status =  MPI_ERROR

	return
	end																	!InTfout!

