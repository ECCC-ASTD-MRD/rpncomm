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

        SUBROUTINE RPN_COMM_sendrecv( sendbuf, sendcount, sendtype,&    !InTfout!
     &          dest, sendtag, recvbuf, recvcount, recvtype, recv, &    !InTfout!
     &          recvtag, com, status, ierr )                            !InTfout!
                                                                        !InTfout!
!	Luc Corbeil, 2002-10-15

        use mpi
        use rpn_comm, only: RPN_COMM_datyp, RPN_COMM_comm, RPN_COMM_grank
        implicit none                                                   !InTfout!
        integer recvcount,ierr,sendtag,dest                             !InTfout!
        integer sendcount,comm,recvtag,recv                             !InTfout!
        integer status                                                  !InTfout!
!!#define IgnoreTypeKindRank sendbuf                                    !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
!!#define IgnoreTypeKindRank recvbuf                                    !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
        integer sendbuf, recvbuf
        integer datyp1, datyp2                                          !InTfout!
        character(len=*) recvtype, sendtype ,com                        !InTfout!

        integer status2(MPI_STATUS_SIZE)

        datyp1=rpn_comm_datyp(sendtype)
        datyp2=rpn_comm_datyp(recvtype)
	comm=rpn_comm_comm(com)
!        if(.not.RPN_COMM_grank(com)) return
        call mpi_sendrecv( sendbuf, sendcount, datyp1,&
     &          dest, sendtag, recvbuf, recvcount, datyp2, recv, &
     &          recvtag, comm, status2, ierr )
        call mpi_get_count( status2, datyp2, status, ierr)
	return
	end                                                             !InTfout!
