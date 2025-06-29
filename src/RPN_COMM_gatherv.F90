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

      SUBROUTINE RPN_COMM_gatherv(sendbuf,sendcount,sendtype,&          !InTfout!
     &     recbuf,reccountv,disp,rectype,root,com,ierr)                 !InTfout!
!     Luc Corbeil, 2001-09-19
      use rpn_comm, only: RPN_COMM_datyp, RPN_COMM_oper, RPN_COMM_comm, RPN_COMM_grank
      implicit none                                                     !InTfout!
!!#define IgnoreTypeKindRank sendbuf                                    !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
!!#define IgnoreTypeKindRank recbuf                                     !InTfout!
!!#include "IgnoreTypeKindRank.hf"                                      !InTfout!
      integer sendbuf, recbuf
      integer sendcount,reccountv(*),comm,ierr                          !InTfout!
      integer datyp,datyp2,oper,root, disp(*)                           !InTfout!
      character(len=*) rectype,sendtype,com                             !InTfout!
      
      datyp=rpn_comm_datyp(sendtype)
      datyp2=rpn_comm_datyp(rectype)
      comm=rpn_comm_comm(com)
      
      if(.not.RPN_COMM_grank(com)) return
      call mpi_gatherv(sendbuf,sendcount,datyp,recbuf&
     &     ,reccountv,disp,datyp2,root,comm,ierr)
      
      return
      end                                                               !InTfout!
      
