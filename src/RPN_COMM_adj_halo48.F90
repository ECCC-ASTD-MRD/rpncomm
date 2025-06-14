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

    SUBROUTINE RPN_COMM_adj_halo4(g,minx,maxx,miny,maxy, &      !InTfout!
                  ni,nj,nk,halox,haloy,periodx,periody, &       !InTfout!
                  gni,npol_row)                                 !InTfout!
!!    implicit none                                             !InTfout!
!!    integer minx,maxx,miny,maxy,ni,nj,nk,halox,haloy          !InTfout!
!!    integer gni,npol_row                                      !InTfout!
!!    logical periodx,periody                                   !InTfout! 
!!#define IgnoreTypeKindRank g                                  !InTfout!
!!#include "IgnoreTypeKindRank.hf"                              !InTfout!
#include <RPN_COMM_adj_halo.hf>

    entry adj_halo4(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,periodx,periody)
                 
    globalni=ni
    polarrows=0
    goto 1

    end !InTfout!

    SUBROUTINE RPN_COMM_adj_halo8(g,minx,maxx,miny,maxy, &      !InTfout!
                  ni,nj,nk,halox,haloy,periodx,periody, &       !InTfout!
                  gni,npol_row)                                 !InTfout!
!!    implicit none                                             !InTfout!
!!    integer minx,maxx,miny,maxy,ni,nj,nk,halox,haloy          !InTfout!
!!    integer gni,npol_row                                      !InTfout!
!!    logical periodx,periody                                   !InTfout! 
!!#define IgnoreTypeKindRank g                                  !InTfout!
!!#include "IgnoreTypeKindRank.hf"                              !InTfout!
#define DOUBLE8
#include <RPN_COMM_adj_halo.hf>

    entry adj_halo8(g,minx,maxx,miny,maxy,ni,nj,nk,halox,haloy,periodx,periody)
                  
    globalni=ni
    polarrows=0
    goto 1

    end !InTfout!
