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
!InTf!
    subroutine RPN_COMM_const(IER)                 !InTf!
    use rpn_comm_globals
    use rpn_comm_mpi
    implicit none                                  !InTf!
    integer, intent(OUT) :: IER                    !InTf!
    real :: VALEUR
    integer :: FLAG

    if(pe_me .eq.pe_pe0) then
      call CONSTNT(VALEUR,FLAG,'DUMMY',0)
      print *,' first call to constnt to prime tables'
    endif
    call CONSTNT_X(VALEUR,FLAG,'DUMMY',4,&
                   MPI_Bcast,MPI_INTEGER,0,pe_defcomm,IER)
    if(pe_me .eq.0) then
      print *,' constnt tables have been distributed'
    endif
    return
    end   subroutine RPN_COMM_const                !InTf!
