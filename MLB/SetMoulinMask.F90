!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! * 
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! * 
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the 
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/

!------------------------------------------------------------------------------
SUBROUTINE SetMoulinMask( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the Poisson equation!
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh, materials, BCs, etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear & nonlinear equation solver options
!
!  REAL(KIND=dp) :: dt,
!     INPUT: Timestep size for time dependent simulations
!
!  LOGICAL :: TransientSimulation
!     INPUT: Steady state or transient simulation
!
!******************************************************************************
  USE DefUtils
  USE SolverUtils
  
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  LOGICAL :: AllocationsDone = .FALSE., Found
  TYPE(Element_t),POINTER :: Element

  INTEGER :: BC, N, i, istat, active, MinNode
  TYPE(Mesh_t), POINTER :: Mesh
  REAL(KIND=dp), POINTER :: moulincoords(:,:)
  REAL(KIND=dp) :: MinDist, Eps, localcoords(2)
  INTEGER, POINTER :: moulinnodenumbers(:)
  INTEGER, ALLOCATABLE :: foundnodes(:)
  TYPE(Variable_t), POINTER :: Var
  INTEGER, POINTER :: VarPerm(:)
  REAL(KIND=dp), POINTER :: VarValues(:)
  TYPE(Valuelist_t), POINTER :: Valuelist, SolverPars
  LOGICAL :: LDummy
  CHARACTER(LEN=MAX_NAME_LEN) :: moulinfilename
  
  SAVE moulincoords, moulinnodenumbers, AllocationsDone
!------------------------------------------------------------------------------

  Var => Solver % Variable
  IF (ASSOCIATED(Var)) THEN
    VarPerm => Var % Perm
    VarValues => Var % Values
    VarValues = 0.0_dp
  ELSE
    CALL FATAL('SetMoulinMask','No Variable associated')
  END IF

  SolverPars => GetSolverParams()
  moulinfilename = ListGetString(SolverPars,'Moulin Filename',Found )
  IF (.NOT.Found) CALL FATAL ("SetMoulinMaks",'"Moulin Filename" not found.')
  
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------

  Mesh => GetMesh()

  IF (.NOT.ASSOCIATED(Mesh)) CALL FATAL("SetMoulinMask","mesh not found")

  IF (.NOT.AllocationsDone) THEN
    CALL ReadCoords(moulincoords,N,moulinfilename)
    IF (N>0) THEN
      ALLOCATE(moulinnodenumbers(N))
    ELSE
      CALL FATAL("SetMoulinMask","No moulin nodes found")
    END IF
    AllocationsDone = .TRUE.
  END IF

  DO i=1,N
    PRINT *, "Coord(",i,")=",moulincoords(i,1),moulincoords(i,2)
    Eps = 50000.0_dp
    localcoords(1:2)=moulincoords(i,1:2)
    !MinNode = 0
    CALL FindClosestNode(Mesh,localcoords,MinDist,MinNode,ParEnv % PEs>1,Eps,Perm=VarPerm)
    PRINT *, "minNode", MinNode
    IF (MinNode > 0) THEN
      moulinnodenumbers(i) = MinNode
      VarValues(VarPerm(moulinnodenumbers(i))) = i * 1.0_dp
      WRITE(Message, *) "moulin#",moulinnodenumbers(i),&
           Mesh % Nodes % x(moulinnodenumbers(i)), Mesh % Nodes % y(moulinnodenumbers(i)), Mesh % Nodes % z(moulinnodenumbers(i))
      CALL INFO("SetMoulinMask",Message,Level=1)
      DO BC=1,Model % NumberOfBCs
        !PRINT *, BC
        ValueList => Model % BCs(BC) % Values
        IF (.NOT.ASSOCIATED(ValueList)) CALL FATAL("SetMoulinMask","BC not asssociated")
        LDummy = GetLogical(ValueList,'Set Moulin Flux', Found)
        PRINT *, BC, "SetMoulinFlux", Found, LDummy
        !IF( .NOT.Found) CYCLE
        IF( .NOT. ListCheckPresent( ValueList,'Set Moulin Flux' )) CYCLE
        
        WRITE (Message,*) "Adding following point to BC", BC,":"
        CALL INFO("SetMoulinMask",Message,Level=1)
        WRITE (Message,*) moulinnodenumbers(i)
        CALL INFO("SetMoulinMask",Message,Level=1)

        !CALL ListAddInteger( ValueList,'Target Nodes', &
        !     moulinnodenumbers(i)) 
      END DO      
    ELSE
      WRITE(Message,*) "Did not find closest node for moulin #",i
      !CALL FATAL("SetMoulinMask",Message)
      
      PRINT *, Message
    END IF
  END DO
  DO BC=1,Model % NumberOfBCs
    PRINT *, BC
    ValueList => Model % BCs(BC) % Values
    IF( .NOT. ListCheckPresent( ValueList,'Set Moulin Flux' )) CYCLE
    CALL ListAddIntegerArray( ValueList,'Target Nodes', N, moulinnodenumbers)
  END DO
! That should add the Target nodes automatically to the BC where Moulin Flux is declared - so, no need to add them manually
  
!  PRINT *, "Model % NumberOfBCs", Model % NumberOfBCs

  
CONTAINS

  SUBROUTINE ReadCoords(moulincoords,nrows,moulinfilename)
    IMPLICIT NONE
    INTEGER :: i, ios, nrows
    REAL(KIND=dp), POINTER :: moulincoords(:, :)
    REAL(KIND=dp) :: val1, val2
    CHARACTER(LEN=MAX_NAME_LEN) :: moulinfilename
    
    ! First pass: count number of rows
    nrows = 0
    OPEN(unit=10, file=moulinfilename, status='old', action='read')
    DO
        READ(10, *, iostat=ios)
        IF (ios /= 0) EXIT
        nrows = nrows + 1
    END DO
    CLOSE(10)

    ! Allocate array with correct size
    ALLOCATE(moulincoords(nrows, 2))

    ! Second pass: read actual data
    OPEN(unit=10, file=moulinfilename, status='old', action='read')
    DO i = 1, nrows
        READ(10, *) moulincoords(i, 1), moulincoords(i, 2)
    END DO
    close(10)

    ! Output to verify
    WRITE(Message, *) "Read ", nrows, " rows."
    CALL INFO("SetMoulinMask",Message,Level=1)
    WRITE(Message, *) "First row: ", moulincoords(1,1), moulincoords(1,2)
    CALL INFO("SetMoulinMask",Message,Level=10)

  END SUBROUTINE ReadCoords


!------------------------------------------------------------------------------
END SUBROUTINE SetMoulinMask
!------------------------------------------------------------------------------
