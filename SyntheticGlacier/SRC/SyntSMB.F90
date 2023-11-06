!/*****************************************************************************/
! *
! *  Elmer/Ice, a glaciological add-on to Elmer
! *  http://elmerice.elmerfem.org
! *
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
! ******************************************************************************
! *
! *  Authors: Olivier Gagliardini
! *  Email: olivier.gagliardini@univ-grenoble-alpes.fr   
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: Feb 2021 
! * 
! *****************************************************************************
!> A solver to create a synthetical SMB from Adikari 2021 PhD                 
!> SMB = SMB0R [1 - beta_m(Zmax - Zs)^r]
SUBROUTINE SyntSMB( Model, Solver, dt, TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  For the Upper surface, creates and updates a mask which may be equal to -1, 0 or 1
!
!  Icy Mask = + 1 if Icy (glacier)
!           = - 1 if Ice Free 
!           = 0   on the contouur of the glacier (first node with H=Hmin)
!           < -1 for Icy Isolated nodes (useful to move them back to initial elevation) 
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

  IMPLICIT NONE
  !------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation

  !------------------------------------------------------------------------------
  ! Local variables
  !------------------------------------------------------------------------------

  TYPE(Element_t),POINTER :: Element
  TYPE(ValueList_t), POINTER :: SolverParams
  TYPE(Variable_t), POINTER :: PointerToVariable, ZsVar
  TYPE(Nodes_t), SAVE :: Nodes

  LOGICAL :: AllocationsDone = .FALSE., GotIt, stat, UnFoundFatal=.TRUE., &
             FirstTime = .TRUE.

  INTEGER :: i, mn, n, t, Nn, istat, DIM
  INTEGER, POINTER :: Permutation(:), ZsPerm(:)

  REAL(KIND=dp) :: SMB0, betaM, SMBr, ELA, Zs0
  REAL(KIND=dp), POINTER :: VariableValues(:), ZsValues(:)
  REAL(KIND=dp), ALLOCATABLE :: Zs(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName = 'SyntSMB'
       
  SAVE AllocationsDone, DIM, SolverName, Zs, SMB0, betaM, SMBr, Zs0, ELA, &
       FirstTime               
  !------------------------------------------------------------------------------

  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values

  CALL INFO(SolverName, 'Compute SMB', level=3)

  !--------------------------------------------------------------
  ! Allocate some permanent storage:
  !--------------------------------------------------------------
  IF ( (.NOT. AllocationsDone) .OR. Solver % Mesh % Changed ) THEN
     DIM = CoordinateSystemDimension()
     mn = Solver % Mesh % MaxElementNodes
     IF (AllocationsDone) DEALLOCATE(Zs)     
     ALLOCATE(Zs(mn), STAT=istat )
     IF ( istat /= 0 ) THEN
        CALL FATAL( SolverName, 'Memory allocation error.' )
     END IF
     CALL INFO( SolverName, 'Memory allocation done.',Level=1 )
     AllocationsDone = .TRUE.
  END IF
  
  IF (FirstTime) THEN 
    SolverParams => GetSolverParams()
    SMB0 = GetConstReal(SolverParams, 'SMB Glacier Head ', GotIt)
    IF (.NOT.GotIt) THEN
      CALL FATAL(SolverName, 'No < SMB Glacier Head > given in Solver SyntSMB')
    END IF
    Zs0 = GetConstReal(SolverParams, 'SMB Glacier Head Elevation', GotIt)
    IF (.NOT.GotIt) THEN
      CALL FATAL(SolverName, 'No < SMB Glacier Head Elevation > given in Solver SyntSMB')
    END IF
    ELA = GetConstReal(SolverParams, 'SMB ELA', GotIt)
    IF (.NOT.GotIt) THEN
      CALL FATAL(SolverName, 'No < SMB ELA > given in Solver SyntSMB')
    END IF
    SMBr = GetConstReal(SolverParams, 'SMB Exponent', GotIt)
    IF (.NOT.GotIt) THEN
      CALL FATAL(SolverName, 'No < SMB Exponent > given in Solver SyntSMB')
    END IF
    IF (ELA.GE.Zs0) THEN
      CALL FATAL(SolverName, 'SMB ELA cannot be larger than Glacier Head Elevation!')
    ELSE
      betaM = 1.0/(Zs0-ELA)**SMBr
    END IF
    FirstTime = .FALSE. 
  END IF
  

  ! Read the Thickness variable
  ZsVar => VariableGet(Model % Mesh % Variables, 'Zs', UnFoundFatal=UnFoundFatal)
  ZsValues => ZsVar % Values
  ZsPerm => ZsVar % Perm
  
  !--------------------------------------------------------------
  ! Icy/Ice Free Mask is based on variable Thickness
  !--------------------------------------------------------------
  DO t = 1, Solver % NumberOfActiveElements
    Element => GetActiveElement(t)
    n = GetElementNOFNodes()
    CALL GetElementNodes( Nodes )

    Zs(1:n) = ZsValues(ZsPerm(Element % NodeIndexes))

    DO i = 1, n
      Nn = Permutation(Element % NodeIndexes(i))
      IF (Nn==0) CYCLE

      IF (Zs(i) > Zs0) THEN
        VariableValues(Nn) = 0.0_dp
      ELSE
        VariableValues(Nn) = SMB0*(1.0-betaM*(Zs0-Zs(i))**SMBr) 
      END IF
    END DO
  END DO
     
  CALL INFO( SolverName , 'Done')
 
END SUBROUTINE SyntSMB 
!------------------------------------------------------------------------------
