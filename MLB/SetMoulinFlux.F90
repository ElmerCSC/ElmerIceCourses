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
SUBROUTINE SetMoulinFlux( Model,Solver,dt,TransientSimulation )
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

  INTEGER :: BC, t, N, M, i, istat, active, MinNode, molinno, dayofyear
  TYPE(Mesh_t), POINTER :: Mesh
  REAL(KIND=dp), POINTER :: moulinfluxes(:,:), timeofyearinseconds(:), totalflux(:)
  REAL(KIND=dp) :: MinDist, Eps, localcoords(2), scalefactor=1.0_dp
  TYPE(Variable_t), POINTER :: Var, MaskVar, TimeVar
  INTEGER, POINTER :: VarPerm(:), MaskPerm(:), TimePerm(:)
  REAL(KIND=dp), POINTER :: VarValues(:), MaskValues(:), TimeValue(:)
  TYPE(Valuelist_t), POINTER :: Valuelist, SolverPars
  CHARACTER(LEN=MAX_NAME_LEN) :: moulinfluxfilename
  
  SAVE moulinfluxes, timeofyearinseconds, totalflux, AllocationsDone
!------------------------------------------------------------------------------

  Var => Solver % Variable
  IF (ASSOCIATED(Var)) THEN
    VarPerm => Var % Perm
    VarValues => Var % Values
    VarValues = 0.0_dp
  ELSE
    CALL FATAL('SetMoulinFlux','No Variable associated')
  END IF
  TimeVar => VariableGet( Model % Variables, 'Time')
  IF (ASSOCIATED(TimeVar)) THEN
    TimePerm => TimeVar % Perm
    TimeValue => TimeVar % Values
  ELSE
    CALL FATAL('SetMoulinFlux','No Variable "Time" associated - that is very odd')
  END IF
  MaskVar => VariableGet( Model % Variables, 'Moulin Mask') 
  IF (ASSOCIATED(MaskVar)) THEN
    MaskPerm => MaskVar % Perm
    MaskValues => MaskVar % Values
  ELSE
    CALL FATAL('SetMoulinFlux','No Variable "Moulin Mask" associated')
  END IF
  
  SolverPars => GetSolverParams()
  moulinfluxfilename = ListGetString(SolverPars,'Moulin Flux Filename',Found )
  
  scalefactor = GetConstReal(SolverPars,'Moulin Flux Scale Factor',Found)
  IF (FOUND) THEN
    WRITE (Message, *) "Scalefactor found and set to ",  scalefactor
    CALL INFO ("SetMoulinMaks",Message,Level=1)
  ELSE
    scalefactor=1.0_dp
  END IF
  
  IF (.NOT.Found) CALL FATAL ("SetMoulinMaks",'"Moulin Flux Filename" not found.')
  
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------

  Mesh => GetMesh()

  IF (.NOT.ASSOCIATED(Mesh)) CALL FATAL("SetMoulinFlux","mesh not found")

  IF (.NOT.AllocationsDone) THEN
    CALL ReadFluxes(moulinfluxes,N,M,moulinfluxfilename,timeofyearinseconds,totalflux,TimeValue(1))
    AllocationsDone = .TRUE.
  END IF

  !PRINT *, GetNOFBoundaryElements()
  
  !DO t=1,GetNOFBoundaryElements()
  !  Element => GetBoundaryElement(t)
  !  ValueList => GetBC()
  !  BC = GetBCID( Element )
  !  IF( .NOT. ListCheckPresent( ValueList,'Set Moulin Flux' )) CYCLE
  !  PRINT *, BC
  !END DO
  !PRINT *, Model  % NumberOfNodes, TimeValue(1)
  !DO i=1, Model % NumberOfNodes
  !  IF (maskPerm(i) == 0) CYCLE
  !  PRINT *, MaskValues(maskPerm(i)), maskPerm(i)
  !  IF (MaskValues(maskPerm(i)) > 0.0_dp) THEN
  !    VarValues(VarPerm(i)) = moulinfluxes(1,INT(MaskValues(maskPerm(i))))
  !    PRINT *, i, VarValues(VarPerm(i))
  !  END IF
  !END DO  

 
  dayofyear = FLOOR(MODULO(TimeValue(1),365.0*24.0*3600.0)/(3600.0_dp*24.0_dp))
  IF (dayofyear < 1) dayofyear=1
  IF (dayofyear > 365) dayofyear=365
  WRITE(Message,*) "Time (s) =", TimeValue(1)," Day of year: ",  dayofyear, "year#",  TimeValue(1)/(365.0*24.0*3600.0)
  CALL INFO('SetMoulinFlux',Message,Level=3)
  VarValues = 0.0_dp
  DO i=1, Model % NumberOfNodes
    IF (maskPerm(i) == 0) CYCLE
    IF (MaskValues(maskPerm(i)) > 0.0_dp) THEN
      VarValues(VarPerm(i)) = scalefactor*moulinfluxes(dayofyear,INT(MaskValues(maskPerm(i))))
      PRINT *, dayofyear, i, VarValues(VarPerm(i))
    END IF
  END DO
! That should add the Target nodes automatically to the BC where Moulin Flux is declared - so, no need to add them manually
  
!  PRINT *, "Model % NumberOfBCs", Model % NumberOfBCs

  
CONTAINS

  SUBROUTINE ReadFluxes(moulinfluxes,nrows,ncols,moulinfilename, timeofyearinseconds, totalflux, currenttime)
    IMPLICIT NONE
    INTEGER :: i, j, ios, nrows, ncols, dayofyear, meltseasonstart=365, meltseasonend=0
    REAL(KIND=dp), POINTER :: moulinfluxes(:, :), totalflux(:), timeofyearinseconds(:)
    REAL(KIND=dp), ALLOCATABLE :: infield(:)
    REAL(KIND=dp) :: val1, val2, currenttime
    CHARACTER(LEN=MAX_NAME_LEN) :: moulinfilename
    CHARACTER(LEN=1000) :: inputline


    PRINT *, moulinfilename
    ! First pass: count number of rows
    nrows = 0
    OPEN(unit=10, file=moulinfilename, status='old', action='read')
    DO
      READ(10, "(A)", IOSTAT=ios) inputline
      !PRINT *, "inputline: ", inputline
      ncols = count_words(inputline)
      !PRINT *, "ncols", ncols
      IF (ios /= 0) EXIT
      nrows = nrows + 1
    END DO
    CLOSE(10)

    WRITE (Message, *) "Found rows x cols", nrows, ncols
    CALL INFO("SetMoulinFlux", Message, Level = 10)
    
    ! Allocate array with correct size
    ALLOCATE(moulinfluxes(365, ncols-2),totalflux(365), timeofyearinseconds(365),infield(ncols))

    moulinfluxes = 0.0
    DO i=1,365
       timeofyearinseconds(i) = i*24.0_dp*3600.0_dp
    END DO
    ! Second pass: read actual data
    OPEN(unit=10, file=moulinfilename, status='old', action='read')
    DO i=1,nrows
      READ(10, *, END=10, IOSTAT=ios, ERR=20) (infield(j), j = 1, ncols)
      dayofyear = INT(infield(1))
      IF (meltseasonstart > dayofyear) meltseasonstart = dayofyear
      IF (meltseasonend < dayofyear) meltseasonend = dayofyear
      timeofyearinseconds(dayofyear) = 24.0 * 3600.0 * infield(1)
      !timeofyearinseconds(i) = infield(1)  !!CHANGE BACK
      totalflux(dayofyear) = infield(2)
      moulinfluxes(dayofyear, 1:ncols-2) = infield(3:ncols)
      WRITE(Message,*) timeofyearinseconds(i), totalflux(i), moulinfluxes(i,1:ncols)
      CALL INFO("SetMoulinFlux",Message,Level=10)
    END DO
10  CLOSE(10)

    ! Output to verify
    WRITE(Message, *) "Read ", nrows, " rows."
    CALL INFO("SetMoulinFlux",Message,Level=1)
    WRITE(Message, *) "First row with values (day ",meltseasonstart,": ",&
         timeofyearinseconds(meltseasonstart), totalflux(meltseasonstart), moulinfluxes(meltseasonstart,1:5), &
         " ... ", moulinfluxes(meltseasonstart,ncols - 2)
    CALL INFO("SetMoulinFlux",Message,Level=3)
    WRITE(Message, *) "last row with values (day ",meltseasonend,": ",&
         timeofyearinseconds(meltseasonend), totalflux(meltseasonend), moulinfluxes(meltseasonend,1:5),&
         " ... ", moulinfluxes(meltseasonend,ncols - 2)
    CALL INFO("SetMoulinFlux",Message,Level=3)
    RETURN
20  CALL FATAL("SetMoulinFlux","I/O Error")
  END SUBROUTINE ReadFluxes

! Function to count words (columns) in a line
  INTEGER FUNCTION count_words1(str)
    CHARACTER(len=*), intent(in) :: str
    INTEGER :: i, lenstr
    LOGICAL :: in_word

    lenstr = len_trim(str)
    count_words1 = 0
    in_word = .false.

    DO i = 1, lenstr
       IF (str(i:i) /= ' ') THEN
          IF (.NOT. in_word) THEN
             count_words1 = count_words1 + 1
             in_word = .TRUE.
          END IF
       ELSE
          in_word = .FALSE.
       END IF
    END DO
  END FUNCTION count_words1
  
  integer function count_words(str)
    character(len=*), intent(in) :: str
    INTEGER :: i, lenstr
    logical :: in_word

    lenstr = len_trim(str)
    count_words = 0
    in_word = .false.

    DO i = 1, lenstr
       if (str(i:i) /= ' ' .and. str(i:i) /= char(9)) then
          if (.not. in_word) then
             count_words = count_words + 1
             in_word = .true.
          end if
       else
          in_word = .false.
       end if
     END DO
     RETURN 
  end function count_words

!------------------------------------------------------------------------------
END SUBROUTINE SetMoulinFlux
!------------------------------------------------------------------------------
