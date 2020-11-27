!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  USER FUNCTIONS FOR TETE ROUSSE CAVITY PROBLEM
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! User Function MaskCavity
! Mask = -1 for cavity nodes (zb > b), Mask = 1 for bedrock (zb=b)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION MaskCavity ( Model, nodenumber, Input) RESULT(Mask)
   USE types
   USE CoordinateSystems
   USE SolverUtils
   USE ElementDescription
   USE DefUtils
   IMPLICIT NONE
   TYPE(Model_t) :: Model
   TYPE(Solver_t), TARGET :: Solver
   INTEGER :: nodenumber  
   REAL(KIND=dp) :: Input(2), Mask
   REAL(KIND=dp) :: znode, zbed

   znode = Input(1)
   zbed = Input(2)

   IF (znode > zbed+0.01) THEN
      Mask = -1.0
   ELSE
      Mask = 1.0
   END IF

END FUNCTION MaskCavity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! User Function WaterPressure
! Return the water pressure as a function of time and znode         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION WaterPressure ( Model, nodenumber, Input) RESULT(Pw)
   USE types
   USE CoordinateSystems
   USE SolverUtils
   USE ElementDescription
   USE DefUtils
   IMPLICIT NONE
   TYPE(Model_t) :: Model
   TYPE(Solver_t), TARGET :: Solver
   INTEGER :: nodenumber  
   REAL(KIND=dp) :: Input(2), Pw
   REAL(KIND=dp) :: znode, time
   REAL(KIND=dp) :: rhow, gravity, hw
   REAL(KIND=dp) :: dh = 70.0, dt = 20.0
   INTEGER :: bf_id
   LOGICAL :: FirstTime = .TRUE.

   SAVE FirstTime, rhow, gravity

   IF (FirstTime) THEN
      FirstTime = .FALSE.
      
      ! Read rhow and gravity in the sif file
      rhow = ListGetConstReal( Model % Constants, 'Water Density', UnFoundFatal=.TRUE. )
      bf_id = ListGetInteger( Model % Bodies(1) % Values,&
           'Body Force',  minv=1, maxv=Model % NumberOFMaterials)
      gravity = ListGetConstReal( Model % BodyForces(bf_id) % Values, 'Flow BodyForce 3', UnFoundFatal=.TRUE. )
   END IF

   time = Input(1)
   znode = Input(2)

   hw = MAX(3170.0 - time * 365.25 * dh/dt,3100.0) 
   Pw = rhow * gravity * (hw - znode)

END FUNCTION WaterPressure
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
