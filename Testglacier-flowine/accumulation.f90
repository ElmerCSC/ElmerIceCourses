FUNCTION getAccumulation(  Model, Node, InputArray) RESULT(accum)
  ! provides you with most Elmer functionality
  USE DefUtils
  ! saves you from stupid errors
  IMPLICIT NONE
  ! the external variables
  !----------------------------------------------------------------------------
  TYPE(Model_t) :: Model     ! the access point to everything about the model
  INTEGER :: Node            ! the current Node number
  REAL(KIND=dp) :: InputArray(2) ! Contains the arguments passed to the function
  REAL(KIND=dp) :: accum      ! the result 
  !----------------------------------------------------------------------------
  ! internal variables
  !----------------------------------------------------------------------------
  REAL(KIND=dp) :: lapserate, ela0, dElaDt, elaT, accumulationAtSl,&
       inittime, time, elevation, cutoff, offset
  LOGICAL :: FirstTime=.TRUE.
  ! Remember this value
  SAVE FirstTime, inittime

  ! lets hard-code our values (if we have time we can later make them being read from SIF)
  lapserate = 11.0_dp/2750.0_dp
  ela0 = 400.0_dp
  dElaDt = -0.05_dp
  cutoff = 600.0_dp
  offset = 1500.0

  ! copy input (should match the arguments!)
  elevation = InputArray(1)
  time = InputArray(2)
  WRITE (Message, '(A,E10.2,A,E10.2)')  "elevation=", elevation, "time=", time
  CALL INFO("getAccumulation", Message, Level=9)

  ! store the initial time, to be sure to have relative times
  IF (FirstTime) THEN
     inittime = time
     FirstTime = .FALSE.
  END IF


  ! get change of ELA with time
  IF (time > offset) THEN
     elaT = ela0 - dElaDt * (time - offset)
  ELSE
     elaT = ela0
  END IF
 
  ! lets do the math
  accumulationAtSl = -elaT*lapserate
  IF (elevation > cutoff) elevation = cutoff
  accum = lapserate*elevation + accumulationAtSl

  RETURN

END FUNCTION getAccumulation
