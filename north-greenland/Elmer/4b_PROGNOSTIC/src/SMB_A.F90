!#
      FUNCTION SMB_ANOM(Model,nodenumber,VarIn)  RESULT(VarOut)
       USE Types
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn(3) !Time,smb,dsmb
       REAL(kind=dp) :: VarOut
       !-----------------
       INTEGER :: pert
       REAL(kind=dp), SAVE :: t=-1._dp

       IF (VarIn(1).GT.t) THEN
         PRINT *, "Time:",VarIn(1)
         t=VarIn(1)
       END IF
       pert=MAX(0,floor(VarIn(1)))

       If (Varin(1).LT.40._dp) THEN
          VarOut=VarIn(2)+ (pert/40.0)*VarIn(3)
       Else
          VarOut=VarIn(2)+VarIn(3)
       End if

      END FUNCTION SMB_ANOM
!#
