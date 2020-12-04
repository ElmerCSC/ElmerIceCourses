!#
      FUNCTION SUB_MELT(Model,nodenumber,VarIn)  RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn(2) !GM,Zb
       REAL(kind=dp) :: VarOut
       !-----------------
       TYPE(ValueList_t), POINTER :: BodyForce
       TYPE(Element_t), POINTER :: CurEl
       REAL(kind=dp) :: meltA
       REAL(kind=dp) :: meltB
       
       CurEl => Model % CurrentElement
         BodyForce => GetBodyForce( CurEl)
         IF (.NOT.ASSOCIATED(BodyForce)) &
                CALL FATAL("SUB_MELT","No body Force !")
         meltA   = ListGetConstReal(BodyForce,'SUB_MELT meltA',UnFoundFatal=.TRUE.)
         meltB   = ListGetConstReal(BodyForce,'SUB_MELT meltB',UnFoundFatal=.TRUE.)

         IF (VarIn(1).GT.-0.5) THEN
                 VarOut=0._dp
         ELSE
                 VarOut=meltA+meltB*VarIn(2)*VarIn(2)
         ENDIF
       END FUNCTION SUB_MELT


      FUNCTION DISC_MELT(Model,nodenumber,VarIn)  RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn !nodal melt
       REAL(kind=dp) :: VarOut
       !-----------------

       TYPE(Element_t), POINTER :: CurEl
       TYPE(Variable_t), POINTER :: GM
       INTEGER, POINTER :: NodeIndexes(:)
       REAL(kind=dp), POINTER :: GMValues(:)
       INTEGER, POINTER :: GMPerm(:)

       CurEl => Model % CurrentElement
       NodeIndexes => CurEl % NodeIndexes(:)

       GM => VariableGet(Model%Mesh%Variables,"groundedmask",UnFoundFatal=.TRUE.)
       GMValues => GM % Values(:)
       GMPerm => GM % Perm(:)

       IF (ANY(GMValues(GMPerm(NodeIndexes(:))).GT.-0.5)) THEN
         VarOut=0._dp
       ELSE
         VarOut = VarIn
       END IF

      END FUNCTION DISC_MELT
!#
