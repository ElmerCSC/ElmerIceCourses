SUBROUTINE TransientMassBalance( Model,Solver,dt,TransientSimulation )
  
USE DefUtils
USE SolverUtils
USE ElementUtils

IMPLICIT NONE
TYPE(Model_t) :: Model
TYPE(Variable_t), POINTER :: Accumulation,SurfGrad1,Surfgrad2,Time
TYPE(Variable_t), POINTER :: MB,Dens,Firn,SE,Melting,Refreeze,Raining,PotRad
TYPE(Solver_t), POINTER :: Solver
TYPE(Element_t),POINTER :: Element
TYPE(ValueList_t), POINTER :: SolverParams
INTEGER, POINTER :: NodeIndexes(:)

INTEGER :: n,i,j,k,cont,nb_surf,nb_vert,io,nb_day,it,day,YearDay,nb_jour,first_day,nb_line,i1,i2,nx,ny,nxDEM,nyDEM

REAL(KIND=dp) :: f, z, deg_pos,accu,melt_local, temp_10m,rain,t_simu,ksnow,kice,k0,T,dt
REAL(KIND=dp) :: z_precip,grad_accu,grad,z_temp,seuil_precip,albedo_snow,albedo_ice,Rad_fact,alpha
REAL(KIND=dp) :: Pfact,temp_correc,firn_param,precip_correc,sigma,melt

REAL(KIND=dp) :: Sr,rho_w,rho_ice,L_heat,T0,g1,g2,g3,Mean_Temp_Air,x_output,y_output

REAL(KIND=dp) :: DEMReliefRes,zDEM,x_ray,y_ray,z_ray,MaskRes

REAL(KIND=dp) :: fx,fy,slop,asp,S0,dr,lat,L,term1,term2,term3,tau_r,tau_d,tau_b,srad,sinAlpha,R,M,Is
REAL(KIND=dp) :: Ir,Iday,I0,hsr,hs,cos_i,dS,Idiff,reflec,Norm,year,x,y,yearstartdata,precip_fact,MinAltFact,MaxAltFact

REAL(KIND=dp), DIMENSION(:,:),allocatable :: Mask_l,DEMRelief_l,PotRadNodes
REAL(KIND=dp), DIMENSION(:,:,:),allocatable ::Mask,DEMRelief

REAL(KIND=dp), DIMENSION(:),allocatable :: TempAirMoy,Precip,FirnNodes,MaskAccu

character(LEN=MAX_NAME_LEN) :: filenamePrecip,filenameMoy,filenameMaskRelief
CHARACTER(LEN=MAX_NAME_LEN), SAVE :: fileMaskAccu                        

CHARACTER(LEN=MAX_NAME_LEN), SAVE :: SolverName='Transient Mass Balance'

LOGICAL :: first_time=.true.,TransientSimulation, GotIt, PrecipData, node_output=.false.,GotNode=.false.,Output1D,SigmaOK 
LOGICAL :: OutputFirn,OutputDens,OutputMelting,OutputAccumulation,Execute_steady=.true., UnFoundFatal=.TRUE.
LOGICAL :: OutputRefreeze,OutputRaining,OutputPotRad,TransientMB,MaskRelief,Obstacle,AccuCorrectionMask

SAVE first_time,nb_surf,nb_vert,TempAirMoy,nb_day,DEMReliefRes,AccuCorrectionMask,&
&PrecipData,Precip,PotRadNodes,FirnNodes,TransientMB,Execute_steady,MaskAccu,MaskRelief,DEMRelief,nxDEM,nyDEM

! Read the Principal variable of the solver 
MB => VariableGet( Model % Variables, 'Mass Balance', UnFoundFatal=UnFoundFatal)

!===============================================================================
!Initialization=================================================================
IF (first_time) THEN 
  first_time=.false.
  SolverParams => GetSolverParams()
  TransientMB =  GetLogical( SolverParams,'Transient MB', GotIt)
  IF (.NOT. GotIt) TransientMB = .false.

  filenameMoy= ListGetString(Model % Constants,'MoyAirTemperatureFile', GotIt)
  IF (.NOT.GotIt) THEN
    CALL FATAL(SolverName, 'No file in Constant section for MoyAirTemperatureFile')
  END IF

  fileMaskAccu= ListGetString(Model % Constants,'MaskAccuFile', AccuCorrectionMask)
  IF (AccuCorrectionMask) THEN
    IF (ParEnv % MyPE<1) THEN
      CALL WARN(SolverName, 'Correction mask on accumulation applied')
	ENDIF
  END IF

  filenamePrecip= GetString(Model % Constants,'PrecipFile', PrecipData)
  IF (.NOT.PrecipData) THEN
    IF (ParEnv % MyPE<1) THEN
      CALL WARN(SolverName, 'No file for daily precipitation defined in Constants section (PrecipFile=)')
      CALL WARN(SolverName, 'Using constant precipitation rate defined by the "Precip" keyword')
    ENDIF
  END IF

  filenameMaskRelief = ListGetString(Model % Constants,'MaskReliefFile', MaskRelief)
  IF (MaskRelief) THEN
    IF (ParEnv % MyPE<1) THEN
      CALL WARN(SolverName, 'Mask relief activated for potential radiation')
    ENDIF
  END IF

  OPEN(1,file=filenameMoy,status='old')
  nb_day = 0
  DO
    READ(1,*,iostat=io)
    nb_day = nb_day + 1
    IF (io/=0) EXIT
  END DO
  CLOSE(1)
  nb_day=nb_day-1

  IF (PrecipData) THEN
    OPEN(1,file=filenamePrecip,status='old')
    cont = 0
    DO
      READ(1,*,iostat=io)
      cont = cont + 1
      IF (io/=0) EXIT
    END DO 
    CLOSE(1)
    cont=cont-1

    IF (cont.ne.nb_day) THEN
      print*, 'Lenght precip file = ',cont
      print*, 'Lenght temperature file = ', nb_day
      CALL FATAL(SolverName, 'Precip and temperature files have to be same lenght')
    ENDIF

    ALLOCATE(Precip(nb_day))

    OPEN(1,file=filenamePrecip,status='old')
    DO i=1,nb_day
      READ(1,*) Precip(i)
    END DO
    CLOSE(1)

  ENDIF

  ALLOCATE(TempAirMoy(nb_day))

  OPEN(1,file=filenameMoy,status='old')
  DO i=1,nb_day
    READ(1,*) TempAirMoy(i)
  END DO 
  CLOSE(1)

!Get number of surface nodes-----------------------------------
!--------------------------------------------------------------

  cont=0
  DO n=1,model % NumberOfNodes
    IF (MB % Perm(n).NE.0) THEN
      cont=cont+1
    ENDIF 
  END DO

  nb_surf=cont
  nb_vert=model % NumberOfNodes/cont
        
  ALLOCATE(FirnNodes(model % NumberOfNodes), &
           PotRadNodes(365,model % NumberOfNodes), &
           MaskAccu(model % NumberOfNodes))

  IF (MaskRelief) THEN
    OPEN(1,file=filenameMaskRelief,status='old')
    nb_line = 0
    DO
      READ(1,*,iostat=io)
      nb_line = nb_line + 1
      IF (io/=0) EXIT
    END DO
    CLOSE(1)
    nb_line=nb_line-1

    ALLOCATE(DEMRelief_l(nb_line,3))
  
    OPEN(1,file=filenameMaskRelief,status='old')
    DO i=1,nb_line
      READ(1,*) DEMRelief_l(i,1),DEMRelief_l(i,2),DEMRelief_l(i,3)
    END DO
    CLOSE(1)
	
	nxDEM=1
	DO WHILE (DEMRelief_l(nxDEM,2)==DEMRelief_l(nxDEM+1,2))
		nxDEM=nxDEM+1
	ENDDO
	
	nyDEM=1
	DO WHILE (DEMRelief_l(nyDEM,1)==DEMRelief_l(nyDEM+1,1))
		nyDEM=nyDEM+1
	ENDDO
		
	
	IF (nxDEM==1) THEN
		nxDEM=nb_line/nyDEM
		ALLOCATE(DEMRelief(nyDEM,nxDEM,3))
		cont=0
		DO j=1,nxDEM
			DO i=1,nyDEM
			cont=cont+1
			DEMRelief(i,j,1)=DEMRelief_l(cont,1)
			DEMRelief(i,j,2)=DEMRelief_l(cont,2)
			DEMRelief(i,j,3)=DEMRelief_l(cont,3)
			END DO
		END DO
	ELSE
		nyDEM=nb_line/nxDEM
		ALLOCATE(DEMRelief(nyDEM,nxDEM,3))
		cont=0
		DO i=1,nyDEM
			DO j=1,nxDEM
			cont=cont+1
			DEMRelief(i,j,1)=DEMRelief_l(cont,1)
			DEMRelief(i,j,2)=DEMRelief_l(cont,2)
			DEMRelief(i,j,3)=DEMRelief_l(cont,3)
			END DO
		END DO
	ENDIF
  
  
  DEMReliefRes=abs(DEMRelief(1,1,1)-DEMRelief(1,2,1))

 ENDIF
  
  
  
  
  IF (AccuCorrectionMask) THEN
  
  OPEN(1,file=fileMaskAccu,status='old')
  nb_line = 0
  DO
    READ(1,*,iostat=io)
    nb_line = nb_line + 1
    IF (io/=0) EXIT
  END DO
  CLOSE(1)

  nb_line=nb_line-1

  ALLOCATE(Mask_l(nb_line,3))
  OPEN(1,file=fileMaskAccu,status='old')
  DO i=1,nb_line
    READ(1,*) Mask_l(i,1),Mask_l(i,2),Mask_l(i,3)
  END DO
  CLOSE(1)
  

	nx=1
	DO WHILE (Mask_l(nx,2)==Mask_l(nx+1,2))
		nx=nx+1
	ENDDO
	
	ny=1
	DO WHILE (Mask_l(ny,1)==Mask_l(ny+1,1))
		ny=ny+1
	ENDDO
		

	
	IF (nx==1) THEN
		nx=nb_line/ny
		ALLOCATE(Mask(ny,nx,3))
		cont=0
		DO j=1,nx
			DO i=1,ny
			cont=cont+1
			Mask(i,j,1)=Mask_l(cont,1)
			Mask(i,j,2)=Mask_l(cont,2)
			Mask(i,j,3)=Mask_l(cont,3)
			END DO
		END DO
	ELSE
		ny=nb_line/nx
		ALLOCATE(Mask(ny,nx,3))
		cont=0
		DO i=1,ny
			DO j=1,nx
			cont=cont+1
			Mask(i,j,1)=Mask_l(cont,1)
			Mask(i,j,2)=Mask_l(cont,2)
			Mask(i,j,3)=Mask_l(cont,3)
			END DO
		END DO
	ENDIF
  
  MaskRes=abs(Mask(1,1,1)-Mask(1,2,1))

  DO n=1,model % NumberOfNodes
    x = model % nodes % x (n)
    y = model % nodes % y (n)

    k=floor((x-Mask(1,1,1))/MaskRes)+1
    j=floor((y-Mask(1,1,2))/MaskRes)+1

    IF ((j<=1).or.(j>=ny).or.(k<=1).or.(k>=nx)) THEN
      MaskAccu(n)=1.0
    ELSE
      MaskAccu(n)=Mask(j,k,3)*(Mask(j,k+1,1)-x)*(Mask(j+1,k,2)-y)+Mask(j,k+1,3)*(x-Mask(j,k,1))&
      &*(Mask(j+1,k,2)-y)+Mask(j+1,k,3)*(Mask(j,k+1,1)-x)*(y-Mask(j,k,2))+Mask(j+1,k+1,3)*&
      &(x-Mask(j,k,1))*(y-Mask(j,k,2))

      MaskAccu(n)=MaskAccu(n)/MaskRes/MaskRes
    END IF
  END DO
  
  ELSE
  
	MaskAccu=1.0
  
  ENDIF
  
  
  
  
END IF !end first_time


!Allocating variables====================================================================
!========================================================================================

CALL GET_VARIABLES()
!Computing potential radiation for each surface node=====================================
!========================================================================================

S0 = 1367         
dr= 0.0174532925
lat=GetConstReal(Model % Constants, "Latitude")
reflec=0.6

DO n=1,model % NumberOfNodes
  IF (MB % Perm(n).NE.0) THEN
    fx=SurfGrad1 % Values (SurfGrad1 % perm (n))
    fy=SurfGrad2 % Values (SurfGrad2 % perm (n))

    slop=atan(sqrt(fx**2+fy**2))
    asp=atan2(fx,fy)*(-1)
    L=lat*dr

    term1 = sin(L)*cos(Slop) - cos(L)*sin(Slop)*cos(Asp)
    term2 = cos(L)*cos(Slop) + sin(L)*sin(Slop)*cos(Asp)
    term3 = sin(Slop)*sin(Asp)

    srad=0.0
    DO i=1,365 
      !clear sky solar radiation    
      I0 = S0 * (1.0 + 0.0344*cos(360.0*dr*(194.0+i)/365.0))
      !sun declination dS
      dS = 23.45 * dr* sin(360.0*dr * ( (284.0+i)/365.0 ) )
      !angle at sunrise/sunset
      hsr = acos(-tan(L)*tan(dS))
      !sun hours during day 
      It=nint(12.0*(1.0+hsr/Pi)-12.0*(1.0-hsr/Pi))

      Iday=0
      DO j=1,It 
        !hourangle of sun hs      
        hs=hsr-15.0*dr*j  
        !solar altitude angle (alpha)		
        sinAlpha = sin(L)*sin(dS)+cos(L)*cos(dS)*cos(hs)
        !correction  using atmospheric transmissivity taub_b
        M=sqrt(1229.0+((614.0*sinAlpha))**2)-614.0*sinAlpha !Air mass ratio
        tau_b = 0.56 * (exp(-0.65*M) + exp(-0.095*M))
        tau_d = 0.271-0.294*tau_b !radiation diffusion coefficient for diffuse insolation
        tau_r = 0.271+0.706*tau_b !reflectance transmitivity
        !correct for local incident angle
        cos_i = (sin(dS)*term1) + (cos(dS)*cos(hs)*term2) + (cos(dS)*term3*sin(hs))
        Is = I0 * tau_b
        R = Is * cos_i
        IF (R<0.0) R=0.0
        Idiff = I0 * tau_d * cos(Slop)*cos(Slop)/2.0 * sinAlpha !diffuse radiation
        Ir = I0 * reflec * tau_r * sin(Slop)*sin(Slop)/2.0 * sinAlpha !reflectance

        IF (MaskRelief) THEN
          alpha=asin(sinAlpha)
        
          z_ray=model % nodes % z (n)
          x_ray=model % nodes % x (n)
          y_ray=model % nodes % y (n)

          Obstacle=.false.
          !Compare trajectory of sun light and surface elevation:
          DO k=1,20
            z_ray=z_ray+500.0*sin(alpha)
            x_ray=x_ray+500.0*cos(alpha)*sin(alpha)
            y_ray=y_ray-500.0*cos(alpha)*cos(hs)

            i1=floor((x_ray-DEMRelief(1,1,1))/DEMReliefRes)+1
            i2=floor((y_ray-DEMRelief(1,1,2))/DEMReliefRes)+1

            IF ((i2<=1).or.(i2>=nyDEM).or.(i1<=1).or.(i1>=nxDEM)) THEN

            ELSE
              zDEM=DEMRelief(i2,i1,3)*(DEMRelief(i2,i1+1,1)-x_ray)*(DEMRelief(i2+1,i1,2)-y_ray)+ &
              &DEMRelief(i2,i1+1,3)*(x_ray-DEMRelief(i2,i1,1))&
              &*(DEMRelief(i2+1,i1,2)-y_ray)+DEMRelief(i2+1,i1,3)*(DEMRelief(i2,i1+1,1)-x_ray)*(y_ray-DEMRelief(i2,i1,2))+&
              &DEMRelief(i2+1,i1+1,3)*(x_ray-DEMRelief(i2,i1,1))*(y_ray-DEMRelief(i2,i1,2))

              zDEM=zDEM/DEMReliefRes/DEMReliefRes

              IF (z_ray<zDEM) Obstacle=.true.
            END IF
            
          END DO
        
          IF (Obstacle) R=0.0

        END IF !MaskRelief

        R= R + Idiff + Ir
        IF (R<0.0) R=0.0
        Iday=Iday+R
      END DO !j=1,It 

      PotRadNodes(i,n) = Iday/24.0

    END DO !i=1,365 
  ENDIF ! if Perm > 0
END DO ! loop over all nodes

!!!! ------- If Steady  ------- !!!!
IF (Execute_steady) THEN
  CALL GET_PARAMETERS()
  
  IF (OutputPotRad) PotRad % Values (:) = 0.0
  MB % values (:) = 0.0
  IF (OutputMelting) Melting % values (:) = 0.0
  IF (OutputRaining) Raining % values (:) = 0.0
  IF (OutputAccumulation) Accumulation % values (:) = 0.0
  IF (OutputFirn) Firn % values (:) = 0.0
  
  !initial firn thickness :
  FirnNodes(:)=2.0
  
  yearstartdata = ListGetConstReal( Model % Constants, 'yearstartdata')
  
  
!===============================================================================
!Run Mass Balance model=========================================================

    DO n=1,model % NumberOfNodes
      IF (MB % Perm(n).NE.0) THEN
	  
		DO day=1,nb_day
		
			year = yearstartdata + day/365.25
			i1=floor((year-floor(year))*365.0)+1
			if (i1>365) then
				i1=i1-365
			endif

			IF (OutputPotRad) THEN
			PotRad % Values (PotRad % perm (n)) = &
				PotRad % Values (PotRad % perm (n)) + PotRadNodes(i1,n)/nb_day
			ENDIF

			z= model % nodes % z(n)! + FirnNodes(n)/0.45 - FirnNodes(n)
			rain=0.0
			accu=0.0

			IF (PrecipData) Pfact=Precip(day)*365.25*precip_correc

			T=TempAirMoy(day)+grad*(z_temp-z)+temp_correc

			precip_fact=max((1.0+(z-z_precip)*grad_accu)*MaskAccu(n),MinAltFact)
			precip_fact=min(precip_fact,MaxAltFact)
			
			IF (FirnNodes(n)>0.0) THEN
				melt=(T*ksnow+k0+(1.0-Albedo_snow)*Rad_fact*PotRadNodes(i1,n))*24.0*3600.0/L_heat/rho_w
			ELSE
				melt=(T*kice+k0+(1.0-Albedo_ice)*Rad_fact*PotRadNodes(i1,n))*24.0*3600.0/L_heat/rho_w
			END IF
			
			IF (melt<0) melt=0.0
			
			IF (T<=seuil_precip) accu=Pfact/365.25*precip_fact
			IF (T>seuil_precip) rain=Pfact/365.25*precip_fact

!===============================================================================
!Compute MB (Gilbert et al., 2016)==============================================

			
			MB % values (MB % perm(n)) = &
			MB % values (MB % perm(n)) + ((accu-melt)/(rho_ice/rho_w))*365.25/nb_day
          
			IF (OutputMelting) Melting % values (Melting % perm(n)) = &
			  Melting % values (Melting % perm(n))  + melt*365.25/nb_day
          
			IF (OutputRaining) Raining % values (Raining % perm(n)) = &
			  Raining % values (Raining % perm(n)) + rain*365.25/nb_day
          
			IF (OutputAccumulation) Accumulation % values (Accumulation % perm(n)) =  &
			  Accumulation % values (Accumulation % perm(n)) + accu*365.25/nb_day
          
!===============================================================================
!Compute Firn Thickness=========================================================

			FirnNodes(n) = FirnNodes(n) + (accu-melt) - FirnNodes(n)/firn_param/365.25
			IF (FirnNodes(n)<0.0) FirnNodes(n)= 0.0
		
		
		ENDDO ! Day loop
		
		!Initialize the firn thickness according to mean mass balance
		
		IF (OutputFirn) Firn % values (Firn % perm(n)) = max(MB % values (MB % perm(n))*(rho_ice/rho_w)*firn_param,0.0)
		FirnNodes(n) = max(MB % values (MB % perm(n))*(rho_ice/rho_w)*firn_param,0.0)
		
      END IF ! if Perm >0 
    END DO !Nodes loop
	
	
END IF !Execute Steady


!!!! ------- If Transient  ------- !!!!
IF (TransientMB) THEN

  Execute_steady=.false.
  
  CALL GET_PARAMETERS()

  IF (OutputPotRad) PotRad % Values (:) = 0.0
  MB % values (:) = 0.0
  IF (OutputMelting) Melting % values (:) = 0.0
  IF (OutputRaining) Raining % values (:) = 0.0
  IF (OutputAccumulation) Accumulation % values (:) = 0.0
  
  yearstartdata = ListGetConstReal( Model % Constants, 'yearstartdata')

  Time => VariableGet( Model % Variables, 'Time' )
  year = Time % Values (1) - dt
  nb_jour=NINT(dt*365.25)
  nb_jour=MAX(nb_jour,1)

  first_day = floor((year-yearstartdata)*365.25)+1
  YearDay= floor((year-floor(year))*365.25)+1

IF (first_day<1) call FATAL('Mass balance Solver','Simulation starts before data input')

!==============================================================================
!Compute potential solar radiation=================================================

  DO day=first_day,first_day+nb_jour-1

    i1=YearDay+day-first_day
	if (i1>365) then
		i1=i1-365
	endif

!===============================================================================
!Run Mass Balance model=========================================================

    DO n=1,model % NumberOfNodes
      IF (MB % Perm(n).NE.0) THEN

        IF (OutputPotRad) THEN
          PotRad % Values (PotRad % perm (n)) = &
		    PotRad % Values (PotRad % perm (n)) + PotRadNodes(i1,n)/nb_jour
        ENDIF

        z= model % nodes % z(n)! + FirnNodes(n)/0.45 - FirnNodes(n)
        rain=0.0
        accu=0.0

        IF (PrecipData) Pfact=Precip(day)*365.25*precip_correc

        T=TempAirMoy(day)+grad*(z_temp-z)+temp_correc

        precip_fact=max((1.0+(z-z_precip)*grad_accu)*MaskAccu(n),MinAltFact)
        precip_fact=min(precip_fact,MaxAltFact)

		IF (FirnNodes(n)>0.0) THEN
			melt=(T*ksnow+k0+(1.0-Albedo_snow)*Rad_fact*PotRadNodes(i1,n))*24.0*3600.0/L_heat/rho_w
		ELSE
			melt=(T*kice+k0+(1.0-Albedo_ice)*Rad_fact*PotRadNodes(i1,n))*24.0*3600.0/L_heat/rho_w
		END IF

        IF (melt<0) melt=0.0
        IF (T<=seuil_precip) accu=Pfact/365.25*precip_fact
        IF (T>seuil_precip) rain=Pfact/365.25*precip_fact

!===============================================================================
!Compute MB (Gilbert et al., 2016)==============================================

        
        MB % values (MB % perm(n)) = &
          MB % values (MB % perm(n)) + ((accu-melt)/(rho_ice/rho_w))*365.25/nb_jour
          
        IF (OutputMelting) Melting % values (Melting % perm(n)) = &
          Melting % values (Melting % perm(n))  + melt*365.25/nb_jour
          
        IF (OutputRaining) Raining % values (Raining % perm(n)) = &
          Raining % values (Raining % perm(n)) + rain*365.25/nb_jour
          
        IF (OutputAccumulation) Accumulation % values (Accumulation % perm(n)) = &
          Accumulation % values (Accumulation % perm(n)) + accu*365.25/nb_jour
          

!===============================================================================
!Compute Firn Thickness=========================================================

        FirnNodes(n) = FirnNodes(n) + (accu-melt) - FirnNodes(n)/firn_param/365.25
        IF (FirnNodes(n)<0.0) FirnNodes(n)= 0.0
        IF (OutputFirn) Firn % values (Firn % perm(n)) = FirnNodes(n)
        

      END IF ! if Perm >0 
    END DO !Nodes loop
  END DO !Day loop
ENDIF ! Transient case

CONTAINS

  SUBROUTINE GET_VARIABLES()
  SurfGrad1 => VariableGet( Model % Variables, 'SurfGrad 1', UnFoundFatal=UnFoundFatal)

  SurfGrad2 => VariableGet( Model % Variables, 'SurfGrad 2', UnFoundFatal=UnFoundFatal)

  Firn => VariableGet( Model % Variables, 'Firn')
  IF ( .not. ASSOCIATED( Firn ) ) THEN
    OutputFirn=.false.
  ELSE
    OutputFirn=.true.
  ENDIF

  Dens => VariableGet(Model % Mesh % Variables, 'Densi' )
  IF ( .not. ASSOCIATED( Dens ) ) THEN
    OutputDens=.false.
  ELSE
    OutputDens=.true.
  ENDIF

  Melting => VariableGet( Model % Variables, 'Melting')
  IF ( .not. ASSOCIATED( Melting ) ) THEN
    OutputMelting=.false.
  ELSE
    OutputMelting=.true.
  ENDIF

  Accumulation => VariableGet( Model % Variables, 'Accu')
  IF ( .not. ASSOCIATED( Accumulation ) ) THEN
    OutputAccumulation=.false.
  ELSE
    OutputAccumulation=.true.
  ENDIF

  Refreeze => VariableGet( Model % Variables, 'Refreeze')
  IF ( .not. ASSOCIATED( Refreeze ) ) THEN
    OutputRefreeze=.false.
  ELSE
    OutputRefreeze=.true.
  ENDIF

  Raining => VariableGet( Model % Variables, 'Rain')
  IF ( .not. ASSOCIATED( Raining ) ) THEN
    OutputRaining=.false.
  ELSE
    OutputRaining=.true.
  ENDIF

  PotRad => VariableGet( Model % Variables, 'PotRad')
  IF ( .not. ASSOCIATED( PotRad ) ) THEN
    OutputPotRad=.false.
  ELSE
    OutputPotRad=.true.
  ENDIF
  END SUBROUTINE GET_VARIABLES
 
  SUBROUTINE GET_PARAMETERS()
  z_temp=GetConstReal(Model % Constants, "z_temp")
  z_precip=GetConstReal(Model % Constants, "z_precip")
  seuil_precip=GetConstReal(Model % Constants, "seuil_precip")
  grad=GetConstReal(Model % Constants, "GradTemp")
  Rad_fact=GetConstReal(Model % Constants, "RadFact")
  Albedo_ice=GetConstReal(Model % Constants, "Albedo_ice")
  Albedo_snow=GetConstReal(Model % Constants, "Albedo_snow")
  kice=GetConstReal(Model % Constants, "kice")
  ksnow=GetConstReal(Model % Constants, "ksnow")
  k0=GetConstReal(Model % Constants, "k0")
  firn_param=GetConstReal(Model % Constants, "firn_param")

  grad_accu=GetConstReal(Model % Constants, "GradPrecip")
  MaxAltFact=GetConstReal(Model % Constants, "MaxAltFact")
  MinAltFact=GetConstReal(Model % Constants, "MinAltFact")

  temp_correc=GetConstReal(Model % Constants, "TempCorrec",GotIt)
  IF (.not.GotIt) temp_correc=0.0

  rho_w=GetConstReal(Model % Constants, "Water Density")
  rho_ice=GetConstReal(Model % Constants, "Ice Density")
  L_heat=GetConstReal(Model % Constants, "L_heat")

  IF (PrecipData) THEN
    precip_correc=GetConstReal(Model % Constants, "PrecipCorrec",GotIt)
    IF (.not.GotIt) precip_correc=1.0
  ELSE
    Pfact=GetConstReal(Model % Constants, "Precip",GotIt)
    IF (.not.GotIt) THEN
      CALL FATAL(SolverName,'No precipition file, need to define mean precipition (Precip = )')
    ENDIF
  ENDIF
  END SUBROUTINE GET_PARAMETERS

END SUBROUTINE TransientMassBalance
