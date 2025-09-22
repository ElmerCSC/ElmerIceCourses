
-- -----------------------------
--  GlaDS
-- -----------------------------
rhow=1000.0
ub = 1.0e-6  
--  For the sheet
Aglen = 2.5e-25
Ar = Aglen
alphas = 1.25 
betas = 1.5 
lr = 2.0 
hr = 0.1 -- 0.1 
Ks = -10.0*2.0/(rhow*gravity)  -- 0.05
Hs = 0.01 --  0.01 IC for h
ev = 0.0001 -- 0
Source = 0.5/yearinsec

--  For the Channels
alphac = 1.25 
betac = 1.5 
Kc = 0.5 -- 0.1
Ac = Aglen  
lc = 2.0 --  2.0 
Ct = 7.5e-8
Cw = 4220.0
Lw = 334000.0


--  For the Moulins
Am = 4.0

-- Permafrost
p0 = 101032.0
T0 = 273.15
ng = 3.0
Rg=8.314
sedimentheight = 1.0

-- heat conductivity W/(m K)

kappaw0 = 0.56

function kappaw(T)
  kappawref = 0.6065
  Tkappawref = 298.15
  cof0 = -1.48445
  cof1 = 4.12292
  cof2 = -1.63866
  Tr = T/Tkappawref
  lstar = cof0 + cof1*Tr + cof2*Tr*Tr
  return kappawref*lstar
end  

function htcoeff(kappa,thickness)
  gap = max(thickness, 0.001)
  return kappa/gap
end  

function settimestepsize(nt)
  dt = dtInit*(dtIncr^nt)
  print(">>>>>>>> Current timestep",dt)
  if ( dt > dtMax ) then
    dt = dtMax
  end
  return dt
end

function getmeltrate(load,weights,grdmsk)
 if (grdmsk <= 0.0) then
   return 0.0
 else  
   return max(-load/(weights*rhoi*Lw),0.0)
 end
end

function arrhenius(Trel)
  if (Trel > -10.0) then
    A0 = A2
    Q = Q2
  else
   A0 = A1
    Q = Q1
  end
  T = min(0.0,Trel)
  A = A0 * math.exp(-Q/(Rg*(273.15 - T)))
  -- print(">>>>>>>> Arrhenius", T, A)
  return A 
end  

