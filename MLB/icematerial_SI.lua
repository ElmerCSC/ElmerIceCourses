-- #############################################################
--  A list of usefull constants for ice-sheet simulations
--   
--   - Unit System =
--      -- m
--      -- a
--      -- MPa
-- #############################################################



exp=math.exp

-- ## constants declaration
yearinsec=365.25*24.0*3600.0
Pa2MPa=1.0E-06
rhoi=917.0
gravity = -9.81

A1 = 3.985E-13         --- MPa^(-3) a^(-1) -> SI
A2 = 1.916E3          --- MPa^(-3) a^(-1) -> SI
Q1 = 60.0E3 
Q2 = 139.0E3

-- # utility functions
function max(a,b)
  if (a > b) then
    return a
  else
    return b
  end  
end    
function min(a,b)
  if (a < b) then
    return a
  else
    return b
  end  
end

function IfThenElse(condition,t,f) 
   if condition then
     return t
   else
     return f
   end 
end
-- ## thermal properties
function conductivity(Tin)
 T = Tin
 if (T > 273.15) then
   T = 273.15
 end 
 k=9.828*exp(-5.7E-03*T)
 return k
end

-- ## heat capacity
function capacity(Tin)
  T = Tin
  if (T > 273.15) then
    T = 273.15
  end 
  c=146.3+(7.253*T)
  return c
end

-- ## relative temperature
function reltemp(T)
  Th= T - 273.15
  return Th
end

-- friction loads
function frictionloads(lx,ly,lz,vx,vy,vz)
  cx = max(-lx*vx,0)
  cy = max(-ly*vy,0)
  cz = max(-ly*vy,0)
  return cx + cy + cz
end  
