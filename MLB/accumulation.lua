

function accum(Z,glaciated)
  lapserate = (11.0/2750.0)
  ela = 400.0
  asl = -ela*lapserate
  ac = lapserate*Z + asl
  if ((glaciated == -1) and (ac < 0.0) )
  then
    return 0.0
  else
    return ac
  end  
end

function initzs(surf,bed)
  zs = surf
  if ((surf - bed) < MINH) then
    zs = bed + MINH
  end
  return zs
end

function meshupdate(surf,bed,zs)
  mu = zs - surf
  if ((surf - bed) < MINH) then
     mu = zs - bed + MINH
  end
  return mu
end  

function settemperate(z)
  if (z > 400.0) then
    rv = 1.0
  else
    rv = -1.0
  end
  return rv
end  