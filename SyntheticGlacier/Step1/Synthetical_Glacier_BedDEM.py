#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 22 18:26:18 2021

@author: ogagliardini

Make a synthetical bed DEM from the Adhikari 2012 PhD thesis

NetCDF from this page: https://pyhogs.github.io/intro_netcdf4.html

"""

import numpy as np
import matplotlib.pyplot as plt

import netCDF4 as nc4
from datetime import datetime

plt.close('all')

def bed(x,y,abx, DD, rr, ar, lr, Ly, q1, q2, am, lm): 
    y = (0.5*Ly+q1*x*np.exp(-q2*x))*2*y/Ly + am*np.sin(2*np.pi*x/lm)
    bed = 5000.0 - abx*x-DD*(1-(y/(DD*rr))**2) - ar*np.sin(2*np.pi*x/lr)
    return bed

bed_version = 'bed1'
MinH = 1.0

# create a grid with to interpolate teh continuous DEM and save it
# dx = dy = 20
Lx = 12000.0
Ly = 5000.0
X0 = 0.0
Y0 = -2500.0
dx = 100.0
dy = 100.0
Nx = round(Lx/dx)
Ny = round(Ly/dy)
x = np.linspace(X0,X0+Lx,Nx+1)
y = np.linspace(Y0,Y0+Ly,Ny+1)
X, Y = np.meshgrid(x,y)

abx=0.15      # Mean bedrock slope [-] - 0.15
DD=500.0      # valley depth [m] - 500.0
rr=2.0        # lateral aspect ratio [-] - 2.0
ar=0.0        # Roughness amplitude [m] - 0.0
lr=5000.0     # Roughness wavelength [m] - infinity
#Ly=Ly,       # Half domain width [m] - 2000.0
q1=0.0        # Width coefficient [-] - 0.0 
q2=1/1000.0   # Width exponent [m^-1] - infinity
am=1000.0      # Meander amplitude [m] - 0.0
lm=15000.0    # Wavelength of glacier curvature [m] - 15000.0


zb = bed(X, Y, 
          abx=abx,
          DD=DD,       
          rr=rr,         
          ar=ar,         
          lr=lr,
          Ly=Ly,          
          q1=q1,          
          q2=q2,    
          am=am,      
          lm=lm      
)

# 4400 is the maximum altitude (SMB is zero above this altitude)

cs = plt.contour(x,y,zb, [4400])
p = cs.collections[0].get_paths()[0]
v = p.vertices
n = len(v[:,0])
xc = np.zeros(n+2)
yc = np.zeros(n+2)
xc[0:n] = v[0:n,0]
yc[0:n] = v[0:n,1]
xc[n] = Lx
yc[n] = Y0+Ly
xc[n+1] = xc[0]
yc[n+1] = yc[0]

# plot this bed 
plt.figure()
plt.contourf(X, Y, zb)
#plt.contour(x,y,zb, [4400])
plt.title('Synthetical bed')
plt.colorbar()
plt.show()

# Save the contour in an ascii file 
np.savetxt('SyntBed_Contour_'+bed_version+'.dat',np.column_stack((xc,yc)), fmt="%10.2f %10.2f")


# Save it as a netcdf grid
f = nc4.Dataset('SyntBed_DEM_'+bed_version+'.nc','w', format='NETCDF4') 


f.createDimension('x', len(x))
f.createDimension('y', len(y))

xcoord = f.createVariable('X', 'f4', 'x')
ycoord = f.createVariable('Y', 'f4', 'y')
elevation = f.createVariable('bedDEM', 'f4', ('y', 'x'))
surface = f.createVariable('surfDEM', 'f4', ('y', 'x'))

xcoord[:] = x
ycoord[:] = y
elevation[:,:] = zb
surface[:,:] = zb + MinH

xcoord.units = 'm'
xcoord.description = 'dx = '+str(dx)+' m'
ycoord.units = 'm'
ycoord.description = 'dy = '+str(dy)+' m'
elevation.units = 'm'
elevation.description = 'Bed elevation '
surface.units = 'm'
surface.description = 'Surface elevation : Zs = Zb + MinH with MinH = '+str(MinH)+' m'


# Create also scalar variables to save the bed description
abx_v = f.createVariable('abs', 'f4')
abx_v[0] = abx
abx_v.units = '-'
abx_v.long_name = 'Mean berock slope'

DD_v = f.createVariable('DD', 'f4')
DD_v[0] = DD
DD_v.units = 'm'
DD_v.long_name = 'Valley depth'

rr_v = f.createVariable('rr', 'f4')
rr_v[0] = rr
rr_v.units = '-'
rr_v.long_name = 'Lateral aspect ratio'

ar_v = f.createVariable('ar', 'f4')
ar_v[0] = ar
ar_v.units = 'm'
ar_v.long_name = 'Roughness amplitude'

lr_v = f.createVariable('lr', 'f4')
lr_v[0] = lr
lr_v.units = 'm'
lr_v.long_name = 'Roughness wavelength'

Ly_v = f.createVariable('Ly', 'f4')
Ly_v[0] = lr
Ly_v.units = 'm'
Ly_v.long_name = 'Half domain width'

q1_v = f.createVariable('q1', 'f4')
q1_v[0] = q1
q1_v.units = '-'
q1_v.long_name = 'Width coefficient'

q2_v = f.createVariable('q2', 'f4')
q2_v[0] = q2
q2_v.units = 'm^{-1}'
q2_v.long_name = 'Width exponent'

am_v = f.createVariable('am', 'f4')
am_v[0] = am
am_v.units = 'm'
am_v.long_name = 'Meander amplitude'

lm_v = f.createVariable('lm', 'f4')
lm_v[0] = lm
lm_v.units = 'm'
lm_v.long_name = 'Wavelength of glacier curvature'


f.description = 'Synthetical Bedrock from Adhikari PhD 2012'
today = datetime.today()
f.history = "Created " + today.strftime("%d/%m/%y")
f.close()



