# -*- coding: utf-8 -*-
# Create a geo (gmsh input file) file from two contour files
# the contour files contain the (x,y) coordinates of the ordered
# points defining the contour of the glacier and the contour of the cavity
#
import numpy as np
import matplotlib.pyplot as plt

# Test these options
    # edge size of the elements
el_size = 18.0
el_sizec = 5.0    

    # Spline or line 
spline = True

Contour = np.loadtxt('./../Data/Contour_TR_glacier.dat')
x = Contour[:,0]
y = Contour[:,1]

Contourc = np.loadtxt('./../Data/Contour_TR_cavity.dat')
xc = Contourc[:,0]
yc = Contourc[:,1]

if x[0]==x[-1] and y[0]==y[-1]:
    print('Same first and last points in contour file for the glacier contour')
    Npt = len(x)-1
else:
    Npt = len(x)    
    
if xc[0]==xc[-1] and yc[0]==yc[-1]:
    print('Same first and last points in contour file for the cavity contour')
    Nptc = len(xc)-1
else:
    Nptc = len(xc)     

# Open the output file
geo = open('teterousse.geo', 'w')
geo.write('// This a a geo file created using the python script Makegeo_2.py // \n')
geo.write('Mesh.Algorithm=5; \n')
geo.write('// To controle the element size, one can directly modify the lc values in the geo file // \n')
geo.write('lc = {0} ; \n'.format(el_size))
geo.write('lcc = {0} ; \n'.format(el_sizec))

# write the points coordinates (x,y,0,lc)
np=0
for j in range(0,Npt):
    np=np+1
    geo.write('Point({0}) = '.format(np)+r'{'+' {0}, {1}, 0.0, lc'.format(x[j],y[j])+r'}'+'; \n')

for j in range(0,Nptc):
    np=np+1
    geo.write('Point({0}) = '.format(np)+r'{'+' {0}, {1}, 0.0, lcc'.format(xc[j],yc[j])+r'}'+'; \n')    

# if spline
if spline: 
    geo.write('Spline(1) = {')
    for j in range(0,Npt):
        geo.write('{0},'.format(j+1))
    geo.write('1}; \n')
    geo.write('Spline(2) = {')
    for j in range(0,Nptc):
        geo.write('{0},'.format(Npt+j+1))
    geo.write('{0}'.format(Npt+1)+'}; \n')
    
    geo.write('Line Loop(3) = {1}; \n')
    geo.write('Line Loop(4) = {2}; \n')
    geo.write('Plane Surface(5) = {3,4}; \n')
    geo.write('Plane Surface(6) = {4}; \n')
    geo.write('Physical Line(7) = {1}; \n')
    geo.write('Physical Surface(8) = {5,6}; \n')
    
    
# else it is lines, as a spline might not work in all case
else:
    nl=0
    for j in range(0,Npt-1):
        nl=nl+1
        geo.write('Line({0}) = '.format(nl)+r'{'+'{0},{1}'.format(j+1,j+2)+r'}'+'; \n')
    geo.write('Line({0}) = '.format(nl+1)+r'{'+'{0},{1}'.format(j+2,1)+r'}'+'; \n')
    
    nl = nl+1
    for j in range(0,Nptc-1):
        nl=nl+1
        geo.write('Line({0}) = '.format(nl)+r'{'+'{0},{1}'.format(Npt+j+1,Npt+j+2)+r'}'+'; \n')
    geo.write('Line({0}) = '.format(nl+1)+r'{'+'{0},{1}'.format(Npt+j+2,Npt+1)+r'}'+'; \n')
    
    geo.write('Compound Line({0}) = '.format(nl+2)+r'{')
    for j in range(0,Npt-1):
        geo.write('{0}, '.format(j+1))
    geo.write('{0}'.format(j+2)+'}; \n')
    
    geo.write('Compound Line({0}) = '.format(nl+3)+r'{')
    for j in range(0,Nptc-1):
        geo.write('{0}, '.format(Npt+j+1))
    geo.write('{0}'.format(Npt+j+2)+'}; \n')
    
    geo.write('Line Loop({0}) = '.format(nl+4)+r'{'+'{0}'.format(nl+2)+r'};'+' \n')
    geo.write('Line Loop({0}) = '.format(nl+5)+r'{'+'{0}'.format(nl+3)+r'};'+' \n')
    
    geo.write('Plane Surface({0}) = '.format(nl+6)+r'{'+'{0},{1}'.format(nl+4,nl+5)+r'};'+' \n')
    geo.write('Plane Surface({0}) = '.format(nl+7)+r'{'+'{0}'.format(nl+5)+r'};'+' \n')
    
    geo.write('Physical Line({0}) = '.format(nl+8)+r'{'+'{0}'.format(nl+2)+r'};'+' \n')
    geo.write('Physical Surface({0}) = '.format(nl+9)+r'{'+'{0},{1}'.format(nl+6,nl+7)+r'};'+' \n')

geo.close()
