# -*- coding: utf-8 -*-
# Create a geo (gmsh input file) file from a contour file
# the contour file contains the (x,y) coordinates of the ordered
# points defining the contour of the domain
#
import numpy as np
import matplotlib.pyplot as plt

# Test these options
    # edge size of the elements
el_size = 150.0    

    # Spline or line 
spline = False

#Contour = np.loadtxt('../Data/contour_Argentiere_large.dat')
Contour = np.loadtxt('../Data/contour_2014_ARG.dat')
x = Contour[:,0]
y = Contour[:,1]

if x[0]==x[-1] and y[0]==y[-1]:
    print('Same first and last points in contour file')
    Npt = len(x)-1
else:
    Npt = len(x)    

# Open the output file
geo = open('ARG_mesh.geo', 'w')
geo.write('// This a a geo file created using the python script Makegeo.py // \n')
geo.write('Mesh.Algorithm=5; \n')
geo.write('// To controle the element size, one can directly modify the lc value in the geo file // \n')
geo.write('lc = {0} ; \n'.format(el_size))

# write the points coordinates (x,y,0,lc)
np=0
for j in range(0,Npt):
    np=np+1
    geo.write('Point({0}) = '.format(np)+r'{'+' {0}, {1}, 0.0, lc'.format(x[j],y[j])+r'}'+'; \n')

# if spline
if spline: 
    geo.write('Spline(1) = {')
    for j in range(0,Npt):
        geo.write('{0},'.format(j+1))
    geo.write('1}; \n')
    
    geo.write('Line Loop(2) = {1}; \n')
    geo.write('Plane Surface(3) = {2}; \n')
    geo.write('Physical Line(4) = {1}; \n')
    geo.write('Physical Surface(5) = {3}; \n')
    
     
# else it is lines, as a spline might not work in all case
else:
    nl=0
    for j in range(0,Npt-1):
        nl=nl+1
        geo.write('Line({0}) = '.format(nl)+r'{'+'{0},{1}'.format(j+1,j+2)+r'}'+'; \n')
    geo.write('Line({0}) = '.format(nl+1)+r'{'+'{0},{1}'.format(j+2,1)+r'}'+'; \n')
    
    geo.write('Line Loop({0}) = '.format(nl+2)+r'{')
    for j in range(0,Npt-1):
        geo.write('{0}, '.format(j+1))
    geo.write('{0}'.format(j+2)+'}; \n')
    
    geo.write('Plane Surface({0}) = '.format(nl+3)+r'{'+'{0}'.format(nl+2)+r'};'+' \n')
    geo.write('Physical Line({0}) = '.format(nl+4)+r'{')
    for j in range(0,Npt-1):
        geo.write('{0}, '.format(j+1))
    geo.write('{0}'.format(j+2)+'}; \n')

    geo.write('Physical Surface({0}) = '.format(nl+5)+r'{'+'{0}'.format(nl+3)+r'};'+' \n')

geo.close()
