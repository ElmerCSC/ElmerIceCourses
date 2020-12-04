#!/bin/bash


python ../../Codes/PreProcessing/Contour2geo.py -r 2000.0 -i ../../Data/domain/bassin.shp -o mesh.geo

gmsh -2 mesh.geo

ElmerGrid 14 2 mesh.msh -autoclean

python ../../Codes/PreProcessing/MeshToShp.py -d mesh
gdalsrsinfo  -o wkt "EPSG:3413" > mesh_shp/elements.prj
gdalsrsinfo  -o wkt "EPSG:3413" > mesh_shp/boundaries.prj
