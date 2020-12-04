#  Create the mesh of the domain.

# Description 

The model domain is given as a shape file in *../../Data/domain*
The shapefile contains lines with the attribute **BC** to define different boundary conditions.
We will use python codes to make a gmsh .geo file and mesh the domain.

Original source codes and documentation are given [here](https://github.com/ElmerCSC/elmerfem/tree/elmerice/elmerice/Meshers/GIS)

# Run the test

1. Convert a shape file to a gmsh .geo file
	```shell
	python ../../Codes/PreProcessing/Contour2geo.py -r 2000.0 -i ../../Data/domain/bassin.shp -o mesh.geo
	```

2. Mesh the domain: 
	```shell
	gmsh -2 mesh.geo 
	```

3. Convert to Elmer format: 
	```shell
	ElmerGrid 14 2 mesh.msh -autoclean 
	```

4. For visualisation you can convert the Elmer mesh to a shapefile: 
	```shell
	python ../../Codes/PreProcessing/MeshToShp.py -d mesh 
	gdalsrsinfo  -o wkt "EPSG:3413" > mesh_shp/elements.prj 
	gdalsrsinfo  -o wkt "EPSG:3413" > mesh_shp/boundaries.prj 
	```

> Run *MakeMesh.sh* to do it automatically:
> ```shell
> . ./MakeMesh.sh
> ```
