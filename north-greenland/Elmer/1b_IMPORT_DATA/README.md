#  Importing data

## Description

We illustrate how to interpolate real data in netcdf on the mesh.

| :warning: WARNING          |
|:---------------------------|
| To run this test case you Elmer has to be compile with netcdf support and with the external libraries ([nn](http://code.google.com/p/nn-c/) and [csa](http://code.google.com/p/csa-c/)).  |

Two solvers can be used to read netcdf files:

1. [GridDataReader](http://elmerfem.org/elmerice/wiki/doku.php?id=solvers:griddatareader)
2. [Scattered2DDataInterpolator](http://elmerfem.org/elmerice/wiki/doku.php?id=solvers:scattered)
	- This solver depends on external libraries ([nn](http://code.google.com/p/nn-c/) and [csa](http://code.google.com/p/csa-c/)) and requires additional steps for the compilation. ** See solver documention ** 
	- If you don't have this solver you can edit the *.sif* file and remove lines corresponding to **Solver 2**


## Run the test

1. copy the mesh directory from **Step1**: 
	```shell
	cp -r ../1_MESH_INI/mesh .
	```

2. Execute the sif file
	```shell
	ElmerSolver IMPORT.sif
	```
