# Mesh Optimisation

# Description

This test cas illustrates the 2D mesh adaptation capabilities of Elmer/Ice.

| :warning: WARNING          |
|:---------------------------|
| To run this test case Elmer has to be compiled with the mesh adaptation library *mmg*.  |


See the [documentation](http://elmerfem.org/elmerice/wiki/doku.php?id=mesh:meshadaptation) for the installation procedure.

For additionnal informations, you can see this [presentation](http://elmerfem.org/elmerice/wiki/lib/exe/fetch.php?media=courses:2017:mesh_adapt_2017.pdf) that was presented at the [Advanced Elmer/Ice Workshop - IGE 2017](http://elmerfem.org/elmerice/wiki/doku.php?id=courses:ige2017)

# Run the test

1. Copy the mesh dirtectory from **step1**: 
	```shell
	cp -r ../1_MESH_INI/mesh .
	```

2. Compile required user functions: 
	```shell
	make
	```

3. Run the test:
	```shell
	ElmerSolver MESH_OPTIM.sif
	```

