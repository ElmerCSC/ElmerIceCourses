# diagnostic simulation

Compute the 2D horizontal velocity field using the shallow shelf approximation

## Run the test

1. copy the mesh directory from the **INITIALISATION** step:
        ```shell
        cp -r ../3_INITIALISATION/MESH_1 .
        ```

2. Execute the .sif file
        ```shell
        ElmerSolver SSA.sif
	```

3. Process the results with pvpython
	```shell
        pvpython PlotScript.py
        ```
