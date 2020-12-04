# prognostic simulations simulation

In this example we make prognostic simulations for 50 years.

- **CTRL.sif**: a control experiment with no basal melting and constant surface mass balance forcing.
- **dSMB.sif**: we apply the [InitMIP Greenland](https://tc.copernicus.org/articles/12/1433/2018/) surface mass balance anomaly.
- **BMB.sif**: we apply sub-shelf melting using an ad-hoc quadratic function of the bottom surface.

## Run the test

1. copy the mesh directory from the **INITIALISATION** step:
        ```shell
        cp -r ../3_INITIALISATION/MESH_1 .
        ```

2. Compile required user functions: 
	```shell
	make
	```

3. Run the test:
	```shell
	ElmerSolver CTRL.sif
	```
