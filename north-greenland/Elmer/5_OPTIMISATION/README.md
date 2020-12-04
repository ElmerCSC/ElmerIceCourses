# Inverse method

Optimisation of the friction coefficient to reduce the mismatch with observed velocities.

The method is impelmented in the template *.sif* file **OPTIM_BETA.TEMP.sif**.

The bash scritpt **MAKE_ASSIM.sh** is use to test several regularisation parameters.

## Run the test

1. copy the mesh directory from the **INITIALISATION** step:
        ```shell
        cp -r ../3_INITIALISATION/MESH_1 .
        ```

2. Compile required user functions: 
	```shell
	make
	```
3. Run the test for several regularisation parameters:
	```shell
	. ./MAKE_ASSIM.sh
	```

4. PostProcessing
    - Plot the L-Curve, i.e. extract last lines of Cost files to plot $J_reg=f(J_0)$
	```shell
        processing/MakeLCurve.sh
        ```
    - Make convergence plots
       ```shell
       python  processing/MakeReport.py "RUN_ID"
       ```
