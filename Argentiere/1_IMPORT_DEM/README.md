In this first step, only read the different DEMs needed for the simulation and execute a diagnostic simulations for this geometry

Execute the simulation:<br>
Serial: 'ElmerSolver initialise_DEM.sif'<br> 
Parallel: `mpirun -np 4 ElmerSolver initialise_DEM.sif`  

The simulation can also be run using the Blatter-Pattyn solution instead of the Stokes using 'initialise_1stO_DEM.sif' 
Serial: 'ElmerSolver initialise_1stO_DEM.sif'<br> 
Parallel: `mpirun -np 4 ElmerSolver initialise_1stO_DEM.sif`  

Take care that the number of nodes required (np) is compatible with the number of partitions of the mesh (4 here) 
