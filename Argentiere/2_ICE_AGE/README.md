In this second step, the particle solver is added to solve for the ice age and trajectories 

Execute the simulation:<br>
Serial: 'ElmerSolver age_of_ice.sif'<br> 
Parallel: `mpirun -np 4 ElmerSolver_mpi age_of_ice.sif`  

Take care that the number of nodes required (np) is compatible with the number of partitions of the mesh (4 here) 
