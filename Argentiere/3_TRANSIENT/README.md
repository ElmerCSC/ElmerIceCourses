In this third step, we move to a transient simulation assuming a given Surface Mass Balance

Execute the simulation:<br>
Serial: 'ElmerSolver transient.sif'<br> 
Parallel: `mpirun -np 4 ElmerSolver transient.sif`  

A directory GlacierOut has to be created to store the output .dat files

Take care that the number of nodes required (np) is compatible with the number of partitions of the mesh (4 here) 
