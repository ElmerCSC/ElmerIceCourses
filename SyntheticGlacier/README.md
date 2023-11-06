# Synth Glacier
Elmer/Ice setup for Synth Glacier based on a workflow by Gagliardini using Adhikari PhD 2012 
This setup could be easily adapted to a real glacier by just changing the Bed and Surface DEM as well as the Surface Mass Balance. 

1. `Step1`<br>
Tools to prepare the mesh<br>
- Python script Synthetical_Glacier_BedDEM.py to make surface and bedrock DEM (netcdf file) and export a contour of the domain<br>
- initialise_DEM.sif: first read the bedrock and surface DEMs and used them for initialisation of variable Zs in the following steps<br>

2. `Step2`<br>
- steady_climat0.sif: Start to grow the glacier (just SMB, no dynamics)<br>
- steady_climat.sif: A steady surface mass balance is applied and the model is run long enough to find a steady state geometry of the glacier.<br>
- Extrusion.sif: Defines internal extrusion of mesh (read by other sif files)

3. `Step3`<br>
TO BE DONE - Run transient simulations with time dependent surface mass balance

4. `Parameters`<br>
- List of physical parameters used in model simulations
