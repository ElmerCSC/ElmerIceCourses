# TeteRousse
Tete Rousse test case for Elmer/Ice beginner course

The TeteRousse example contains the following material: <br>
**Data**: Contour\_TR\_cavity.dat, Contour\_TR\_glacier.dat, DEM\_TR\_bed.dat, DEM\_TR\_cavity.dat, DEM\_TR\_surf.dat<br>
**PROG**: USF\_TR.f90<br>
**Step1**: Makegeo.py, teterousse1.sif<br>
**Step2a**: Makegeo\_2.py, teterousse2a.sif<br>
**Step2b**: teterousse2b.sif<br>
**Step3a**: teterousse3a.sif<br>
**Step3b**: teterousse3b.sif<br>
**Step3c**: teterousse3c.sif<br>

Main commands for running **Step1**:<br>

1. Go into **Step1** directory:
`cd Step1`<br>

2. To create the geo input file for gmsh:<br>
`python Makegeo.py`<br>

3. To make the mesh from the geo file:<br>
`gmsh teterousse0.geo -2`<br>
`ElmerGrid 14 2 teterousse0.msh -autoclean`<br>

4. To visualize the mesh:<br>
`ElmerGrid 14 5 teterousse0.msh -autoclean`<br>
`paraview teterousse0.vtu`<br>

5. To run the simulation:<br>
`ElmerSolver teterousse1.sif`<br>

6. To compile the user function in PROG:<br>
`cd PROG`<br>
`elmerf90 USF_TR.f90 -o USF_TR`<br>

7. To visualise the results with paraview:<br>
`paraview teterousse0/teterousse_step1_t0001.vtu`<br>


