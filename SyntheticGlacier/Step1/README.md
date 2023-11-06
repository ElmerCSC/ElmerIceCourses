In this initial step, DEMs and contour for the mesh are built using `Synthetical_Glacier_BedDEM.py` 
A geo gmsh mesh file is then created using `Contour2geo.py` in `elmerice/Meshers/GIS/`:<br>
 `python Contour2geo.py -r 100.0 -i SyntBed_Contour_bed1.dat -o Mesh2d.geo`<br>

You can change the resolution of the mesh directly by modifying the **lc** variable in Mesh2d.geo<br>

Then make the Elmer mesh:<br>
`gmsh -1 -2 Mesh2d.geo`<br>
`ElmerGrid 14 2 Mesh2d.msh -autoclean`<br> 

The mesh directory should then be copied into `Step2`.<br>

Then read the different DEMs needed for the simulation:<br>
- `initialise_DEM.sif`: first read the bedrock and surface DEMs and use them for initialisation of variable Zs in the following steps.<br>
- `Extrusion.sif`: defines mesh extrusion parameters; read by other sifs
