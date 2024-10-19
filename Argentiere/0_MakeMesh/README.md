Read a contour of the area around the Argentiere glacier and build the `geo`input file that will be used to make the mesh
`python Contour2geo.py -r 150.0 -i ../Data/contour_2014_ARG.dat -o ARG_mesh.geo`<br>

You can change the resolution of the mesh directly by modifying the **lc** variable in ARG_mesh.geo

Then make the Elmer mesh:<br>
`gmsh -1 -2 ARG_mesh.geo -o ARG_mesh.msh`<br>
`ElmerGrid 14 2 ARG_mesh.msh -autoclean`<br> 

If going parallel:<br>
`ElmerGrid 14 2 ARG_mesh.msh -autoclean -metis 4 0`<br> 

Create a vtu file to visualise the mesh:<br>
`ElmerGrid 14 5 ARG_mesh.msh -autoclean -metis 4 0`<br>
`ElmerGrid 14 5 ARG_mesh.msh -autoclean`<br>

At the end, the directory ARG_mesh should be copied in all the directories:<br>
'cp -r ARG_mesh ../1_IMPORT_DEM/'<br>
'cp -r ARG_mesh ../2_ICE_AGE/'<br>
'cp -r ARG_mesh ../3_TRANSIENT/'<br>



