In this step, grow Synth Glacier to a steady state:
- `steady_climat_Stokes.sif`: grows the glacier with Stokes until a steady state is reached

Then we perturb it:
- Modify the SMB parameters in PARAMETERS.IN (go up one level to find it).

To parallelise the mesh:
`ElmerGrid 2 2 Mesh2d -autoclean -partdual -metiskway 2`

BCs and linsys folders contain various code blocks (similar to `Extrusion.sif`) that can be included in the main SIFs to allow you to easily switch between different options.
