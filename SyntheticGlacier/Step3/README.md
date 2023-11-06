In this step, we do some more complex simulations to show some of the basic Elmer/Ice functionality and how different factors affect the modelled glacier:
- Slip.sif: basal sliding is turned on
- SMB.sif: we change the SMB (parameters in Physical_Parameters_New.IN)
- TempDiagnostic.sif: we solve for the steady-state temperature field in the ice
- All.sif: we include all of the above changes at once (restarting from the TempDiagnostic solution)

If runs are going to be done in parallel, you need to make a parallel mesh using:
`ElmerGrid 2 2 Mesh2d -autoclean -partdual -metiskway 2`
(This is already done for you, but just so you've got the command!)
