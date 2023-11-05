## 2\_STEADY\_T0

### Introduction
Run a transient simulation with a constant climate (look at a steady solution)
Example build for the 2023 Elmer/Ice Beginner course held in Helsinki 6&7 November 2023
Input from Adrien Gilbert, Fabien Gillet-Chaulet, Leo Clauzel and Olivier Gagliardini

The material furnished is:
*Data*: DEM_bedrock_ArgentiereLarge.dat, DEM_surface_Argentiere1998.dat, contour_2014_ARG.dat, contour_Argentiere_large.dat, safran_daily_tas.dat, safran_daily_pr.dat, MaskRelief.dat , MaskAccu.dat<br> 
*SRC*: Compute2DNodalGradient.F90, Scalar_OUTPUT_Glacier.F90, TransientMassBalance_MaskRelief.F90, README.md<br>
*bin*: Compile.sh<br>
*0_MakeMesh*: README.md, Makegeo.py<br>
*1_IMPORT_DEM*: README.md, initialise_DEM.sif<br>
*2_ICE_AGE*: README.md, age_of_ice.sif<br>
*3_TRANSIENT*: README.md, transient.sif, GlacierOut (directory)<br>


Related papers from which this example is inpired:<br> 
Gilbert A., F. Gimbert, K. Thøgersen, T. V. Schuler and A. Kääb, 2022. A Consistent Framework for Coupling Basal Friction With Subglacial Hydrology on Hard-Bedded Glaciers, Geophysical Research Letters, 49, e2021GL097507<br>
Vincent C., A. Gilbert, A. Walpersdorf, F. Gimbert, O. Gagliardin, B. Jourdain, J.P. Roldan Blasco, O. Laarman, L. Piard, D. Six, L. Moreau, D. Cusicanqui and E. Thibert, 2022. Evidence of seasonal uplift in the Argentière glacier (Mont Blanc area, France). Journal of Geophysical Research: Earth Surface, 127, e2021JF006454<br>
Clauzel L., M. Ménégoz, A. Gilbert, O. Gagliardini, D. Six, G. Gastineau and C. Vincent, 2023. Sensitivity of glaciers in the European Alps to anthropogenic atmospheric forcings: Case study of the Argentière Glacier. Geophysical Research Letters, 50, e2022GL100363<br>
Gilbert A., O. Gagliardini, C. Vincent and F. Gimbert, 2023. Inferring the Basal Friction Law from long term changes of Glacier Length, Thickness and Velocity on an Alpine Glacier, Geophysical Research Letters 50, e2023GL104503

