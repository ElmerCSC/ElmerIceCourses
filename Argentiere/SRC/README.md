
Scalar_OUTPUT_Glacier/F90 is in the ELmer/Ice distrib, but we use a slightly improved version that will be pushed soon<br>
Compute2DNodalGradient.F90 is in the Elmer/Ice distrib, but we will compile it locally in case mmg not working on your instal<br>
TransientMassBalance_MaskRelief.F90 is not yet in the Elmer/Ice distrib but should be committed soon 

To compile these source file, go in the bin directory and execute Compile.shi<br>
'elmerf90 -o MassBalance ../SRC/TransientMassBalance_MaskRelief.F90'<br>
'elmerf90 -o Scalar_Output ../SRC/Scalar_OUTPUT_Glacier.F90'<br>
'elmerf90 -o Compute2DNodalGradient ../SRC/Compute2DNodalGradient.F90'<br>
