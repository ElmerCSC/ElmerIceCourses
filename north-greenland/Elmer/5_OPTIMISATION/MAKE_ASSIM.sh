#!/bin/bash
###################################################
# Description:
#   Run the assimilation for several regularisation parameters (lambda)
# Usage:
#   . ./MAKE_ASSIM.sh
####################################################

# list of regularisation parameters
lambda=(1.0e1 1.0e3 1.0e4 5.0e4 1.0e5 5.0e5 1.0e6)

# number of lambda values
n=${#lambda[*]}

# loop through all values
for ((i=1; i<=$n; i++ ))
do

  ll=${lambda[$((i-1))]}
  echo $i $ll

 sed  "s/<ID>/$i/g;s/<LAMBDA>/$ll/g" OPTIM_BETA.TEMP.sif > OPTIM_BETA_"$i".sif

 ElmerSolver OPTIM_BETA_"$i".sif

done
