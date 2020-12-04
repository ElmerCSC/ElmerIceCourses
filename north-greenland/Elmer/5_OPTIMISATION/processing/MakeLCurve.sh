#!/bin/bash
###################################################
# Description:
#   Loop through the results of the assimilation and extract last line
#   to plot the LCurve
# Usage:
#    ./MakeLCurve.sh
####################################################

rm -f tmp.dat

i=1
File=Cost_"$i"_.dat

while [ -f "$File" ]
do
  echo $File

  L=$(grep -i lambda CostReg_"$i"_.dat | awk -F, '{print $2}')

  echo $(tail -n 1 Cost_"$i"_.dat  | awk '{print $2}') $(tail -n 1 CostReg_"$i"_.dat | awk '{print $2}')  $(tail -n 1 Cost_"$i"_.dat  | awk '{print $3}') $L $i >> tmp.dat

  i=$((i+1))
  File=Cost_"$i"_.dat
done


echo '# J_0 J_Reg RMS Lambda RUN' > LCurve.dat
sort -g -k4 tmp.dat >> LCurve.dat

rm -f tmp.dat

