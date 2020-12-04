# Topography data set

## Original Source

- SMB.nc : Surface mass balance from the regional climate model MARv3.9 produced for ISMIP6 [MARv3.9-ERA-Interim-1980-1999.nc](ftp://ftp.climato.be/fettweis/MARv3.9/ISMIP6/GrIS/ERA_1958-2017/MARv3.9-ERA-Interim-1980-1999.nc)

- dSMB.nc: Synthetic surface mass balance anomaly used for the initMIP-Greenland intercomparison exercice.
Original data set is available [here](https://zenodo.org/record/1173088#.X7eGZl5CegQ).

## Processing:

- For the pupose of the example we have extracted only a subset of the original data sets:
	```shell
	ncks -dx,-600000.0,85000.0,2 -dy,-1513000.0,-809000.0,2 "in.nc" "out.nc"
	```

- dSMB.nc:
	- We have added the *x* and *y* coordinates and deleted the *coordinates* attribute
	```shell
	ncap2 -O -s "x=array(-720000.0,1000.0,$x) -s y=array(-3450000.0,1000.0,$y)" "in.nc" "out.nc"
	ncatted -a coordinates,DSMB,d,, "in.nc" "out.nc"
	```
	

