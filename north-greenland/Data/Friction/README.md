# Friction data set

## Original Source

Friction.nc : This netcdf has been created from an Elmer/Ice greenland simulation where the basal friction coefficient has been optimised to fit the observed velocity (MEaSUREs Multi-year Greenland Ice Sheet Velocity Mosaic, Version 1). See [here](http://elmerfem.org/elmerice/wiki/doku.php?id=eis:greenland)

## Processing:

- For the pupose of the example we have extracted only a subset of the original data set:
	```shell
	ncks -dx,-600000.0,85000.0,2 -dy,-1513000.0,-809000.0,2 "in.nc" "out.nc"
	```



