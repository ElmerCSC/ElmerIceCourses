# Velocity data set

## Original Source

This netcdf has been created from the [MEaSUREs Multi-year Greenland Ice Sheet Velocity Mosaic, Version 1 data set](https://nsidc.org/data/NSIDC-0670/versions/1)
	
## Processing:

- The orignal .tiff files have been converted to netcdf
- For the pupose of the example we have extracted only a subset of the original data set:
	```shell
	ncks -dx,-600000.0,85000.0,2 -dy,-1513000.0,-809000.0,2 greenland_vel_mosaic250_v1.nc velocity.nc
	```

