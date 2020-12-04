# Topography data set

## Original Source

Topography.nc : This netcdf has been created from the [IceBridge BedMachine Greenland, Version 3](https://nsidc.org/data/idbmg4)

## Processing:

- For the pupose of the example we have extracted only a subset of the original data set:
	```shell
	ncks -dx,-600000.0,85000.0,2 -dy,-1513000.0,-809000.0,2 "in.nc" "out.nc"
	```

- Thickness has been set to a **no value** (using _FillValue attribute) for the ocan and non Greenland land:
	```shell
	ncap2 -s "where(mask<0.5 || mask>3.5) thickness=-9999" "in.nc" "out.nc"
	ncatted -O -a _FillValue,thickness,o,f,-9999 "out.nc"
	```

- Thickness has been converted from *short* to *float*:
	```shell
	ncap2 -s "thickness=thickness.convert(NC_FLOAT)" "in.nc" "out.nc"
	```

