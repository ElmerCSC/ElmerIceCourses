# Upgrading Elmer on the Virtual Machine

Here are some installation instructions to upgrade the Elmer installation on the virtual machine and run all teh test cases.

1. To visualise the .nc files you can install ncview
	```shell
        sudo apt install ncview
	```

2. To use the python tools and create the mesh from the shapefile you the the python library **pyshp**. Gdal is also usefull to manipulate the shapefiles
	```shell
	sudo apt install python3-pip
	pip3 install pyshp
	sudo apt install gdal-bin
	```

3. To install the external libraries required by Elmer for the interpolation (Scattered2DDataInterpolator) and the mesh adptation (mmg), see the installation scipt **Install_NN_CSA_MMG.sh**

4. buiold elmer with support to these libraries; run the updated installation script **buildelmer.sh**
