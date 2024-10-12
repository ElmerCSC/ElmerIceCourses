

  
#!/bin/bash
echo "cloning git repository (be patient, this make take a while)"
git clone https://github.com/ElmerCSC/elmerfem.git
cd elmerfem
echo "Adding Zoltan as git-submodule:"
git submodule sync 
git submodule update --init 
echo "starting to configure"
cd ..
rm -fr build/*
mkdir build
cd build
if  ! cmake -S /home/ubuntu/elmerfem -DCMAKE_INSTALL_PREFIX="/usr/local"\
      -DWITH_OpenMP:BOOLEAN=TRUE \
      -DWITH_MPI:BOOLEAN=TRUE \
      -DWITH_LUA:BOOL=TRUE \
      -DWITH_Mumps:BOOL=TRUE \
      -DWITH_Hypre:BOOL=TRUE \
      -DHypre_INCLUDE_DIR="/usr/include/hypre" \
      -DWITH_GridDataReader:BOOL=TRUE \
      -DWITH_Zoltan:BOOL=TRUE \
      -DWITH_ElmerIce:BOOL=TRUE \
      -DWITH_ScatteredDataInterpolator=BOOL=TRUE \
      -DCSA_LIBRARY="/usr/local/lib/libcsa.a" \
      -DCSA_INCLUDE_DIR="/usr/local/include" \
      -DNN_LIBRARY="/usr/local/lib/libnn.a" \
      -DNN_INCLUDE_DIR="/usr/local/include" \
      -DWITH_MMG:BOOL=TRUE \
      -DPARMMGROOT="/usr/local" \
      -DMMG_INCLUDE_DIR="/usr/local/include" \
      -DMMG_LIBRARY="/usr/local/lib/libmmg.so" \
      -DWITH_NETCDF:BOOL=TRUE \
      -DNETCDF_LIBRARY="/usr/lib/x86_64-linux-gnu/libnetcdf.so" \
      -DNETCDFF_LIBRARY="/usr/lib/x86_64-linux-gnu/libnetcdff.so" \
      -DNETCDF_INCLUDE_DIR="/usr/include" \
      -DWITH_ELMERGUI:BOOL=FALSE \
      -DWITH_QT5:BOOL=TRUE \
      -DQWT_INCLUDE_DIR="/usr/include/qwt" \
      -DQWT_LIBRARY="/usr/lib/libqwt-qt5.so" \
      -DWITH_OCC:BOOL=TRUE \
      -DWITH_PARAVIEW:BOOL=TRUE \
      -DWITH_QWT:BOOL=TRUE \
      -DWITH_VTK:BOOL=FALSE \
      -DWITH_MATC:BOOL=FALSE \
      -DWITH_PYTHONQT:BOOL=FALSE \
      -DMPIEXEC_MAX_NUMPROCS="2" \
      -DMPIEXEC_NUMPROC_FLAG="--oversubscribe -np" \
      -Wno-dev; then
    echo "Configuration failed"
    cd ..
    exit -1
fi
echo "Starting compilation - this may take a while (depending on the speed of your computer's CPU"
make -j 4
sudo make install
echo "done with compilation"
exit 0
