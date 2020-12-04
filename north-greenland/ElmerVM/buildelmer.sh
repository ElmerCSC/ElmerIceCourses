#!/bin/bash
rm -fr build/*
cd build
if  ! cmake ../elmerfem -DCMAKE_INSTALL_PREFIX="/opt/elmer"\
      -DWITH_OpenMP:BOOLEAN=TRUE \
      -DWITH_MPI:BOOLEAN=TRUE \
      -DWITH_LUA:BOOL=TRUE \
      -DWITH_Mumps:BOOL=TRUE \
      -DWITH_Hypre:BOOL=TRUE \
      -DHypre_INCLUDE_DIR="/usr/include/hypre" \
      -DWITH_GridDataReader:BOOL=TRUE \
      -DWITH_Zoltan:BOOL=TRUE \
      -DWITH_ElmerIce:BOOL=TRUE \
      -DWITH_ELMERGUI:BOOL=TRUE \
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
      -DWITH_ScatteredDataInterpolator=BOOL=TRUE \
      -DCSA_LIBRARY=/usr/local/lib/libcsa.a \
      -DCSA_INCLUDE_DIR=/usr/local/include \
      -DNN_LIBRARY=/usr/local/lib/libnn.a \
      -DNN_INCLUDE_DIR=/usr/local/include \
      -Wno-dev; then
    echo "Configuration failed"
    cd ..
    exit -1
fi
echo "Starting compilation - this may take a while"
make -j 2 install
echo "done with compilation"
exit 0
  
