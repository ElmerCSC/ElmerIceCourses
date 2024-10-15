#!/bin/bash
mkdir ElmerDependencies
cd ElmerDependencies
echo "#############################"
echo "#############################"
echo "#############################"
echo "get nn and csa sources"
echo "#############################"
echo "#############################"
echo "#############################"
git clone https://github.com/sakov/nn-c 
git clone https://github.com/sakov/csa-c
echo "#############################"
echo "#############################"
echo "#############################"
echo "compile nn and csa"
echo "#############################"
echo "#############################"
echo "#############################"
cd /home/ubuntu/ElmerDependencies/nn-c/nn
CFLAGS="-fPIC -O2" ./configure 
# manually compile trinagle.c with -fPIC (for some odd reason CFLAGS are ignored here)
mv makefile makefile.tmp
awk '/^CFLAGS_TRIANGLE/{gsub(/=/,"= -fPIC ")};{print}' makefile.tmp > makefile
rm -f makefile.tmp
make -j 2
sudo make install
cd /home/ubuntu/ElmerDependencies/csa-c/csa
CFLAGS="-fPIC -O2" ./configure 
make -j 2
sudo make install
echo "#############################"
echo "#############################"
echo "#############################"
echo "get mmg sources"
echo "#############################"
echo "#############################"
echo "#############################"
#wget https://github.com/MmgTools/mmg/archive/v5.3.10.tar.gz
#tar xvzf v5.3.10.tar.gz
cd /home/ubuntu/ElmerDependencies
git clone  https://github.com/MmgTools/mmg.git
cd mmg
git checkout develop
#git reset --hard 565d6849
echo "#############################"
echo "#############################"
echo "#############################"
echo "compile mmg"
echo "#############################"
echo "#############################"
echo "#############################"
mkdir build
cd build
cmake -D CMAKE_INSTALL_PREFIX="/usr/local" -D CMAKE_BUILD_TYPE=RelWithDebInfo -D BUILD_SHARED_LIBS:BOOL=TRUE -D MMG_INSTALL_PRIVATE_HEADERS=ON -D CMAKE_C_FLAGS="-fPIC  -g" -D CMAKE_CXX_FLAGS="-fPIC -std=c++11 -g"  ..
sudo make -j $(nproc) install
#cd build/
#cmake -S "/home/ubuntu/ElmerDependencies/mmg" -DBUILD_SHARED_LIBS:BOOL=TRUE -DLIBMMG3D_SHARED:BOOL=TRUE -DLIBMMG2D_SHARED:BOOL=TRUE -D CMAKE_C_FLAGS="-fPIC -O2" -D CMAKE_CXX_FLAGS="-fPIC -O2"
make
sudo make install
echo "#############################"
echo "#############################"
echo "#############################"
echo "get ParMMG sources"
echo "#############################"
echo "#############################"
echo "#############################"
cd /home/ubuntu/ElmerDependencies
git clone https://github.com/MmgTools/ParMmg.git
cd ParMmg
git checkout develop
echo "#############################"
echo "#############################"
echo "#############################"
echo "compile ParMMG"
echo "#############################"
echo "#############################"
echo "#############################"
mkdir build
cd build
cmake -D CMAKE_INSTALL_PREFIX="/usr/local" -D CMAKE_BUILD_TYPE=RelWithDebInfo -D BUILD_SHARED_LIBS:BOOL=TRUE -D DOWNLOAD_MMG=OFF  -D MMG_DIR="/usr/local"  ..
sudo make -j $(nproc) install
# cleaning up
cd /home/ubuntu/
echo "if you want to clean up, then remove:"
echo "rm -fr /home/ubuntu/ElmerDependencies"
echo "#############################"
echo "#############################"
echo "#############################"
echo "ALL DONE"
echo "#############################"
