mkdir Progs
cd Progs/

echo "#############################
echo "#############################
echo "#############################
echo "get nn and csa sources"
echo "#############################
echo "#############################
echo "#############################

git clone https://github.com/sakov/nn-c 
git clone https://github.com/sakov/csa-c

echo "#############################
echo "#############################
echo "#############################
echo "get mmg sources"
echo "#############################
echo "#############################
echo "#############################
wget https://github.com/MmgTools/mmg/archive/v5.3.10.tar.gz
tar xvzf v5.3.10.tar.gz

echo "#############################
echo "#############################
echo "#############################
echo "compile nn and csa"
echo "#############################
echo "#############################
echo "#############################


cd /home/elmeruser/Progs/nn-c/nn
CFLAGS=-fPIC ./configure 
make
sudo make install

cd /home/elmeruser/Progs/csa-c/csa
CFLAGS=-fPIC ./configure 
make
sudo make install


echo "#############################
echo "#############################
echo "#############################
echo "compile mmg"
echo "#############################
echo "#############################
echo "#############################

cd /home/elmeruser/Progs/mmg-5.3.10
mkdir build
cd build/
cmake .. -D CMAKE_C_FLAGS=-fPIC -D CMAKE_CXX_FLAGS=-fPIC
make
sudo make install
