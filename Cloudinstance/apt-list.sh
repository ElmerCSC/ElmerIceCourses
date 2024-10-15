#!/bin/bash
DEBIAN_FRONTEND=noninteractive sudo apt install -y --no-install-recommends build-essential git cmake gmsh paraview xterm gnuplot
DEBIAN_FRONTEND=noninteractive sudo apt install -y --no-install-recommends g++ gfortran make libopenblas-dev liblapack-dev libmumps-dev libparmetis-dev libhypre-dev libopenblas-dev libopenmpi-dev libparmetis-dev libnetcdf-dev libnetcdff-dev

