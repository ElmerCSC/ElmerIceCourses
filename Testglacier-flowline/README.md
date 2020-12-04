# Testglacier flowline
This is a simple synthetic setup of a Stokes problem for 2D (flowline) glacier. It consists of two major parts
* *diagnostic* (=steady state) problem
* *prognostic* (=transient) problem 
The example is thought to be a starting point for any Elmer/Ice novice.

## Learning Objectives
These examples are a good entry point for starting Elmer/Ice simulations, as they in an incremental manner include all necessary aspects to conduct a real-world simulation experiment. At the end of the *diagnostic* part, the reader should be familiar with:
* reading **digital elevetion models** in 2D using array functions
* introducing **sliding**
* introducing **thermo-mechanical** coupling
* **constraining** for the upper limit of temperature given by the **pressure-melting point**
* understand the principle of the *Solver Input File* (SIF)
* using **MATC** to introduce variable dependent properties in the SIF

At the end of the *prognostic* part, the reader should be able to:
* run a transient flow-line model including the deformation and **evolution of the free surface**
* couple the glacier to a given** surface mass balance**
* use **LUA** for variable dependent properties in the SIF
* use **User Defined Functions** (UDF) for variable dependent properties in the SIF

## Contents
This folder contains the following sub-folders:
* **Slides**: contains the instruction slides (currently only PDF)
* **SolutionFiles**: contains the SIF's for the solution of the tasks given in the slides
* **testglacier**: contains the mesh, should - for what reason ever - the meshing process from the Gmsh input mesh not work out

## Getting started
The first diagnostic run is launched by
```bash
$ ElmerSolver Stokes_diagnostic.sif
```
