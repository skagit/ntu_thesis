In order to run the data you will need to have Julia language runtime, which is freely available from https://julialang.org/ on all major operating systems.

Before begining the simulation, you will have to create two directories in your copy of the repository:

tmp

csv

The tmp directory will collect results from individual replications, and at the end of all replications the data will be compiled into two data frames in the csv folder. One will contain the data of interest, and the other with the _ed extension the network structure data.

The code is designed to run on one or more processing cores, so if you have them available please start the julia runtime with the --procs=auto flag to get the quickest possible results.
