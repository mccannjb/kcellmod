The kcellmod program and assisting scripts were written by James McCann (mccannjb (at) gmail (dot) com).
These scripts require a number of user input values and at least two exisiting files.

Use the kcellPrep.py script to take an existing CSV file of substation locations and match those locations to
locations in an existing CAMx point source emissions input file. kcellPrep will generate multiple outputs so
that the user may group points at a later time.

The kcellmod fortran program will take the outputs from kcellPrep and create a modified CAMx point source
emissions file. This modified file will have the identified point sources assigned a KCELL value for DDM
tracking purposes as well as reduced emissions.


