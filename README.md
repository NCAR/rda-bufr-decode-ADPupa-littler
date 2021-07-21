# rda-bufr-decode-ADPupa-littler

This project contains Fortran source code to read BUFR files containing NCEP ADP upper air observations 
that are archived in the [NCAR Research Data Archive (RDA) dataset ds351.0](https://doi.org/10.5065/39C5-Z211).
Output is written in ASCII format.

To compile, run the `install.sh` script under the `/install` directory to complete the compilations.
The most recent version of BUFRLIB is required to compile this code; software download and 
installation instructions are provided at:

https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php

The executables will be placed in the `/exe` directory.  Run
the desired executable and enter the BUFR input file name to extract
the basic meteorological varibles into obs format text format.  
```
exe/dumpbufr.x:           used to dump out all contents of BUFR files.
exe/bufr_upa2ob.x:        used to convert gdas.adpupa.tHHz.YYYYMMHH.bufr files to obs format.
exe/bufr_craft2ob.x:      used to convert gdas.aircft.tHHz.YYYYMMHH.bufr files to obs format.
exe/bufr_aircar2ob.x:     used to convert gdas.aircar.tHHz.YYYYMMHH.bufr files to obs format.
exe/bufr_sat2ob.x:        used to convert gdas.satwnd.tHHz.YYYYMMHH.bufr files to obs format.
exe/runob2lit_imd_obs.x:  used to convert/combine obs format files into one little_r format file.
exe/files.txt  edit to include desired obs files to be input into the runob2lit_imd_obs.x program.
               example files are currently listed in the files.txt file. 
```
To convert muliple files, place input BUFR data in `/bufrobs`,
then edit and run the `exe/convert.csh` script accordingly. **

# References
================================================================================

A guide to the BUFR libraries can be found at:

https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php

Definitions for BUFR MNEMONIC headers can be found in the doc directory 
and at: 
https://www.emc.ncep.noaa.gov/BUFRLIB/tables/bufrtab_tableb.html 		
