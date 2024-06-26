# rda-bufr-decode-ADPupa-littler

This project contains Fortran source code to read BUFR files containing NCEP ADP upper air observations,
convert the observations to ASCII format, and then convert the ASCII data to little_r format. The 
little_r formatted files then can be used as input to the [WRF model](https://www2.mmm.ucar.edu/wrf/users/).  
BUFR observation files are archived and available for download in the 
[NCAR Research Data Archive (RDA) dataset ds351.0](https://doi.org/10.5065/39C5-Z211).

To compile, run the `install.sh` script under the `/install` directory to complete the compilations.
The most recent version of BUFRLIB is required to compile this code; software download and 
installation instructions are provided at:

https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php

The executables will be placed in the `/exe` directory.  Run
the desired executable and enter the BUFR input file name to extract
the basic meteorological varibles into obs format text format.  
```
exe/bufr_adpupa2ob.x:        used to convert gdas.adpupa.tHHz.YYYYMMHH.bufr files to obs format.
exe/bufr_aircft2ob.x:        used to convert gdas.aircft.tHHz.YYYYMMHH.bufr files to obs format.
exe/bufr_aircar2ob.x:        used to convert gdas.aircar.tHHz.YYYYMMHH.bufr files to obs format.
exe/bufr_satwnd2ob.x:        used to convert gdas.satwnd.tHHz.YYYYMMHH.bufr files to obs format.
exe/upperair_obs2littler.x:  used to convert/combine obs format files into one little_r format file.
exe/files.txt  edit to include desired obs files to be input into the upperair_obs2littler.x program.
               example files are currently listed in the files.txt file. 
```
To convert muliple files, place input BUFR data in `/bufrobs`,
then edit and run the `exe/convert.csh` script accordingly.

# References
A guide to the NCEP BUFR libraries can be found at:
https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php

Definitions for BUFR MNEMONIC headers can be found in the doc directory 
and at: 
https://www.emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib/tables/bufrtab_tableb.html

WRF documentation on the LITTLE_R format:
https://www2.mmm.ucar.edu/wrf/users/wrfda/OnlineTutorial/Help/littler.html
