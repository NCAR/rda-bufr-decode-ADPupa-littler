#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make executables to extract data from ADP BUFR
#  input files, write the data into a basic text file, and convert
#  the text files into LITTLE_R format.  It is used to
#  extract data from these kinds of files:
#      gdas.adpupa.tHHz.YYYYMMDD.bufr 
#      gdas.aircft.tHHz.YYYYMMDD.bufr
#      gdas.satwnd.tHHz.YYYYMMDD.bufr 
#      gdas.aircar.tHHz.YYYYMMDD.bufr
#
#  bufr_adpupa2ob.x: decodes ADPUPA observations and writes to a text file
#  bufr_aircar2ob.x: decodes AIRCAR observations and writes to a text file
#  bufr_aircft2ob.x: decodes AIRCFT observations and writes to a text file
#  bufr_satwnd2ob.x: decodes SATWND observations and writes to a text file
#  upperair_obs2littler.x: reads observation text files and converts to little_r format
#
#  Note: this script assumes this will be run and compiled on a linux 
#        system with the Gnu Fortran compiler
#  ------------------------------------------------------------------------
 
set -eua
 
SRC=../src
LIB=/path/to/libbufr_4.a
INCL=/path/to/bufrlib/include/directory
EXE=../exe
INSTALL=.
 
export FC=gfortran
export CC=gcc
fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
cflag=" -O3 -DUNDERSCORE -w"

#  Compile the decode programs
#  --------------------------------------- 
echo "Compiling bufr decoding programs..."
$FC $fflag -c $SRC/bufr_adpupa2ob.f -I $INCL
$FC $fflag -c $SRC/bufr_aircar2ob.f -I $INCL
$FC $fflag -c $SRC/bufr_aircft2ob.f -I $INCL
$FC $fflag -c $SRC/bufr_satwnd2ob.f -I $INCL
$FC $fflag -c $SRC/upperair_obs2littler.f
 
#  link and load the executables
#  -----------------------------
echo "Linking..."
$FC $fflag -o $EXE/bufr_adpupa2ob.x bufr_adpupa2ob.o $LIB -I $INCL
$FC $fflag -o $EXE/bufr_aircar2ob.x bufr_aircar2ob.o $LIB -I $INCL
$FC $fflag -o $EXE/bufr_aircft2ob.x bufr_aircft2ob.o $LIB -I $INCL
$FC $fflag -o $EXE/bufr_satwnd2ob.x bufr_satwnd2ob.o $LIB -I $INCL
$FC $fflag -o $EXE/upperair_obs2littler.x upperair_obs2littler.o

#  clean up
#  --------
rm -f *.o *.mod
echo "Finished."
