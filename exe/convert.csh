#!/bin/csh

# Script to process all BUFR tar files located in "bufrdecodelr/bufrobs"

# !! Edit procdir directory to reflect your local system !!

set procdir=`pwd`/..

cd $procdir/bufrobs

foreach file (gdasupaobs.*????.tar*)

# gdasupaobs.yyyymmdd.tar.gz
   if ("$file" =~ *.gz)  then
       tar -xvzf $file
   else
       tar -xvf $file
   endif
end

foreach dir (upaobs.*????)

 set date=`echo $dir | awk -F. '{print $2}'` 

  foreach hour (`ls -1 $dir | cut -d. -f3 | sort | uniq`)
      set hh=`echo $hour | sed 's|[A-Za-z]||g'`
    
      set datehh=$date$hh
    
      echo $datehh
      echo $hour
    
      echo "$procdir/exe/bufr_aircar2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircar.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_aircar2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircar.$hour.$date.bufr $datehh

      echo "$procdir/exe/bufr_aircft2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircft.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_aircft2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircft.$hour.$date.bufr $datehh 

      echo "$procdir/exe/bufr_adpupa2ob.x $procdir/bufrobs/upaobs.$date/gdas.adpupa.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_adpupa2ob.x $procdir/bufrobs/upaobs.$date/gdas.adpupa.$hour.$date.bufr $datehh 

      echo "$procdir/exe/bufr_satwnd2ob.x $procdir/bufrobs/upaobs.$date/gdas.satwnd.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_satwnd2ob.x $procdir/bufrobs/upaobs.$date/gdas.satwnd.$hour.$date.bufr $datehh 
    
      echo aircar$datehh.obs >files.txt
      echo aircft$datehh.obs >>files.txt
      echo satwnd$datehh.obs >>files.txt
      echo adpupa$datehh.obs >>files.txt
    
      echo "$procdir/exe/upperair_obs2littler.x files.txt $datehh"
      $procdir/exe/upperair_obs2littler.x files.txt $datehh
    
      rm aircar$datehh.obs 
      rm aircft$datehh.obs
      rm satwnd$datehh.obs 
      rm adpupa$datehh.obs
      rm files.txt
      mv *OBS* $procdir/lrobs
  end
   
end
