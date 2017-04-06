#!/bin/csh -f
#
# How deal with models from WIRE: fill in <fldr+tracf>
#
#
# JS: nz=601, nx=576, ny=351
#
# Seems grid is: IL 2200-4500(4), XL 3600-5000(4)
#
echo "**********************************"
echo "*** Convert binary files to SU ***"
echo "**********************************"

# Test options

if ($#argv == 0) then
    echo "No arguments"
    goto error
endif
if ( `where suaddhead` == "" ) goto suerror

if (! -e $1) then
    echo "No such file: $1"
    goto error
endif

# Set files

set filami = $1:t:r
echo "Binary file:" $1
set filut = ${filami}.su
echo "SU file:" $filut

# Set hdrs

set ils = 3600; set ile = 5000; set ilinc = 4
@ nil = ( 1 + ($ile - $ils) / $ilinc ) 
set xls = 2200; set xle = 4500; set xlinc = 4
@ nxl = ( 1 + ($xle - $xls) / $xlinc ) 

echo "IL Range : " $ils"-"$ile 
echo "XL Range : " $xls"-"$xle 

echo "Number of ILs : " $nil
echo "Number of XLs : " $nxl

echo "IL/XL headers located in fldr and tracf"

suaddhead ns=601 <$1 | sushw key=dt a=5000 | \
    sushw key=fldr,tracf a=$ils,$xls b=0,$ilinc c=$xlinc,0 j=$nxl,$nxl > $filut

segyclean <$filut | surange >${filami}.surange

# Testing transpose to depth slices
#transp3d <$1 n1=601 n2=$nxl n3=$nil perm=231 > $1.transp
# 
#suaddhead < $1.transp ns=$nil | sushw key=dt a=1000 | suximage wclip=1500 bclip=4000
# > $filame.z.su 


### Finish
done:
    exit 0
suerror:
    echo "SU not set"; exit 1
error:
    echo "Usage: models.csh filename"; exit 1
