#!/bin/csh -f
#
# Plot models
#
#
# JS: nz=601, nx=576, ny=351
#
set NAME="`basename $0`"
echo "******************************************"
echo "*** Plot Vpz models for selected IL/XL ***"
echo "******************************************"
if ($#argv == 0) then
    echo $NAME": No arguments"
    goto error
endif
if ($#argv < 3) then
    echo $NAME": Incorrect arguments"
    goto error
endif
if ( `where suaddhead` == "" ) goto suerror

echo DEBUG number arguments $#argv

# Set some sort definitions
set sortselect = $1
set hdrrange = $2

switch($sortselect)
    case [iI][lL]:
	set sortoption = "fldr"; set suhdrmath = "tracf"
	set plotlabel = XL; set	titleref = IL
	breaksw
    case [xX][lL]:
	set sortoption = "tracf"; set suhdrmath = "fldr"
	set plotlabel = IL; set	titleref = XL
	breaksw
    case [nN][zZ]:
	set sortoption = "depth"
	set plotlabelx = IL; set plotlabely = XL
	echo "DEBUG: Depth slice in development"
	breaksw
    default:
        echo Incorrect setting sort
	goto error
endsw
echo Sort option set to: $sortoption

set i = 3
while ( $i <= $#argv )
    # echo " i =" $i $argv[$i]
    if ( ! -e $argv[$i] ) then
	echo "No Vpz file:" $argv[$i]
	goto error
    else
	echo "Found file:" $argv[$i]
    endif
    @ i++
end

# Set SUplot options
@ refplotx1=100
@ refploty1=100 

#####
#
# Plot IL/XL
#
#####
if ($sortoption == "fldr" || $sortoption == "tracf") then

    set suplotbox = "wbox=600 hbox=400"
    set i = 3
    while ( $i <= $#argv )

    set plottitle = "${argv[$i]}:${titleref}=${hdrrange}"

    suwind key=$sortoption min=$hdrrange max=$hdrrange < $argv[$i] | \
    sushw key=d2 a=4 | suchw key1=f2 key2=$suhdrmath | \
    suximage wclip=1500 bclip=4000 cmap=hsv2 legend=1 units="m/s" \
    title=$plottitle  d2num=250 \
    label1="Depth" label2=$plotlabel \
    labelcolor=black xbox=$refplotx1 ybox=$refploty1 $suplotbox &

    @ refplotx1 = ${refplotx1} + 20
    @ refploty1 = ${refploty1} + 20
    
    @ i++
    end

endif
#####
# 
# Plot depth slices
# 
#####
if ($sortoption == "depth") then
    set suplotbox = "wbox=600 hbox=400"
    set i = 3
    while ( $i <= $#argv )
    
    set plottitle = "${argv[$i]}:${sortoption}=${hdrrange}"

    echo "TITLE= "$plottitle

    suwind itmin=${hdrrange} itmax=${hdrrange} < $argv[$i]  | sumax output=segy | sustrip > /tmp/areo_bin
    ximage n1=576 wclip=1500 bclip=2250 legend=1 < /tmp/areo_bin \
    f1=2200 d1=4 d1num=250 f2=3600 d2=4 d2num=250\
    title=$plottitle \
    labelcolor=black xbox=$refplotx1 ybox=$refploty1 $suplotbox \
    style=normal label1="XL" label2="IL" &
    rm -f /tmp/areo_bin

    @ refplotx1 = ${refplotx1} + 20
    @ refploty1 = ${refploty1} + 20
    
    @ i++
    end
    
endif

###
done:
    exit 0
suerror:
    echo "SU not set"; exit 1
error:
    echo "Usage: " $NAME" [il/xl/nz] #num vpzfile1 .... Using su headers fldr and tracf as keys"
    exit 1
