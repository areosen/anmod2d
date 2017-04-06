#!/bin/csh -f
#
# Plot models
#
#
# JS: nz=601, nx=576, ny=351
#
echo "******************************************"
echo "*** Plot VTI models for selected IL/XL ***"
echo "******************************************"
if ($#argv == 0) then
    echo "No arguments"
    goto error
endif
if ( `where suaddhead` == "" ) goto suerror

echo DEBUG $#argv
#
if (! -e $1) then
    echo "No Vpz file: $1"
    goto error
endif
set ploteps = 1; set plotdel = 1
if ($#argv == 2) then
    if (! -e $2) then
	echo "No Eps file: $2"
	goto error
    endif
    set ploteps = 0
endif

if ($#argv == 3) then
    if (! -e $2) then
	echo "No Eps file: $2"
	goto error
    endif
    if (! -e $3) then
	echo "No Del file: $3"
	goto error
    endif
    set ploteps = 0; set plotdel = 0
endif

# Report input files provided

echo "Model files:" Vpz=$1 Eps=$2 Del=$3
 
# Ask for what to plot

echo -n "** Select IL or XL : "
set sortselect = $<
echo "You selected: " $sortselect

echo -n "** Input header value : "
set hdrrange = $<
echo Plot for il/xl number: $hdrrange

# Set some sort definitions

switch($sortselect)
    case [iI][lL]:
	set sortoption = "fldr"; set suhdrmath = "tracf"
	set plotlabel = XL; set	titleref = IL
	breaksw
    case [xX][lL]:
	set sortoption = "tracf"; set suhdrmath = "fldr"
	set plotlabel = IL; set	titleref = XL
	breaksw
    default:
        echo Incorrect setting sort
	goto error
endsw
echo Sort option set to : $sortoption

#

set suplotbox = "wbox=600 hbox=400"
set refplotx1 = 100; 
set refplotx2 = 710; 
set refplotx3 = 1320; 
# Vpz
suwind key=$sortoption min=$hdrrange max=$hdrrange < $1 | \
    sushw key=d2 a=4 | suchw key1=f2 key2=$suhdrmath | \
    suximage wclip=1500 bclip=4000 cmap=hsv2 legend=1 units="m/s" \
    title="Vpz - $titleref $hdrrange"  \
    label1="Depth" label2=$plotlabel \
    labelcolor=black xbox=$refplotx1 $suplotbox &
# Eps
if ($ploteps != 1) then
    suwind key=$sortoption min=$hdrrange max=$hdrrange < $2 | \
    sushw key=d2 a=1 | suchw key1=f2 key2=$suhdrmath | \
    sumath a=100 op=mult |\
    suximage wclip=0 bclip=20 cmap=hsv2 legend=1 units="%%" \
    title="Eps - $titleref $hdrrange"  \
    label1="Depth" label2=$plotlabel \
    labelcolor=black xbox=$refplotx2 $suplotbox &
endif
    # Del
if ($plotdel != 1) then
    suwind key=$sortoption min=$hdrrange max=$hdrrange < $3 | \
    sushw key=d2 a=1 | suchw key1=f2 key2=$suhdrmath | \
    sumath a=100 op=mult |\
    suximage wclip=0 bclip=20 cmap=hsv2 legend=1 units="%%" \
    title="Del - $titleref $hdrrange"  \
    label1="Depth" label2=$plotlabel \
    labelcolor=black xbox=$refplotx3 $suplotbox &
endif
###
done:
    exit 0
suerror:
    echo "SU not set"; exit 1
error:
    echo "Usage: plot3vti.csh vpzfile epsfile delfile"
    exit 1
