########################################################################
#
# Filname : .cshrc
#
# Private .cshrc 
#
# This file should be copied to the users HOME directory 
#
########################################################################
#
set ext=cshrc
set LoginPath=/adm/login
#
# Set personal configuration here.
# Read the file /adm/login/VarHelp.cshrc for information.
#========================================================
# 
#
# DO NOT CHANGE THIS LINE
source $LoginPath/DEFAULT.$ext
#
# Personal variables after this line :
#========================================================
#
# set FixedPrompt="%~%#"
# set prompt="${FixedPrompt}"
# set prompt="%~>"
if ($?prompt) then # Works with tcsh
    # set prompt="(%m)%~>" # dont print home
    # set prompt="(%m)%/>" # include home
    set prompt="%m:%~>"
endif
set history=2000 
set savehist=2000
#
#---------------------------------------------------------
# ProMAX setup
#---------------------------------------------------------
#setenv machtype solaris
#setenv new_menu t
#setenv PROMAX_OLD_HOME_2 /prog/promax/v7.2_irix6.5
#setenv PROMAX_HOME   /prog/promax/v1998.6_I64
#setenv PROMAX_NEW_HOME       /prog/promax/v2003_irix
#setenv PROMAX_ETC_CONFIG_FILE_HOME $PROMAX_HOME/etc/config_file
#setenv XFILESEARCHPATH   "$PROMAX_HOME/port/lib/X11/%T/%N%S:."
#setenv PROMAX_ETC_CONFIG_FILE_HOME $PROMAX_HOME/etc/config_file
#if ( $ARCH == i686 ) then
#    newgrp psdm
#endif
# 
if ( $?prompt == 0 || $?VUE != 0 ) exit
#
# Added by Stian Vestre to run Geodepth in Trondheim; 11.09.2006
#   set path = ($path)
#
# Personal aliases after this line :
#========================================================
#
if (-f ~/.alias) source ~/.alias
unalias cd
umask 2
# xrdb -load $HOME/.Xdefaults
# xrdb -merge $HOME/.Xresources
#
# NO CHANGES AFTER THIS LINE
#
source $LoginPath/END.$ext
