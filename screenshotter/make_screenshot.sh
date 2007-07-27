#!/bin/sh

THEME=$1
BKGD=$2

XSERV=Xnest
DISP=:4

#export DISPLAY=:0
$XSERV -ac $DISP -geometry 600x450+5+5 &
export DISPLAY=$DISP

display -window root -size 256x256 xc:grey50
#xsetroot -solid grey

prep_config.sh $THEME
sawfish &
sleep 1
bgimg=`ls $HOME/.sawfish/themes/$THEME/*.jpg | head -1`
if [ x"$bgimg" = "x" ]; then
  feh --bg-scale $BKGD
else
  feh --bg-scale "$bgimg"
fi
killall pager
apps.sh $THEME
import -window root -quality 85 $THEME.jpg
sleep 1
killall $XSERV
