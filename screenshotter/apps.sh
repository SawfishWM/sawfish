#!/bin/bash

XTERM="xterm -fg white -bg black"
#XTERM="rxvt -fg white -bg black"

$XTERM -geom 34x10-10-10 -T Top-border \
   -e /bin/bash -c "cowsay \"A top border window\" ; sleep 200" &

#$XTERM -geom 28x9-10+150 -T Title-only \
#   -e /bin/bash -c "cowsay \"A title only window\" ; sleep 200" &

xeyes -geom 180x90-10+10 &

xclock -render -geom 130x100-10+130 &
#sleep 0.1

$XTERM -geom 47x6+10-10 -T Border-only \
   -e /bin/bash -c "echo \"A border only window\" ; sleep 200" &
sleep 0.4

$XTERM -geom 44x7+75+295 -T Shaded \
   -e /bin/bash -c "echo \"Eeeek! A Shaded window!\" ; sleep 200" &
sleep 0.6

$XTERM -geom 52x18+10+10 -T "Normal Window with Really Really Really Long Title" \
   -e /bin/bash -c "normal-window.sh "$1" ; sleep 200" &
   #"cowsay Theme \'$1\' README is below. \
   #This window has focus. ; sleep 200" &

sleep 1
