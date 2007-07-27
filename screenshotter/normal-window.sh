#!/bin/sh
p="$HOME/.sawfish/themes/$1/README"
if [ -e "$p" ]; then
  echo "Theme: '$1'"
  cat "$p" | fmt -w 51 | head -16
else
  cowsay Theme:"'$1'".  This window has focus.
fi
