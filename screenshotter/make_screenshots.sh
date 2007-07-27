#!/bin/sh

BKGD=`dirname $0`/bkgd.jpg

ERR=/tmp/screenshot.errors
rm -f $ERR

for tarball in $HOME/.sawfish/tarballs/*.tar.gz ; do
  echo "===================="
  echo "Tarball: $tarball"
  pushd $HOME/.sawfish/tarballs > /dev/null
  theme=`echo $tarball | perl -ne '/\/([^\/]*)(-[0-9\.]+)(-\d{8,14}).tar.gz/ && print "$1\n";'`
  echo "Theme: $theme"
  cd $HOME/.sawfish/themes
  rm -rf $HOME/.sawfish/themes/*
  tar xfz $tarball
  tardir=`ls`
  popd > /dev/null
  if [ x"$tardir" != x"$theme" ]; then
    echo "WARN: $theme != $tardir/" | tee -a $ERR
    if [ x"$tardir" != x ]; then
      theme=$tardir
      #make_screenshot.sh $theme $BKGD
      ## allow Xnest to really die
      #sleep 1
    else
      echo "ERROR: $tarball contains nothing?" | tee -a $ERR
      continue
    fi
  fi
  make_screenshot.sh $theme $BKGD
  # allow Xnest to really die
  sleep 1
done

