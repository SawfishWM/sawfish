#!/bin/sh

if [ -f Makefile ]; then
	make gitclean
fi

if [ -f aclocal.m4 ]; then
	rm -fv aclocal.m4
fi

if [ -d m4 ]; then
	rm -fv m4/*
fi

if [ -f configure.in ]; then
  if grep "AC_CONFIG_HEADER" configure.in >/dev/null; then
      echo "Running autoheader"
      autoheader || exit 1
  fi
  if grep "AM_PROG_LIBTOOL" configure.in >/dev/null; then
    echo "Running libtoolize"
    lver=$(libtool --version | grep 1.5)
    if [ "x${lver}" != "x" ]; then
	    libtoolize --force --copy || exit 1
    else    libtoolize --force --copy --install || exit 1
    fi
  fi
#  echo "Running intltoolize"
#  intltoolize --copy --force
  echo "Running aclocal $ACLOCAL_FLAGS"
  aclocal -I m4 $ACLOCAL_FLAGS || exit 1

  echo "Running autoconf $AUTOCONF_FLAGS"
  autoconf $AUTOCONF_FLAGS || exit 1
fi

if [ "${1}" != "--nocfg" ]; then
	./configure "$@"
fi
