dnl  Process this file with autoconf to produce a configure script.
dnl  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
dnl  $Id: configure.in,v 1.140 2008/02/18 20:59:59 jkozicki Exp $
dnl
dnl  This file is part of sawfish.
dnl
dnl  sawfish is free software; you can redistribute it and/or modify it
dnl  under the terms of the GNU General Public License as published by
dnl  the Free Software Foundation; either version 2, or (at your option)
dnl  any later version.
dnl
dnl  sawfish is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU General Public License for more details.
dnl
dnl  You should have received a copy of the GNU General Public License
dnl  along with sawfish; see the file COPYING.  If not, write to
dnl  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

AC_REVISION($Revision: 1.141 $)
AC_INIT(src/sawfish.h)

AC_CONFIG_HEADER(config.h)
AC_CONFIG_MACRO_DIR([m4])

AM_PROG_LIBTOOL

dnl Release versioning info
version="1.13.0"
subversion=""
codename="Bolero"

VERSION=${version}

PACKAGE="sawfish"
GETTEXT_PACKAGE=${PACKAGE}

install_sh=../install-sh

SUBDIRS="src lisp scripts themes sounds man"

sawfishdir='${datadir}/sawfish'
lispdir='${sawfishdir}/lisp'
localedir='${datadir}/locale'
sawfishexecdir='${libdir}/sawfish'

dnl Several checks
AC_C_CONST
AC_C_INLINE
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(gethostname socket snprintf getpagesize)
AC_PROG_INSTALL
AC_CHECK_LIB(nsl, xdr_void)
AC_CHECK_LIB(socket, bind)
AC_HEADER_SYS_WAIT
AC_HEADER_TIME
AC_CHECK_HEADERS(fcntl.h sys/time.h sys/utsname.h)
PKG_PROG_PKG_CONFIG

dnl Check for X headers, etc
_cppflags="${CPPFLAGS}"
CPPFLAGS="${CPPFLAGS} ${X_CFLAGS}"

AC_ARG_WITH(xorg-x11r7,
  [  --with-xorg-x11r7	  Use X.Org X11R7
  --without-xorg-x11r7	  Use X.Org/XFree86 X11R6 instead], [], [with_xorg_x11r7=yes])

if test "$with_xorg_x11r7" != "no"; then

AC_CHECK_HEADERS(X11/extensions/Xdbe.h, [], [], [#include <X11/Xlib.h> ])

dnl Checks for X extensions/libraries
XINERAMA_LIBS=""
XRANDR_LIBS=""
XFT_LIBS=""
XFT_CFLAGS=""

AC_ARG_WITH([xinerama], [AS_HELP_STRING([--with-xinerama], [Include Xinerama support])], [], [with_xinerama=yes])

if test "$with_xinerama" != "no"; then

PKG_CHECK_MODULES(libXinerama, xinerama >= 1.0
		  ,XINERAMA_LIBS="-lXinerama"
		  AC_DEFINE(HAVE_X11_EXTENSIONS_XINERAMA_H, 1, [Have xinerama])
		  ,AC_MSG_ERROR([cannot locate libXinerama]))

fi

PKG_CHECK_MODULES(libXRandR, xrandr >= 1.0
		  ,XRANDR_LIBS="-lXrandr"
		  AC_DEFINE(HAVE_X11_EXTENSIONS_XRANDR_H, 1, [Have xrandr])
		  ,AC_MSG_ERROR([cannot locate libXRandR]))

PKG_CHECK_MODULES(libXft, xft >= 1.0
		  ,XFT_LIBS="-lXft"
		  XFT_CFLAGS="`$PKG_CONFIG --cflags xft`"
		  AC_DEFINE(HAVE_X11_XFT_XFT_H, 1, [Have xft])
		  ,AC_MSG_ERROR([cannot locate libXft]))

dnl is X available?
X_LIBS=""

PKG_CHECK_MODULES(libX11, x11 >= 1.0
		  ,AC_DEFINE(HAVE_X11, 1, [Have X11])
		  X_LIBS="`$PKG_CONFIG --libs x11`"
		  X_CFLAGS="`$PKG_CONFIG --cflags x11`"
		  AC_DEFINE(X_DISPLAY_MISSING, 0, [Have x11])
		  ,AC_MSG_ERROR([cannot locate libX11]))

X_LIBS="${X_LIBS} -lXext -lXrender"

X_EXTRA_LIBS=""

PKG_CHECK_MODULES(libICE, ice >= 1.0
		  ,X_EXTRA_LIBS="${X_EXTRA_LIBS} -lICE"
		  AC_DEFINE(HAVE_X11_ICE_ICE_H, 1, [Have libice])
		  ,AC_MSG_ERROR([cannot locate libICE]))

PKG_CHECK_MODULES(libSM, sm >= 1.0
		  ,X_EXTRA_LIBS="${X_EXTRA_LIBS} -lSM"
		  AC_DEFINE(HAVE_X11_SM_SMLIB_H, 1, [Have smlib])
		  ,AC_MSG_ERROR([cannot locate libSM]))

PKG_CHECK_MODULES(XTest, xtst >= 1.0
		  ,X_EXTRA_LIBS="${X_EXTRA_LIBS} -lXtst"
		  AC_DEFINE(HAVE_X11_EXTENSIONS_XTST_H, 1, [Have Xtest (libXtst)])
		  ,AC_MSG_ERROR([cannot locate Xtest (libXtst)]))

X_SERVER="X.Org X11R7"

CPPFLAGS="${_cppflags}"

else

AC_CHECK_HEADERS(X11/ICE/ICE.h X11/SM/SMlib.h X11/extensions/Xdbe.h X11/extensions/XTest.h, [], [], [#include <X11/Xlib.h> ])

dnl Checks for X extensions/libraries
XINERAMA_LIBS=""
XRANDR_LIBS=""
XFT_LIBS=""
XFT_CFLAGS=""

if test "$with_xinerama" != "no"; then

AC_CHECK_LIB(Xinerama, XineramaQueryScreens,
	     [XINERAMA_LIBS="-lXinerama"
	      AC_CHECK_HEADERS(X11/extensions/Xinerama.h)],
	     [],[$X_LIBS -lX11 -lXext])

fi

AC_CHECK_LIB(Xrandr, XRRUpdateConfiguration,
	     [XRANDR_LIBS="-lXrandr"
	      AC_CHECK_HEADERS(X11/extensions/Xrandr.h, [], [],
	         [#include <X11/Xlib.h> ])],
	     [],[$X_LIBS -lX11 -lXext])

dnl Try using $PKG_CONFIG first, since it looks like Xft2 may fail our
dnl hand-crafted tests
PKG_CHECK_MODULES(XFT, xft >= 1.0,
 [_cppflags=$CPPFLAGS
  CPPFLAGS="$CPPFLAGS $XFT_CFLAGS"
  AC_CHECK_HEADERS(X11/Xft/Xft.h)
  CPPFLAGS=$_cppflags],
 [AC_CHECK_LIB(Xft, XftDrawCreate,
	       [XFT_LIBS="-lXft -lXrender"
	       AC_CHECK_HEADERS(X11/Xft/Xft.h)],
	       [], [$X_LIBS -lX11 -lXext])])

dnl is X available?
if test "${no_x}" != "yes"; then
  AC_DEFINE(HAVE_X11, 1, [Have X11])
  X_LIBS="${X_LIBS} -lXext -lXrender"
  X_EXTRA_LIBS="-lSM -lICE -lXtst"
else
  AC_MSG_ERROR([You need X11 for sawfish])
fi

X_SERVER="X.Org/XFree86 X11R6"

CPPFLAGS="${_cppflags}"

fi

dnl Checks for pango

AC_ARG_WITH(pango,
  [  --with-pango		  Use Pango instead of Xft
  --without-pango], [], [with_pango=yes])

if test "$with_pango" = "yes"; then

PANGO_MIN_VER="1.8.0"

PANGO_MODULES="pango "

PKG_CHECK_MODULES(PANGO, pango >= ${PANGO_MIN_VER}
		  ,AC_DEFINE(HAVE_PANGO, 1, [Have pango]) FONT_LOADER="Pango"
		  ,AC_MSG_ERROR([cannot locate pango >= $PANGO_MIN_VER]))

PKG_CHECK_MODULES(PANGOXFT, pangoxft >= ${PANGO_MIN_VER}
		  ,AC_DEFINE(HAVE_PANGO_XFT, 1, [Have pangoxft]) PANGO_MODULES="${PANGO_MODULES} pangoxft" have_pango_xft="yes" FONT_LOADER="Pango XFT" AC_SUBST(have_pango_xft)
		  ,AC_MSG_WARN([pango xft support not available $PANGO_MIN_VER]))

PANGO_LIBS=`$PKG_CONFIG --libs $PANGO_MODULES`
PANGO_CFLAGS=`$PKG_CONFIG --cflags $PANGO_MODULES`

else FONT_LOADER="X.Org"
fi

dnl Check for librep
REP_MIN_VER="0.92.3"

AC_CHECK_PROG(REP_BIN, [rep], [yes])
if test "$REP_BIN" != "yes"; then
	echo "rep binary not found!"
	exit 1
fi

PKG_CHECK_MODULES(LIBREP, librep >= ${REP_MIN_VER}
		  ,REP_EXECDIR="`$PKG_CONFIG --variable=repcommonexecdir librep`"
		  REP_CFLAGS="`$PKG_CONFIG --cflags librep`"
		  REP_LIBS="`$PKG_CONFIG --libs librep`"
		  REP_MSGFMT="`which msgfmt`"
		  REP_VERSION="`$PKG_CONFIG --variable=version librep`"
		  ,AC_MSG_ERROR([cannot locate librep >= $REP_MIN_VER]))

dnl Check for GTK+2
GTK_MIN_VER="2.24.0"
GDK_MIN_VER="2.23.0"

PKG_CHECK_MODULES(GTK, gtk+-2.0 >= ${GTK_MIN_VER}
		  ,GTK_LIBS="`$PKG_CONFIG --libs gtk+-2.0`"
		  GTK_CFLAGS="`$PKG_CONFIG --cflags gtk+-2.0`"
		  AC_DEFINE(HAVE_GTK, 1, [Have GTK])
		  ,AC_MSG_ERROR([cannot locate gtk+ >= $GTK_MIN_VER]))

dnl Check for rep-gtk
REP_GTK_MIN_VER="0.90.7"

PKG_CHECK_MODULES(REP_GTK, rep-gtk >= ${REP_GTK_MIN_VER}
		  ,
		  ,AC_MSG_ERROR([cannot locate rep-gtk >= $REP_GTK_MIN_VER]))

dnl Check for image handling libraries
AC_ARG_WITH(gdk-pixbuf,
  [  --with-gdk-pixbuf	  Use gdk-pixbuf for image management
  --without-gdk-pixbuf	  Use Imlib1 instead], [], [with_gdk_pixbuf=yes])

if test "$with_gdk_pixbuf" != "no"; then
	PKG_CHECK_MODULES(GDK_PIXBUF, gdk-pixbuf-xlib-2.0 >= ${GDK_MIN_VER}
			  ,IMAGE_CFLAGS="`$PKG_CONFIG --cflags gdk-pixbuf-xlib-2.0`"
			  IMAGE_LIBS="`$PKG_CONFIG --libs gdk-pixbuf-xlib-2.0`"
			  IMAGE_PC="gdk-pixbuf-xlib-2.0 >= ${GDK_MIN_VER}"
			  AC_DEFINE(HAVE_GDK_PIXBUF, 1, [Have gdk-pixbuf])
			  AC_DEFINE(NEED_PIXMAP_CACHE, 1, [Need pixmap cache])
			  IMAGE_LOADER="GDK Pixbuf Xlib"
			  imageloader="gdk-pixbuf-xlib"
			  with_gdk_pixbuf=yes
			  ,AC_MSG_ERROR([cannot locate gdk-pixbuf-xlib-2.0 >= $GTK_MIN_VER])
			  with_gdk_pixbuf=no)
fi

if test "$with_gdk_pixbuf" = "no"; then
	PKG_CHECK_MODULES(IMLIB1, imlib >= 1.9
			  ,IMAGE_CFLAGS="-I`$PKG_CONFIG --variable includedir imlib`"
			  IMAGE_LIBS="`$PKG_CONFIG --libs imlib`"
			  IMAGE_PC="imlib >= 1.9"
			  AC_DEFINE(HAVE_IMLIB, 1, [Have Imlib1])
			  IMLIB_VERSION="`$PKG_CONFIG --modversion imlib`"
			  IMAGE_LOADER="IMLIB 1"
			  imageloader="imlib1"
			  ,AC_MSG_ERROR([cannot locate imlib1 library]))
fi

dnl Check wether to install mo files

AC_ARG_WITH(nls,
  [  --with-nls		  Install translations
  --without-nls], [], [with_nls=yes])

if test "$with_nls" = "yes"; then
	SUBDIRS="${SUBDIRS} po"
fi

dnl session files stuff

AC_ARG_WITH(kde4session,
  [  --with-kde4session 	install kde4 session files
  --without-kde4session], [], [with_kde4session=yes])

KDE4_SESSION="$with_kde4session"

if test "$with_kde4session" = "yes"; then
	KDED4IR="`kde4-config --install data`"
fi

dnl fallback
if test "x$KDE4DIR" = x; then
       KDE4DIR="${datadir}/kde4/apps/"
fi

AC_ARG_WITH(kde5session,
  [  --with-kde5session 	install kde5 session files
  --without-kde5session], [], [with_kde5session=yes])

KDE5_SESSION="$with_kde5session"

if test "$with_kde5session" = "yes"; then
	KDE5DIR="`kf5-config --install data`"
fi

dnl fallback
if test "x$KDE5DIR" = x; then
       KDE5DIR="${datadir}/"
fi

AC_ARG_WITH(luminasession,
  [  --with-luminasession 	install lumina session files
  --without-luminasession], [], [with_luminasession=yes])

LUMINA_SESSION="$with_luminasession"

AC_ARG_WITH(matesession,
  [  --with-matesession 	install mate session files
  --without-matesession], [], [with_matesession=yes])

MATE_SESSION="$with_matesession"

AC_ARG_WITH(xfcesession,
  [  --with-xfcesession 	install xfce session files
  --without-xfcesession], [], [with_xfcesession=yes])

XFCE_SESSION="$with_xfcesession"

AC_ARG_WITH(gnome2session,
  [  --with-gnome2session 	install gnome2 session files
  --without-gnome2session], [], [with_gnome2session=yes])

GNOME2_SESSION="$with_gnome2session"

dnl Check for GNOME location
AC_ARG_WITH(gnome_prefix,
 [  --with-gnome-prefix=DIR Root of GNOME installation], [],
 [ with_gnome_prefix="" ])
AC_MSG_CHECKING([for GNOME prefix])
if test "x$with_gnome_prefix" = "x"; then
  with_gnome_prefix='${prefix}'
fi
AC_MSG_RESULT([$with_gnome_prefix])
GNOMEDIR=$with_gnome_prefix

dnl How do we get dependency lines in the Makefile?
if test "x${GCC}" = "xyes"; then
  MAKEDEP='$(CC) -MM'
else
  case ${host} in
    *-dec-osf*)
      dnl works on Tru64
      MAKEDEP='$(CC) -M'
      ;;
    *-sun-solaris*)
      dnl works on Solaris
      MAKEDEP='/usr/ccs/lib/cpp -M'
      ;;
    *)
      dnl disable dependences?
      MAKEDEP='true'
      ;;
  esac
fi

AC_MSG_CHECKING(for languages to install)
AC_ARG_ENABLE(linguas,
  [  --enable-linguas=LANGS... Install language catalogs for language codes LANGS],
  [ LINGUAS="$enableval" ], [ LINGUAS="" ])
if test "x$LINGUAS" = x; then
  changequote({,})
  LINGUAS="`echo $srcdir/po/*.po | sed -e 's/[^ ]*\/po\/\([^ ]*\)\.po/\1/g'`"
  changequote([,])
fi
dnl too gross for words..
if test $REP_MSGFMT = true; then
  LINGUAS=""
fi
AC_MSG_RESULT($LINGUAS)

dnl Nonstandard exported symbols
AC_SUBST(version)
AC_SUBST(subversion)
AC_SUBST(sawfishdir)
AC_SUBST(sawfishexecdir)
AC_SUBST(lispdir)
AC_SUBST(localedir)
AC_SUBST(install_sh)
AC_SUBST(datarootdir)
AC_SUBST(imageloader)
AC_SUBST(MAKEDEP)
AC_SUBST(GNOMEDIR)
AC_SUBST(KDEDIR)
AC_SUBST(SUBDIRS)
AC_SUBST(HAVE_X11)
AC_SUBST(CPPFLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(LIBS)
AC_SUBST(X_CFLAGS)
AC_SUBST(X_LIBS)
AC_SUBST(X_EXTRA_LIBS)
AC_SUBST(XINERAMA_LIBS)
AC_SUBST(XRANDR_LIBS)
AC_SUBST(XFT_LIBS)
AC_SUBST(XFT_CFLAGS)
AC_SUBST(PANGO_MIN_VER)
AC_SUBST(PANGO_LIBS)
AC_SUBST(PANGO_CFLAGS)
AC_SUBST(REP_EXECDIR)
AC_SUBST(REP_CFLAGS)
AC_SUBST(REP_LIBS)
AC_SUBST(REP_VERSION)
AC_SUBST(REP_MSGFMT)
AC_SUBST(IMAGE_LIBS)
AC_SUBST(IMAGE_CFLAGS)
AC_SUBST(IMAGE_PC)
AC_SUBST(GTK_MIN_VER)
AC_SUBST(GTK_LIBS)
AC_SUBST(GTK_CFLAGS)
AC_SUBST(REP_MIN_VER)
AC_SUBST(REP_GTK_MIN_VER)
AC_SUBST(VERSION)
AC_SUBST(PACKAGE)
AC_SUBST(GETTEXT_PACKAGE)
AC_SUBST(LINGUAS)
AC_SUBST(KDE4_SESSION)
AC_SUBST(GNOME2_SESSION)
AC_SUBST(KDE5_SESSION)
AC_SUBST(LUMINA_SESSION)
AC_SUBST(MATE_SESSION)
AC_SUBST(XFCE_SESSION)
AC_SUBST(KDE4DIR)
AC_SUBST(KDE5DIR)
AC_SUBST(codename)

dnl Build all files
AC_CONFIG_FILES([
Makedefs
Makefile
data/sawfish.ebuild
data/sawfish.pc
data/sawfish.spec
lisp/Makefile
lisp/sawfish/cfg/Makefile
lisp/sawfish/gtk/Makefile
lisp/sawfish/gtk/widgets/font.jl
man/Makefile
po/Makefile
scripts/Makefile
sounds/Makefile
src/Makefile
themes/Makefile
])

AC_OUTPUT

mv data/sawfish.ebuild sawfish-$version.ebuild
mv data/sawfish.spec sawfish.spec

REP_VERSION="`$PKG_CONFIG --modversion librep`"
REP_GTK_VERSION="`$PKG_CONFIG --modversion rep-gtk`"
GLIB_VERSION="`$PKG_CONFIG --modversion glib-2.0`"
PANGO_VERSION="`$PKG_CONFIG --modversion pango`"

if $PKG_CONFIG --exists cairo; then
	CAIRO_VERSION="`$PKG_CONFIG --modversion cairo`"
fi

GTK_VERSION="`$PKG_CONFIG --modversion gtk+-2.0`"

echo "
  == == == == == == == == == == == == ==

  sawfish:	$version '$codename'

  == == == == == == == == == == == == ==

  prefix:	$prefix
  exec_prefix:	$exec_prefix
  libdir:	$libdir

  == == == == == == == == == == == == ==

  compiler:	$CC
  cflags:	$CFLAGS $DEVELOPMENT_CFLAGS
  preprocessor:	$CPP
  cppflags:	$CPPFLAGS
  ldflags:	$LDFLAGS
  makeflags:	$MAKEFLAGS

  == == == == == == == == == == == == ==

  rep:		$REP_VERSION
  rep-gtk:	$REP_GTK_VERSION
  glib:		$GLIB_VERSION"

if $PKG_CONFIG --exists cairo; then
	echo "  cairo:	$CAIRO_VERSION"
fi

echo "  pango:	$PANGO_VERSION
  gtk+:		$GTK_VERSION"

if test "$with_gdk_pixbuf" = "no"; then
echo "  imlib1:	$IMLIB_VERSION"
fi

echo "
  == == == == == == == == == == == == ==

  xserver:	$X_SERVER

  == == == == == == == == == == == == ==

  image loader:	$IMAGE_LOADER

  == == == == == == == == == == == == ==

  font loader:	$FONT_LOADER

  == == == == == == == == == == == == ==

  Be sure to read README.IMPORTANT!

  == == == == == == == == == == == == ==
  "

dnl If it doesn't look like GNU Make is being used, give a friendly warning
tem=`make --version -f /dev/null 2>&1 | grep GNU`
if test "x$tem" = "x"; then
  AC_MSG_WARN([You need to use GNU Make when compiling sawfish])
fi

dnl Local variables:
dnl major-mode: sh-mode
dnl End:
