
%define nam	sawmill
%define ver	0.6
%define rel	1

Summary: a highly configurable and extensible X11 window manager
Name: %{nam}
Version: %{ver}
Release: %{rel}
Requires: librep >= 0.5, rep-gtk >= 0.4
Copyright: GPL
Group: User Interface/Desktops
Source: ftp.dcs.warwick.ac.uk:/people/John.Harper/sawmill/sawmill-%{ver}.tar.gz
URL: http://www.dcs.warwick.ac.uk/~john/sw/sawmill/
Packager: John Harper <john@dcs.warwick.ac.uk>

%description
This is an extensible window manager using an Emacs Lisp-like scripting
language--all window decorations are configurable, the basic idea is to
have as much user-interface policy as possible controlled through the
Lisp language.

All configuration may be performed through a GTK interface; sawmill is
mostly-GNOME compliant.

%package gnome
Summary: GNOME support for sawmill
Group: User Interface/Desktops
Requires: %{nam}

%description gnome
GNOME support for sawmill. Includes a wm-entries spec, and a control
center applet

%prep
%setup

%build
./configure --enable-capplet --prefix %{_prefix} %{_host}
make CFLAGS="$RPM_OPT_FLAGS"

%install
rm -f %{_prefix}/info/sawmill*
make install
gzip -9nf %{_prefix}/info/sawmill*

%files
%doc README NEWS TODO
%{_prefix}/bin/sawmill
%{_prefix}/bin/sawmill-client
%{_prefix}/bin/sawmill-ui
%{_prefix}/share/sawmill/%{ver}
%{_prefix}/libexec/sawmill/%{ver}/%{_host}
%{_prefix}/info/sawmill*

%files gnome
%{_prefix}/bin/sawmill-capplet
%{_prefix}/share/control-center/Sawmill
%{_prefix}/share/gnome/wm-properties/Sawmill.desktop
