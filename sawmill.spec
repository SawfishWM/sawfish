
%define ver	0.3
%define rel	1

Summary: a highly configurable and extensible X11 window manager
Name: sawmill
Version: %{ver}
Release: %{rel}
Requires: librep >= 0.3, rep-gtk
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

%prep
%setup

%build
./configure --prefix %{_prefix} %{_host}
make CFLAGS="$RPM_OPT_FLAGS"

%install
make install

%files
%doc README NEWS TODO
%{_prefix}/bin/sawmill
%{_prefix}/bin/sawmill-client
%{_prefix}/bin/sawmill-menu
%{_prefix}/bin/sawmill-ui
%{_prefix}/share/sawmill/images/absolute-e/*.png
%{_prefix}/share/sawmill/images/brushed-metal/*.png
%{_prefix}/share/sawmill/images/misc/*.png
%{_prefix}/share/sawmill/%{ver}/lisp/themes/*.jl
%{_prefix}/share/sawmill/%{ver}/lisp/themes/*.jlc
%{_prefix}/share/sawmill/%{ver}/lisp/*.jl
%{_prefix}/share/sawmill/%{ver}/lisp/*.jlc
%{_prefix}/share/sawmill/%{ver}/DOC.*
%{_prefix}/libexec/sawmill/%{ver}/%{_host}/
%{_prefix}/share/gnome/wm-properties/Sawmill.desktop
