/* sawmill_subrs.h -- prototypes
   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawmill.

   sawmill is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawmill is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawmill; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef SAWMILL_SUBRS_H
#define SAWMILL_SUBRS_H

/* from colors.c */
extern int color_type;
extern repv Qdefault_foreground;
extern repv Fget_color (repv name);
extern repv Fcolor_name (repv color);
extern repv Fcolorp (repv arg);
extern void colors_init (void);
extern void colors_kill (void);

/* from cursors.c */
extern int cursor_type;
extern repv default_cursor, Qcursor_shape;
extern repv Fget_cursor (repv name);
extern repv Vdefault_cursor (repv arg);
extern void cursors_init (void);
extern void cursors_kill (void);

/* from commands.c */
extern repv this_command, last_command, Qpost_command_hook, Qpre_command_hook;
extern repv var_this_command (repv arg);
extern repv var_last_command (repv arg);
extern repv var_prefix_arg (repv arg);
extern repv var_current_prefix_arg (repv arg);
extern repv Fcall_command (repv cmd, repv Farg);
extern repv Fprefix_numeric_argument (repv arg);
extern repv Finteractive (repv arg_list);
extern repv Fcommandp (repv arg);
extern void commands_init (void);

/* from display.c */
extern char *prog_name;
extern Display *dpy;
extern int screen_num, screen_width, screen_height, screen_depth;
extern Visual *screen_visual;
extern Colormap screen_cmap;
extern Window root_window, no_focus_window;
extern Atom xa_wm_state, xa_wm_change_state, xa_wm_protocols,
    xa_wm_delete_window, xa_wm_colormap_windows, xa_wm_take_focus;
extern int shape_event_base, shape_error_base;
extern bool sys_init (char *program_name);
extern void sys_kill (void);
extern repv x_atom_symbol (Atom atom);
extern Window x_win_from_arg (repv arg);
extern void send_client_message (Window w, Atom a, Time time);

/* from events.c */
extern void (*event_handlers[LASTEvent])(XEvent *ev);
extern Time last_event_time;
extern XEvent *current_x_event;
extern repv Qvisibility_notify_hook, Qdestroy_notify_hook, Qmap_notify_hook,
    Qunmap_notify_hook, Qenter_notify_hook, Qleave_notify_hook,
    Qfocus_in_hook, Qfocus_out_hook, Qclient_message_hook;
extern repv Qiconify_window, Quniconify_window;
extern void unclick_current_fp (void);
extern void map_request (XEvent *ev);
extern void send_synthetic_configure (Lisp_Window *w);
extern void handle_input_mask(long mask);
extern void handle_sync_input(int fd);
extern repv Fquery_pointer (repv get);
extern repv Fquery_last_pointer (void);
extern repv Fquery_pointer_window (void);
extern void events_init (void);
extern void events_kill (void);

/* from fonts.c */
extern int font_type;
extern repv Qdefault_font;
extern repv Fget_font(repv name);
extern repv Ffont_get(repv font, repv prop);
extern repv Ffont_put(repv font, repv prop, repv value);
extern repv Ffont_name (repv font);
extern repv Ffontp (repv font);
extern void fonts_init (void);
extern void fonts_kill (void);

/* from frames.c */
extern repv Qdefault_frame, Qnil_frame;
extern void set_frame_part_bg (struct frame_part *fp);
extern void set_frame_part_fg (struct frame_part *fp);
extern void create_window_frame (Lisp_Window *w);
extern void destroy_window_frame (Lisp_Window *w);
extern struct frame_part *find_frame_part_by_window (Window id);
extern void frame_part_exposer (XExposeEvent *ev, struct frame_part *fp);
extern void mark_frame_parts (Lisp_Window *w);
extern void frames_init (void);
extern void frames_kill (void);

/* from functions.c */
extern repv Qroot;
extern repv Fraise_window (repv win);
extern repv Flower_window (repv win);
extern repv Fraise_lower_window (repv win);
extern repv Fcirculate_up (void);
extern repv Fcirculate_down (void);
extern repv Fdelete_window (repv win);
extern repv Fdestroy_window (repv win);
extern repv Fwarp_cursor (repv x, repv y);
extern repv Fwarp_cursor_to_window (repv win, repv x, repv y);
extern repv Fmove_window_to (repv win, repv x, repv y);
extern repv Fresize_window_to (repv win, repv x, repv y);
extern repv Fgrab_server (void);
extern repv Fungrab_server (void);
extern repv Fgrab_pointer (repv win, repv cursor);
extern repv Fungrab_pointer (void);
extern repv Fdraw_window_outline (repv mode, repv x, repv y,
				  repv width, repv height);
extern repv Ferase_window_outline (repv mode, repv x, repv y,
				   repv width, repv height);
extern repv Fdelete_x_property (repv win, repv prop);
extern repv Flist_x_properties (repv win);
extern repv Fget_x_property (repv win, repv prop);
extern repv Fset_x_property (repv win, repv prop, repv data,
			     repv type, repv format);
extern repv Fsend_client_message (repv win, repv type, repv data, repv format);
extern repv Fcreate_window (repv parent, repv x, repv y,
			    repv width, repv height);
extern repv Fx_atom (repv symbol);
extern repv Fx_atom_name (repv atom);
extern void functions_init (void);

/* from images.c */
extern int image_type;
extern ImlibData *imlib_id;
extern repv Fmake_image (repv file, repv plist);
extern repv Fcopy_image (repv source);
extern repv Fimage_get (repv image, repv prop);
extern repv Fimage_put (repv image, repv prop, repv value);
extern repv Fimage_dimensions (repv img);
extern repv Fimage_border (repv img);
extern repv Fset_image_border (repv img, repv, repv, repv, repv);
extern void images_init (void);
extern void images_kill (void);

/* from keys.c */
extern repv Qglobal_keymap, Qunbound_key_hook, Qkeymap;
extern bool (*event_proxy_fun)(XEvent *ev, long code, long mods);
extern repv eval_input_event (repv context_map);
extern repv Fmake_keymap (void);
extern repv Fmake_sparse_keymap (repv base);
extern repv Fbind_keys (repv args);
extern repv Funbind_keys (repv args);
extern repv Fgrab_keymap (repv map);
extern repv Fungrab_keymap (repv map);
extern repv Fcurrent_event_string (void);
extern repv Fcurrent_event (void);
extern repv Fcurrent_event_window (void);
extern repv Fproxy_current_event (repv win);
extern repv Flast_event (void);
extern repv Fevent_name (repv ev);
extern repv Flookup_event (repv name);
extern repv Flookup_event_binding (repv ev);
extern repv Fsearch_keymap (repv ev, repv km);
extern repv Fkeymapp (repv arg);
extern repv Feventp (repv arg);
extern void grab_window_events (Lisp_Window *w, bool grab);
extern void grab_keymap_events (Window grab_win, repv keymap, bool grab);
extern void keys_init (void);

/* from main.c */
extern jmp_buf clean_exit_jmp_buf;
extern repv Qsawmill_directory, Qsawmill_lisp_lib_directory,
    Qsawmill_site_lisp_directory, Qsawmill_exec_directory;
extern repv Qwindow_error, Qinvalid_pos, Qbad_event_desc;
extern repv Qbefore_exit_hook;
extern int main (int argc, char **argv);
extern void add_hook (repv sym, repv fun);

/* from windows.c */
extern Lisp_Window *window_list;
extern int window_type;
extern Lisp_Window *focus_window;
extern repv Qadd_window_hook, Qplace_window_hook, Qwindow_state_change_hook;
extern repv Qiconified;
extern bool mapped_not_override_p (Window id);
extern void focus_on_window (Lisp_Window *w);
extern void fix_window_size (Lisp_Window *w);
extern Lisp_Window *find_window_by_id (Window id);
extern Lisp_Window *x_find_window_by_id (Window id);
extern void install_window_frame (Lisp_Window *w);
extern void remove_window_frame (Lisp_Window *w);
extern Lisp_Window *add_window (Window id);
extern void set_window_shape (Lisp_Window *w);
extern void remove_window (Lisp_Window *win, repv destroyed, repv from_error);
extern repv Fwindow_get (repv win, repv prop);
extern repv Fwindow_put (repv win, repv prop, repv value);
extern repv Fwindow_name (repv win);
extern repv Fwindow_full_name (repv win);
extern repv Fwindow_icon_name (repv win);
extern repv Fwindow_mapped_p (repv win);
extern repv Fwindow_frame (repv win);
extern repv Fset_window_frame (repv win, repv frame);
extern repv Fwindow_position (repv win);
extern repv Fwindow_dimensions (repv win);
extern repv Fwindow_frame_dimensions (repv win);
extern repv Fwindow_frame_offset (repv win);
extern repv Fwindowp (repv win);
extern repv Fset_input_focus (repv win);
extern repv Finput_focus (void);
extern repv Fmanaged_windows (void);
extern repv Fwindow_visibility (repv win);
extern repv Fwindow_transient_p (repv win);
extern repv Fwindow_shaped_p (repv win);
extern repv Fhide_window (repv win);
extern repv Fshow_window (repv win);
extern repv Fwindow_visible_p (repv win);
extern repv Fwindow_id (repv win);
extern repv Fwindow_group_id (repv win);
extern repv Fwindow_size_hints (repv win);
extern repv Fcall_window_hook (repv hook, repv win, repv args, repv type);
extern repv Fset_client_state (repv win);
extern void manage_windows (void);
extern void windows_init (void);
extern void windows_kill (void);

#endif /* SAWMILL_SUBRS_H */
