/* sawfish_subrs.h -- prototypes
   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef SAWFISH_SUBRS_H
#define SAWFISH_SUBRS_H

/* from colors.c */
extern int color_type;
extern repv Qdefault_foreground;
extern repv Fget_color_rgb (repv red, repv green, repv blue, repv alpha);
extern repv Fget_color (repv name, repv alpha);
extern repv Fcolor_name (repv color);
extern repv Fcolor_rgb (repv color);
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
extern repv Qthis_command, Qlast_command, Qprefix_arg, Qcurrent_prefix_arg;
extern repv Fcall_command (repv cmd, repv Farg);
extern repv Fprefix_numeric_argument (repv arg);
extern repv Finteractive (repv arg_list);
extern repv Fcommandp (repv arg);
extern void commands_init (void);

/* from display.c */
extern Display *dpy;
extern int screen_num, screen_width, screen_height, preferred_depth;
extern Visual *preferred_visual;
extern Window root_window, no_focus_window;
extern Atom xa_wm_state, xa_wm_change_state, xa_wm_protocols,
    xa_wm_delete_window, xa_wm_colormap_windows, xa_wm_take_focus,
  xa_compound_text, xa_wm_net_name, xa_wm_net_icon_name, xa_utf8_string;
extern int shape_event_base, shape_error_base;
extern repv Qdisplay_name, Qcanonical_display_name;
extern bool sys_init (char *program_name);
extern void sys_kill (void);
extern repv x_atom_symbol (Atom atom);
extern Window x_win_from_arg (repv arg);
extern void send_client_message (Window w, Atom a, Time time);
#if XlibSpecificationRelease < 6
extern Status XGetAtomNames (Display *dpy, Atom *atoms,
			     int count, char **names_ret);
#endif
extern void db_printf(char *fmt, ...);

/* from events.c */
extern void (*event_handlers[LASTEvent])(XEvent *ev);
extern Time last_event_time;
extern XEvent *current_x_event;
extern repv Qvisibility_notify_hook, Qdestroy_notify_hook, Qmap_notify_hook,
    Qunmap_notify_hook, Qreparent_notify_hook, Qenter_notify_hook,
    Qleave_notify_hook, Qfocus_in_hook, Qfocus_out_hook, Qclient_message_hook,
    Qwindow_moved_hook, Qwindow_resized_hook, Qrandr_change_notify_hook;
extern repv Qiconify_window, Quniconify_window;
extern struct frame_part *clicked_frame_part;
extern void save_timestamp (Time t);
extern void invalidate_cached_mouse_position (void);
extern void unclick_current_fp (void);
extern void synthesize_button_release (void);
extern void map_request (XEvent *ev);
extern void send_synthetic_configure (Lisp_Window *w);
extern long get_event_mask (int type);
extern Time get_server_timestamp (void);
extern void mark_pointer_grabbed (void);
extern void ungrab_pointer (void);
extern void register_event_handler (Window w, void (*fun)(XEvent *ev));
extern void deregister_event_handler (Window w);
extern void handle_input_mask(long mask);
extern void handle_sync_input(int fd);
extern repv Fquery_pointer (repv get);
extern repv Fquery_button_press_pointer (void);
extern repv Fquery_button_press_window (void);
extern repv Fquery_pointer_window (void);
extern repv Fcurrent_event_window (repv win);
extern void events_init (void);
extern void events_kill (void);

/* from fonts.c */
extern int font_type;
extern repv Qdefault_font;
extern int x_text_width (repv font, char *string, size_t len);
extern void x_draw_string (Window id, repv font, GC gc, Lisp_Color *fg_color,
			   int x, int y, char *string, size_t len);
extern repv Fget_font(repv name);
extern repv Ffont_get(repv font, repv prop);
extern repv Ffont_put(repv font, repv prop, repv value);
extern repv Ffont_name (repv font);
extern repv Ftext_width (repv string, repv font);
extern repv Ffont_height (repv font);
extern repv Ffontp (repv font);
extern void fonts_init (void);
extern void fonts_kill (void);

/* from frames.c */
extern int frame_part_type;
extern repv Qdefault_frame, Qnil_frame;
extern repv Qfocused, Qhighlighted, Qclicked;
extern repv Qhide_client, Qclass, Qframe_part_classes;
extern repv Qinternal, Qtiled, Qcenter, Qright, Qleft, Qtop, Qbottom;
extern repv Qtext, Qx_justify, Qy_justify, Qbackground, Qforeground;
extern repv Qrenderer, Qrender_scale, Qfont, Qwidth, Qheight;
extern repv Qleft_edge, Qtop_edge, Qright_edge, Qbottom_edge;
extern repv Qcursor, Qfocused, Qhighlighted, Qclicked;
extern bool frame_state_mutex;
extern int current_state (struct frame_part *fp);
extern void set_frame_shapes (Lisp_Window *w, bool atomic);
extern void queue_reshape_frame (Lisp_Window *w);
extern void commit_queued_reshapes (void);
extern void refresh_frame_part (struct frame_part *fp);
extern void refresh_frame_parts (Lisp_Window *w);
extern void create_window_frame (Lisp_Window *w);
extern void destroy_window_frame (Lisp_Window *w, bool leave_frame_win);
extern struct frame_part *find_frame_part_by_window (Window id);
extern void frame_part_exposer (XExposeEvent *ev, struct frame_part *fp);
extern repv get_keymap_for_frame_part (struct frame_part *fp);
extern void mark_frame_parts (Lisp_Window *w);
extern void reset_frame_parts (Lisp_Window *w);
extern void restack_frame_parts (Lisp_Window *w);
extern repv Vframe_draw_mutex (repv arg);
extern repv Vframe_state_mutex (repv arg);
extern void frames_init (void);
extern void frames_kill (void);

/* from functions.c */
extern repv Qroot, Qafter_restacking_hook, Qposition, Qspacing;
extern repv Fdelete_window (repv win);
extern repv Fdestroy_window (repv win);
extern repv Frestack_windows (repv list);
extern repv Fx_raise_window (repv win, repv above);
extern repv Fx_lower_window (repv win, repv below);
extern repv Fwarp_cursor (repv x, repv y);
extern repv Fmove_window_to (repv win, repv x, repv y);
extern repv Fresize_window_to (repv win, repv x, repv y);
extern repv Fmove_resize_window_to (repv win, repv x, repv y, repv w, repv h);
extern repv Fgrab_server (void);
extern repv Fungrab_server (void);
extern repv Fserver_grabbed_p (void);
extern void regrab_server (void);
extern repv Fgrab_pointer (repv win, repv cursor, repv ptr_sync,
			   repv kbd_sync, repv confine_to);
extern repv Fungrab_pointer (void);
extern repv Fgrab_keyboard (repv win, repv ptr_sync, repv kbd_sync);
extern repv Fungrab_keyboard (void);
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
extern repv Fdisplay_message (repv text, repv attrs);
extern void update_xinerama_info (void);
extern void functions_init (void);
extern void functions_kill (void);

/* from images.c */
extern int image_type;
extern Colormap image_cmap;
extern Visual *image_visual;
extern int image_depth;
extern Pixmap make_bitmap (repv file, unsigned *widthp, unsigned *heightp,
			   int *x_hotp, int *y_hotp);
extern repv Fmake_image (repv file, repv plist);
extern repv Fmake_image_from_x_drawable (repv id, repv mask_id);
extern repv Fwindow_icon_image (repv win);
extern repv Fcopy_image (repv source);
extern repv Fimage_get (repv image, repv prop);
extern repv Fimage_put (repv image, repv prop, repv value);
extern repv Fimagep (repv arg);
extern repv Fimage_dimensions (repv img);
extern repv Fimage_shape_color (repv img);
extern repv Fset_image_shape_color (repv img, repv shape);
extern repv Fimage_border (repv img);
extern repv Fset_image_border (repv img, repv, repv, repv, repv);
extern repv Fmake_sized_image (repv w, repv h, repv color);
extern repv Fbevel_image (repv img, repv border, repv up, repv bevel_percent);
extern repv Fclear_image (repv img, repv color);
extern repv Ftile_image (repv dst, repv src);
extern void pixmap_cache_flush_image (Lisp_Image *im);
extern void image_render (Lisp_Image *image, int width, int height,
			  Pixmap *pixmap, Pixmap *mask);
extern void image_free_pixmaps (Lisp_Image *image, Pixmap pixmap, Pixmap mask);
extern int best_color_match (int red, int green, int blue);
extern void paste_image_to_drawable (Lisp_Image *img, Drawable d,
				     int x, int y, int w, int h);
extern int image_width (Lisp_Image *im);
extern int image_height (Lisp_Image *im);
extern unsigned char *image_pixels (Lisp_Image *im);
extern int image_row_stride (Lisp_Image *im);
extern int image_channels (Lisp_Image *im);
extern void image_changed (Lisp_Image *im);
extern void images_init (void);
extern void images_kill (void);

/* from keys.c */
extern repv Qglobal_keymap, Qunbound_key_hook, Qkeymap;
extern repv eval_input_event (repv context_map);
extern bool print_event_prefix(void);
extern repv Fmake_keymap (void);
extern repv Fmake_sparse_keymap (repv base);
extern repv Fbind_keys (repv args);
extern repv Funbind_keys (repv args);
extern repv Fgrab_keymap (repv map);
extern repv Fungrab_keymap (repv map);
extern repv Fcurrent_event_string (void);
extern repv Fcurrent_event (void);
extern repv Fproxy_current_event (repv win, repv mask, repv prop);
extern repv Fallow_events (repv mode);
extern repv Flast_event (void);
extern repv Fevent_name (repv ev);
extern repv Flookup_event (repv name);
extern repv Flookup_event_binding (repv ev);
extern repv Fsearch_keymap (repv ev, repv km);
extern repv Fkeymapp (repv arg);
extern repv Feventp (repv arg);
extern void grab_window_events (Lisp_Window *w, bool grab);
extern void grab_keymap_events (Window grab_win, repv keymap, bool grab);
extern void update_keyboard_mapping (void);
extern void keys_init (void);

/* from main.c */
extern int exit_code;
extern repv Qsawfish_directory, Qsawfish_lisp_lib_directory,
      Qsawfish_site_lisp_directory, Qsawfish_exec_directory,
      Qsawfish_user_lisp_directory;
extern repv Qwindow_error, Qinvalid_pos, Qbad_event_desc;
extern repv Qbefore_exit_hook, Qfonts_are_fontsets;
extern bool batch_mode_p (void);
extern int main (int argc, char **argv);
extern repv Fquit (void);
extern repv Frestart (void);
extern void add_hook (repv sym, repv fun);
extern repv module_symbol_value (repv mod, repv sym);
extern repv global_symbol_value (repv sym);

/* from multihead.c */
extern void multihead_init (int *argcp, char ***argvp);

/* from pixmap-cache.c */
#ifdef NEED_PIXMAP_CACHE
extern bool pixmap_cache_ref (Lisp_Image *im, int width, int height,
			      Pixmap *p1, Pixmap *p2);
extern void pixmap_cache_unref (Lisp_Image *im, Pixmap p1, Pixmap p2);
extern void pixmap_cache_set (Lisp_Image *im, int width, int height,
			      Pixmap p1, Pixmap p2);
extern void pixmap_cache_flush_image (Lisp_Image *im);
#endif
extern repv Fpixmap_cache_control (repv max);
extern void pixmap_cache_init (void);

/* from property-cache.c */
extern repv property_cache_ref (repv id, repv prop);
extern void property_cache_set (repv id, repv prop, repv value, int invals);
extern void property_cache_invalidate_window (repv id);
extern void property_cache_invalidate (repv id, repv prop);

/* from server.c */
extern void server_init (void);
extern void server_kill (void);

/* from session.c */
extern repv Qsm_save_yourself;
extern repv Fsm_set_property (repv prop, repv value);
extern repv Fsm_delete_property (repv prop);
extern repv Fsm_connect (repv id);
extern repv Fsm_disconnect (void);
extern void session_init (void);
extern void session_kill (void);

/* from windows.c */
extern Lisp_Window *window_list;
extern int window_type;
extern Lisp_Window *focus_window;
extern int pending_destroys;
extern repv Qadd_window_hook, Qbefore_add_window_hook, Qplace_window_hook;
extern bool mapped_not_override_p (Window id);
extern void commit_queued_focus_change (void);
extern void focus_on_window (Lisp_Window *w);
extern void focus_off_window (Lisp_Window *w);
extern void fix_window_size (Lisp_Window *w);
extern Lisp_Window *find_window_by_id (Window id);
extern Lisp_Window *x_find_window_by_id (Window id);
extern void get_window_protocols (Lisp_Window *w);
extern void before_local_map (Lisp_Window *w);
extern void after_local_map (Lisp_Window *w);
extern void install_window_frame (Lisp_Window *w);
extern void remove_window_frame (Lisp_Window *w);
extern Lisp_Window *add_window (Window id);
extern void remove_window (Lisp_Window *win, bool destroyed, bool from_error);
extern void emit_pending_destroys (void);
extern repv Fwindow_get (repv win, repv prop, repv checker);
extern void register_property_monitor (repv prop, void (*callback)
				       (Lisp_Window *, repv, repv, repv));
extern repv Fwindow_put (repv win, repv prop, repv value);
extern repv Fwindow_remprop (repv win, repv prop);
extern repv Fwindow_plist (repv win);
extern repv Fwindow_name (repv win);
extern repv Fwindow_full_name (repv win);
extern repv Fwindow_icon_name (repv win);
extern repv Fwindow_mapped_p (repv win);
extern repv Fwindow_frame (repv win);
extern repv Fset_window_frame (repv win, repv frame);
extern repv Frebuild_frame (repv win);
extern repv Fwindow_position (repv win);
extern repv Fwindow_dimensions (repv win);
extern repv Fwindow_frame_dimensions (repv win);
extern repv Fwindow_frame_offset (repv win);
extern repv Fwindowp (repv win);
extern repv Fset_input_focus (repv win);
extern repv Finput_focus (void);
extern repv Fwindow_wants_input_p (repv win);
extern repv Fmanaged_windows (void);
extern repv Fget_window_by_id (repv id);
extern repv Fstacking_order (void);
extern repv Fwindow_visibility (repv win);
extern repv Fwindow_shaped_p (repv win);
extern repv Fhide_window (repv win);
extern repv Fshow_window (repv win);
extern repv Fwindow_visible_p (repv win);
extern repv Fwindow_reparented_p (repv win);
extern repv Fwindow_id (repv win);
extern repv Fwindow_group_id (repv win);
extern repv Fwindow_size_hints (repv win);
extern repv Fcall_window_hook (repv hook, repv win, repv args, repv type);
extern void manage_windows (void);
extern void windows_init (void);
extern void windows_kill (void);

/* from stacking-list.c */
extern bool window_in_stacking_list_p (Lisp_Window *w);
extern void remove_from_stacking_list (Lisp_Window *w);
extern void insert_in_stacking_list_above_all (Lisp_Window *w);
extern void insert_in_stacking_list_below_all (Lisp_Window *w);
extern void insert_in_stacking_list_above (Lisp_Window *w, Lisp_Window *x);
extern void insert_in_stacking_list_below (Lisp_Window *w, Lisp_Window *x);
extern void restack_window (Lisp_Window *w);
extern repv make_stacking_list (void);

#endif /* SAWFISH_SUBRS_H */
