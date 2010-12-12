#include "sawfish.h"
#include <sys/resource.h>
#include <time.h>

/* fixme: this assumes a 255-color xterm! */

#ifdef USE_XKB
#include <X11/XKBlib.h>         /* extensions/XKB.h */
#include <X11/extensions/XKBstr.h>
extern int usexkb;
#endif  /* USE_XKB */

#include <errno.h>
#include <assert.h>

#include "debug.h"
#include "debug-colors.h"


const char*
window_name (Lisp_Window *w)
{
    /* todo:  root_window or no_focus_window ? */
    return ((w && w->name) ? (char *) rep_STR(w->name) : "unknown");
}

const char*
window_name_or (Lisp_Window *w, const char* fallback)
{
    return ((w && w->name) ? (char *) rep_STR(w->name) : fallback);
}


/* mmc: make a nice string format from the Time T
 * only for debugging
 */

char*
my_timestamp(Time t)
{
    static char buf[30];	/*  like asctime */

    /* fixme:  non standard ?? */
    int sec= (int) t / 1000;
    int min= (int) sec / 60;
    int rest=(int) min / 60;

    min= min % 60;
    sec= sec % 60;
    
                                
    
    sprintf(buf, "%s%u:%2u:%02u:%02lu%s",
            time_color,
            rest,
            min,
            sec,
            t % 1000,             /* (t /10 ) */
            color_reset);
    return buf;
}

/* a useless confirmation? */
bool
window_is_frame_part (Window id, Lisp_Window *w)
{
   struct frame_part* p = w->frame_parts;
   while (p != NULL)
      {
         if (p->id == id)
            return TRUE;
         p = p->next;
      }
   return FALSE;
}

Lisp_Window *
find_window_by_frame (Window id)
{
   Lisp_Window *w;

   w = window_list;
   while (w != 0 && w->frame != id)
      w = w->next;
   return w;
}


inline char*
window_relation_desc (Lisp_Window *w, Window id)
{
    return ((id == w->id)? "(" window_color "itself" color_reset ")" :
            ( (id == w->frame) ? "(" frame_window_color "frame" color_reset")" :
              (window_is_frame_part (id, w) ? "frame_part" : "??" )));
}


static char* focus_detail_names[]=
{
   "NotifyAncestor",
   "NotifyVirtual",
   "NotifyInferior",                                                 
   "NotifyNonlinear",
   "NotifyNonlinearVirtual",
   "NotifyPointer",                                         
   "NotifyPointerRoot",
   "NotifyDetailNone"
};


char* focus_mode_string[] =
{
   /* taken from /usr/include/X11/X.h */
   "NotifyNormal",
   "NotifyGrab",
   "NotifyUngrab",
   "NotifyWhileGrabbed",
};

char* buffer;

#define color_fg(string, fg) (buffer = alloca(BUFFER_SIZE),            \
                              color_fg_string(string, fg, buffer))

void
describe_focus_in(XEvent *ev,Lisp_Window *w)
{
   assert (ev->xfocus.detail < 8); /* what is 8? */

   char* window_name = "root_window";
   if (!(ev->xfocus.window == root_window))
   {
         window_name = w ? (char *) rep_STR(w->name) :
            ((ev->xfocus.window == no_focus_window) ? "no_focus_window" : "unknown");
         /* mmc: w never references no_focus_window !!! */
      };
   int color= ((ev->xfocus.mode) == NotifyGrab)?145:
      (((ev->xfocus.mode) == NotifyUngrab)?146:147);
         
   DB (("%s%s received %lu win = %s(%s) mode: %s%s%s detail: %s%s%s\n",
        color_fg("\x1b[38;5;",color),
        /* "%s received %d win = %s mode: %s detail:\n", */
        //(ev->xfocus.type == FocusIn)? "focus_in": "focus_out",
        __FUNCTION__,

        ev->xfocus.serial,
        window_name,
        (w) ? window_relation_desc (w,ev->xfocus.window):"",

        color_fg("\x1b[38;5;", 116+ev->xfocus.mode),
        (ev->xfocus.mode < sizeof(focus_mode_string)/sizeof(focus_mode_string[0])) ?
        focus_mode_string[ev->xfocus.mode]:"<unexpected>!",
        color_reset,
              
        color_fg("\x1b[38;5;", color),
        /* focus_mode_color, */
        /* ev->xfocus.detail, */
        focus_detail_names[ev->xfocus.detail],
        color_reset)); 
}

/*static */
void
describe_focus_out(XEvent *ev, Lisp_Window *w)
{
    if (!(ev->xfocus.detail < (sizeof(focus_detail_names) /sizeof(focus_detail_names[0]))))
        return;
   
    if (!w)
        w = find_window_by_frame (ev->xfocus.window);
   
    char* window_name = w ? (char *) rep_STR(w->name) :
        ((ev->xfocus.window == no_focus_window) ? "no_focus_window" :
         (ev->xfocus.window == root_window) ? "root_window" : "unknown");
    int color= ((ev->xfocus.mode) == NotifyGrab)? 36:
        (((ev->xfocus.mode) == NotifyUngrab)?34:33);

    DB (("%s%s%s serial %lu win = %" FMT_WIN " %s(%s) mode: %s%s%s detail: %s%s%s\n", /* %d */
         /* "%s received %d win = %s mode: %s detail:\n", */
         color_fg("\x1b[38;5;",color),
         __FUNCTION__,  // (ev->xfocus.type == FocusIn)? "focus_in": "focus_out",
         color_reset,
         ev->xfocus.serial,
         ev->xfocus.window,
         window_name,
         (w) ? window_relation_desc (w,ev->xfocus.window):"",
               
         color_fg("\x1b[38;5;", 116 + ev->xfocus.mode),
         ((ev->xfocus.mode < sizeof(focus_mode_string)/sizeof(focus_mode_string[0]))?
          focus_mode_string[ev->xfocus.mode]:"<unexpected>!"),
         color_reset,

         /* detail: */
         color_fg("\x1b[38;5;", color),
         focus_detail_names[ev->xfocus.detail],              /* ev->xfocus.detail, */
         color_reset));
};

/* extern int restack_fast; */

int
set_int_variable(int* var, repv value)
{
   rep_DECLARE1 (value, rep_INTP);

   int old=*var;
   *var = rep_INT(value);
   return rep_MAKE_INT(old);
}


/* mmc: i should add  incrementing/decrementing &  commit-on-zero? */
/* DEFUN("set-restack-fast", Fset_restack_fast, Sset_restack_fast, (repv debug), rep_Subr1) */ /*
   ::doc:sawfish.wm.events::set-restack-fast
make the following restacking operations immediate if 0, and posponed if 1
::end:: */
/* {
   rep_DECLARE1 (debug, rep_INTP);
   return set_int_variable(&restack_fast, debug);
} */

extern bool be_proactive_in_move;

#if 0
DEFUN("set-proactive-move", Fset_proactive_move, Sset_proactive_move, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events#set-proactive-move::
set-proactive-move 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&be_proactive_in_move, debug);
}
#endif


DEFUN("set-debug-events", Fset_debug_events, Sset_debug_events, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events#set-debug-events::
set-debug-events 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_events, debug);
}

DEFUN("set-debug-windows", Fset_debug_windows, Sset_debug_windows, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.windows#set-debug-windows::
set-debug-windows 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_windows, debug);
}


DEFUN("set-debug-keys", Fset_debug_keys, Sset_debug_keys, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-keys
set-debug-keys 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_keys, debug);
}

DEFUN("set-debug-stacking", Fset_debug_stacking, Sset_debug_stacking, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-keys
set-debug-keys 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_stacking, debug);
}


#ifdef USE_XKB
/* work-in-progress? */
DEFUN("set-xkb-event-mask", Fset_xkb_event_mask, Sset_xkb_event_mask, (repv mask, repv value), rep_Subr2) /*
::doc:sawfish.wm.events::set-debug-keys
set-debug-keys 0/1
::end:: */
{
   rep_DECLARE1 (mask, rep_INTP);
   rep_DECLARE2 (value, rep_INTP);

   if (usexkb)
    /* i want to report them. */
      XkbSelectEvents(dpy, XkbUseCoreKbd, rep_INT(mask), rep_INT(value)); /* `set' */
/*   XkbSelectEventsDetails(dpy, XkbUseCoreKbd, XkbMapNotifyMask, XkbMapNotifyMask);  `set' */
/*    return set_int_variable(&xallowevents_handling, debug); */
    return Qnil;
}


#endif



DEFUN("set-debug-functions", Fset_debug_functions, Sset_debug_functions, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-functions
set-debug-functions 0/1
::end:: */
{
   rep_DECLARE1 (debug, rep_INTP);
   return set_int_variable(&debug_functions, debug);
}


DEFUN("set-debug-display", Fset_debug_display, Sset_debug_display, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-display
set-debug-display 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_display, debug);
}


DEFUN("set-debug-images", Fset_debug_images, Sset_debug_images, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-images
set-debug-images 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_images, debug);
}

DEFUN("set-debug-cache", Fset_debug_cache, Sset_debug_cache, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-cache
set-debug-cache 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    return set_int_variable(&debug_cache, debug);
}

#if 0
DEFUN("set-grab-owner", Fset_grab_owner, Sset_grab_owner, (repv owner), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-display
set-debug-display 0/1
::end:: */
{
    rep_DECLARE1 (owner, rep_INTP); /* fixme: should be boolean! */
    return set_int_variable(&default_grab_owner, owner);
}
#endif

DEFUN("next-request", Fnext_request, Snext_request, (void), rep_Subr0) /*
::doc:sawfish.wm.events::next-request
next-request 
::end:: */
{
   return rep_MAKE_INT(NextRequest(dpy));
}

DEFUN("set-debug-frames", Fset_debug_frames, Sset_debug_frames, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-frames
set-debug-frames 0/1
::end:: */
{
   rep_DECLARE1 (debug, rep_INTP);
   return set_int_variable(&debug_frames, debug);
}

#if 0
DEFUN("set-debug-frame-part", Fset_debug_frame_part, Sset_debug_frame_part, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-debug-frames
set-debug-frames 0/1
::end:: */
{
   rep_DECLARE1 (debug, rep_INTP);
   return set_int_variable(&debug_frame_part, debug);
}


DEFUN("set-dont-allow-events", Fset_dont_allow_events, Sset_dont_allow_events, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-dont-allow-event
::set-dont-allow-event 0/1
::end:: */
{
    rep_DECLARE1 (debug, rep_INTP);
    DB(("set-dont-allow-events %d -> %d (now)\n", dont_allow_events, rep_INT(debug)));
    return set_int_variable(&dont_allow_events, debug);
}

extern int frame_options;

DEFUN("set-frame-options", Fset_frame_options, Sset_frame_options, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-frame-options
set-frame-options 0/1
::end:: */
{
   rep_DECLARE1 (debug, rep_INTP);
   return set_int_variable(&frame_options, debug);
}



extern int window_options;

DEFUN("set-window-options", Fset_window_options, Sset_window_options, (repv debug), rep_Subr1) /*
::doc:sawfish.wm.events::set-frame-options
set-frame-options 0/1
::end:: */
{
   rep_DECLARE1 (debug, rep_INTP);
   return set_int_variable(&window_options, debug);
}

#endif



/* Run external program w/ UID  (su)
 * set & get priority.
 * */

DEFUN("get-priority", Fget_priority, Sget_priority, (repv which, repv who), rep_Subr2) /*
::doc:sawfish.wm.events::set-debug-display
(get-priority 0 0)  to get the  sawfish WM process's priority.
::end:: */
{
   rep_DECLARE1(which, rep_INTP);
   rep_DECLARE2(who, rep_INTP);

#if 0
enum __priority_which
{
  PRIO_PROCESS = 0,		/* WHO is a process ID.  */
#define PRIO_PROCESS PRIO_PROCESS
  PRIO_PGRP = 1,		/* WHO is a process group ID.  */
#define PRIO_PGRP PRIO_PGRP
  PRIO_USER = 2			/* WHO is a user ID.  */
#define PRIO_USER PRIO_USER
};
#endif



   int c_which = rep_INT(which);
   if ((c_which == PRIO_PROCESS)
       || (c_which == PRIO_PGRP)
       || (c_which == PRIO_USER))
       {
           /*  Since  getpriority()  can  legitimately return the value -1, it is necessary to clear the
               external variable errno prior to the call */
           errno = 0;
           int res = getpriority(c_which, rep_INT(who));

           if ((res == -1) && errno)
               {
                   /* throw error! */
                   return rep_FALSE;
                   /* rep_handle_error */
                   return rep_signal_arg_error(which, 0);
               }
           else
               {
                   return rep_MAKE_INT(res);
               }
       }
   else return rep_signal_arg_error(which, 0);
}




static repv
reopen_stdio_stream(FILE** pointer, char* filename)
{
   /* can access? */
   /* FILE* */
   FILE* result = freopen(filename, "a", *pointer);
   if (result){
      
      /* setlinebuf(result); */
      setvbuf(result, (char *)NULL, _IONBF, 0);
   } else
      {
         /*disaster!!! */
         XBell(dpy,0);
         result = freopen("/dev/null", "a", *pointer);
      };
   assert(result);

   *pointer = result;
   DB(("%s: %s %d\n", __FUNCTION__, result?"ok":"error!", result?fileno(result):-1));
   return result?rep_TRUE:rep_FALSE;
}






DEFUN("reopen-stderr", Freopen_stderr, Sreopen_stderr, (repv filename), rep_Subr1) /*
::doc:sawfish.wm.events::reopen-stderr
redirect-stderr-to filename
::end:: */
{
   rep_DECLARE1(filename, rep_STRINGP);

   FILE **stdio_stream = &stderr;
   
   repv result = reopen_stdio_stream (&stderr, rep_STR(filename));
   
   repv stream = Fstderr_file();
   rep_FILE(stream)->file.fh = *stdio_stream;
   return result?rep_TRUE:rep_FALSE;
}


DEFUN("reopen-stdout", Freopen_stdout, Sreopen_stdout, (repv filename), rep_Subr1) /*
::doc:sawfish.wm.events::reopen-stdout
redirect-stdout-to filename
::end:: */
{
   rep_DECLARE1(filename, rep_STRINGP);

   FILE **stdio_stream = &stdout;
   repv result = reopen_stdio_stream (stdio_stream, rep_STR(filename));
   
   repv stream = Fstdout_file();
   rep_FILE(stream)->file.fh = *stdio_stream;
   return result?rep_TRUE:rep_FALSE;
}



#if 0
bool
reopen_lisp_stream(repv stream, char* filename)
{
   /* assert (stream_p()) */
   FILE **stdio_stream = &(rep_FILE(stream)->file.fh); /* ??? */
   /* if (*strio_stream) */
   repv result = reopen_stdio_stream (stdio_stream, rep_STR(filename));
   rep_FILE(stream)->file.fh = *stdio_stream;
}



reopen_lisp_stream(Fstdout_file());
#endif

static int
get_int_variable_from_env(char* name, int* variable)
{
   if (getenv(name) != NULL)
      {
         *variable = atoi(getenv(name));
         DB(("using the ENV VAR $%s = %s -> %d\n", name, getenv(name), *variable));
         return *variable;
      }
   return 0;
}


void
set_init (void)
{
    repv tem;

    tem = rep_push_structure ("sawfish.wm.windows.subrs");


    /* rep_ADD_SUBR(Sstart_restacking); */
    rep_ADD_SUBR(Sreopen_stderr);
    rep_ADD_SUBR(Sreopen_stdout);

    rep_ADD_SUBR(Sget_priority);
    /* rep_ADD_SUBR(Sset_restack_fast); /*

    rep_ADD_SUBR(Sset_debug_functions);
    rep_ADD_SUBR(Sset_debug_display);
    rep_ADD_SUBR(Sset_debug_images);
    rep_ADD_SUBR(Sset_debug_cache);

    rep_ADD_SUBR(Snext_request);
    rep_pop_structure (tem);

    tem = rep_push_structure ("sawfish.wm.events");
#if 0
    rep_ADD_SUBR(Sset_grab_owner);
    rep_ADD_SUBR(Sset_dont_allow_events);
    rep_ADD_SUBR(Sset_debug_frame_part);
    rep_ADD_SUBR(Sset_frame_options);
    rep_ADD_SUBR(Sset_window_options);
    rep_ADD_SUBR(Sset_xkb_event_mask);
    rep_ADD_SUBR(Sset_proactive_move);

    rep_INTERN_SPECIAL(debug_events);
    Fset (Qdebug_events, rep_MAKE_INT(0));
    rep_ADD_SUBR(Sset_xallowevents_handling);
#endif
    rep_ADD_SUBR(Sset_debug_events);
    rep_ADD_SUBR(Sset_debug_windows);
    rep_ADD_SUBR(Sset_debug_keys);
    rep_ADD_SUBR(Sset_debug_stacking);

    rep_ADD_SUBR(Sset_debug_frames);

    rep_pop_structure (tem);

    /* initial debug. */
    get_int_variable_from_env("SAWFISH_DEBUG_STACKING", &debug_stacking);
    get_int_variable_from_env("SAWFISH_DEBUG_WINDOWS", &debug_windows);
    get_int_variable_from_env("SAWFISH_DEBUG_FUNCTIONS", &debug_functions);

    get_int_variable_from_env("SAWFISH_DEBUG_EVENTS", &debug_events);
    get_int_variable_from_env("SAWFISH_DEBUG_KEYS", &debug_keys);
    get_int_variable_from_env("SAWFISH_DEBUG_FRAMES", &debug_frames);
}
