#ifndef DEBUG_H
#define DEBUG_H

#include "sawfish.h"

/* format specifier */
#define FMT_XID "lx" /* XID */
#define FMT_WIN FMT_XID /* Window is unsigned long */


#define DEBUG 1

#define XAllowEvents_string "\x1b[01;33;41mXAllowEvents\x1b[0m"


#define DB_EVENTS_FLOW 1   /* 1 */

#define DB_EVENTS_MAP (1<<1)    /* 2 */
#define DB_EVENTS_UNMAP (1<<2)  /* 4 */
#define DB_EVENTS_FOCUS (1<<3)  /* 8 */               // 188 = 128 + 60 =    32 + 16 + 8 + 4        56 = 32 + 16 + 8

/* 4+8 = 14   +16 = 30 */

#define DB_EVENTS_TIME (1<<4)   /* 16 */

#define DB_EVENTS_COLOR (1<<5)  /* 32 */
#define DB_EVENTS_PROPERTY (1<<6)    /* 64 */
#define DB_EVENTS_MISC (1<<7)/* 128                   216 = 128 + 64 +  16 8         -64  152  */
#define DB_EVENTS_QUEUE (1<<8)  /* 256 */

#define DB_EVENTS_QUEUE (1<<8)  /* 256 */

#define DB_EVENTS_CONFIGURE (1 << 9)   /* 1 */
/*  */

#define DB_FUNCTIONS_GRAB (1<<1)
#define DB_FUNCTIONS_REST (1<<7)
#define DB_FUNCTIONS_MOUSE (1<<2)
#define DB_FUNCTIONS_MOVE (1<<3)  /* 8*/

#define DB_FUNCTIONS_MESSAGE (1<<4)  /* 16 */

#define DB_FUNCTIONS_PROP (1<<5)  /* 32 */



#define DB_WINDOWS_FOCUS 1

#define DB_WINDOWS_PROT (1 << 1) /* protocol */
#define DB_WINDOWS_ADD (1 << 2) /* 4 */
#define DB_WINDOWS_FRAME (1 << 3) /* 8 */


#define DB_WINDOWS_PUT (1 << 4) /* ??? */

#define DB_WINDOWS_HOOKS (1<<6) /*  64 */
#define DB_WINDOWS_REST (1<<7)  /* 128 */

#define DB_WINDOWS_FIND (1<<8)
#define DB_WINDOWS_GC (1<<9)

#define DB_DISPLAY_ERROR 1


#define DB_KEYS_INTER 4
#define DB_KEYS_FLOW 128
#define DB_KEYS_GRAB 2
#define DB_KEYS_WINDOW 8
#define DB_KEYS_WINDOW_DETAIL 16


#define DB_FRAMES_FRAME (1 << 0) /* 2 */

// ???
#define DB_FRAMES_PARTS (1 << 1) /* 2 */


#define DB_FRAMES_SHAPE (1 << 2) /* 4 */
// #define DB_FRAMES_SHAPE (1 << 9) /* 512 */

#define DB_FRAMES_PARTS_CONFIGURE (1 << 3) /* configure FPs to change their size/position */

#define DB_FRAMES_BUILD (1 << 4) /*  16 */


#define DB_FRAMES_ALLOC (1 << 5) /*  32 */
#define DB_FRAMES_RE (1 << 6) /*  64   ... */

#define DB_FRAMES_PARTS_BUILD (1 << 7) /* 128 */

// fixme: should use `DB_FRAMES_RE'
#define DB_FRAMES_PARTS_CHANGE (1 << 8) /* 256 */


#define DB_FRAMES_DRAW (1 <<9)  /* 512 */




#define DB_IMAGES_CREATE (1 << 1) 
#define DB_IMAGES_RENDER (1 << 2) /* 4 */
#define DB_IMAGES_OPS    (1 << 3) 
#define DB_IMAGES_GET (1 << 4)
#define DB_IMAGES_GC (1 << 5)



#define DB_CACHE_DELETE (1 << 1)
#define DB_CACHE_REF (1 << 2)
#define DB_CACHE_MISS (1 << 3)  /* 8 */


#define DB_STACKING_PROT (1 << 1) /* protocol */
#define DB_STACKING_ALGO (1 << 2) /* 4 */
#define DB_STACKING_WINDOWS_FRAME (1 << 3)



/* frame options:
 *  1          Frame bg black & pixmamp None!
 *  2          atomic
 *  4             pixbuf cache works as I want: 
 *  8          DBE
 *  16      ??
 *  32         DO refresh_frame_parts  before Expose iff 1
 *
 *    43 =  32 + 11 = 8 + 3 = 2+1
 *  64   ....  allow XConfigure on the FPs (which need it)

 *  128    do not refresh on expose  a traced FP

 *  256      make all gravity static
 *  512   ... don't reconfigure FPs        .. rely on gravity!
 *  1024  ... predict the reposition & maybe change the gravity?
 *  2048  ... clearArea on each configure
 */


extern int debug_keys;
extern int debug_stacking;
extern int debug_events;
extern int debug_functions;
extern int debug_windows;
extern int debug_frames;
extern int debug_frame_part;
extern int debug_images;
extern int debug_cache;
extern int debug_display;


/* options */
extern int default_grab_owner;
extern int dont_allow_events;


#endif /* DEBUG_H */
