#ifndef DEBUG_COLORS_H
#define DEBUG_COLORS_H


#define color_green "\x1b[43;30m"  /* i have */

/*was printf*/
#define color_reset "\x1b[0m"
/* requests: */
#define unmap_request_color       "\x1b[48;5;209;38;5;232m"
#define map_request_color       "\x1b[48;5;203;38;5;232m"

#define flow_color "\x1b[31m"
#define event_color             "\x1b[01;36m"
#define serial_color  "\x1b[01;33m"
#define events_color "\x1b[01;32;45m" /* how many events */
/* #define event_color "\x1b[46;30m"  */
#define event_name_color        "\x1b[01;45;37m"

#define frame_color       "\x1b[48;5;177;38;5;232m"
/* #define frame_color       "\x1b[38;5;119m" */

#define move_color       "\x1b[38;5;119m"

#define reparent_color       "\x1b[38;5;119m"

/* for stacking! */
#define functions_color   "\x1b[30;43m"
#define message_color   "\x1b[33;1m"


#define property_color   "\x1b[33;1m"
#define time_color    "\x1b[33m"
#define stacking_color   "\x1b[38;5;204m"

#define window_color "\x1b[38;5;123m"
#define frame_window_color  "\x1b[38;5;10m"
#define image_color  "\x1b[48;5;177;38;5;159m" 


/* fixme: I could simply output 2 Esc. sequences! */
static
inline char*
color_fg_string(char* string, int part, char* buffer)
{
#define BUFFER_SIZE   20
#if 0
    static char buffer[BUFFER_SIZE];
#endif

#if 0
    /* number_2_string ? */
    strncat(buffer, BUFFER_SIZE, string, (toa part), );
#else
    snprintf(buffer, BUFFER_SIZE, "%s%dm", string, part);
#endif
    return buffer;
}

#define focus_mode_color "\x1b[01;37m"

/* no! */
#define focus_color   "\x1b[38;5;204m"
#define another_focus_color "\x1b[31m"
#define warning_color "\x1b[48;5;46;38;5;22m" 
#define error_color "\x1b[01;37;41m"

#define xkb_color       "\x1b[01;42;30m"
#define keys_color "\x1b[01;36m"

#define configure_color "\x1b[48;5;153m"

/* events: */
#define unmap_color       "\x1b[38;5;209m"
#define map_color       "\x1b[38;5;203m"
/*#define map_color       "\x1b[01;46;31m" */
#define  grab_color    "\x1b[38;5;46m"
#define  xgrab_color    "\x1b[01;33;41m"


#endif /* DEBUG_COLORS_H */
