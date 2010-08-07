
/* time when timing starts: e.g. Expose event arrives. */
struct timespec commit_delay = {0, 0};

static void
get_time (struct timespec *time)
{
    if (clock_gettime(CLOCK_MONOTONIC, time))
        exit (-1);             /* fixme:  ERETRY!!! */
}

static void
start_timing()
{
    if (commit_delay.tv_sec == 0)
    {
        get_time (&commit_delay);
    }
    else
    {
        struct timespec commit_time;
#if 0 
        if (debug_events  & DB_EVENTS_FLOW)
            DB(("timing already set!\n"));
#endif
        get_time (&commit_time);

        /* more than 1 second -> Flush! */
        if ((commit_delay.tv_sec > commit_time.tv_sec +1) ||
            (((commit_delay.tv_nsec > commit_time.tv_nsec)?1000000000L:0)
            + commit_time.tv_nsec - commit_delay.tv_nsec > 100000000L))
        {
        DB (("time over-> XFlush \n"));
        XFlush(dpy);
        /* reset: */
        commit_delay.tv_sec = 0;
    }
    }
}

/* tell the Timing system, that we sent now -- to stop.
 * /param LABEL is for tracing only. */
void
do_commit2(char* label)
{
   struct timespec commit_time;
   get_time (&commit_time);

   if (commit_delay.tv_sec == 0)
      /* bug! */
      DB (("%s: XFlush without timing\n", label));
   else
   {
       if (debug_events  & DB_EVENTS_FLOW)
           DB (("%s: XFlush after %u: %9.9lu\n", /* time_t  how to print it? */
                label,
                commit_time.tv_sec - commit_delay.tv_sec
                - ((commit_delay.tv_nsec > commit_time.tv_nsec)?1:0),
                /* sorry! todo! */
                (commit_delay.tv_nsec > commit_time.tv_nsec)?
                (1000000000L + commit_time.tv_nsec - commit_delay.tv_nsec):
                (commit_time.tv_nsec - commit_delay.tv_nsec)));
   }
   /* reset: */
   commit_delay.tv_sec = 0;
}

void
do_flush(char* label)
{
   do_commit2(label);
   XFlush (dpy);
}
