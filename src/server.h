/* server.h -- Definitions for client/server
   $Id$ */

#ifndef SAWMILL_SERVER_H
#define SAWMILL_SERVER_H

/* Name of the unix domain socket. It's stored in the user's home directory,
   but this is the basename. %s is fully-qualified host name */
#define SAWMILL_SOCK_NAME ".sawmill-%s"

/* Types of request packet. A byte with one of these values is sent to
   initiate a command. */
enum server_request {
    req_eval,
    req_eval_async,
    req_end_of_session = 255
};

#endif /* SAWMILL_SERVER_H */
