#| ping.jl -- implement the _NET_WM_PING protocol

   $Id$

   Author: John Harper <jsh@eazel.com>

   Copyright (C) 2000 Eazel, Inc.

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
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.util.ping

    (export window-supports-ping-p
	    ping-window)

    (open rep
	  rep.io.timers
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.windows)

  (define make-ping-record cons)
  (define ping-callback car)
  (define ping-timestamp cdr)

  ;; list of ping records sent but not completed
  (define pings-in-transit '())

;;; public functions

  (define (window-supports-ping-p w)
    "Returns true if window W supports the _NET_WM_PING protocol."
    (window-supports-wm-protocol-p w '_NET_WM_PING))

  (define (ping-window w callback timeout-msecs)
    "Assuming that window W supports the _NET_WM_PING protocol (i.e. the
function `window-supports-ping-p' returns true), initiate a ping sequence.

The function CALLBACK will subsequently be called with a single
argument, true if the window responded to the ping within TIMEOUT-MSECS
milliseconds, false otherwise."

    (let ((ping (make-ping-record callback (x-server-timestamp))))
      (send-client-message w 'WM_PROTOCOLS
			   (vector (x-atom '_NET_WM_PING)
				   (ping-timestamp ping)))
      (setq pings-in-transit (cons ping pings-in-transit))
      (make-timer (lambda ()
		    ;; Ensure that this ping hasn't already come back
		    (when (memq ping pings-in-transit)
		      (setq pings-in-transit (delq ping pings-in-transit))
		      (callback nil)))
		  (quotient timeout-msecs 1000) (mod timeout-msecs 1000))))

;;; event handler

  (define (client-message-handler w type data)
    (declare (unused w))
    (when (and (eq type 'WM_PROTOCOLS)
	       (eq (x-atom-name (aref data 0)) '_NET_WM_PING))
      ;; a returning ping (pong?)
      (let ((timestamp (aref data 1)))
	(let loop ((rest pings-in-transit))
	  (cond ((null loop)
		 (format
		  standard-error "Received stray _NET_WM_PING: %s\n" data))

		((eql (ping-timestamp (car rest)) timestamp)
		 ;; found our ping
		 (let ((this (car rest)))
		   (setq pings-in-transit (delq this pings-in-transit))
		   ((ping-callback this) t)))

		(t (loop (cdr rest))))))
      t))

  (add-hook 'client-message-hook client-message-handler))
