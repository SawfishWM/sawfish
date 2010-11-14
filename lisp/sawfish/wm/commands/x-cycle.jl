;; x-cycle.jl -- stack-based window cycling

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; Cycles through windows in MRU order. Whichever key is used to invoke
;; `cycle-windows' will continue to the next window on the stack when
;; pressed again. Releasing the initial modifier ends the cycling and
;; selects the current window

;; Thanks to Kuba Winnicki <blackwine@optimus.wroc.pl> for the idea:

;; [ assumes command invoked by MOD-KEY, i.e. M-TAB by default ]

;; MOD held;

;; * window 1            window 1            window 1          * window 3
;;   window 2    ==>   * window 2    ==>     window 2    ==>     window 1
;;   window 3    KEY     window 3    KEY   * window 3  release   window 2
;;   window 4            window 4            window 4    MOD     window 4

;; If cycle-raise-windows is enabled, focused window is brought to
;; front, but gets back to original placement in order when it's
;; defocused.

;; Also it'd be nice to be able to cycle through hidden windows as
;; well, like here:

;; MOD held;

;; * window 1            window 1            window 1          * window 3
;;  [window 2]   ==>   * window 2    ==>    [window 2]   ==>     window 1
;;  [window 3]   KEY    [window 3]   KEY   * window 3  release  [window 2]
;;   window 4            window 4            window 4    MOD     window 4

;; Each window may have an x-cycle-order property, an integer id        ----------   obsolete !!!
;; defining its position in the window stack, higher numbers equal
;; more recently selected. The ids may not be contiguous

;; Obviously there would be a problem when we overflow rep's integers, but
;; every now and then we compress the stack to make the ids contiguous

;; It might seem as though it should be possible to use the actual
;; window stacking to define MRU order. But since there are multiple
;; layers of windows this wouldn't work (selected windows may not reach
;; the top of the stack)

(define-structure sawfish.wm.commands.x-cycle
    
    (export define-cycle-command
            define-cycle-command-pair
            event-is-modifier?          ;move elsewhere!
            event-is-release?
            windows->ws
            restack-atomically          ;; todo: move it in another module!
            )

    (open rep
	  rep.system
	  rep.regexp
          rep.io.files
	  rep.io.timers

          rep.mmsystem
          sawfish.wm
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.util.window-order
	  sawfish.wm.util.keymap
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.focus
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.viewport-hi
	  sawfish.wm.util.stacking
	  sawfish.wm.events
	  sawfish.wm.util.decode-events
	  sawfish.wm.util.groups
	  sawfish.wm.util.rects
	  sawfish.wm.util.display-window

          rep.trace
          ;;mmc.my-simple                         ;for  push!   i should resume using the code there.
          )
  (define-structure-alias x-cycle sawfish.wm.commands.x-cycle)

  (defvar debug-x-cycle 0 "0 don't print tracing info.")
  (define debug #t "used by rep.trace macros")

;;rep.mmc: 
  (defmacro push! (symbol item)
    `(setq ,symbol (cons ,item ,symbol)))

  (define (event-is-modifier? ev)
    (modifier-keysym-p (nth 2 ev)))

  (define (request-another-key-event)
    ;; keys.c does it automatically:
    ;; (allow-events 'sync-keyboard)
    1)

  (define (event-is-release? ev)
    (memq 'release (nth 1 ev)))

;; i change these settings during the ..
  (define repeat-delay 190)                ;390


;; we bind to disable-auto-raise and tooltips-enabled
  (eval-when-compile (require 'sawfish.wm.ext.auto-raise))
  (eval-when-compile (require 'sawfish.wm.ext.tooltips))

;;; customization options

;;###autoload (defgroup cycle "Window Cycling" :group focus :require sawfish.wm.commands.x-cycle)
  (defgroup cycle "Window Cycling"
    :group focus
    :require sawfish.wm.commands.x-cycle)

  (defcustom cycle-show-window-names t
    "Display window names and icons while cycling through windows."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-include-iconified t
    "Include iconified windows when cycling."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-all-workspaces nil
    "Include windows on all workspaces when cycling."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-all-viewports nil
    "Include windows on all viewports when cycling."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-raise-windows t
    "Raise windows while they're temporarily selected during cycling."
    :type boolean
    :group (focus cycle))

  (defcustom cycle-keymap (make-keymap)
    "Keymap containing bindings active only during window cycling operations."
    :group bindings
    :type keymap)

  (defvar after-cycle-step-hook '()
    "Window hook called after each step of window cycling.")

;; variables

;; the current window
  (define x-cycle-current (make-fluid))

;; the original stacking order
  (define x-cycle-stacking (make-fluid))

;; the list of windows being cycled through
  (define x-cycle-windows (make-fluid))

;; is this call during a cycle operation
  (defvar x-cycle-active (make-fluid))

;; list of all cycle command names
  (define cycle-commands '())

  (define x-cycle-workspaces (make-fluid))

;;; code

  '(define (forwards lst elt count)     ;O(n) !!!
     ;; emulate a ring ?
     (let ((total (length lst))         ;why not (- leght (lentth (member ))) ???
           (current (let loop ((rest lst) (i 0)) ;;  if not found ->0 else the index of ELT
                         (cond ((null rest) 0)
                               ((eq (car rest) elt) i)
                               (t (loop (cdr rest) (1+ i)))))))
       (nth (mod (+ current count) total) lst)))


;; mmc:
  (define (forwards lst elt count)      ;O(n) !!!
    ;; this is Modulo/cyclic
    (let ((len (length lst)))
      (unless (zerop debug-x-cycle)
        (DB "forwards: list of %d elements, we start at %d, skip %d\n" len
            (- len (length (member elt lst)))
            count))
      (nth (mod (+ count (- len (length (member elt lst)))) len)  lst)))

  (define (cycle-display-message)
    (require 'sawfish.wm.util.display-wininfo)
    (display-wininfo (fluid x-cycle-current)
		     (if cycle-all-workspaces
			 (format #f "ALL: (%s)" (workspace-name current-workspace))
		       "()")))

  (define (remove-message)
    (require 'sawfish.wm.util.display-wininfo)
    (display-wininfo nil))


  ;;
  (define (windows->ws windows)
    "the list of windows defines an order(ing). Make a projection to workspaces and
return a list of workspaces in the order of the maximal order of a window (from the list) in it.
Workspaces are just the indexes."
    (DB "windows->ws\n")
    (let* ((ws-list '())
           ;; make a bitmap & accessors:
           (limits (workspace-limits))
           (min    (car limits))
           (bitmap (make-vector (+ 1 (- (cdr limits)
                                        min))
                                #f))
           (ws-present-p (lambda (ws)
                           (aref bitmap (- ws min))))
           (ws-present! (lambda (ws)
                          (aset bitmap (- ws min) 't))))
      (mapc (lambda (w)
                                        ;(DB "windows->ws: adding %s!" (window-name w))
              (if (and (windowp w)      ; gone !!
                       (not (null (window-get w 'workspaces))))
                  (let ((ws (car (window-get w 'workspaces))))
                                        ;(if (null ws)
                
                    (unless (ws-present-p ws)
                                        ;(message (format #f "ws:%d \n" ws))
                      (push! ws-list ws)
                      (ws-present! ws)))
                (DB "the window %s is GONE, or on NO workspace!\n" (window-name w))))
            windows)
      (reverse ws-list)))

;; (windows->ws (window-order #f 't 't))



;; todo: move it in another module!
(defmacro restack-atomically (#!rest body)
  "stop restacking windows, run BODY, and then push all the changes in 1 X request."
  `(progn
    (set-restack-fast 1)
    (DB "x-cycle: restack-atomically!\n")
    (let ((current-stacking-on-server (stacking-order)))
      ,@body
      (set-restack-fast 0)
                                        ;(set-debug-stacking debug)
      ;;fixme: needed: sure, we could stop x-cycle now, and ...  but it could be moved after restack ?
      (commit-restacking (reverse current-stacking-on-server))
      ;;(restack-windows-fast)  ;; my previous version of pushing all restacking to the X server
      )))


  ;;  Select the next window to cycle-to:
(define (cycle-next windows count)
  (DB "cycle-next %d %s:%s  intersect with:  %d  and make %d steps.\n"
      count
      (if cycle-all-workspaces
          "WS-global"
        "WS-local")
      (if cycle-all-viewports
          "vp-global"
        "vp-local")
      (length windows)
      count)
  
    ;(DB  "cycle-next %s" (mapconcat window-name windows "\n"))
                                        ;(format (stderr-file) "cycle-next\n")
    ;; we keep windows even outside window-order !!!!
    (fluid-set x-cycle-windows windows)
    (let ((win (window-order (if cycle-all-workspaces
                                 nil
                               current-workspace)
                             cycle-include-iconified cycle-all-viewports)))
      (setq win (delete-if (lambda (w)  ; --- O(n^2)  ?
                             (not (memq w windows))) win))

      '(DB "cycle-next after delete-if: %s" (mapconcat window-name win "\n"))
      ;;; win ??? 
      (unless win
        (DB "cycle-next: no windows in the intersection!\n")
        (throw 'x-cycle-exit t))
      ;; here starts the real  cycle-next
      (cycle-to win count)))


;;  win-list, count, and current-position (in the list, or 0)....

;; it ends w/  set-input-focus
  (define (cycle-to win count)
    '(message (format #f "cycle-to %s" (mapconcat window-name win "\n")))
    '(display-message (format #f "cycle-to %d" count))
    (DB "cycle-to--------------\n")
    ;; process the `previous' window
    (if (fluid x-cycle-current)
        (progn
          (DB "x-cycle-current: %s\n" (window-name (fluid x-cycle-current)))
          ;; mmc:   this works for my  modulo, but not for simple x-cycle !!!
          ;; here's the problem: the code below skips always(?) the 1st window.
	  ;; But our list, when we skip to another WS
          ;; is incompatible w/ such operation. So we fake it
          (unless
              (member (fluid x-cycle-current)
                      win)
            (setq win (cons (fluid x-cycle-current) win)))
          
          (when (or (window-get (fluid x-cycle-current) 'iconified)
                    ;; how could this happen?
                    (not (window-appears-in-workspace-p
                          (fluid x-cycle-current) current-workspace)))
            (hide-window (fluid x-cycle-current))))


      ;; `first' call, push the currently focused window onto
      ;; the top of the stack
      (if (input-focus)
          (progn
            (DB "setting x-cycle-current to %s\n" (window-name (input-focus)))
            (fluid-set x-cycle-current (input-focus))
            ;; i think it's useless:

            (unless (zerop debug-x-cycle)
              (DB "cycle-to: window-order-push: %s: %d\n"
                  (window-name (fluid x-cycle-current))
                  (window-get (fluid x-cycle-current) 'order)))
            (window-order-push (fluid x-cycle-current)) ;mmc! w/o this it works too!!!!!

            ;; add the `originating' window at the head.
            ;; mmc: why ??
            '(if (member (fluid x-cycle-current) win)
                 (setq win (cons (fluid x-cycle-current)
                                 (delq (fluid x-cycle-current) win)))
               (setq count (- count 1)))

            (setq win (cons (fluid x-cycle-current)
                            (delq (fluid x-cycle-current) win))))
        (progn
          (beep)
          (unless (zerop debug-x-cycle)
              (DB "cycle-to: no input-focus!\n")))
        ))

    ;; `restore' the original stacking.
    ;; choose the `next' window
    (if (fluid x-cycle-current)
        (progn
          (unless (zerop debug-x-cycle)
            (DB "cycle-to: i am confused: going FORWARD\n"))
          (setq win (forwards win (fluid x-cycle-current) count)))
      (progn
        (unless (zerop debug-x-cycle)
          (DB "cycle-to: i am confused: taking HEAD\n"))
        (setq win (car win))))
    ;; reposition ws/vp:
    (fluid-set x-cycle-current win) ; ???


    (print-debug "Showing the window %s!\n" (window-name win))

    (let ((do-restacking
           (lambda ()
             ;; take a snapshot of what we have now. (on the server)
             (restack-atomically
              (when (fluid x-cycle-stacking)
                (restack-windows (fluid x-cycle-stacking)) ;  restore the  original order ??
                (fluid-set x-cycle-stacking nil))          ;no more !
              

              ;; [18 feb 05]
              ;; raise 
              (when cycle-raise-windows
                ;; this might be always the same?
                (fluid-set x-cycle-stacking (stacking-order)) ; why:  we have possibly changed workspace, viewport.

                ;; bug: we have just exposed windows in the ws/vp, but only now we raise the window!
                (DB "now raising window %s\n" (window-name win))
                (raise-window* win))))))   ;   what else ?
      ;;---------------------
      (print-debug "Showing the window %s!\n" (window-name win))
      ;; fixme:  2 fase ???
      (with-server-grabbed
       (if (not (window-get win 'sticky))
	   (progn
	     (DB "cycle-to: select-workspace %d -> %d %s\n"
		 current-workspace
		 (nearest-workspace-with-window ; ---- the last one used w/ that window !!
		  win current-workspace)
		 (window-name win))
	     (if (= current-workspace
		    (nearest-workspace-with-window ; ---- the last one used w/ that window !!
		     win current-workspace))
		 (progn
		   (do-restacking)
		   (when (window-get win 'iconified)
		     (uniconify-window win))
		   ;;(show-window win))           ;does this break?
		   (move-viewport-to-window win)) ;(sync-server)
	       (progn
		 (set-future-input-focus win)
		 (select-workspace (nearest-workspace-with-window
				    ;; ---- the last one used w/ that window !!
				    win current-workspace)
				   #t	;dont-focus ???
				   (lambda ()
				     (do-restacking)
				     (when (window-get win 'iconified)
					;(show-window win)) ;does this break?
				       (uniconify-window win)
				       ;; (sync-server)
				       (DB "should be seen!\n")
				       )
				     ;; shouldn't this be inner-thunk ???
				     (grab-keyboard nil nil t) ; does this mean we can ignore `unmap-fun' ? almomst.
				     (move-viewport-to-window win)))))
       
	     )
	 (progn
	   (do-restacking)
	   (when (window-get win 'iconified)
	     (show-window win))		;does this break?
	   ))))


    ;; inform
    (when cycle-show-window-names
      (cycle-display-message))

    ;; WHY ???   ... replay key ?
    (when (window-really-wants-input-p win)
      (set-input-focus win))
    (request-another-key-event))

  (define (cycle-begin windows step)
    "Cycle through all windows in order of recent selections."
    ;; input-focus ??? i should start from the event window!
    (let ((tail-command nil)
          (grab-win (input-focus)))
      ;; <-------- key GRABBED for that window. When we lose(hide) it, we have to re-grab !!
      
      (DB "cycle-begin: departure from %s, skipping %d\n"
          (if grab-win
              (window-name grab-win)
            "<no window>")
          step)
      (let-fluids ((x-cycle-current nil)
                   (x-cycle-stacking nil) ;  stacking order kept between 2 keyboard actions! I.e. on the next
                   ;; it is sort of the original order, with modifications due to workspace changes.
                   ;; (and newly mapped windows) ???
                   ;; i don't like it much:  b/c  group raising propagates to other WS (for now).
                   (x-cycle-windows windows)
                   (x-cycle-active t)
                   (x-cycle-workspaces  ; the function _must_ ....
                    (if (functionp windows)
                        '()
                      (windows->ws windows)))) ; i have to compute it now, at the beginning of x-cycling, b/c we might need it:
                                        ; if user wants to skip WSs.

        ;; should it be a hook ??
        ;; enter, after the fluids have been created !!

        ;; unmap-notify-hook
        (define (unmap-fun w)
          (message "x-cycle:unmap-fun: %a\n" (window-name w))

          ;; fixme: we should remove from the x-cycle-stacking
          
          '(when (eq w grab-win)         ;mmc: this is another awful hack: we must _prevent_ losing grab!
            (setq grab-win nil)
            ;; regain the keyboard:
            (or (grab-keyboard nil nil t)
                (progn
                  (beep)
                  (throw 'x-cycle-exit nil)))
            (request-another-key-event)))
                                        ; mmc:  is it correct?  maybe we have already issuead such, and an event is on the way to
                                        ; in fact i don't think it's correct!

        (define (map-fun w)             ;see `focus-dont-push' below !!!
          ;; with the hooks, the window is put to stacking order, and even order.
          ;; But we are keeping our copy!
                                        ;(message (format #f "x-cycle:map-fun: %a\n" (window-name w)))
          (DB "x-cycle: new window mapped %s\n" (window-name w))
          ;; fluid-push!


          ;; i want to provoke mess!
          '(if (memq w (fluid x-cycle-stacking))
              (progn
                ;(beep)
                (DB "but the window is already in the saved stacking list!"))
             )
            ;;  trying conditional!
          (fluid-set x-cycle-stacking
                     (cons w (fluid x-cycle-stacking))) ;)
          ;; this is not enough.  We need the window-order as well !
          ;; [06 dic 04] but the window is still not considered by the cycling itself!
          (window-order-push w)
          (DB "new window:  %s\n" (mapcar window-name (fluid x-cycle-stacking))))
                                       ;x-cycle-current

  
;; enter-workspace-hook  ??
        (define (enter-fun space)       ; why ??
          (declare (unused space))

          ;; mmc: but, i grab on the root_window, or, better yet on the no_focus_window!!! this is useless then!
          '(when grab-win
            (setq grab-win nil)
            (DB "enter-fun: we have to regrab !!!!")
            (or (grab-keyboard nil nil t)
                (progn
                  (beep)
                  (throw 'x-cycle-exit nil)))
            (request-another-key-event))) ;mmc: once again! i don't like this!



;; run when unknown key is pressed ?
        (define (unbound-fun)
          (unless (zerop debug-x-cycle)
            (DB "x-cycle: unbound-fun..."))
          (let* ((event (current-event))
                 (ev (decode-event event)))
            ;; ---> (key (release hyper) Hyper_L)
            ;; (key (release control) Control_L)
            (cond
             ;;;  RELEASE
             ((event-is-release? ev) ;; key released
              (unless (zerop debug-x-cycle)
                (DB "x-cycle: release...\n"))

              ;; is it `our' modifier ???   ... the last !!!! one.
              (when (and (event-is-modifier? ev) ; modifier released
                         (eq 2 (length (nth 1 ev)))) ; 'release + 1 modifier
                (throw 'x-cycle-exit t))
              (request-another-key-event))

             ;; so `PRESS' & 
             ((not (event-is-modifier? ev)) 
              ;; real key pressed:    try  cycle-keymap, then try as usual (global-keymap & window + ???)
              (let* ((override-keymap cycle-keymap)
                     (command (lookup-event-binding event)))
                (unless command
                  ;; search cycle-keymap then the usual ones
                  (setq override-keymap nil)
                  (setq command (lookup-event-binding event))) ;mmc:  why doesn't lookup-event-binding accept a keymap argument?
                
                (if (memq command cycle-commands)
                    ;; call without aborting cycle operation
                    (progn
                      (current-event-window (fluid x-cycle-current)) ; it's not focused?
                      
                      (unless (zerop debug-x-cycle)
                        (DB "x-cycle: call-command:\n"))
                      
                      (call-command command)
                      ;;(request-another-key-event)   ; why not?
                      )
                  ;; else:
                  (unless (setq tail-command command)
                    (unless (zerop debug-x-cycle)
                      (DB "x-cycle: exiting with tail-command:\n"))
                    ;; no wm binding, so forward the event to
                    ;; the focused window (this is why we have
                    ;; to grab the keyboard synchronously)
                    (allow-events 'replay-keyboard))

                  (throw 'x-cycle-exit nil)))) ; mmc: but what happens ? 

             (t ;;else   ... fixme:  when ???
              (unless (zerop debug-x-cycle)
                (DB "x-cycle: pressing a modifier. ignoring.\n"))
              (request-another-key-event)))))


        (DB "evaluating release events from now\n")
        (let* ((decoded (decode-event (current-event))) ; this is just for a check & finding the modifiers.
               (eval-modifier-events t)
               (eval-key-release-events t)
               (override-keymap (make-keymap)) ; the trick !!!    we send _all_ events to  unbound-fun.
               (focus-dont-push t)      ; --------------- !!!
               ;; i need it!!

               (disable-auto-raise t)
               (tooltips-enabled nil)
               ;; do these overload the Global variable ??            
                                        ;(unmap-notify-hook (cons unmap-fun unmap-notify-hook)) ; .... we have to pay _more_ attention:
                                        ;(map-notify-hook (cons map-fun map-notify-hook))
                                        ;(enter-workspace-hook (cons enter-fun enter-workspace-hook)) ;.... 

               (unbound-key-hook (list unbound-fun)))
          ;; not re-entrant???
          ;; what if i exit non-locally!
          (unwind-protect
              (progn
                (mm-add-hook 'unmap-notify-hook unmap-fun #f 'x-cycle)
                (mm-add-hook 'map-notify-hook map-fun #f 'x-cycle)
                (mm-add-hook 'enter-workspace-hook enter-fun #f 'x-cycle)

                (unless
                    (and (or (eq 'key (car decoded))
                             (eq 'mouse (car decoded))) ; mmc
                         (nth 1 decoded)) ;???
                  (error "%s must be bound to a key event with modifiers."
                         this-command))

      ;;; hook:
                (if (functionp windows)
                    (setq windows (windows)))

                ;; grab synchronously, so that event replaying works
                                        ;(when (grab-keyboard grab-win nil t)
                (call-with-keyboard-grabbed ;mmc: [08 feb 05]  *  (list grab-win nil t)
                  (lambda ()
                    (unwind-protect
                        (progn
                          (catch 'x-cycle-exit
                            ;; do the first step
                            (cycle-next windows step)
                            (setq focus-ignore-pointer-events t) ;???
                            (sync-server) ;???
                            (allow-events 'sync-keyboard) ;
                            (recursive-edit))
                          (if (fluid x-cycle-current)
                              (progn
                                (DB "x-cycle: exiting ....%s\n" (window-name (fluid x-cycle-current)))
                                (display-window (fluid x-cycle-current)))
                            (DB "x-cycle: exiting, but no final window selected!\n"))) ;???
                      (remove-message)
                                        ;(ungrab-keyboard)
                      (make-timer (lambda () ; why timer ??
                                    (setq focus-ignore-pointer-events nil))
                                  0 100)))))
            ;; what if i exited non-locally!
            (mm-remove-hook-symbol 'unmap-notify-hook  'x-cycle)
            (mm-remove-hook-symbol 'map-notify-hook 'x-cycle)
            (mm-remove-hook-symbol 'enter-workspace-hook  'x-cycle)))


        (when tail-command
          ;; make sure that the command operates on the newly-focused
          ;; window, not the window that was focused when the original
          ;; event was received
          (DB "running the non-cycle command on %s\n" (window-name (input-focus)))
          (current-event-window (input-focus))
          (call-command tail-command)))))




;;; Defining commands

  (define (define-cycle-command name body . rest)
    "Create a command that will not cause the current cycle operation
to abort before execution.

All arguments are passed to define-command."
    (unless (memq name cycle-commands)
      (setq cycle-commands (cons name cycle-commands)))	;we keep a list of "priviledged" commands
    (apply define-command name body rest))


;; inside cycling all the commands  apply selector & (cycle-next)
;;  But the selector can be a constant list ? no. (nonsense)
  (define (define-cycle-command-pair forward-name reverse-name selector . rest)
    "Create a pair of commands for cycling through windows. The command named
FORWARD-NAME cycles forwards, while the command named REVERSE-NAME cycles
backwards.

SELECTOR is called when initializing the cycle environment, it should
return the list of windows to cycle through, or the symbol `t' to
denote all cyclable windows.

Any extra arguments are passed to each call to define-command."
    (define (command-body step)
      (lambda args
        (let ((windows (apply selector args))) ; <--- 1st step 
          (when windows
            (if (fluid x-cycle-active)	; if _already_ IN, 
                (cycle-next windows step)
              (unwind-protect
                  (progn                ;(system "xset -r") ;(message "xset -r")
                    ;(set-repeat-rate repeat-delay 1000)
                    ;(display-message "cycle-begin")
                    (cycle-begin windows step))
                                        ; (system "xset r")
                ;(set-repeat-rate repeat-delay 40) ; normal
                (display-message #f)))))))
;; so, a command is simply a selector.
    (when forward-name
      (apply define-cycle-command forward-name (command-body +1) rest))
    (when reverse-name
      (apply define-cycle-command reverse-name (command-body -1) rest)))

;;; commands

  (define-cycle-command-pair
    'cycle-windows 'cycle-windows-backwards
    (lambda () (filter-windows window-in-cycle-p)))

  (define-cycle-command-pair
    'cycle-group 'cycle-group-backwards
    (lambda (w)
      (delete-if-not window-in-cycle-p (windows-in-group w)))
    #:spec "%W")

(define (windows-on-different-workspace)
  "get the windows on next workspace in the window order: 
   the order of workspaces is constructed on the first call from `cycle-begin', which stores it in 
   the `x-cycle-workspaces' fluid to '() or ...."
  (unless (fluid x-cycle-workspaces)
    (fluid-set x-cycle-workspaces
               (windows->ws
                ;; why not ??  (filter-windows window-in-cycle-p)  ;; faster?
                (delete-if-not
                    window-in-cycle-p
                    (window-order #f cycle-include-iconified #t)))) ;cycle-all-viewports
    (unless (zerop debug-x-cycle)
      (DB "windows-on-different-workspace: the order of WS: %d: %s\n" current-workspace (fluid x-cycle-workspaces))))

  ;; go ahead in the WS cycle:
  (let* ((this-ws current-workspace)    ; (nearest-workspace-with-window w current-workspace))
         (ws (forwards (fluid x-cycle-workspaces) this-ws 1))
         (windows (delete-if-not window-in-cycle-p
                                        ;(workspace-windows ws cycle-include-iconified)
                      ;workspace-windows
                      (window-order ws cycle-include-iconified #t)))) ;all viewports!
                                        ;(filter-windows window-in-cycle-p)
    (unless (zerop debug-x-cycle)
      (DB "windows-on-different-workspace: changing WS: %d -> %d. There are %d windows\n" this-ws ws (length windows)))

    ;; if no windows on that workspace? impossible!
    (if (car windows)
        (unless (zerop debug-x-cycle)
          (DB "cycle-modulo-ws: top window: %s\n" (window-name (car windows)))))
                                        ;(setq cycle-all-workspaces #t) ; i want to ...
    windows))


;; bug:  sometime i cannot move away. dunno why.

;; modulo group/ WS ...
;; fixme: if no window is visible in the viewport (focused??)

;; mmc: i cannot use `define-cycle-command-pair', so i repeat all the code here:
  (define-cycle-command
    'cycle-modulo-ws                    ;'cycle-modulo-ws-backwards
    (lambda ()
<<<<<<< HEAD
      "Only cycle the top members of each group."
      (let loop
	  ((wl (window-order
		(if cycle-all-workspaces
		    nil
		  current-workspace)
		cycle-include-iconified cycle-all-viewports))
	   grps
	   retval)
	(when (and (window-in-cycle-p (car wl))
		   (not (memq (window-group-id (car wl)) grps)))
	  (when (window-group-id (car wl))
	    ;; Some windows don't have group.
	    (setq grps (cons (window-group-id (car wl)) grps)))
	  (setq retval (cons (car wl) retval)))
	(if (cdr wl)
	    (loop (cdr wl) grps retval)
	  (nreverse retval)
	  ))))

=======
      ;(unless (zerop debug-x-cycle) (DB "cycle-modulo-ws:\n"))
      (if (fluid x-cycle-active)        ; if _already_ IN,

          ;; dynamic scope!
          (let ((cycle-all-workspaces #t)
                (cycle-all-viewports #t))
            (DB "cycle-modulo-ws: running cycle-next\n")
            (cycle-next (windows-on-different-workspace) 1))


        ;; `begin' First invocation
        (let ((cycle-all-workspaces #t)
                (cycle-all-viewports #t))
          (DB "cycle-modulo-ws: entering cycle-begin\n")
          (unwind-protect
              (progn
                ;(set-repeat-rate repeat-delay 1000)
                (cycle-begin windows-on-different-workspace 1))
            ;(set-repeat-rate repeat-delay 40)              ; normal
            (display-message #f))))))

  ;;;
>>>>>>> replacing with my lisp files
  (define-cycle-command-pair
    'cycle-prefix 'cycle-prefix-backwards
    (lambda (w)
      (when (string-match "^([^:]+)\\s*:" (window-name w))
        (let* ((prefix (expand-last-match "\\1"))
               (re (concat ?^ (quote-regexp prefix) "\\s*:")))
          (delete-if-not window-in-cycle-p
              (filter-windows
               (lambda (x)
                 (string-match re (window-name x))))))))
    #:spec "%W")


;;; same class
  (define-cycle-command-pair
    'cycle-class 'cycle-class-backwards
    (lambda (w)
      (let ((class (window-class w)))
        (delete-if-not window-in-cycle-p
            (filter-windows
             (lambda (x) (equal (window-class x) class))))))
    #:spec "%W")


;; step given by the 1st command -> x-cycle-active
  (define-cycle-command-pair
    'cycle-step 'cycle-step-backwards
    (lambda ()
      (if (fluid x-cycle-active)        ; this doesn't work as the 1st (entering) command ! cycle must be already active
          (fluid x-cycle-windows)
        (error "%s must be bound to a key event in the cycle keymap."
               this-command))))


;; dock ?? gnome panel(s)
  (define-cycle-command-pair
    'cycle-dock 'cycle-dock-backwards
    (lambda ()
      (delete-if-not (lambda (x) (window-in-cycle-p x #:ignore-cycle-skip t))
          (filter-windows dock-window-p)))))

#| autoload cookies:

###autoload (autoload-command 'cycle-windows 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-windows-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-group 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-group-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-prefix 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-prefix-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-class 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-class-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-dock 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-dock-backwards 'sawfish.wm.commands.x-cycle)

|#

#| doc strings for the cycle commands:

::doc:sawfish.wm.commands.x-cycle#cycle-windows::
Cycle through all windows in order of recent selections.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-windows-backwards::
Cycle through all windows in reverse order of recent selections.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-group::
Cycle through all windows in the same group as the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-group-backwards::
Reverse cycle through all windows in the same group as the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-prefix::
Cycle through all windows whose names match the leading colon-delimited
prefix of the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-prefix-backwards::
Reverse cycle through all windows whose names match the leading
colon-delimited prefix of the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-class::
Cycle through all windows with the same class as the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-class-backwards::
Reverse cycle through all windows with the same class as the current
window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-step::
Step one window forwards through the current window cycle list.
This command should only be used in the cycle keymap.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-step-backwards::
Step one window backwards through the current window cycle list.
This command should only be used in the cycle keymap.
::end::

|#
