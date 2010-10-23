(define-structure sawfish.wm.tabs.util

	(export set-tab-adjustments)

	(open rep
	      rep.system

	      sawfish.wm.gaol)

     (define-structure-alias tab-util sawfish.wm.tabs.util)

  (define (set-tab-adjustments #!key left-dec right-dec left-margin right-margin left-margin-transient right-margin-transient)
    (if left-dec
        (defvar-setq tab-left-dec-width left-dec))
    (if right-dec
	(defvar-setq tab-right-dec-width right-dec))
    (if left-margin
	(defvar-setq tab-left-margin left-margin))
    (if right-margin
        (defvar-setq tab-right-margin right-margin))
    (if left-margin-transient
        (defvar-setq tab-left-margin-transient left-margin-transient))
    (if right-margin-transient
        (defvar-setq tab-right-margin-transient right-margin-transient)))

  (gaol-add set-tab-adjustments))