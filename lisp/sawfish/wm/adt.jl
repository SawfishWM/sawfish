;; Definition of VIEWPORT and WORKSPACE records.
;; I have to isolate it here (rather than in viewport.jl), because
;; reloading destroys the accessors to the old records.
;; So, it's quite impossible to reload

(define-structure sawfish.wm.adt
    (export
      make-viewport viewport?
      x-of y-of
      set-x! set-y!
      ;; 
      make-workspace workspace?
      viewport-of set-viewport!
      name-of set-name!
      )
    (open
     rep
     rep.system
     rep.data.records
     )
  
  (define-record-type :viewport
    (make-viewport) viewport?
    (x x-of set-x!)
    (y y-of set-y!))



  (define-record-type :workspace
    (make-workspace) workspace?
    (name name-of set-name!)			
    (viewport viewport-of set-viewport!)
    ;;(dimension dimension set-dimention!)	; and modifiers
    )
  )
