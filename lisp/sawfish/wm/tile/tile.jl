
(define-structure sawfish.wm.tile
    (export align-workspace-windows
            tile-workspace
            untile-window
            next-tiling
            col-tiling
            tall-tiling
            tall-rotate-left
            tall-rotate-right
            increase-max-windows
            decrease-max-windows
	    increase-max-cols
	    decrease-max-cols)
    (open rep
          rep.system
          sawfish.wm.tile.utils
          sawfish.wm.tile.tiler
          sawfish.wm.tile.tall
          sawfish.wm.tile.col))
