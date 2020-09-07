;imports
(import 'apps/whiteboard/app.inc)

;read args from parent (shared dlist tuple)
(defq dlist (mail-read (task-mailbox)) flatten (elem-get +dlist_flatten+ dlist))

(defun-bind fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0.0 0.0 mode _))

(defun-bind redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (elem-get +dlist_mask+ dlist) 1))
		(defq canvas (elem-get +dlist_commited_canvas+ dlist))
		(canvas-fill canvas 0)
		(each (lambda ((col poly)) (fpoly canvas col 1 poly)) (elem-get +dlist_commited_polygons+ dlist))
		(canvas-swap canvas))
	(when (/= 0 (logand (elem-get +dlist_mask+ dlist) 2))
		(defq canvas (elem-get +dlist_overlay_canvas+ dlist))
		(canvas-fill canvas 0)
		(each (lambda (p)
			(bind '(col poly) (flatten p))
			(fpoly canvas col 1 poly)) (elem-get +dlist_overlay_paths+ dlist))
		(canvas-swap canvas))
	(elem-set +dlist_mask+ dlist 0))

(defun-bind main ()
	;until quit
	(until (mail-poll (array (task-mailbox)))
		(redraw dlist)
		(task-sleep (elem-get +dlist_rate+ dlist))))
