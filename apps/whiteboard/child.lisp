;imports
(import 'apps/whiteboard/app.inc)

;read args from parent (shared dlist tuple)


(defun-bind fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0.0 0.0 mode _))

(defun-bind redraw (dlist)
	;redraw layer/s

		(canvas-fill canvas 0)

		(canvas-swap canvas))

		(canvas-fill canvas 0)
		(each (lambda (p)
			(bind '(col poly) (flatten p))

		(canvas-swap canvas))
	(elem-set +dlist_mask+ dlist 0))

(defun-bind main ()
	;until quit
	(until (mail-poll (array (task-mailbox)))
		(redraw dlist)

