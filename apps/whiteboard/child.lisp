;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'dlist 0
	(byte 'mask 'rate 'commited_canvas 'overlay_canvas 'commited_polygons 'overlay_polygons))

;read args from parent (shared dlist tuple)
(defq dlist (mail-read (task-mailbox)))

(defun-bind fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0 0 mode _))

(defun-bind redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (tuple-get dlist_mask dlist) 1))
		(defq canvas (tuple-get dlist_commited_canvas dlist))
		(canvas-fill canvas 0)
		(each (lambda ((col poly)) (fpoly canvas col 1 poly)) (tuple-get dlist_commited_polygons dlist))
		(canvas-swap canvas))
	(when (/= 0 (logand (tuple-get dlist_mask dlist) 2))
		(defq canvas (tuple-get dlist_overlay_canvas dlist))
		(canvas-fill canvas 0)
		(each (lambda ((col poly)) (fpoly canvas col 1 poly)) (tuple-get dlist_overlay_polygons dlist))
		(canvas-swap canvas))
	(tuple-set dlist_mask dlist 0))

(defun-bind main ()
	;until quit
	(until (mail-poll (array (task-mailbox)))
		(redraw dlist)
		(task-sleep (tuple-get dlist_rate dlist)))
	(mail-read (task-mailbox)))
