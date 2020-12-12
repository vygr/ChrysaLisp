;imports
(import "apps/whiteboard/app.inc")

;read args from parent (shared dlist tuple)
(defq dlist (mail-read (task-mailbox)) flatten (elem +dlist_flatten+ dlist))

(defun fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(. canvas :set_color col)
	(. canvas :fpoly 0.0 0.0 mode _))

(defun redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (elem +dlist_mask+ dlist) 1))
		(defq canvas (elem +dlist_commited_canvas+ dlist))
		(. canvas :fill 0)
		(each (lambda ((col poly)) (fpoly canvas col 1 poly)) (elem +dlist_commited_polygons+ dlist))
		(. canvas :swap))
	(when (/= 0 (logand (elem +dlist_mask+ dlist) 2))
		(defq canvas (elem +dlist_overlay_canvas+ dlist))
		(. canvas :fill 0)
		(each (lambda (p)
			(bind '(col poly) (flatten p))
			(fpoly canvas col 1 poly)) (elem +dlist_overlay_paths+ dlist))
		(. canvas :swap))
	(elem-set +dlist_mask+ dlist 0))

(defun main ()
	;until quit
	(until (mail-poll (list (task-mailbox)))
		(redraw dlist)
		(task-sleep (elem +dlist_rate+ dlist)))
	(profile-report "Whiteboard Child"))
