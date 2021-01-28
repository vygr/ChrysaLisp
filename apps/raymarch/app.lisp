;jit compile apps native functions
(import "sys/lisp.inc")
(jit "apps/raymarch/" "lisp.vp" '("tile"))

;imports
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/farm.inc")

(structure '+event 0
	(byte 'close+))

(structure '+select 0
	(byte 'main+ 'task+ 'reply+ 'timer+))

(defun child-msg (reply &rest _)
	(cat reply (apply cat (map (# (char %0 (const long_size))) _))))

(defq canvas_width 800 canvas_height 800 canvas_scale 1
	timer_rate (/ 1000000 1) retry_timeout 5000000 id t dirty nil
	select (list (task-mailbox) (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
	jobs (map (lambda (y) (child-msg (elem +select_reply+ select)
			0 y (* canvas_width canvas_scale) (inc y)
			(* canvas_width canvas_scale) (* canvas_height canvas_scale)))
		(range (dec (* canvas_height canvas_scale)) -1)))

(ui-window mywindow ()
	(ui-title-bar _ "Raymarch" (0xea19) +event_close+)
	(ui-canvas canvas canvas_width canvas_height canvas_scale))

(defun tile (canvas data)
	; (tile canvas data) -> area
	(defq data (string-stream data) x (read-int data) y (read-int data)
		x1 (read-int data) y1 (read-int data) yp (dec y))
	(while (/= (setq yp (inc yp)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(.-> canvas (:set_color (read-int data)) (:plot xp yp)))
		(task-sleep 0))
	(* (- x1 x) (- y1 y)))

;native versions
(ffi tile "apps/raymarch/tile" 0)

(defun dispatch-job (child)
	;send another job to child
	(when (defq val (. farm :find child))
		(cond
			((defq job (pop jobs))
				(.-> val
					(:insert :job job)
					(:insert :timestamp (pii-time)))
				(mail-send child job))
			(t	;no jobs in que
				(.-> val
					(:erase :job)
					(:erase :timestamp))))))

(defun create (nodes)
	; (create nodes)
	;function called when entry is created
	(open-task "apps/raymarch/child.lisp" (elem (random (length nodes)) nodes)
		kn_call_child (elem +select_task+ select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(mail-send key "")
	(when (defq job (. val :find :job))
		(push jobs job)
		(. val :erase :job)))

(defun main ()
	;add window
	(.-> canvas (:fill +argb_black+) :swap)
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(defq farm (Farm create destroy (* 2 (length (mail-nodes)))))
	(mail-timeout (elem +select_timer+ select) timer_rate)
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main+)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) +event_close+)
						;close button
						(setq id nil))
					(t (. mywindow :event msg))))
			((= idx +select_task+)
				;child launch responce
				(defq child (slice (const long_size) (const (+ long_size net_id_size)) msg))
				(. farm :insert child (emap))
				(dispatch-job child))
			((= idx +select_reply+)
				;child responce
				(dispatch-job (slice (dec (neg net_id_size)) -1 msg))
				(setq dirty t)
				(tile canvas msg))
			(t	;timer event
				(mail-timeout (elem +select_timer+ select) timer_rate)
				(. farm :refresh retry_timeout)
				(when dirty
					(setq dirty nil)
					(. canvas :swap)
					(when (= 0 (length jobs))
						(defq working nil)
						(. farm :each (lambda (key val)
							(setq working (or working (. val :find :job)))))
						(unless working (. farm :close)))))))
	;close window and children
	(. farm :close)
	(each mail-free-mbox (slice 1 -1 select))
	(. mywindow :hide))
