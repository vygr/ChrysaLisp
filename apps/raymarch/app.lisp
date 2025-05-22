(import "././login/env.inc")
;jit compile apps native functions
(jit "apps/raymarch/" "lisp.vp" '("tile"))

(import "gui/lisp.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defq +width 800 +height 800 +scale 1
	+timer_rate (/ 1000000 1) id :t dirty :nil
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 50000000 5000000)
	jobs (map (lambda (y)
			(setf-> (str-alloc +job_size)
				(+job_x 0)
				(+job_y y)
				(+job_x1 (* +width +scale))
				(+job_y1 (inc y))
				(+job_w (* +width +scale))
				(+job_h (* +height +scale))))
		(range (dec (* +height +scale)) -1)))

(ui-window *window* ()
	(ui-title-bar _ "Raymarch" (0xea19) +event_close)
	(ui-canvas canvas +width +height +scale))

(defun tile (canvas data)
	; (tile canvas data) -> area
	(defq data (string-stream data) x (read-int data) y (read-int data)
		x1 (read-int data) y1 (read-int data) yp (dec y))
	(while (/= (setq yp (inc yp)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(.-> canvas (:set_color (read-int data)) (:plot xp yp)))
		(task-slice))
	(* (- x1 x) (- y1 y)))

;native versions
(ffi "apps/raymarch/tile" tile)

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> job
					(+job_key key)
					(+job_reply (elem-get select +select_reply)))))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/raymarch/child.lisp" (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun main ()
	(defq select (alloc-select +select_size))
	(.-> canvas (:fill +argb_black) (:swap 0))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(defq farm (Farm create destroy (* 2 (length (lisp-nodes)))))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					(:t (. *window* :event msg))))
			(+select_task
				;child launch responce
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(def val :child child)
					(dispatch-job key val)))
			(+select_reply
				;child responce
				(defq key (get-long msg (- (length msg) +long_size)))
				(when (defq val (. farm :find key))
					(dispatch-job key val))
				(setq dirty :t)
				(tile canvas msg))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(. farm :refresh +retry_timeout)
				(when dirty
					(setq dirty :nil)
					(. canvas :swap 0)
					(when (= 0 (length jobs))
						(defq working :nil)
						(. farm :each (lambda (key val)
							(setq working (or working (get :job val)))))
						(unless working
							(mail-timeout (elem-get select +select_timer) 0 0)
							(. farm :close)))))))
	;close window and children
	(. farm :close)
	(free-select select)
	(gui-sub-rpc *window*))
