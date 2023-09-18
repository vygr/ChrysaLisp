(import "././login/env.inc")
;jit compile apps native functions
(import "sys/lisp.inc")
(jit "apps/mandelbrot/" "lisp.vp" '("tile"))

(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defq +width 800 +height 800 +scale 2 +timer_rate (/ 1000000 1) id :t dirty :nil
	center_x (mbfp-from-fixed -0.5) center_y (mbfp-from-fixed 0.0) zoom (mbfp-from-fixed 1.0)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 50000000 5000000)
	jobs :nil farm :nil)

(ui-window *window* ()
	(ui-title-bar _ "Mandelbrot" (0xea19) +event_close)
	(ui-canvas canvas +width +height +scale))

(defun tile (canvas data)
	; (tile canvas data) -> area
	(defq data (string-stream data) x (read-int data) y (read-int data)
		x1 (read-int data) y1 (read-int data) yp (dec y))
	(while (/= (setq yp (inc yp)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(defq r (read-char data) r (if (= r 255) 0 r)
				g (<< (logand r 0x7f) 9) b (<< (logand r 0x3f) 2))
			(.-> canvas (:set_color (+ +argb_black (<< r 16) g b)) (:plot xp yp)))
		(task-slice))
	(* (- x1 x) (- y1 y)))

;native versions
(ffi tile "apps/mandelbrot/tile" 0)

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> job
					(+job_key key)
					(+job_reply (elem-get +select_reply select)))))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/mandelbrot/child.lisp" (elem-get (random (length nodes)) nodes)
		+kn_call_child key (elem-get +select_task select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun reset ()
	(if farm (. farm :close))
	(mail-free-mbox (elem-get +select_reply select))
	(elem-set +select_reply select (mail-alloc-mbox))
	(setq jobs (map (lambda (y)
			(setf-> (str-alloc +job_size)
				(+job_x 0)
				(+job_y y)
				(+job_x1 (* +width +scale))
				(+job_y1 (inc y))
				(+job_w (* +width +scale))
				(+job_h (* +height +scale))
				(+job_cx center_x)
				(+job_cy center_y)
				(+job_z zoom)))
			(range (dec (* +height +scale)) -1))
		farm (Farm create destroy (* 2 (length (lisp-nodes))))))

(defun main ()
	(defq select (alloc-select +select_size))
	(.-> canvas (:fill +argb_black) (:swap 0))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(reset)
	(mail-timeout (elem-get +select_timer select) +timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((and (= id (. canvas :get_id))
							(= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;mouse click on the canvas view, zoom in/out, re-center
						(bind '(w h) (. canvas :get_size))
						(defq rx (- (getf msg +ev_msg_mouse_rx) (/ (- w +width) 2))
							ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2)))
						(setq center_x (+ center_x (mbfp-offset rx +width zoom))
							center_y (+ center_y (mbfp-offset ry +height zoom))
							zoom (mbfp-mul zoom (if (= 0 (logand (getf msg +ev_msg_mouse_buttons) 2))
								(mbfp-from-fixed 0.5) (mbfp-from-fixed 2.0))))
						(reset))
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
				(mail-timeout (elem-get +select_timer select) +timer_rate 0)
				(. farm :refresh +retry_timeout)
				(when dirty
					(setq dirty :nil)
					(. canvas :swap 0)
					(when (= 0 (length jobs))
						(defq working :nil)
						(. farm :each (lambda (key val)
							(setq working (or working (get :job val)))))
						(unless working (. farm :close)))))))
	;close window and children
	(. farm :close)
	(free-select select)
	(gui-sub *window*))
