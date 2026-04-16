(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defun generate-lut ()
	(defq lut (list) i -1)
	(while (< (++ i) 255)
		(defq
			; Frequency
			f 0.1 fi (n2f i)
			; Phase shifts for R, G, B
			r (n2i (+ (* (sin (* f fi)) 127.0) 128.0))
			g (n2i (+ (* (sin (+ (* f fi) 2.0)) 127.0) 128.0))
			b (n2i (+ (* (sin (+ (* f fi) 4.0)) 127.0) 128.0)))
		; Combine into ARGB, force Alpha 0xFF
		(push lut (char (+ 0xFF000000 (<< r 16) (<< g 8) b) +int_size)))
	; Set index 255 to Black
	(push lut (char +argb_black +int_size)))

(defq +width 800 +height 800 +job_rect_size 32 +scale 2
	+timer_rate (/ 500000 1) id :t dirty :nil
	center_x +real_-1/2 center_y +real_0 zoom +real_1
	+retry_timeout (task-timeout 5) jobs :nil farm :nil
	+mandel_lut `',(generate-lut))

(ui-window *window* ()
	(ui-title-bar _ "Mandelbrot" (0xea19) +event_close)
	(ui-canvas *canvas* +width +height +scale))

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
	(open-task (const (cat *app_root* "child.lisp")) (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun reset ()
	(mail-timeout (elem-get select +select_timer) 0 0)
	(if farm (. farm :close))
	(elem-set select +select_reply (mail-mbox))
	(setq jobs (list))
	(each (lambda (y)
		(each (lambda (x)
			(push jobs (setf-> (str-alloc +job_size)
				(+job_x x)
				(+job_y y)
				(+job_x1 (min (* +width +scale) (+ x (* +job_rect_size +scale))))
				(+job_y1 (min (* +height +scale) (+ y (* +job_rect_size +scale))))
				(+job_w (* +width +scale))
				(+job_h (* +height +scale))
				(+job_cx center_x)
				(+job_cy center_y)
				(+job_z zoom))))
			(range 0 (* +width +scale) (* +job_rect_size +scale))))
		(range 0 (* +height +scale) (* +job_rect_size +scale)))
	(setq farm (Farm create destroy (min (length jobs) (* 2 (length (lisp-nodes))))))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0))

(defun main ()
	(defq select (task-mboxes +select_size))
	(.-> *canvas* (:fill +argb_black) (:swap 0))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(reset)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((and (= id (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;mouse click on the canvas view, zoom in/out, re-center
						(bind '(w h) (. *canvas* :get_size))
						(defq rx (- (getf msg +ev_msg_mouse_rx) (/ (- w +width) 2))
							ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2)))
						(setq center_x (+ center_x (real-offset (n2r rx) (n2r +width) zoom))
							center_y (+ center_y (real-offset (n2r ry) (n2r +height) zoom))
							zoom (* zoom (if (bits? (getf msg +ev_msg_mouse_buttons) 2)
								+real_2 +real_1/2)))
						(reset))
					(:t (. *window* :event msg))))
			(+select_task
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(def val :child child)
					(dispatch-job key val)))
			(+select_reply
				;child response
				(bind '(key x y x1 y1 ix iy ix1 iy1 fill_value) (getf-> msg
					+job_reply_key +job_reply_x +job_reply_y +job_reply_x1 +job_reply_y1 
					+job_reply_ix +job_reply_iy +job_reply_ix1 +job_reply_iy1 +job_reply_fill_value))
				(when (defq val (. farm :find key)) (dispatch-job key val))
				(setq dirty :t)
				(if (and (/= fill_value -1) (= ix x) (= iy y) (= ix1 x1) (= iy1 y1))
					;fill covers the WHOLE area, skip :tile completely!
					(.-> *canvas*
						(:set_color (get-int (elem-get +mandel_lut fill_value) 0))
						(:fbox x y (- x1 x) (- y1 y)))
					(progn
						;tile the full buffer (draws computed perimeter rings + uninitialized interior garbage)
						(. *canvas* :tile (apply cat (map! (# (elem-get +mandel_lut (code %0)))
							(list msg) (list) +job_reply_size -1)) x y x1 y1)
						;overwrite the uninitialized interior garbage with any solid fill!
						(when (/= fill_value -1)
							(.-> *canvas*
								(:set_color (get-int (elem-get +mandel_lut fill_value) 0))
								(:fbox ix iy (- ix1 ix) (- iy1 iy)))))))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(. farm :refresh +retry_timeout)
				(when dirty
					(setq dirty :nil)
					(. *canvas* :swap +pixmap_mode_normal)
					(when (= 0 (length jobs))
						(defq working :nil)
						(. farm :each (lambda (key val)
							(setq working (or working (get :job val)))))
						(unless working
							(mail-timeout (elem-get select +select_timer) 0 0)
							(. farm :close)))))))
	;close window and children
	(. farm :close)
	(gui-sub-rpc *window*))