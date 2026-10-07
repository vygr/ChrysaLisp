(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defq +width 800 +height 800 +job_rect_size 32 +scale 2
	+timer_rate (/ 500000 1) id :t dirty :nil
	center_x +real_-1/2 center_y +real_0 zoom +real_1
	+retry_timeout (task-timeout 5) jobs :nil
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their squares straight onto them
	shared_canvas (canvas-shared +width +height +scale)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Mandelbrot" (0xea19) +event_close)
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height +scale)) (:color 0)))

(defun reset ()
	;the picture is started again. The children are too, one may be part
	;way through a square of the old picture, and its answer is then left
	;alone
	(. jobs :restart)
	(defq work (list))
	(each (lambda (y)
		(each (lambda (x)
			(push work (setf-> (str-alloc +rect_size)
				(+rect_x x)
				(+rect_y y)
				(+rect_x1 (min (* +width +scale) (+ x (* +job_rect_size +scale))))
				(+rect_y1 (min (* +height +scale) (+ y (* +job_rect_size +scale))))
				(+rect_w (* +width +scale))
				(+rect_h (* +height +scale))
				(+rect_shared shared_key)
				(+rect_cx center_x)
				(+rect_cy center_y)
				(+rect_z zoom))))
			(range 0 (* +width +scale) (* +job_rect_size +scale))))
		(range 0 (* +height +scale) (* +job_rect_size +scale)))
	(. jobs :add work)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0))

(defun main ()
	(defq select (task-mboxes +select_size))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq jobs (Jobs (cat *app_root* "child.lisp")
		(elem-get select +select_task) (elem-get select +select_reply)
		(* 2 (max 1 (length (lisp-nodes))))))
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
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(. jobs :launched msg))
			(+select_reply
				;a square is done. One from a child of the old picture is
				;left alone
				(when (defq out (. jobs :answered msg))
					(setq dirty :t)
					;a child that could not reach the canvas sends what to draw
					(when (= (getf msg +rect_reply_drawn) 0)
						(apply draw-rect (cat (list *canvas* (slice msg +rect_reply_size -1))
							(getf-> msg +rect_reply_x +rect_reply_y +rect_reply_x1 +rect_reply_y1
								+rect_reply_ix +rect_reply_iy +rect_reply_ix1 +rect_reply_iy1
								+rect_reply_fill_value))))
					(when (= out 0)
						;the picture is whole
						(mail-timeout (elem-get select +select_timer) 0 0)
						(setq dirty :nil)
						(. *canvas* :swap +swap_write))))
			(:t ;timer event, show what there is of the picture so far
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(. jobs :refresh +retry_timeout)
				(when dirty
					(setq dirty :nil)
					(. *canvas* :swap +swap_write)))))
	;close window and children
	(. jobs :close)
	(gui-sub-rpc *window*))
