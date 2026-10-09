(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")

(enums +event 0
	(enum close)
	(enum zoom_in zoom_out home))

(enums +select 0
	(enum main task reply timer tip))

(defq +width 800 +height 800 +job_rect_size 32 +scale 2
	+timer_rate (/ 500000 1) id :t dirty :nil
	center_x +real_-1/2 center_y +real_0 zoom +real_1 level 0
	+retry_timeout (task-timeout 5) jobs :nil
	+min_top 256 +level_top 64 +max_level 42
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their squares straight onto them
	shared_canvas (canvas-shared +width +height +scale)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Mandelbrot" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_zoom_in +sym_zoom_out +sym_reset) +event_zoom_in))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height +scale)) (:color 0)))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("zoom in" "zoom out" "home")))

(defun zoom-by (step)
	;a level further in, the picture is of half as much, or a level out.
	;No further out than the whole set, or in than a real can tell one
	;pixel from the next
	(when (<= 0 (+ level step) +max_level)
		(setq level (+ level step)
			zoom (* zoom (if (> step 0) +real_1/2 +real_2)))))

(defun reset ()
	;the picture is started again. The children are too, one may be part
	;way through a square of the old picture, and its answer is then left
	;alone. The deeper the picture, the more turns a point is given to get
	;out of the set, or the detail there is taken to be inside it
	(. jobs :restart)
	(defq work (list) top (+ +min_top (* level +level_top)))
	(each (lambda (y)
		(each (lambda (x)
			(push work (setf-> (str-alloc +rect_size)
				(+rect_x x)
				(+rect_y y)
				(+rect_x1 (min (const (* +width +scale)) (+ x (const (* +job_rect_size +scale)))))
				(+rect_y1 (min (const (* +height +scale)) (+ y (const (* +job_rect_size +scale)))))
				(+rect_w (const (* +width +scale)))
				(+rect_h (const (* +height +scale)))
				(+rect_shared shared_key)
				(+rect_top top)
				(+rect_cx center_x)
				(+rect_cy center_y)
				(+rect_z zoom))))
			(range 0 (const (* +width +scale)) (const (* +job_rect_size +scale)))))
		(range 0 (const (* +height +scale)) (const (* +job_rect_size +scale))))
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
	(tooltips)
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
						(setq center_x (+ center_x (real-offset (n2r rx) (const (n2r +width)) zoom))
							center_y (+ center_y (real-offset (n2r ry) (const (n2r +height)) zoom)))
						(zoom-by (if (bits? (getf msg +ev_msg_mouse_buttons) 2) -1 1))
						(reset))
					((= id +event_zoom_in)
						(zoom-by 1)
						(reset))
					((= id +event_zoom_out)
						(zoom-by -1)
						(reset))
					((= id +event_home)
						(setq center_x +real_-1/2 center_y +real_0 zoom +real_1 level 0)
						(reset))
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(. jobs :launched msg))
			(+select_tip
				;tip event
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
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
								+rect_reply_inside))))
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
