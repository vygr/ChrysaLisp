(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/motion/rostrum.inc")

;Scenes of vector figures that look after themselves, lib/motion/. The story
;of where this system came from, and a playground. The window keeps the shape
;of the picture, which is 640 by 480 whatever size it is drawn

(enums +event +event_user
	(enum play pause home scene depth))

(enums +select 0
	(enum main timer tip))

(defq +width 640 +height 480 +rate (/ 1000000 60) +focus 700.0
	id :t select :nil stage :nil restart :nil
	;how long a tick and a draw took, and when that was last shown
	tick_us 0 draw_us 0 frames 0 last_show 0 last_t 0)

(defun make-canvas (w h)
	(. (Canvas w h 1) :set_canvas_flags +canvas_flag_antialias))

(ui-window *window* ()
	(ui-title-bar _ "Motion" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_media_play +sym_media_pause +sym_media_start) +event_play))
		(. (ui-radio-bar *scene_toolbar* ("Story" "Balls") (:font :font_body)) :connect +event_scene)
		(. (ui-radio-bar *depth_toolbar* ("Flat" "Depth") (:font :font_body)) :connect +event_depth)
		(ui-label *status* (:text " 400 figures  100.0 tick  100.0 draw " :font :font_body
			:flow_flags +flow_flag_align_vcenter))
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-backdrop *picture* (:style :plain :paper +argb_black :ink +argb_black
			:min_width +width :min_height +height)
		(ui-element *canvas* (make-canvas +width +height) (:paper 0))))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun action-layout ()
	;the window is a new size, the canvas is made to fill the picture
	(bind '(w h) (. *picture* :get_size))
	(when (and (> w 0) (> h 0))
		(. *canvas* :sub)
		(setq *canvas* (make-canvas w h))
		(def *canvas* :paper 0)
		(. *picture* :add_child *canvas*)
		(. *canvas* :change 0 0 w h)
		(. *window* :layout)))

(import "./scene.inc")
(import "./balls.inc")
(import "./story.inc")

(defun scene-new ()
	;a new stage and what is to start on it
	(setq stage (Rostrum +width +height (if (= (. *depth_toolbar* :get_selected) 1) +focus)) restart :nil)
	(if (= (. *scene_toolbar* :get_selected) 0) (story-new) (balls-new)))

(defun pointer-move ()
	;the pointer figure goes where the mouse is, in the picture's space
	(bind '(mx my &ignore) (gui-info))
	(bind '(px py &ignore) (. (penv *window*) :get_relative *canvas*))
	(bind '(w &ignore) (. *canvas* :get_size))
	(defq k (/ (n2f +width) (n2f (max w 1))))
	(if (defq pointer (get :pointer stage))
		(def pointer :x (* k (n2f (- mx px))) :y (* k (n2f (- my py))))))

(defun frame ()
	;the time since the last, it all goes on by that, then the picture
	(defq now (pii-time) dt (/ (n2f (min 100000 (- now last_t))) 1000000.0))
	(setq last_t now)
	(if restart (scene-new))
	(pointer-move)
	(. stage :tick dt)
	(defq t1 (pii-time))
	(bind '(w h) (. *canvas* :get_size))
	(defq k (/ (n2f w) (n2f +width)))
	(. *canvas* :fill (ifn (get :paper_is stage) +argb_black))
	(. stage :draw *canvas* (fixeds k 0.0 0.0 0.0 k 0.0))
	(. *canvas* :swap +swap_write)
	(setq tick_us (+ tick_us (- t1 now)) draw_us (+ draw_us (- (pii-time) t1)) frames (inc frames))
	(when (>= (- now last_show) 500000)
		(defq n (length (. stage :children)))
		(set-label *status* (cat " " (str n) " figures  "
			(str (/ tick_us frames 1000)) "." (str (/ (% (/ tick_us frames) 1000) 100)) " tick  "
			(str (/ draw_us frames 1000)) "." (str (/ (% (/ draw_us frames) 1000) 100)) " draw "))
		(setq tick_us 0 draw_us 0 frames 0 last_show now)))

(defun main ()
	(setq select (task-mboxes +select_size))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar* '("play" "pause" "restart"))
	(ui-tool-tips *scene_toolbar* '("story" "balls"))
	(ui-tool-tips *depth_toolbar* '("flat" "depth"))
	(. *scene_toolbar* :set_selected 0)
	(. *depth_toolbar* :set_selected 1)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;from now on it can be half that, and keeps its shape
	(def *picture* :min_width (/ +width 2) :min_height (/ +height 2))
	(def *window* :aspect_view *picture* :aspect (list +width +height))
	(scene-new)
	(setq last_t (pii-time) last_show last_t)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_layout)
						(action-layout))
					((or (= id +event_home) (= id +event_scene))
						(scene-new))
					((= id +event_play)
						(def stage :speed 1.0))
					((= id +event_pause)
						;all stands still where it is
						(def stage :speed 0.0))
					((= id +event_depth)
						(def stage :focus (if (= (. *depth_toolbar* :get_selected) 1) +focus)))
					((. *window* :event msg))))
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(:t ;a frame, the next one a frame from when this one started
				(defq start (pii-time))
				(frame)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +rate (- (pii-time) start))) 0))))
	(gui-sub-rpc *window*))
