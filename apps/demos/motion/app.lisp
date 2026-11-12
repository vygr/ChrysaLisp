(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/motion/rostrum.inc")

;Scenes of vector figures that look after themselves, lib/motion/. The story
;of where this system came from, a playground, and the build of a function
;from its source to a call of it. The window keeps the shape of the picture,
;which is 640 by 480 whatever size it is drawn

(enums +event +event_user
	(enum play pause home scene timeline))

(enums +select 0
	(enum main timer tip))

(defq +width 640 +height 480 +rate (/ 1000000 60)
	;of each frame, how long the run that finds a scene's length may have,
	;and a seek, in us
	+dry_us 12000 +seek_us 10000
	;the bar's length while the scene's is not known, so the elevator is small and
	;still at the start, not drawn against a length still to be found
	+bar_unknown 1000000000
	;how many steps apart the I-frames are kept, copies of the whole scene
	;a seek goes on from
	+iframe_steps 300
	id :t select :nil stage :nil restart :nil playing :t
	;the scene: what starts it, its seed, how many steps long it is once
	;that is known, the run that finds out, its I-frames, (step copy), and
	;the step a seek is going to
	maker :nil seed 1 steps :nil dry :nil iframes (list) target :nil
	;how long a tick and a draw took, and when that was last shown
	tick_us 0 draw_us 0 frames 0 last_show 0 last_t 0)

(defun make-canvas (w h)
	(. (Canvas w h 1) :set_canvas_flags +canvas_flag_antialias))

(ui-window *window* ()
	(ui-title-bar _ "Motion" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_media_play +sym_media_pause +sym_media_start) +event_play))
		(. (ui-radio-bar *scene_toolbar* ("Story" "Balls" "Build") (:font :font_body)) :connect +event_scene)
		(ui-label *status* (:text " 400 figures  100.0 tick  100.0 draw " :font :font_body
				:flow_flags +flow_flag_align_vcenter))
		(ui-backdrop _ (:paper :paper_toolbar)))
	(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_timeline)
	(ui-backdrop *picture* (:style :plain :paper +argb_black :ink +argb_black
			:min_width (/ +width 2) :min_height (/ +height 2) :want_width +width :want_height +height)
		(ui-element *canvas* (make-canvas +width +height))))

(defun frame-draw ()
	;the picture as it is now. What is round the canvas, as the window grows and
	;before it is made again, is the colour of the picture
	(bind '(w h) (. *canvas* :get_size))
	(defq k (/ (n2f w) (n2f +width)) paper (ifn (get :paper_is stage) +argb_black))
	(def *picture* :paper paper)
	(. *canvas* :fill paper)
	(. stage :draw *canvas* (fixeds k 0.0 0.0 0.0 k 0.0))
	(. *canvas* :swap +swap_write))

(defun action-layout ()
	;the window is a new size, the canvas is made to fill the picture and drawn
	;at once, a new one has nothing to show until it is
	(bind '(w h) (. *picture* :get_size))
	(when (and (> w 0) (> h 0))
		(. *canvas* :sub)
		(setq *canvas* (make-canvas w h))
		(def *canvas* :paper 0)
		(. *picture* :add_child *canvas*)
		(. *canvas* :change 0 0 w h)
		(. *window* :layout)
		(frame-draw)))

(import "./scene.inc")
(import "./balls.inc")
(import "./story.inc")
(import "./build.inc")

(defun scene-view ()
	;playing or not, put on whichever rostrum is the one shown, made or
	;sought to
	(def stage :ros_speed (if playing 1.0 0.0)))

(defun scene-make ()
	;the scene from its start, by its seed: the same scene every time. A
	;scene that wants depth sets its own focus, the camera is its own
	(setq stage (Rostrum +width +height :nil seed) restart :nil)
	(maker)
	(scene-view))

(defun dry-step ()
	;the scene run again with no picture, a little each frame, to find how
	;many steps it is, keeping a copy of it every so often, an I-frame. The
	;scene's code knows only stage and restart, so they are this run's
	;while it goes
	(defq live stage live_restart restart start (pii-time))
	(setq stage dry restart :nil)
	(while (and (not restart) (< (- (pii-time) start) +dry_us))
		(. stage :tick +ros_step)
		(and (not restart) (= 0 (% (defq at (get :ros_ticks stage)) +iframe_steps))
			(push iframes (list at (. stage :copy)))))
	;at its end the bar is the scene's length, and goes to where it is
	(when restart
		(setq steps (get :ros_ticks dry) dry :nil)
		(def *timeline* :maximum (dec steps))
		(. *timeline* :dirty_all))
	(setq stage live restart live_restart))

(defun scene-new ()
	;a scene, a new seed for it, and a run that finds its length. Balls is
	;played with, it has no end and is not sought
	(setq maker (elem-get (list (const story-new) (const balls-new) (const build-new))
			(. *scene_toolbar* :get_selected))
		seed (inc (random 1000000000)) steps :nil dry :nil iframes (list) target :nil)
	(def *timeline* :value 0 :maximum +bar_unknown)
	(. *timeline* :dirty_all)
	(unless (eql maker (const balls-new))
		(scene-make)
		(setq dry stage))
	(scene-make))

(defun seek-from ()
	;where a seek goes on from: where it is, if that is between the last
	;I-frame before the step and the step, else a copy of that I-frame, or
	;the start if there is none
	(defq from :nil now (get :ros_ticks stage))
	(each (lambda (iframe) (if (<= (first iframe) target) (setq from iframe))) iframes)
	(cond
		((and (<= now target) (or (not from) (>= now (first from)))))
		(from
			(setq stage (. (second from) :copy) restart :nil)
			(scene-view))
		((scene-make))))

(defun seek-step ()
	;on toward the step sought, a little each frame, from the nearest place
	;to go on from
	(seek-from)
	(defq start (pii-time))
	(while (and target (< (- (pii-time) start) +seek_us))
		(if (< (get :ros_ticks stage) target)
			(. stage :tick +ros_step)
			(setq target :nil))))

(defun pointer-move ()
	;the pointer figure goes where the mouse is, in the picture's space
	(bind '(mx my &ignore) (gui-info))
	(bind '(px py &ignore) (. (penv *window*) :get_relative *canvas*))
	(bind '(w &ignore) (. *canvas* :get_size))
	(defq k (/ (n2f +width) (n2f (max w 1))))
	(if (defq pointer (get :pointer stage))
		(. pointer :set_place :fig_x (* k (n2f (- mx px))) :fig_y (* k (n2f (- my py))))))

(defun frame ()
	;the time since the last, it all goes on by that, then the picture
	(defq now (pii-time) dt (/ (n2f (min 100000 (- now last_t))) 1000000.0))
	(setq last_t now)
	;at its end a scene begins again, the same
	(if restart (scene-make))
	(cond
		(target (seek-step))
		(:t (pointer-move)
			(. stage :advance dt)))
	(if dry (dry-step))
	(defq t1 (pii-time))
	(frame-draw)
	;where it is on the bar, unless the bar is being held
	(when (and steps (= (get :state *timeline*) 0)
			(/= (get :value *timeline*) (get :ros_ticks stage)))
		(def *timeline* :value (min (get :maximum *timeline*) (get :ros_ticks stage)))
		(. *timeline* :dirty_all))
	(setq tick_us (+ tick_us (- t1 now)) draw_us (+ draw_us (- (pii-time) t1)) frames (inc frames))
	(when (>= (- now last_show) 500000)
		(defq n (length (. stage :children)))
		(. *status* :set_text (cat " " (str n) " figures  "
				(str (/ tick_us frames 1000)) "." (str (/ (% (/ tick_us frames) 1000) 100)) " tick  "
				(str (/ draw_us frames 1000)) "." (str (/ (% (/ draw_us frames) 1000) 100)) " draw "))
		(setq tick_us 0 draw_us 0 frames 0 last_show now)))

(defun main ()
	(setq select (task-mboxes +select_size))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar* '("play" "pause" "restart"))
	(ui-tool-tips *scene_toolbar* '("story" "balls" "build"))
	(def *timeline* :tip_text "seek")
	(. *scene_toolbar* :set_selected 0)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(.-> *window* (:connect +event_layout) :open)
	;it keeps its shape
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
					((= id +event_scene)
						(scene-new))
					((= id +event_home)
						;from the start, the same scene
						(setq target :nil)
						(scene-make))
					((= id +event_timeline)
						;to the step the bar is at, once the scene's length is known
						(if steps (setq target (get :value *timeline*))))
					((= id +event_play)
						(setq playing :t)
						(scene-view))
					((= id +event_pause)
						;all stands still where it is
						(setq playing :nil)
						(scene-view))
					((. *window* :event msg))))
			(+select_tip
				(. *window* :tip msg))
			(:t ;a frame, the next one a frame from when this one started
				(defq start (pii-time))
				(frame)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +rate (- (pii-time) start))) 0))))
	(. *window* :close))
