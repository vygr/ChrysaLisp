(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/gpu/render_farm.inc")
(import "./job.inc")

;Scenes of vector figures that look after themselves, lib/motion/. The story
;of where this system came from, a playground, and the build of a function
;from its source to a call of it. The window keeps the shape of the picture,
;which is 640 by 480 whatever size it is drawn.
;
;The scene is not here: it goes on in a child, ./child.lisp, a farm of one
;on another node of this machine, which draws each frame on the canvas,
;its pixels shared, and is given the next when it answers. Another child,
;a farm of its own, runs the scene to its end to find its length. So
;nothing of a scene, a seek or its length, holds up the window

(enums +event +event_user
	(enum play pause home scene timeline))

(enums +select 0
	(enum main tip measure_task measure_reply timer))

(defq +width 640 +height 480 +rate (/ 1000000 60)
	;the scenes, Balls the one with no end, played with and not sought
	+scene_balls 1
	;how long a child that finds a length is waited for
	+measure_patience 60000000
	id :t select :nil render :nil measurer :nil answer :nil playing :t
	;the frames' child has started, and a length is to be found when it has
	frames_up :nil measure_due :nil
	;the scene: its number, its seed, how many steps long it is once that is
	;known, the step the child last said it was at, and the step a seek is
	;going to
	scene 0 seed 1 steps :nil at 0 target :nil
	;when the last frame was given, how long the ticks and draws of the
	;child took, and when that was last shown
	given 0 tick_us 0 draw_us 0 frames 0 last_show 0 last_t 0
	;what it was showing when it was last closed, and where, (config-save)
	*config* :nil +config_version 1 *config_file* (cat (get :home *env*) "motion.tre"))

(defun make-canvas (w h)
	;the child draws on it, so its pixels are shared
	(canvas-shared w h 1))

(ui-window *window* ()
	(ui-title-bar _ "Motion" (+sym_close +sym_max +sym_min) +event_close)
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

(defun action-layout ()
	;the window is a new size, the canvas is made to fill the picture, and
	;the child draws on it from the next frame. What is round the canvas,
	;as the window grows, is the colour of the picture
	(bind '(w h) (. *picture* :get_size))
	(when (and (> w 0) (> h 0))
		(. *canvas* :sub)
		(setq *canvas* (make-canvas w h))
		(def *canvas* :paper 0)
		(.-> *canvas* (:fill (get :paper *picture*)) (:swap +swap_write))
		(. *picture* :add_child *canvas*)
		(. *canvas* :change 0 0 w h)
		(. *window* :layout)
		(if render (. render :canvas *canvas*))))

(defun picture-next (now most way)
	; (picture-next now most way) -> k
	;the size a step larger, way 1, or smaller, -1, than now, each in
	;quarters of the width, a picture of 4 by 3 of them so each is whole:
	;640 by 480, 800 by 600, 1024 by 768 and on, 640 by 480 the least and
	;most, the largest the screen has room for, the most
	(defq steps (cat (filter (# (< %0 most)) '(160 200 256 320 400 512 640 800)) (list most)))
	(if (> way 0)
		(ifn (some (# (if (> %0 now) %0)) steps) most)
		(ifn (rsome (# (if (< %0 now) %0)) steps) 160)))

(defun picture-step (way)
	;the picture a step larger or smaller than it is now, (picture-next),
	;the window round it. As large as the screen has room for, of its shape,
	;the window round it too, is the most. A window sized by hand is any
	;size, these go on from where it is
	(bind '(ww wh) (. *window* :get_size))
	(bind '(pw ph) (. *picture* :get_size))
	(bind '(& & sw sh) (gui-info))
	(defq k (picture-next (/ pw 4) (max 160 (min (/ (- sw (- ww pw)) 4) (/ (- sh (- wh ph)) 3))) way))
	(def *picture* :want_width (* k 4) :want_height (* k 3))
	;a fit sends no layout event, as a drag does: the canvas is made the
	;new size here
	(. *window* :fit)
	(action-layout))

(defun motion-job (kind)
	; (motion-job kind) -> job
	;a job for a child, of this scene: a frame of it, or its length
	(setf-> (str-alloc +motion_job_size)
		(+motion_job_kind kind) (+motion_job_scene scene) (+motion_job_seed seed)
		(+motion_job_film (if (= scene +scene_balls) 0 1))))

(defun frame-jobs (& & ready)
	; (frame-jobs kind count ready) -> :nil | jobs
	;the job of the next frame: the step the child last said, a seek if
	;one is wanted, the time gone since the last, where the pointer is in
	;the picture, and the canvas. The child is not asked if it is ready,
	;:nil to that, the first frame is only later
	(unless ready
		(defq job (motion-job +motion_kind_frame) now (pii-time) dt (min 100000 (- now last_t)))
		(setq last_t now)
		(bind '(mx my &ignore) (gui-info))
		(bind '(px py &ignore) (. (penv *window*) :get_relative *canvas*))
		(bind '(w h) (. *canvas* :get_size))
		(defq k (/ (n2q +width) (n2q (max w 1))))
		(list (setf-> job
			(+motion_job_step at) (+motion_job_target (ifn target -1))
			(+motion_job_shared (canvas-key *canvas*)) (+motion_job_width w) (+motion_job_height h)
			(+motion_job_px (* k (n2q (- mx px)))) (+motion_job_py (* k (n2q (- my py))))
			(+motion_job_dt dt) (+motion_job_playing (if playing 1 0))))))

(defun frame-give ()
	;the next frame, out with the child
	(setq given (pii-time))
	(. render :frame))

(defun frame-answered (msg)
	;the child has drawn a frame, the render has shown it: where the scene
	;is now, on the bar unless the bar is being held, and the colour round
	;the picture. A seek that has landed is over
	(bind '(step figures paper tick draw) (getf-> msg +motion_reply_step +motion_reply_figures
		+motion_reply_paper +motion_reply_tick +motion_reply_draw))
	(setq at step)
	(if (eql target step) (setq target :nil))
	(def *picture* :paper paper)
	(when (and steps (not target) (= (get :state *timeline*) 0) (/= (get :value *timeline*) at))
		(def *timeline* :value (min (get :maximum *timeline*) at))
		(. *timeline* :dirty_all))
	(setq tick_us (+ tick_us tick) draw_us (+ draw_us draw) frames (inc frames))
	(when (>= (- (defq now (pii-time)) last_show) 500000)
		(. *status* :set_text (cat " " (str figures) " figures  "
				(str (/ tick_us frames 1000)) "." (str (/ (% (/ tick_us frames) 1000) 100)) " tick  "
				(str (/ draw_us frames 1000)) "." (str (/ (% (/ draw_us frames) 1000) 100)) " draw "))
		(setq tick_us 0 draw_us 0 frames 0 last_show now)))

(defun seek (step)
	;to that step, from the next frame. A frame that is out, a seek
	;perhaps, is not waited for, and its child leaves it for the next
	(setq target step)
	(if (. render :busy) (. render :drop)))

(defun measure-end ()
	;the child that finds a length let go of
	(setq measurer (jobs-end measurer select +select_measure_task +select_measure_reply)))

(defun measure-start ()
	;a child that finds the scene's length, once the frames' child is up:
	;then the kernel puts this one on the node of this machine with least
	;to do, not that one's, which its farm has away from this node
	(when (and measure_due frames_up)
		(setq measure_due :nil measurer (Jobs (cat *app_root* "child.lisp")
			(elem-get select +select_measure_task) (elem-get select +select_measure_reply) (list 1)))
		(. measurer :add (list (motion-job +motion_kind_length)))))

(defun measured (msg)
	;the child has found the scene's length, the bar is that long now and
	;can be sought, and goes to where the scene is
	(when (and measurer (. measurer :answered msg))
		(setq steps (getf msg +motion_reply_step))
		(def *timeline* :maximum (dec steps) :value (min (dec steps) at))
		(. *timeline* :dirty_all)
		(measure-end)))

(defun scene-new (&optional with_seed)
	;a scene, a new seed for it or the one given, from its start, and a
	;child that finds its length. Balls is played with, it has no end and
	;is not sought
	(setq scene (. *scene_toolbar* :get_selected) seed (ifn with_seed (inc (random 1000000000)))
		steps :nil at 0 target :nil)
	;till the scene's length is known the bar has none, and its elevator
	;is all of it: it can not be sought yet
	(def *timeline* :value 0 :maximum 0)
	(. *timeline* :dirty_all)
	(when select
		(measure-end)
		(if (. render :busy) (. render :drop))
		(setq measure_due (/= scene +scene_balls))
		(measure-start)))

(defun config-load ()
	;as it was when last closed, or as it is the first time: :x :nil, the
	;window put where the screen puts a new one
	(setq *config* (with-read-lock *config_file*
			(tree-load (file-stream *config_file*))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version :x :nil :y :nil
			:width +width :height +height :scene 0 :seed :nil :step 0 :playing :t))))

(defun config-save ()
	;the scene, its seed and the step it is at, or is being sought to,
	;playing or not, where the window is and how big the picture
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *picture* :get_size))
	(scatter *config* :x x :y y :width w :height h
		:scene (. *scene_toolbar* :get_selected) :seed seed
		:step (ifn target at) :playing playing)
	(with-write-lock *config_file*
		(tree-save (file-stream *config_file* +file_open_write) *config*)))

(defun window-place (x y w h)
	;the window where it was, as far as the screen lets it, round a picture
	;of that size, and the canvas made that size
	(def *picture* :want_width w :want_height h)
	(bind '(ww wh) (. *window* :want_size))
	(bind '(x y ww wh) (view-fit x y ww wh))
	(. *window* :change x y ww wh)
	(. *window* :open :here)
	(action-layout))

(defun main ()
	(setq select (task-mboxes +select_size)
		render (Render-work *canvas* (cat *app_root* "child.lisp")))
	;the frames by a child of one, on this machine off this node if it has
	;another, its canvas shared
	(def render :jobs_fn (const frame-jobs) :reply_fn (lambda (msg) (setq answer msg))
		:launched_fn (lambda (& &) (setq frames_up :t)) :herd (list 1) :away :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar* '("play" "pause" "restart"))
	(ui-tool-tips *scene_toolbar* '("story" "balls" "build"))
	(def *timeline* :tip_text "seek")
	;the scene it was showing, its seed, at the step it was at, so it is
	;the same, seen where it was
	(config-load)
	(bind '(x y w h was_scene was_seed step was_playing)
		(gather *config* :x :y :width :height :scene :seed :step :playing))
	(. *scene_toolbar* :set_selected was_scene)
	(scene-new was_seed)
	(setq playing was_playing)
	(and (> step 0) (/= scene +scene_balls) (setq target step))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :connect +event_layout)
	(if x (window-place x y w h) (. *window* :open))
	;it keeps its shape
	(def *window* :aspect_view *picture* :aspect (list +width +height))
	(setq last_t (pii-time) last_show last_t)
	(frame-give)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while id
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;the render's mail: a frame the child has drawn is shown, and
				;one that took a tick or more drives the next
				(when answer
					(frame-answered answer)
					(setq answer :nil))
				(measure-start)
				(and (not (. render :busy)) (>= (- (pii-time) given) +rate) (frame-give)))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_layout)
						(action-layout))
					((= id +event_max)
						(picture-step 1))
					((= id +event_min)
						(picture-step -1))
					((= id +event_scene)
						(scene-new))
					((= id +event_home)
						;from the start, the same scene
						(seek 0))
					((= id +event_timeline)
						;to the step the bar is at, once the scene's length is known
						(if steps (seek (get :value *timeline*))))
					((= id +event_play)
						(setq playing :t))
					((= id +event_pause)
						;all stands still where it is
						(setq playing :nil))
					((. *window* :event msg))))
			((= idx +select_tip)
				(. *window* :tip msg))
			((= idx +select_measure_task)
				(if measurer (. measurer :launched msg)))
			((= idx +select_measure_reply)
				(measured msg))
			(:t ;the timer, a frame a tick if the last is drawn, and the
				;children's patience
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless (. render :busy) (frame-give))
				(. render :tick)
				(if measurer (. measurer :refresh +measure_patience)))))
	(config-save)
	(measure-end)
	(. render :close)
	(. *window* :close))
