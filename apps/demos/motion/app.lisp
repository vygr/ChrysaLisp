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
;a farm of its own, is a scout: it runs the scene ahead to its end, to find
;its length, and every so many steps sends the child that draws the scene
;as it is there, an I-frame, a place a seek starts from. It says how far it
;has got as it goes: a bar of that is in front of the timeline till it is
;done, the length known and the places to seek from made, and the scene
;can be sought. So nothing of a scene, a seek or its length, holds up the
;window

(enums +event +event_user
	(enum play pause home scene timeline))

(enums +select 0
	(enum main tip scout_task scout_reply timer))

(defq +width 640 +height 480 +rate (/ 1000000 60)
	;the scenes, Balls the one with no end, played with and not sought
	+scene_balls 1
	;how long a scout may say nothing
	+scout_patience 60000000
	;about how many steps each scene is, what the bar of its first scan is
	;of: after that it is of the length last found, kept with the config
	+scene_steps ''(6824 0 10566) lengths :nil
	id :t select :nil render :nil scout :nil answer :nil playing :t
	;the size of the canvas on show; and the canvas of a new size the child
	;is to draw on, not on show till a frame on it is whole, its size, and
	;has a frame for it been given
	shown_size (list +width +height) next :nil next_size :nil next_out :nil
	;the frames' child has started, where a scout's I-frames are to go, the
	;mailbox that child says, and a scout is to be started when both are
	frames_up :nil player_box :nil scout_due :nil
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
	;where the scene is, dragged to seek; in front of it, till the scene's
	;length is found, how far the child that finds it has got
	(ui-flow _ (:flow_flags +flow_stack_fill)
		(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_timeline)
		(ui-progress *scan* (:value 0 :maximum 1 :min_height 14)))
	(ui-backdrop *picture* (:style :plain :paper +argb_black :ink +argb_black
			:min_width (/ +width 2) :min_height (/ +height 2) :want_width +width :want_height +height)
		(ui-element *canvas* (make-canvas +width +height))))

(defun canvas-want ()
	;a canvas of the size the picture is now, for the child to draw on
	;from the next frame, if the one it draws on is another size. It is not
	;shown yet. And not made while a frame is out on one that is not shown
	;yet: that frame is shown when it comes, whatever size the window has
	;gone to since, and the size is looked at again then, (canvas-next). A
	;window dragged to a size asks at each move, and one made at each move
	;would never have a frame finished on it
	(bind '(w h) (. *picture* :get_size))
	(bind '(dw dh) (ifn next_size shown_size))
	(when (and (> w 0) (> h 0) (not next_out) (not (and (= w dw) (= h dh))))
		(setq next (make-canvas w h) next_size (list w h))
		(def next :paper 0)
		(. next :change 0 0 w h)
		(if render (. render :canvas next))))

(defun action-layout ()
	;the window is a new size. The canvas on show stays, in the middle of
	;the picture, the size it is, till a frame of a new size is whole: the
	;child draws straight on a canvas's pixels, so a new one put on show
	;at once is a colour till a frame lands on it. What is round the
	;canvas, as the window grows, is the colour of the picture
	(bind '(w h) (. *picture* :get_size))
	(when (and (> w 0) (> h 0))
		(. *canvas* :change 0 0 w h)
		(. *window* :layout)
		(canvas-want)))

(defun canvas-next ()
	;a frame has been answered: if it was drawn on a canvas not shown yet,
	;that is whole, and is the one on show from now. Then one of the size
	;the picture has now, if that has changed again
	(when (and next next_out)
		(. *canvas* :sub)
		(setq *canvas* next shown_size next_size next :nil next_size :nil next_out :nil)
		(bind '(w h) (. *picture* :get_size))
		(. *picture* :add_child *canvas*)
		(. *canvas* :change 0 0 w h)
		(. *window* :layout)
		(canvas-want)))

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
	;a job for a child, of this scene: a frame of it, or a scout of it,
	;with where its I-frames are to go
	(setf-> (str-alloc +motion_job_size)
		(+motion_job_kind kind) (+motion_job_scene scene) (+motion_job_seed seed)
		(+motion_job_film (if (= scene +scene_balls) 0 1))
		(+motion_job_iframes (ifn player_box (task-mbox)))))

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
		;drawn on the canvas of the new size if there is one, (action-layout)
		(defq on (ifn next *canvas*))
		(if next (setq next_out :t))
		(bind '(w h) (ifn next_size shown_size))
		(defq k (/ (n2q +width) (n2q (max w 1))))
		(list (setf-> job
			(+motion_job_step at) (+motion_job_target (ifn target -1))
			(+motion_job_shared (canvas-key on)) (+motion_job_width w) (+motion_job_height h)
			(+motion_job_px (* k (n2q (- mx px)))) (+motion_job_py (* k (n2q (- my py))))
			(+motion_job_dt dt) (+motion_job_playing (if playing 1 0))))))

(defun frame-give ()
	;the next frame, out with the child
	(setq given (pii-time))
	(. render :frame))

(defun scout-end ()
	;the scout let go of
	(setq scout (jobs-end scout select +select_scout_task +select_scout_reply)))

(defun frame-answered (msg)
	;the child has drawn a frame, the render has shown it: where the scene
	;is now, on the bar unless the bar is being held, and the colour round
	;the picture. A seek that has landed is over. And where it has a
	;scout's I-frames sent: a mailbox not the one known is a child started
	;again, with none of them, and a scout is started for it
	(bind '(step figures paper tick draw box) (getf-> msg +motion_reply_step +motion_reply_figures
		+motion_reply_paper +motion_reply_tick +motion_reply_draw +motion_reply_iframes))
	(canvas-next)
	(unless (eql box player_box)
		(setq player_box box scout_due (/= scene +scene_balls))
		(scout-end))
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

(defun scout-start ()
	;a scout of the scene, once the frames' child is up and has said where
	;I-frames are to go. It works flat out, so it is kept off this node,
	;the window's, and off that child's, if the machine has another: on
	;either it would be felt in the picture
	(when (and scout_due frames_up player_box)
		(setq scout_due :nil scout (Jobs (cat *app_root* "child.lisp")
			(elem-get select +select_scout_task) (elem-get select +select_scout_reply) (list 1)
			(list (task-nodeid player_box))))
		(. scout :add (list (motion-job +motion_kind_scout)))))

(defun scan-show (step)
	; (scan-show step)
	;how far the scout has got, on the bar in front of the timeline, of
	;the length the scene last had; :nil, it is done, or the scene has no
	;length, and the timeline is in front
	(cond
		(step (defq most (max 1 (elem-get lengths scene)))
			(def (. *scan* :dirty) :maximum most :value (min step most))
			(. *scan* :to_front))
		((. (. *timeline* :dirty) :to_front))))

(defun scouted (msg)
	;the scout has said how far it has got, and it is heard from, slow not
	;gone: the bar in front shows it, while the scene's length is not
	;known. Or it is done: the length is kept for the bar of the next scan,
	;the timeline is that long now and can be sought, and goes to where the
	;scene is. One started again for a child that draws started again has
	;the same length to tell, and the timeline is left as it is
	(cond
		((not scout))
		((/= 0 (getf msg +motion_reply_more))
			(when (defq job (. scout :job msg))
				(. scout :heard (slice job +job_size -1))
				(unless steps (scan-show (getf msg +motion_reply_step)))))
		((. scout :answered msg)
			(unless steps
				(setq steps (getf msg +motion_reply_step))
				(elem-set lengths scene steps)
				(def *timeline* :maximum (dec steps) :value (min (dec steps) at))
				(scan-show :nil))
			(scout-end))))

(defun scene-new (&optional with_seed)
	;a scene, a new seed for it or the one given, from its start, and a
	;scout of it. Balls is played with, it has no end and is not sought
	(setq scene (. *scene_toolbar* :get_selected) seed (ifn with_seed (inc (random 1000000000)))
		steps :nil at 0 target :nil)
	;till the scene's length is known the timeline has none, its elevator
	;is all of it and it can not be sought: the bar of the scan is in
	;front of it, but for Balls, which has no length to find
	(def *timeline* :value 0 :maximum 0)
	(scan-show (if (/= scene +scene_balls) 0))
	(when select
		(scout-end)
		(if (. render :busy) (. render :drop))
		(setq scout_due (/= scene +scene_balls))
		(scout-start)))

(defun config-default ()
	;the first time: :x :nil, the window put where the screen puts a new
	;one, and each scene about as long as it was when this was written
	(scatter (Emap) :version +config_version :x :nil :y :nil
		:width +width :height +height :scene 0 :seed :nil :step 0 :playing :t
		:lengths (cat +scene_steps)))

(defun config-load ()
	;as it was when last closed: the defaults with what was kept laid over
	;them, so a key added since has its default. A file of a later version
	;than this, or none, gives the defaults
	(setq *config* (config-default))
	(and (defq old (with-read-lock *config_file*
				(tree-load (file-stream *config_file*))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val)))))
	(setq lengths (. *config* :find :lengths)))

(defun config-save ()
	;the scene, its seed and the step it is at, or is being sought to,
	;playing or not, where the window is and how big the picture
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *picture* :get_size))
	(scatter *config* :x x :y y :width w :height h
		:scene (. *scene_toolbar* :get_selected) :seed seed
		:step (ifn target at) :playing playing :lengths lengths)
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
				(scout-start)
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
			((= idx +select_scout_task)
				(if scout (. scout :launched msg)))
			((= idx +select_scout_reply)
				(scouted msg))
			(:t ;the timer, a frame a tick if the last is drawn, and the
				;children's patience
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless (. render :busy) (frame-give))
				(. render :tick)
				(if scout (. scout :refresh +scout_patience)))))
	(config-save)
	(scout-end)
	(. render :close)
	(. *window* :close))
