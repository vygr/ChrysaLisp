;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The child of the Motion app, ./app.lisp, of a farm of one on a node of this
; machine, so the scene goes on off the app's node and the app's loop is
; only its window. ./job.inc has what it is told and what it says.
;
; A frame job: the scene, from its seed, brought to the step the app last
; had, sought to a step or gone on by the time since the last frame, and
; drawn on the app's canvas, whose pixels are shared, (canvas-attach). The
; scene and its I-frames, copies of it every so often to seek from, are a
; cache: kept from job to job, and a child started again makes them anew
; from the seed, the same scene. A seek goes a step at a time, the node's
; other tasks between, and is left as soon as another job comes, the app
; no longer wants it.
;
; A length job: the scene run from its seed to its end with no picture,
; how many steps it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")
(import "./job.inc")
(import "./scene.inc")
(import "./balls.inc")
(import "./story.inc")
(import "./build.inc")

(defq +width 640 +height 480
	;how many steps apart the I-frames are, and how long a seek or a
	;length goes on before the node's other tasks have a turn, in us: a
	;turn at every step costs a seek nearly half as much again
	+iframe_steps 300 +slice_us 5000
	;the scene's code knows only stage and restart: the Rostrum of the
	;moment, and the word that it is over and to begin again
	stage :nil restart :nil
	;the canvas of the app, (canvas-attach); the scene this child has, (scene
	;seed), is it a film, its I-frames, (step copy), and which every
	;+iframe_steps steps have one
	held (list) have :nil film :nil iframes (list) kept (Fset 11)
	;when the node's other tasks last had a turn
	sliced 0)

(defun scene-make (scene seed)
	;the scene from its start, by its seed: the same scene every time
	(setq stage (Rostrum +width +height :nil seed) restart :nil)
	((elem-get (list (const story-new) (const balls-new) (const build-new)) scene)))

(defun scene-have (scene seed is_film)
	;the scene asked for: the one this child has, or made from its start,
	;with no I-frames yet
	(unless (and have (= scene (first have)) (= seed (second have)))
		(setq have (list scene seed) film is_film iframes (list) kept (Fset 11))
		(scene-make scene seed)))

(defun iframe-keep ()
	;a copy of a film the first time each +iframe_steps steps of it is
	;come to, to seek from
	(defq at (get :ros_ticks stage) bucket (/ at +iframe_steps))
	(when (and film (not (. kept :find bucket)))
		(. kept :insert bucket)
		(push iframes (list at (. stage :copy)))))

(defun slice-due ()
	;the node's other tasks have a turn every +slice_us of a long run
	(when (> (- (defq now (pii-time)) sliced) +slice_us)
		(task-slice)
		(setq sliced (pii-time))))

(defun seek-from (target)
	;where a seek goes on from: where it is, if that is between the latest
	;I-frame before the step and the step, else a copy of that I-frame, or
	;the start if there is none
	(defq from :nil now (get :ros_ticks stage))
	(each (lambda (iframe)
			(and (<= (first iframe) target) (or (not from) (> (first iframe) (first from)))
				(setq from iframe)))
		iframes)
	(cond
		((and (<= now target) (or (not from) (>= now (first from)))))
		(from (setq stage (. (second from) :copy) restart :nil))
		((apply (const scene-make) have))))

(defun seek-to (target mbox)
	; (seek-to target mbox) -> :t | :nil
	;on to the step sought, a step at a time, the node's other tasks
	;between. :nil if another job came first, this one is not wanted
	(seek-from target)
	(while (and (< (get :ros_ticks stage) target) (not restart) (not (mail-poll (list mbox))))
		(. stage :tick +ros_step)
		(iframe-keep)
		(slice-due))
	(>= (get :ros_ticks stage) target))

(defun frame (msg mbox)
	;the scene brought on to where the app wants it, and drawn
	(bind '(scene seed step target shared px py dt width height playing is_film)
		(getf-> msg +motion_job_scene +motion_job_seed +motion_job_step +motion_job_target
			+motion_job_shared +motion_job_px +motion_job_py +motion_job_dt
			+motion_job_width +motion_job_height +motion_job_playing +motion_job_film))
	(scene-have scene seed (/= is_film 0))
	(defq start (pii-time) wanted :t)
	(cond
		;at its end a scene begins again, the same
		(restart (scene-make scene seed))
		;a film not where the app last had it is a child started again
		((and film (/= (get :ros_ticks stage) step))
			(setq wanted (seek-to step mbox))))
	(cond
		((not wanted))
		((>= target 0) (setq wanted (seek-to target mbox)))
		(:t (if (defq pointer (get :pointer stage))
				(. pointer :set_place :fig_x px :fig_y py))
			(def stage :ros_speed (if (/= playing 0) 1.0 0.0))
			(. stage :advance (/ (n2q dt) 1000000.0))
			(iframe-keep)))
	(defq drawn (pii-time) paper (ifn (get :paper_is stage) +argb_black))
	;the picture where a seek lands, not the way there
	(when (and wanted (defq canvas (canvas-attach held width height 1 shared +canvas_flag_antialias)))
		(defq k (/ (n2q width) (n2q +width)))
		(. canvas :fill paper)
		(. stage :draw canvas (fixeds k 0.0 0.0 0.0 k 0.0)))
	(mail-send (getf msg +job_reply) (setf-> (str-alloc +motion_reply_size)
			(+job_reply_key (getf msg +job_key))
			(+motion_reply_step (get :ros_ticks stage))
			(+motion_reply_figures (length (. stage :children)))
			(+motion_reply_paper paper)
			(+motion_reply_tick (- drawn start))
			(+motion_reply_draw (- (pii-time) drawn)))))

(defun measure (msg mbox)
	;how many steps the scene is: run from its seed to its end with no
	;picture, the node's other tasks between. The scene of this child is
	;then that one at its end, made again for a frame
	(bind '(scene seed) (getf-> msg +motion_job_scene +motion_job_seed))
	(scene-make scene seed)
	(setq have :nil)
	(while (and (not restart) (not (mail-poll (list mbox))))
		(. stage :tick +ros_step)
		(slice-due))
	(when restart
		(mail-send (getf msg +job_reply) (setf-> (str-alloc +motion_reply_size)
				(+job_reply_key (getf msg +job_key))
				(+motion_reply_step (get :ros_ticks stage))))))

(defun main ()
	;one with nothing to do for a while goes, the app has gone or its
	;frames have stopped
	(job-child 5000000 (lambda (msg mbox)
			(if (= (getf msg +motion_job_kind) +motion_kind_length)
				(measure msg mbox)
				(frame msg mbox)))))
