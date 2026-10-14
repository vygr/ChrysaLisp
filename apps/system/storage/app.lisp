;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a store watched as it works, and played back
;
; What the members of a store do, from a recording of them, ./replay.inc:
; the members down the side, the blocks of one object across and a square
; where a member keeps a block, the objects under it, and what was done,
; event by event, beside it. The timeline under the buttons is the whole
; of the recording: drag it, and the store is drawn as it was then.
;
;	on the left, what is watched, played and stepped through:
;	prev, step	an event back, an event on, or of the object shown
;			alone, with trace
;	play, pause	the recording at the speed it was made, from where it
;			is, or still, there, whatever comes in
;	start, end	to the start, or to the end, and what comes is followed
;	record		the store this system is a member of: what it has now,
;			and then what it does. It is what is shown first, if
;			this system is a member of a store and it was on when
;			the app was last closed. Again, it stops
;	find		trace, only what was done to the object shown and its
;			blocks
;	on the right, the scenes of tests/storage/, and run runs the one
;	chosen, a store of its own, recorded and drawn as it goes
;
; A press on an object of the list, and that is the one shown, on an event
; of what was done, and the store is drawn as it was then. The wheel
; over the objects goes up and down them, over what was done it goes an
; event back or on, and the timeline is for the long way.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/doc.inc")
(import "service/lock/app.inc")
(import "service/storage/scene.inc")
(import "./replay.inc")

(enums +select 0
	(enum main run tip timer))

(enums +event 0
	(enum close max min)
	(enum play pause start back on end record trace run scene timeline))

(defq +width 900 +height 560 +grid_w 540 +row_h 17 +log_h 16 +tick 50000 +columns 60 +wheel_line 4
	+hues ''(0xff2a7fff 0xffff7f2a 0xff2fb344 0xffc0399f 0xffd6a800 0xff20b2aa 0xffe0483e 0xff7a5cff)
	+ink 0xff202020 +dim 0xff909090 +paper 0xfff4f4f0 +none 0xffe2e2dc +trail 0xff000000 +mark 0xffdfe8f7)

;what is kept for the user, from one time it runs to the next: the scene
;chosen, trace, the object shown, and whether the store of this system is
;being recorded, which it opens on
(defq +config_file (cat *env_home* "storage.tre") +config_version 2)

(defun config-load ()
	(defq config (with-read-lock +config_file (tree-load (file-stream +config_file))))
	(if (or (not config) (/= (. config :find :version) +config_version))
		(setq config (scatter (Emap) :version +config_version :scene "demo" :trace :nil :shown "" :record :t)))
	(if (defq at (find (. config :find :scene) *scenes*)) (. *scene* :set_selected at))
	;the object that was shown is wanted, and shown when it is there
	(setq *trace* (. config :find :trace) *want* (. config :find :shown) *record* (. config :find :record)
		*saved* (config-of))
	(if (eql *want* "") (setq *want* :nil)))

(defun config-of ()
	;what is to be kept, as it is now
	(list (elem-get *scenes* (ifn (. *scene* :get_selected) 0)) (if *trace* :t :nil) (ifn *want* (ifn *shown* ""))
		(if *record* :t :nil)))

(defun config-save ()
	;only when it is not as it was kept
	(defq now (config-of))
	(unless (eql (str now) (str *saved*))
		(setq *saved* now)
		(bind '(scene trace shown record) now)
		(with-write-lock +config_file
			(tree-save (file-stream +config_file +file_open_write)
				(scatter (Emap) :version +config_version :scene scene :trace trace :shown shown :record record)))))

;the scenes there are, a list made as this starts: not a + name, which is
;put in where it is used as it is read, and a list there is a call
(defq *scene_list* (scene-list) *scenes* (map (const first) *scene_list*))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Storage" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		;what is watched, played and stepped through, on the left
		(ui-tool-bar *debug* ()
			(ui-buttons (+sym_media_play +sym_media_pause +sym_media_start +sym_media_back +sym_media_on +sym_media_end
				+sym_media_record +sym_find) +event_play))
		;the scenes, and Run, on the right
		(ui-flow _ (:flow_flags +flow_left_fill)
			(ui-tool-bar *tests* (:color (const *env_toolbar2_col*))
				(ui-buttons (+sym_play_all) +event_run))
			;the scenes there are as it starts, (ui-radio-bar) takes its names as written
			(. (ui-element *scene* (Radiobar *scenes*) (:color *env_toolbar2_col* :font *env_body_font*
				:border *env_button_border*)) :connect +event_scene)
			(ui-backdrop _ (:color (const *env_toolbar_col*)))))
	(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 18 :color *env_toolbar2_col*)) :connect +event_timeline)
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-canvas *canvas* +width +height 1))

(ui-tool-tips *debug*
	'("play, at the speed it was made" "stop, and stay here" "to the start" "an event back" "an event on"
		"to the end, and follow what comes" "record the store of this system, or stop" "trace the object shown"))
(ui-tool-tips *tests* '("run the scene chosen"))
;each scene's own first line, what it is for
(ui-tool-tips *scene* (map (const second) *scene_list*))

(defun say (&rest text)
	(set *status* :text (apply (const cat) (map (const str) text)))
	(.-> *status* :layout :dirty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what the store was, at the event the timeline is at
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun members ()
	;those of the store, not the scene that says what it does
	(filter (# (nql %0 "-")) (. *replay* :find :members)))

(defun paths-now ()
	;the paths a member has now, deleted or not, in order
	(defq out (list) all (members))
	(. (. *replay* :find :all_paths) :each (lambda (path)
		(if (some (# (replay-path *replay* %0 path)) all) (push out path))))
	(sort out))

(defun manifest-of (path)
	;what the members that have the path have it as, the first that has
	(some (lambda (id) (and (defq m (replay-path *replay* id path)) (nql m "-") m)) (members)))

(defun said-now ()
	;the last thing the scene said, at or before the event
	(defq events (. *replay* :find :events) at (. *replay* :find :at) i at text "")
	(while (and (> i 0) (eql text ""))
		(-- i)
		(defq e (elem-get events i))
		(if (eql (elem-get e +ev_kind) "say") (setq text (join (elem-get e +ev_words) " "))))
	text)

(defun trace-set ()
	;the shown object's manifest and blocks, for (replay-about?), as a set
	(defq set (Fset 31))
	(when (defq m (manifest-of *shown*))
		(. set :insert m)
		(each (# (. set :insert %0)) (ifn (replay-blocks *replay* m) (list))))
	set)

(defun traced ()
	;the events of the shown object and its blocks, their indexes
	(defq set (trace-set) out (list))
	(each (lambda (e)
		(if (or (eql (elem-get e +ev_path) *shown*)
				(and (nempty? (elem-get e +ev_words)) (. set :find (first (elem-get e +ev_words)))))
			(push out (!))))
		(. *replay* :find :events))
	out)

(defun fit (text most)
	; (fit text most) -> text
	;no more than so many characters, so it stays out of the event list
	;beside it: of a path, the end, which is what tells two apart
	(if (<= (length text) most) text (cat "..." (slice text (- (length text) (max 1 (- most 3))) -1))))

(defun short (id)
	;a member as it is shown, a machine's id is 32 hex digits
	(slice id 0 (min 8 (length id))))

(defun event-text (e)
	;an event as a line of the log, its time from the first
	(defq t0 (elem-get (first (. *replay* :find :events)) +ev_time) ms (/ (- (elem-get e +ev_time) t0) 1000))
	(cat (str (/ ms 1000)) "." (pad (str (% ms 1000)) 3 "000") " " (short (elem-get e +ev_member)) " " (elem-get e +ev_kind)
		(cond
			((empty? (elem-get e +ev_words)) "")
			;what a scene says is all of it, the rest are names
			((eql (elem-get e +ev_kind) "say") (cat " " (join (elem-get e +ev_words) " ")))
			((cat " " (join (slice (elem-get e +ev_words) 0 (min 3 (length (elem-get e +ev_words)))) " "))))
		(if (elem-get e +ev_path) (cat " " (elem-get e +ev_path)) "")))

;;;;;;;;;;
; drawing
;;;;;;;;;;

(defun lights ()
	;the controls that are on are lit, as a radio bar's are: playing,
	;still, following what comes, recording, and trace. Only when that is
	;not as it was, a button changed is drawn again
	(defq now (list *playing* (not (or *playing* *follow*)) :nil :nil :nil (and *follow* (not *playing*))
			(and *run* *live*) *trace*)
		now (map (# (if %0 :t :nil)) now))
	(unless (eql (str now) (str *lit*))
		(setq *lit* now)
		(defq lit (canvas-brighter (get :color *debug*)))
		(each (lambda (button on)
			(undef (. button :dirty) :color)
			(if on (def button :color lit)))
			(. *debug* :children) now)))

(defun draw ()
	(lights)
	(defq items (list (cwb-shape (cwb-d-rect 0 0 +width +height) :fill +paper :stroke 0))
		all (members) events (. *replay* :find :events) at (. *replay* :find :at) count (length events)
		paths (paths-now) manifest (if *shown* (manifest-of *shown*))
		blocks (if manifest (ifn (replay-blocks *replay* manifest) (list)) (list))
		left 200 top 50 row_h 34 grid_w +grid_w
		;no more columns than there is room for, the rest of a long object is
		;not drawn: the event list is beside it
		room (- grid_w left 14)
		cols (min (inc (length blocks)) (/ room 6)) cell (if (> cols 0) (max 6 (min 22 (/ room cols))) 16))
	(defun text (words x y &optional size ink)
		(push items (cwb-text words x y :font "fonts/OpenSans-Regular.ctf" :font_size (ifn size 14) :fill (ifn ink +ink))))
	(text (if (= count 0) "Nothing recorded. Run a scene, or Live to watch the store of this system."
			(fit (said-now) 60)) 16 22 14)
	(when *shown*
		(defq about (if manifest (cat ", " (str (length blocks)) " blocks"
				(if (> (length blocks) (dec cols)) (cat ", the first " (str (dec cols)) " shown") "")
				", the first column its manifest") ", not here now"))
		(text (cat (fit *shown* (- 86 (length about))) about) 16 40 11 +dim))
	;the first column, the manifest, is ruled off from the blocks
	(and manifest (> cols 1)
		(push items (cwb-shape (cwb-d-rect (+ left cell) (+ top 4) (+ left cell 2) (+ top (* (length all) row_h) -4) 0)
			:fill +dim :stroke 0)))
	;a row for each member, a square where it keeps a block of the object shown
	(each (lambda (id)
		(defq y (+ top (* (!) row_h)) hue (elem-get +hues (% (!) (length +hues)))
			held (replay-held *replay* id) has (and *shown* (replay-path *replay* id *shown*))
			cut (. (. *replay* :find :cut) :find id))
		(push items (cwb-shape (cwb-d-rect 14 (+ y 6) 28 (+ y 20) 3) :fill hue :stroke 0))
		(text (short id) 34 (+ y 18) 14)
		;cut from most of the others is cut off, from a few it is they that are
		(defq cut (filter (const nempty?) (split cut " ")))
		(text (cat (str (. held :size)) " blocks, sees " (str (length (. (. *replay* :find :peers) :find id)))
			(cond
				((empty? cut) "")
				((> (* 2 (length cut)) (dec (length all))) ", cut off")
				((cat ", cut from " (join (map (const short) cut) " "))))) 34 (+ y 31) 10 +dim)
		(each (lambda (col)
			(defq x (+ left (* col cell) (if (> col 0) 4 0))
				keeps (if (= col 0) (eql has manifest) (. held :find (elem-get blocks (dec col)))))
			(push items (cwb-shape (cwb-d-rect x (+ y 6) (+ x (- cell 2)) (+ y 28) 2) :fill (if keeps hue +none) :stroke 0)))
			(if manifest (range 0 cols) (list))))
		all)
	;where the gopher of the object shown has been, block by block
	(defq trail (. *replay* :find :trail))
	(when (and (nempty? trail) (eql (first trail) *shown*) (> cols 1))
		(defq points (list))
		;in the order the steps were taken: they come as each member sends
		;what it did, not in the order it was done
		(each (lambda ((who block &ignore))
			(and (defq row (find who all)) (< (inc block) cols)
				(push points (+ left (* (inc block) cell) 4 (/ (- cell 2) 2)) (+ top (* row row_h) 17))))
			(sort (rest trail) (lambda ((& a ta) (& b tb)) (if (= ta tb) (- a b) (- ta tb)))))
		(when (>= (length points) 4)
			(push items (cwb-shape (cwb-d-points points) :stroke +trail :width 2.0 :fill 0 :join :round))
			(push items (cwb-shape (cwb-d-ellipse (first points) (second points) 4) :fill +trail :stroke 0))))
	;the objects, a square for each member that has it
	(defq y (+ top (* (max 1 (length all)) row_h) 22) shown_at (ifn (find *shown* paths) 0)
		;as many objects as there is room for under the members
		most (max 1 (/ (- +height y 24) +row_h))
		from (max 0 (min (- shown_at (/ most 2)) (- (length paths) most))) rows (slice paths from (min (length paths) (+ from most))))
	(def *canvas* :rows_y y :rows_from from :rows_most most)
	(text (cat (str (length paths)) " objects") 16 y 12 +dim)
	(each (lambda (path)
		(defq yy (+ y 20 (* (!) +row_h)))
		(if (eql path *shown*) (push items (cwb-shape (cwb-d-rect 10 (- yy 13) (- grid_w 10) (+ yy 4) 3) :fill +mark :stroke 0)))
		(each (lambda (id)
			(defq x (+ 16 (* (!) 11)) m (replay-path *replay* id path) hue (elem-get +hues (% (!) (length +hues))))
			(push items (cwb-shape (cwb-d-rect x (- yy 9) (+ x 9) yy 2)
				:fill (cond ((not m) +none) ((eql m "-") +dim) (hue)) :stroke 0)))
			all)
		(text path (+ 24 (* (length all) 11)) yy 12))
		rows)
	;what was done, the event the timeline is at marked
	;the event marked is the last done, of those listed
	(defq lx (+ grid_w 10) list_at (if *trace* (traced) (range 0 count))
		here (some (# (if (>= %0 (dec at)) (!))) list_at) here (ifn here (length list_at))
		marked (some (# (if (< %0 at) %0)) (reverse (cat list_at)))
		;as many events as there is room for, down the whole of it
		most_log (/ (- +height 44) +log_h)
		start (max 0 (min (- here (/ most_log 2)) (- (length list_at) most_log))))
	(text (cat (if *trace* "What was done to it" "What was done") ", " (str at) " of " (str count)) lx 22 12 +dim)
	(defq log_shown (slice list_at start (min (length list_at) (+ start most_log))))
	(def *canvas* :log_shown log_shown)
	(each (lambda (index)
		(defq e (elem-get events index) yy (+ 44 (* (!) +log_h)))
		(if (eql index marked) (push items (cwb-shape (cwb-d-rect (- lx 4) (- yy 12) (- +width 6) (+ yy 4) 3) :fill +mark :stroke 0)))
		(defq line (event-text e))
		(text (slice line 0 (min 58 (length line))) lx yy 11 (if (< index at) +ink +dim)))
		log_shown)
	(.-> *canvas* (:fill +paper))
	(cwb-draw-items *canvas* items :nil)
	(. *canvas* :swap +swap_write))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; the timeline, and moving
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun timeline ()
	;the timeline is the whole of the recording, and is where it is now
	(set *timeline* :maximum (length (. *replay* :find :events)))
	(set (. *timeline* :dirty) :value (. *replay* :find :at)))

(defun go-to (at)
	(replay-seek *replay* at)
	;the one wanted if it is there now, if not the first till it is
	(defq paths (paths-now))
	(cond
		((and *want* (find *want* paths)) (setq *shown* *want* *want* :nil))
		;one shown that is not there at this event is said so, and kept
		((not *shown*) (setq *shown* (first paths))))
	(timeline)
	(draw))

(defun step (by)
	;so many events on or back, or of the object shown, with trace, and
	;drawn the once
	(defq at (. *replay* :find :at))
	(cond
		(*trace*
			;from the last of them that has been done
			(defq list_at (traced)
				pos (ifn (some (# (if (< %0 at) (!))) (reverse (cat list_at))) -1)
				pos (if (>= pos 0) (- (length list_at) 1 pos) -1)
				to (max -1 (min (dec (length list_at)) (+ pos by))))
			(go-to (if (< to 0) 0 (inc (elem-get list_at to)))))
		((go-to (+ at by))))
	(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events)))))

(defun watch (what)
	;a scene run, or the store of this system, recorded and drawn from the start
	(when *run* (mail-send *run* "stop"))
	(setq *replay* (replay-new) *shown* :nil *follow* :t *playing* :nil *live* (eql what "-")
		*run* (open-child "apps/system/storage/run.lisp" +kn_call_run))
	(mail-send *run* (cat (hex-encode (elem-get *select* +select_run)) (ascii-char 10) what))
	(timeline)
	(draw))

(defun play ()
	;from where it is, at the speed it was made
	(defq events (. *replay* :find :events) at (. *replay* :find :at))
	(when (< at (length events))
		(setq *playing* :t *follow* :nil *play_wall* (pii-time)
			*play_time* (elem-get (elem-get events (max 0 (dec at))) +ev_time))))

(defun tick ()
	;the timer: playing, the events up to now, as they were made
	(defq events (. *replay* :find :events) at (. *replay* :find :at))
	(when *playing*
		(defq now (+ *play_time* (- (pii-time) *play_wall*)))
		(while (and (< at (length events)) (<= (elem-get (elem-get events at) +ev_time) now)) (++ at))
		(if (>= at (length events)) (setq *playing* :nil))
		(go-to at)))

(defun main ()
	;where the mouse was last over the canvas, which list the wheel turns
	(defq id :t select (task-mboxes +select_size) wheel 0 mouse_rx 0
		*select* select *replay* (replay-new) *run* :nil *live* :nil *shown* :nil *want* :nil *trace* :nil *lit* :nil *saved* :nil
		*record* :t
		*follow* :t *playing* :nil *play_wall* 0 *play_time* 0 *changed* :nil)
	(. *scene* :set_selected (ifn (find "demo" *scenes*) 0))
	(config-load)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +tick 0)
	;the store this system is a member of, as it is and then as it works,
	;if it is a member of one
	(cond
		((and (storage-service) *record*)
			(say "The store this system is a member of, as it is now, and then as it works.")
			(watch "-"))
		((storage-service)
			(say "Not recording the store of this system, record does.")
			(draw))
		(:t (say "This system is a member of no store. Choose a scene and Run.")
			(draw)))
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close) (setq id :nil))
					((= id +event_run)
						(defq name (elem-get *scenes* (ifn (. *scene* :get_selected) 0)))
						(say "Running " name ".")
						;the store of this system is not recorded while a scene runs
						(setq *record* :nil)
						(watch name))
					((= id +event_record)
						;the store of this system, or, if that is what is being
						;recorded, stop
						(cond
							((and *run* *live*)
								(mail-send *run* "stop")
								(setq *run* :nil *live* :nil *record* :nil)
								(say "Not recording."))
							(:t (say "The store of this system, as it is now, and then as it works.")
								(setq *record* :t)
								(watch "-"))))
					((= id +event_back) (setq *playing* :nil) (step -1))
					((= id +event_on) (setq *playing* :nil) (step 1))
					((= id +event_play) (play))
					;still, here: not played, and not following what comes in
					;either, end follows again
					((= id +event_pause) (setq *playing* :nil *follow* :nil))
					((= id +event_start)
						(setq *playing* :nil *follow* :nil)
						(go-to 0))
					((= id +event_end)
						(setq *playing* :nil *follow* :t)
						(go-to (length (. *replay* :find :events))))
					((= id +event_trace) (setq *trace* (not *trace*)) (draw))
					((= id +event_timeline)
						(setq *playing* :nil)
						(go-to (get :value *timeline*))
						(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events)))))
					((= (getf msg +ev_msg_type) +ev_type_wheel)
						;the wheel goes up and down the objects, a line for so much
						;of a turn: a trackpad sends a great many small ones
						;added up, and done at the next tick, the once, however many
						;came: one at a time, each drawn, it ran on after the wheel
						(setq wheel (- wheel (getf msg +ev_msg_wheel_y))))
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse)
							(progn (setq mouse_rx (getf msg +ev_msg_mouse_rx)) (= (getf msg +ev_msg_mouse_buttons) 0)))
						;where it is, and no more: the wheel is of the list it is over
						:nil)
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;a press on an object of the list, and it is the one shown, or
						;on an event of what was done, and the store is as it was then
						(bind '(w h) (. *canvas* :get_size))
						(defq ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2)) rx (getf msg +ev_msg_mouse_rx))
						(cond
							((>= rx +grid_w)
								(defq line (/ (- ry 32) +log_h) shown (get :log_shown *canvas*))
								(when (and (>= ry 32) (< line (length shown)))
									(setq *playing* :nil)
									(go-to (inc (elem-get shown line)))
									(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events))))))
							(:t (defq line (/ (- ry (+ (get :rows_y *canvas*) 7)) +row_h) paths (paths-now)
									index (+ (get :rows_from *canvas*) line))
								(when (and (>= ry (+ (get :rows_y *canvas*) 7)) (< line (get :rows_most *canvas*)) (< index (length paths)))
									(setq *want* :nil *shown* (elem-get paths index))
									(draw)))))
					((. *window* :event msg))))
			(+select_tip
				;what a button is for
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(+select_run
				(cond
					((starts-with "lines" msg)
						(each (# (replay-add *replay* %0)) (rest (split msg (ascii-char 10))))
						(setq *changed* :t))
					((starts-with "end " msg)
						(bind '(passed failed) (rest (split msg " ")))
						(setq *run* :nil *live* :nil)
						(say (if (eql passed "-") "Stopped." (cat "Done: " passed " held, " failed " did not."))))))
			(:t (mail-timeout (elem-get select +select_timer) +tick 0)
				;a control that has changed, that drew nothing, is lit or not, and
				;what is kept for the user is kept, if it has changed
				(lights)
				(config-save)
				(when (/= 0 (defq lines (/ wheel +wheel_line)))
					(setq wheel (- wheel (* lines +wheel_line)))
					(cond
						((>= mouse_rx +grid_w)
							;over what was done, so many events, or of the object
							;shown, with trace
							(setq *playing* :nil)
							(step lines))
						((nempty? (defq paths (paths-now)))
							(setq *want* :nil *shown* (elem-get paths (max 0 (min (dec (length paths)) (+ (ifn (find *shown* paths) 0) lines)))))
							(draw))))
				(cond
					(*playing* (tick))
					(*changed*
						(setq *changed* :nil)
						(cond
							(*follow* (go-to (length (. *replay* :find :events))))
							(:t (timeline) (draw))))))))
	(config-save)
	(gui-sub-rpc *window*)
	(mail-timeout (elem-get select +select_timer) 0 0)
	(if *run* (mail-send *run* "stop")))
