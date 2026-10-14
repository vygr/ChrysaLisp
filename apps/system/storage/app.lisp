;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a store watched as it works, and played back
;
; What the members of a store do, from a recording of them, ./replay.inc:
; the members down the side, the blocks of one object across and a square
; where a member keeps a block, the objects under it, and what was done,
; event by event, beside it. The timeline under the buttons is the whole
; of the recording: drag it, and the store is drawn as it was then.
;
;	scenes	the scenes of tests/storage/, Run runs the one chosen, a store
;		of its own, and it is recorded and drawn as it goes
;	Live	the store this system is a member of: what it has now, and
;		then what it does, recorded. It is what is shown first, if
;		this system is a member of a store. Stop stops that
;	< >	an event back, an event on, or of the object shown alone, with
;		Trace
;	Play	the recording at the speed it was made, from where it is
;	Trace	only what was done to the object shown and its blocks
;
; A press on an object of the list, and that is the one shown, and the
; wheel goes up and down the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/doc.inc")
(import "service/storage/scene.inc")
(import "./replay.inc")

(enums +select 0
	(enum main run timer))

(enums +event 0
	(enum close max min)
	(enum scene run live stop back on play trace timeline))

(defq +width 900 +height 560 +rows 9 +log_rows 22 +tick 50000 +columns 60 +wheel_line 4
	+hues ''(0xff2a7fff 0xffff7f2a 0xff2fb344 0xffc0399f 0xffd6a800 0xff20b2aa 0xffe0483e 0xff7a5cff)
	+ink 0xff202020 +dim 0xff909090 +paper 0xfff4f4f0 +none 0xffe2e2dc +trail 0xff000000 +mark 0xffdfe8f7)

;the scenes there are, a list made as this starts: not a + name, which is
;put in where it is used as it is read, and a list there is a call
(defq *scenes* (map (const first) (scene-list)))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Storage" (+sym_close) +event_close)
	(ui-tool-bar *toolbar* (:font *env_body_font*)
		;the scenes there are as it starts, (ui-radio-bar) takes its names as written
		(. (ui-element *scene* (Radiobar *scenes*) (:color *env_toolbar_col* :font *env_body_font*
			:border *env_button_border*)) :connect +event_scene)
		(each (lambda (label event) (. (ui-button _ (:text label :min_width 54)) :connect event))
			'("Run" "Live" "Stop" "<" ">" "Play" "Trace")
			(list +event_run +event_live +event_stop +event_back +event_on +event_play +event_trace)))
	(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 18 :color *env_toolbar2_col*)) :connect +event_timeline)
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-canvas *canvas* +width +height 1))

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

(defun draw ()
	(defq items (list (cwb-shape (cwb-d-rect 0 0 +width +height) :fill +paper :stroke 0))
		all (members) events (. *replay* :find :events) at (. *replay* :find :at) count (length events)
		paths (paths-now) manifest (if *shown* (manifest-of *shown*))
		blocks (if manifest (ifn (replay-blocks *replay* manifest) (list)) (list))
		left 200 top 50 row_h 34 grid_w 540
		cols (min (inc (length blocks)) +columns) cell (if (> cols 0) (max 6 (min 22 (/ (- grid_w left) cols))) 16))
	(defun text (words x y &optional size ink)
		(push items (cwb-text words x y :font "fonts/OpenSans-Regular.ctf" :font_size (ifn size 14) :fill (ifn ink +ink))))
	(text (if (= count 0) "Nothing recorded. Run a scene, or Live to watch the store of this system."
			(said-now)) 16 22 14)
	(if *shown* (text (cat *shown* (if manifest (cat ", " (str (length blocks)) " blocks, the first column its manifest") ", not here now"))
		16 40 11 +dim))
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
		(each (lambda ((who block))
			(and (defq row (find who all)) (< (inc block) cols)
				(push points (+ left (* (inc block) cell) 4 (/ (- cell 2) 2)) (+ top (* row row_h) 17))))
			(rest trail))
		(when (>= (length points) 4)
			(push items (cwb-shape (cwb-d-points points) :stroke +trail :width 2.0 :fill 0 :join :round))
			(push items (cwb-shape (cwb-d-ellipse (first points) (second points) 4) :fill +trail :stroke 0))))
	;the objects, a square for each member that has it
	(defq y (+ top (* (max 1 (length all)) row_h) 22) shown_at (ifn (find *shown* paths) 0)
		from (max 0 (min (- shown_at (/ +rows 2)) (- (length paths) +rows))) rows (slice paths from (min (length paths) (+ from +rows))))
	(def *canvas* :rows_y y :rows_from from)
	(text (cat (str (length paths)) " objects") 16 y 12 +dim)
	(each (lambda (path)
		(defq yy (+ y 20 (* (!) 18)))
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
		start (max 0 (min (- here (/ +log_rows 2)) (- (length list_at) +log_rows))))
	(text (cat (if *trace* "What was done to it" "What was done") ", " (str at) " of " (str count)) lx 22 12 +dim)
	(each (lambda (index)
		(defq e (elem-get events index) yy (+ 44 (* (!) 20)))
		(if (eql index marked) (push items (cwb-shape (cwb-d-rect (- lx 4) (- yy 14) (- +width 6) (+ yy 5) 3) :fill +mark :stroke 0)))
		(defq line (event-text e))
		(text (slice line 0 (min 58 (length line))) lx yy 11 (if (< index at) +ink +dim)))
		(slice list_at start (min (length list_at) (+ start +log_rows))))
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
	(unless *shown* (setq *shown* (first (paths-now))))
	(timeline)
	(draw))

(defun step (by)
	;an event on or back, or of the object shown, with Trace
	(defq at (. *replay* :find :at))
	(cond
		(*trace*
			(defq list_at (traced))
			(go-to (if (> by 0)
				(inc (ifn (some (# (if (>= %0 at) %0)) list_at) (dec at)))
				(ifn (some (# (if (< %0 (dec at)) (inc %0))) (reverse list_at)) 0))))
		((go-to (+ at by))))
	(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events)))))

(defun watch (what)
	;a scene run, or the store of this system, recorded and drawn from the start
	(when *run* (mail-send *run* "stop"))
	(setq *replay* (replay-new) *shown* :nil *follow* :t *playing* :nil
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
	(defq id :t select (task-mboxes +select_size) wheel 0
		*select* select *replay* (replay-new) *run* :nil *shown* :nil *trace* :nil
		*follow* :t *playing* :nil *play_wall* 0 *play_time* 0 *changed* :nil)
	(. *scene* :set_selected (ifn (find "demo" *scenes*) 0))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +tick 0)
	;the store this system is a member of, as it is and then as it works,
	;if it is a member of one
	(cond
		((storage-service)
			(say "The store this system is a member of, as it is now, and then as it works.")
			(watch "-"))
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
						(watch name))
					((= id +event_live)
						(say "The store of this system, as it works from now.")
						(watch "-"))
					((= id +event_stop)
						(when *run* (mail-send *run* "stop") (setq *run* :nil))
						(setq *playing* :nil))
					((= id +event_back) (setq *playing* :nil) (step -1))
					((= id +event_on) (setq *playing* :nil) (step 1))
					((= id +event_play) (if *playing* (setq *playing* :nil) (play)))
					((= id +event_trace) (setq *trace* (not *trace*)) (draw))
					((= id +event_timeline)
						(setq *playing* :nil)
						(go-to (get :value *timeline*))
						(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events)))))
					((= (getf msg +ev_msg_type) +ev_type_wheel)
						;the wheel goes up and down the objects, a line for so much
						;of a turn: a trackpad sends a great many small ones
						(setq wheel (- wheel (getf msg +ev_msg_wheel_y)))
						(when (/= 0 (defq lines (/ wheel +wheel_line)))
							(setq wheel (- wheel (* lines +wheel_line)))
							(when (nempty? (defq paths (paths-now)))
								(setq *shown* (elem-get paths (max 0 (min (dec (length paths)) (+ (ifn (find *shown* paths) 0) lines)))))
								(draw))))
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;a press on an object of the list
						(bind '(w h) (. *canvas* :get_size))
						(defq ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2)) rx (getf msg +ev_msg_mouse_rx)
							line (/ (- ry (+ (get :rows_y *canvas*) 7)) 18) paths (paths-now)
							index (+ (get :rows_from *canvas*) line))
						(when (and (< rx 510) (>= ry (+ (get :rows_y *canvas*) 7)) (< line +rows) (< index (length paths)))
							(setq *shown* (elem-get paths index))
							(draw)))
					((. *window* :event msg))))
			(+select_run
				(cond
					((starts-with "lines" msg)
						(each (# (replay-add *replay* %0)) (rest (split msg (ascii-char 10))))
						(setq *changed* :t))
					((starts-with "end " msg)
						(bind '(passed failed) (rest (split msg " ")))
						(setq *run* :nil)
						(say (if (eql passed "-") "Stopped." (cat "Done: " passed " held, " failed " did not."))))))
			(:t (mail-timeout (elem-get select +select_timer) +tick 0)
				(cond
					(*playing* (tick))
					(*changed*
						(setq *changed* :nil)
						(cond
							(*follow* (go-to (length (. *replay* :find :events))))
							(:t (timeline) (draw))))))))
	(gui-sub-rpc *window*)
	(mail-timeout (elem-get select +select_timer) 0 0)
	(if *run* (mail-send *run* "stop")))
