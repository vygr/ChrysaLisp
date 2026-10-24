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
;	live		the store this system is a member of, what it has and
;			then what it does, or of none, the stores of the other
;			machines, as a client. It is what the app opens on, and
;			what it goes back to after a scene
;	record		all of what the store of this system does is kept, and
;			the timeline goes back to the start. Off, the last few
;			thousand events
;	find		trace, only what was done to the object shown and its
;			blocks
;	on the right, the scenes of tests/storage/, and run runs the one
;	chosen, a store of its own, recorded and drawn as it goes
;
; A press on an object of the list, and that is the one shown, on an event
; of what was done, and the store is drawn as it was then. One of a gopher
; picks that gopher: its object is shown, its way drawn, and trace keeps
; to it. Another event, or it again, and none is picked. The wheel
; over the objects goes up and down them, over what was done it goes an
; event back or on, and the timeline is for the long way.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "././files/picker.inc")
(import "lib/cwb/doc.inc")
(import "service/lock/app.inc")
(import "service/storage/scene.inc")
(import "./replay.inc")

(enums +select 0
	(enum main run tip timer picker))

(enums +event 0
	(enum close max min)
	(enum play pause start back on end live record trace join travels run rec_open rec_save scene timeline
		snap_take snap_restore snap_drop))

(defq +height 560 +grid_w 540 +log_w 360 +live_keep 2000 +row_h 17 +log_h 16 +tick 50000 +columns 60 +wheel_line 4
	+hues ''(0xff2a7fff 0xffff7f2a 0xff2fb344 0xffc0399f 0xffd6a800 0xff20b2aa 0xffe0483e 0xff7a5cff)
	+ink 0xff202020 +dim 0xff909090 +paper 0xfff4f4f0 +none 0xffe2e2dc +trail 0xff000000 +mark 0xffdfe8f7)

;what is kept for the user, from one time it runs to the next: the scene
;chosen, trace, the object shown, and whether the store of this system is
;being recorded, which it opens on
(defq +config_file (cat (get :home *env*) "storage.tre") +config_version 2)

(defun config-of ()
	;what is to be kept, as it is now
	(list (elem-get *scenes* (ifn (. *scene* :get_selected) 0)) (if *trace* :t :nil) (ifn *want* (ifn *shown* ""))
		(if *record* :t :nil)))

(defun config-load ()
	(defq config (with-read-lock +config_file (tree-load (file-stream +config_file))))
	(if (or (not config) (/= (. config :find :version) +config_version))
		(setq config (scatter (Emap) :version +config_version :scene "demo" :trace :nil :shown "" :record :t)))
	(if (defq at (find (. config :find :scene) *scenes*)) (. *scene* :set_selected at))
	;the object that was shown is wanted, and shown when it is there
	(setq *trace* (. config :find :trace) *want* (. config :find :shown) *record* (. config :find :record)
		*saved* (config-of))
	(if (eql *want* "") (setq *want* :nil)))

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
				+sym_media_live +sym_media_record +sym_find +sym_media_join +sym_laptop) +event_play))
		;the scenes, and Run, on the right
		(ui-flow _ (:flow_flags +flow_left_fill)
			(ui-tool-bar *tests* (:paper :paper_toolbar2)
				(ui-buttons (+sym_play_all +sym_open +sym_save) +event_run))
			;the scenes there are as it starts, (ui-radio-bar) takes its names as written
			(. (ui-element *scene* (Radiobar *scenes*) (:paper :paper_toolbar2 :font :font_body
				:border :border_button)) :connect +event_scene)
			(ui-backdrop _ (:paper :paper_toolbar))))
	;snapshots of the store of this system: one taken, the one picked put
	;back, or dropped, and the name a new one is given
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *snapbar* (:paper :paper_toolbar2)
			(ui-buttons (+sym_save +sym_undo +sym_delete) +event_snap_take))
		(ui-textfield *snap_name* (:paper +argb_white :hint_text "the name of a new snapshot" :clear_text "" :min_width 200)))
	(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 18 :paper :paper_toolbar2)) :connect +event_timeline)
	(ui-label *status* (:text "..." :font :font_body))
	;the members and objects, and beside them what was done, two widgets so
	;each has its own turns of the wheel. Chris: "mouse wheel overrun
	;between the 2 lists, because they are not separate widgets"
	(ui-flow _ (:flow_flags +flow_right)
		(ui-canvas *canvas* +grid_w +height 1)
		(ui-canvas *log* +log_w +height 1)))

(ui-tool-tips *debug*
	'("play, at the speed it was made" "stop, and stay here" "to the start" "an event back" "an event on"
		"to the end, and follow what comes" "the store of this system, live"
		"keep all the store of this system does, or the last few thousand" "trace the object shown"
		"make this machine a member of the store of its mesh, now and each time it starts"
		"this machine is taken away, a laptop: it holds all of its user's area"))
(ui-tool-tips *tests* '("run the scene chosen" "open a recording, of storage -r or saved here"
	"save what is recorded, to play again or send"))
(ui-tool-tips *snapbar* '("a snapshot of the whole store, by the name written, or the time"
	"put the store back as it was in the snapshot picked" "drop the snapshot picked"))
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

(defun paths-all ()
	;the paths a member has now, deleted or not, in order
	(defq out (list) all (members))
	(. (. *replay* :find :all_paths) :each (lambda (path)
		(if (some (# (replay-path *replay* %0 path)) all) (push out path))))
	(sort out))

(defun paths-now ()
	;the objects listed: those of the snapshot picked, or those that are
	;not of any snapshot
	(defq at (if *snap* (cat +store_snapshots *snap* "/") +store_snapshots))
	(filter (if *snap* (# (starts-with at %0)) (# (not (starts-with at %0)))) (paths-all)))

(defun snaps-now ()
	; (snaps-now) -> ((name count) ...)
	;the snapshots there are now, of the paths a member has and has not
	;deleted, and how many objects each is of
	(defq counts (Fmap 11) names (list) all (members))
	(each (lambda (path)
		(when (and (starts-with +store_snapshots path)
				(some (lambda (id) (and (defq m (replay-path *replay* id path)) (nql m "-"))) all))
			(defq after (slice path (length +store_snapshots) -1) name (slice after 0 (ifn (find "/" after) 0)))
			(unless (. counts :find name) (push names name))
			(. counts :insert name (inc (ifn (. counts :find name) 0)))))
		(paths-all))
	(map (# (list %0 (. counts :find %0))) (sort names)))

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
	;the events of the gopher picked, or of the shown object and its
	;blocks, their indexes
	(defq set (trace-set) out (list))
	(each (lambda (e)
		(if (if *gopher* (eql (replay-gopher-tag e) *gopher*)
				(or (eql (elem-get e +ev_path) *shown*)
					(and (nempty? (elem-get e +ev_words)) (. set :find (first (elem-get e +ev_words))))))
			(push out (!))))
		(. *replay* :find :events))
	out)

(defun gopher-shown ()
	; (gopher-shown) -> :nil | (tag steps)
	;the gopher whose way is drawn: the one picked, or the last that was done
	;of the object shown, or the last to set out to it, its steps in the order they were taken, they come
	;as each member sends what it did
	(defq tag (ifn *gopher* (and *shown* (ifn (. (. *replay* :find :path_done) :find *shown*)
		(. (. *replay* :find :path_gopher) :find *shown*)))))
	(and tag (defq walk (replay-walk *replay* tag)) (eql (first walk) *shown*)
		(list tag (sort (cat (second walk)) (lambda ((& a ta) (& b tb)) (if (= ta tb) (- a b) (- ta tb)))))))

(defun fit (line most)
	; (fit line most) -> line
	;no more than so many characters, so it stays out of the event list
	;beside it: of a path, the end, which is what tells two apart
	(if (<= (length line) most) line (cat "..." (slice line (- (length line) (max 1 (- most 3))) -1))))

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
	;still, following what comes, recording, trace, and this machine a
	;member. Only when that is not as it was, a button changed is drawn again
	(defq now (list *playing* (not (or *playing* *follow*)) :nil :nil :nil (and *follow* (not *playing*))
			(and *run* *live*) *record* *trace* *joined* *travels*)
		now (map (# (if %0 :t :nil)) now))
	(unless (eql (str now) (str *lit*))
		(setq *lit* now)
		(defq lit (canvas-brighter (get :paper *debug*)))
		(each (lambda (button on)
			(undef (. button :dirty) :paper)
			(if on (def button :paper lit)))
			(. *debug* :children) now)))

(defun draw ()
	(lights)
	(defq items (list (cwb-shape (cwb-d-rect 0 0 +grid_w +height) :fill +paper :stroke 0))
		log_items (list (cwb-shape (cwb-d-rect 0 0 +log_w +height) :fill +paper :stroke 0))
		all (members) events (. *replay* :find :events) at (. *replay* :find :at) count (length events)
		paths (paths-now) manifest (if *shown* (manifest-of *shown*))
		blocks (if manifest (ifn (replay-blocks *replay* manifest) (list)) (list))
		left 200 top 50 row_h 34 grid_w +grid_w
		;no more columns than there is room for, the rest of a long object is
		;not drawn: the event list is beside it
		room (- grid_w left 14)
		cols (min (inc (length blocks)) (/ room 6)) cell (if (> cols 0) (max 6 (min 22 (/ room cols))) 16))
	(defun text (words x y &optional size ink into)
		(push (ifn into items) (cwb-text words x y :font "fonts/OpenSans-Regular.ctf" :font_size (ifn size 14) :fill (ifn ink +ink))))
	(text (if (= count 0) "Nothing recorded. Run a scene, or Live to watch the store of this system."
			(fit (said-now) 60)) 16 22 14)
	(when *shown*
		(defq about (if manifest (cat ", " (str (length blocks)) " blocks"
				(if (> (length blocks) (dec cols)) (cat ", the first " (str (dec cols)) " shown") "")
				", the first column its manifest") ", not here now"))
		;or the gopher picked, and the way it went
		(if (and *gopher* (defq walk (gopher-shown)))
			(text (fit (cat "gopher " (first walk) ", " (str (length (second walk))) " blocks, by way of "
				(join (reduce (lambda (way (who &ignore)) (if (eql (last way) (short who)) way (push way (short who))))
					(second walk) (list)) " ")) 86) 16 40 11 +dim)
			(text (cat (fit *shown* (- 86 (length about))) about) 16 40 11 +dim)))
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
	(when (and (defq walk (gopher-shown)) (> cols 1))
		(defq points (list))
		(each (lambda ((who block &ignore))
			(and (defq row (find who all)) (< (inc block) cols)
				(push points (+ left (* (inc block) cell) 4 (/ (- cell 2) 2)) (+ top (* row row_h) 17))))
			(second walk))
		(when (>= (length points) 4)
			(push items (cwb-shape (cwb-d-points points) :stroke +trail :width 2.0 :fill 0 :join :round))
			(push items (cwb-shape (cwb-d-ellipse (first points) (second points) 4) :fill +trail :stroke 0))))
	;the objects, a square for each member that has it
	;the snapshots, a name each to press, the one picked marked
	(defq y (+ top (* (max 1 (length all)) row_h) 22) chips (list) snaps (snaps-now))
	(when (nempty? snaps)
		(text "snapshots" 16 y 12 +dim)
		(defq x 86)
		(each (lambda ((name count))
			(defq label (cat name " " (str count)) w (+ 10 (* 6 (length label))))
			(when (< (+ x w) (- grid_w 10))
				(push items (cwb-shape (cwb-d-rect (- x 4) (- y 13) (+ x w -4) (+ y 4) 3)
					:fill (if (eql name *snap*) +mark +none) :stroke 0))
				(text label x y 12)
				(push chips (list x (+ x w) name)))
			(setq x (+ x w 6)))
			snaps)
		(setq y (+ y 22)))
	(def *canvas* :chips chips :chips_y (- y 22))
	(defq shown_at (ifn (find *shown* paths) 0)
		;as many objects as there is room for under the members
		most (max 1 (/ (- +height y 24) +row_h))
		from (max 0 (min (- shown_at (/ most 2)) (- (length paths) most))) rows (slice paths from (min (length paths) (+ from most))))
	(def *canvas* :rows_y y :rows_from from :rows_most most)
	(text (cat (str (length paths)) " objects" (if *snap* (cat " of the snapshot " *snap*) "")) 16 y 12 +dim)
	(each (lambda (path)
		(defq yy (+ y 20 (* (!) +row_h)))
		(if (eql path *shown*) (push items (cwb-shape (cwb-d-rect 10 (- yy 13) (- grid_w 10) (+ yy 4) 3) :fill +mark :stroke 0)))
		(each (lambda (id)
			(defq x (+ 16 (* (!) 11)) m (replay-path *replay* id path) hue (elem-get +hues (% (!) (length +hues))))
			(push items (cwb-shape (cwb-d-rect x (- yy 9) (+ x 9) yy 2)
				:fill (cond ((not m) +none) ((eql m "-") +dim) (hue)) :stroke 0)))
			all)
		(text (if *snap* (slice path (+ (length +store_snapshots) (length *snap*) 1) -1) path) (+ 24 (* (length all) 11)) yy 12))
		rows)
	;what was done, the event the timeline is at marked
	;the event marked is the last done, of those listed
	(defq lx 10 list_at (if *trace* (traced) (range 0 count))
		here (some (# (if (>= %0 (dec at)) (!))) list_at) here (ifn here (length list_at))
		marked (rsome (# (if (< %0 at) %0)) list_at)
		;as many events as there is room for, down the whole of it
		most_log (/ (- +height 44) +log_h)
		start (max 0 (min (- here (/ most_log 2)) (- (length list_at) most_log))))
	(text (cat (if *trace* "What was done to it" "What was done") ", " (str at) " of " (str count)) lx 22 12 +dim log_items)
	(defq log_shown (slice list_at start (min (length list_at) (+ start most_log))))
	(def *log* :log_shown log_shown)
	(each (lambda (index)
		(defq e (elem-get events index) yy (+ 44 (* (!) +log_h)))
		(if (eql index marked) (push log_items (cwb-shape (cwb-d-rect (- lx 4) (- yy 12) (- +log_w 6) (+ yy 4) 3) :fill +mark :stroke 0)))
		(defq line (event-text e))
		(text (slice line 0 (min 58 (length line))) lx yy 11 (if (< index at) +ink +dim) log_items))
		log_shown)
	(.-> *canvas* (:fill +paper))
	(cwb-draw-items *canvas* items :nil)
	(. *canvas* :swap +swap_write)
	(.-> *log* (:fill +paper))
	(cwb-draw-items *log* log_items :nil)
	(. *log* :swap +swap_write))

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
				pos (ifn (rsome (# (if (< %0 at) (!))) list_at) -1)
				to (max -1 (min (dec (length list_at)) (+ pos by))))
			(go-to (if (< to 0) 0 (inc (elem-get list_at to)))))
		((go-to (+ at by))))
	(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events)))))

(defun snap-action (id)
	;a snapshot taken, put back, or dropped, of the store of this system:
	;what is done comes back as it is recorded, and is drawn then
	(defq svc (storage-service))
	(cond
		((not (and svc *run* *live*))
			(say "Snapshots are of the store of this system, live goes back to it."))
		((= id +event_snap_take)
			(defq name (. *snap_name* :get_text))
			(when (eql name "")
				(bind '(sec min hour &ignore) (date))
				(setq name (cat "snap-" (pad (str hour) 2 "00") (pad (str min) 2 "00") (pad (str sec) 2 "00"))))
			(cond
				((defq count (storage-snapshot name "" svc))
					(say "Snapshot " name ", of " count " objects.")
					(.-> *snap_name* (:set_text "") :layout :dirty_all))
				((say "Not a name a snapshot may have, no / or space: " name))))
		((not *snap*) (say "Pick a snapshot first, above the objects."))
		((= id +event_snap_restore)
			(cond
				((defq got (storage-restore *snap* "" svc))
					(say (first got) " objects put back as they were in " *snap*
						(if (> (second got) 0) (cat ", " (str (second got)) " made since are left.") ".")))
				((say "No snapshot " *snap*))))
		(:t (defq count (storage-snapshot-drop *snap* svc))
			(say "Snapshot " *snap* " dropped, " count " paths.")
			(setq *snap* :nil *shown* :nil)
			(draw))))

(defun watch (what)
	;a scene run, or the store of this system, recorded and drawn from the start
	(when *run* (mail-send *run* "stop"))
	(setq *replay* (replay-new) *shown* :nil *gopher* :nil *snap* :nil *follow* :t *playing* :nil *live* (eql what "-")
		*run* (open-child "apps/system/storage/run.lisp" +kn_call_run))
	(mail-send *run* (cat (hex-encode (elem-get *select* +select_run)) (ascii-char 10) what))
	(timeline)
	(draw))

(defun picker (save)
	;the Files app's picker, to say where a recording is to go or come from
	(if *picker* (mail-send *picker* ""))
	(setq *picker_save* save *picker*
		(file-pick (elem-get *select* +select_picker) (if save "Save recording..." "Open recording...") "." "" save)))

(defun recording-save (file)
	;every event there is, a line each, as a recorder says them, so that
	;storage -r and this read the same
	(defq events (. *replay* :find :events))
	(cond
		((empty? events) (say "Nothing recorded to save."))
		((save (join (cat (map (const replay-line) events) (list "")) (ascii-char 10)) file)
			(say "Saved " (length events) " events to " file "."))
		((say "Could not save to " file "."))))

(defun recording-open (file)
	;a recording played back from its start: what is running stops, and
	;the store of this system is not watched
	(cond
		((not (defq text (load file))) (say "No file " file "."))
		(:t (when *run* (mail-send *run* "stop") (setq *run* :nil))
			(setq *replay* (replay-new) *shown* :nil *want* :nil *gopher* :nil *snap* :nil
				*follow* :nil *playing* :nil *live* :nil)
			(each (# (replay-add *replay* %0)) (split text (ascii-char 10)))
			(go-to 0)
			(say (if (empty? (. *replay* :find :events)) (cat file " has no events in it.")
				(cat "Opened " (str (length (. *replay* :find :events))) " events of " file ". Play, or the timeline."))))))

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
	;the turns of the wheel over each list, not yet done
	(defq id :t select (task-mboxes +select_size) wheel 0 wheel_log 0
		*select* select *replay* (replay-new) *run* :nil *live* :nil *shown* :nil *want* :nil *trace* :nil *lit* :nil *saved* :nil
		*gopher* :nil *snap* :nil *picker* :nil *picker_save* :nil
		;is this machine a member, it says so in a file, looked at the once
		*joined* (if (storage-member) :t)
		;and one that travels
		*travels* (if (. (ifn (storage-member) (Emap)) :find :travels) :t)
		*record* :t
		*follow* :t *playing* :nil *play_wall* 0 *play_time* 0 *changed* :nil)
	(. *scene* :set_selected (ifn (find "demo" *scenes*) 0))
	(config-load)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *log* :set_canvas_flags +canvas_flag_antialias)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +tick 0)
	;the store this system is a member of, as it is and then as it works,
	;or of none, those of the other machines of the mesh, as a client: what
	;is recorded is of every member there is, wherever it is
	(cond
		((storage-service)
			(say (if *record* "The store of this system, as it is and as it works, all of it recorded."
				(cat "The store of this system, as it is and as it works, the last " (str +live_keep) " events kept. Record keeps all.")))
			(watch "-"))
		((nempty? (storage-record-members))
			(say "This system is a member of no store. The stores of the other machines, as they are and as they work.")
			(watch "-"))
		(:t (say "No store on this system or the mesh. Choose a scene and Run.")
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
					((= id +event_rec_open) (picker :nil))
					((= id +event_rec_save) (picker :t))
					((= id +event_live)
						;back to the store of this system, if it is not what is watched
						(cond
							((and (not (storage-service)) (empty? (storage-record-members)))
								(say "No store on this system or the mesh, a scene is all there is to watch."))
							((and *run* *live*))
							((not (storage-service))
								(say "The stores of the other machines, as they are and as they work.")
								(watch "-"))
							(:t (say "The store of this system, as it is and as it works.")
								(watch "-"))))
					((= id +event_join)
						;this machine made a member, as storage -j does: its store is
						;made if there is none, started, and started by itself from
						;now on, and then it is what is watched
						(cond
							(*joined* (say "This machine is a member already. storage -f and it is not started by itself."))
							((storage-service)
								(say "This system is serving a store already, started by hand. Live watches it."))
							((not (storage-claim +storage_image))
								(say "Another session of this machine is serving its store."))
							(:t (say "Joining: the store of this machine is made if there is none, and started ...")
								(draw)
								(cond
									((storage-join)
										(storage-member-save +storage_image +storage_size)
										(setq *joined* :t)
										(say "This machine is a member, and is each time it starts. Its store, live.")
										(watch "-"))
									((say "The store of this machine could not be started."))))))
					((= id +event_travels)
						;this machine one that is taken away, or not, as storage -L
						(cond
							((not *joined*) (say "This machine is no member of a store, Join first."))
							((setq *travels* (not *travels*))
								(storage-member-travels :t)
								(say (if (defq pinned (storage-user-pin))
									(cat "This machine travels, and holds all of " pinned " from now on.")
									"This machine travels. Its user's area is pinned as its store starts.")))
							(:t (storage-member-travels :nil)
								(say "This machine does not travel now. What it has pinned stays, storage -U to let go."))))
					((= id +event_record)
						;whether all of it is kept, or the last so many events, of
						;the store of this system, now if it is what is watched
						(setq *record* (not *record*))
						(say (if *record* "All of what the store of this system does is kept."
							(cat "The last " (str +live_keep) " events of the store of this system are kept."))))
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
					((find id (list +event_snap_take +event_snap_restore +event_snap_drop))
						(snap-action id))
					((= id +event_timeline)
						(setq *playing* :nil)
						(go-to (get :value *timeline*))
						(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events)))))
					((= (getf msg +ev_msg_type) +ev_type_wheel)
						;the wheel goes up and down the list it was turned over, a
						;line for so much of a turn: a trackpad sends a great many
						;small ones added up, and done at the next tick, the once,
						;however many came: one at a time, each drawn, it ran on
						;after the wheel. The GUI gives a run of turns to the
						;widget the run began over, so each list has its own
						(if (= id (. *log* :get_id))
							(setq wheel_log (- wheel_log (getf msg +ev_msg_wheel_y)))
							(setq wheel (- wheel (getf msg +ev_msg_wheel_y)))))
					((and (= id (. *log* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;a press on an event of what was done, and the store is as
						;it was then
						(bind '(w h) (. *log* :get_size))
						(defq ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2))
							line (/ (- ry 32) +log_h) shown (get :log_shown *log*))
						(when (and (>= ry 32) (< line (length shown)))
							(setq *playing* :nil)
							;an event of a gopher picks it, its object is shown and
							;its way drawn; another event, or it again, and none is
							(defq e (elem-get (. *replay* :find :events) (elem-get shown line))
								tag (replay-gopher-tag e))
							(cond
								((and tag (nql tag *gopher*) (defq walk (replay-walk *replay* tag)))
									(setq *gopher* tag *want* :nil *shown* (first walk)))
								((setq *gopher* :nil)))
							(go-to (inc (elem-get shown line)))
							(setq *follow* (= (. *replay* :find :at) (length (. *replay* :find :events))))))
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;a press on an object of the list, and it is the one shown
						(bind '(w h) (. *canvas* :get_size))
						(defq ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2)) rx (getf msg +ev_msg_mouse_rx))
						(cond
							;a press on a snapshot picks it, its objects are listed;
							;on it again, and the objects of the store are
							((and (<= (- (get :chips_y *canvas*) 13) ry (+ (get :chips_y *canvas*) 4))
									(defq chip (some (lambda ((x0 x1 name)) (if (<= x0 rx x1) name)) (get :chips *canvas*))))
								(setq *snap* (if (eql chip *snap*) :nil chip) *shown* :nil *want* :nil *gopher* :nil)
								(go-to (. *replay* :find :at)))
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
			(+select_picker
				;where a recording is to go, or come from, or "" closed
				(mail-send *picker* "")
				(setq *picker* :nil msg (trim msg))
				(cond
					((eql msg ""))
					(*picker_save* (recording-save msg))
					((recording-open msg))))
			(+select_run
				(cond
					((starts-with "lines" msg)
						(each! (# (replay-add *replay* %0)) (list (split msg (ascii-char 10))) 1 -1)
						;the store of this system not recorded, the last so many kept
						(and *live* (not *record*) (replay-trim *replay* +live_keep))
						(setq *changed* :t))
					((starts-with "end " msg)
						(defq passed (second (split msg " ")) failed (third (split msg " ")))
						(setq *run* :nil *live* :nil)
						(say (if (eql passed "-") "Stopped."
							(cat "Done: " passed " held, " failed " did not. Live goes back to the store of this system."))))))
			(:t (mail-timeout (elem-get select +select_timer) +tick 0)
				;a control that has changed, that drew nothing, is lit or not, and
				;what is kept for the user is kept, if it has changed
				(lights)
				(config-save)
				;over what was done, so many events, or of the object shown,
				;with trace
				(when (/= 0 (defq lines (/ wheel_log +wheel_line)))
					(setq wheel_log (- wheel_log (* lines +wheel_line)) *playing* :nil)
					(step lines))
				;over the objects, so many objects on
				(when (/= 0 (defq lines (/ wheel +wheel_line)))
					(setq wheel (- wheel (* lines +wheel_line)))
					(when (nempty? (defq paths (paths-now)))
						(setq *want* :nil *shown* (elem-get paths (max 0 (min (dec (length paths)) (+ (ifn (find *shown* paths) 0) lines)))))
						(draw)))
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
	(if *picker* (mail-send *picker* ""))
	(if *run* (mail-send *run* "stop")))
