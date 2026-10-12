;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a store and its members, to look at
;
; The members of a store down the side, the blocks of one object across,
; and a square where a member keeps a block. Under it the objects, and how
; many of the members have each.
;
; It shows the store this system is a member of, if its Storage service is
; running, the members on every machine. If not, and when Demo is pressed,
; it starts five members of a store of their own, each on a node of this
; machine, one after another of the nodes there are, each with a little
; store in a file under obj/: all of what a store of five machines does,
; on the one. Then
;
; What the store is now is not asked by this: a store that is busy is slow
; to say, and the window would wait. A task of its own asks, ./watch.lisp,
; and mails what there is to draw. This draws what it was last sent.
;
;	Put	an object of some blocks is put at one of the members
;	Next	the next object is the one shown. A press on a line of the
;		list and that one is, and the wheel goes up and down it
;	Gopher	a gopher is sent to it, to add up its bytes: the line is
;		where it went, member to member, block by block
;	Stop	a member is switched off, Start, those that are, on
;	Cut	a member can reach none of the others, nor they it
;	Heal	all can reach all
;	Fork	a path is written at two members at once: with one cut off,
;		neither knows of the other
;	Pin	the object shown is pinned at a member, which fetches all of
;		it and keeps it: a gopher that gets there goes no further.
;		Again, and it is pinned nowhere
;	Tree	a new store of five, with room, and every file of the
;		ChrysaLisp folder is put in it, as a stream, by a farm of
;		workers, at all the members at once, each file at the next
;		of them, then read back as a stream and compared: ./tree.lisp
;	Step	the members look at each other only when told, and then,
;		each press, the next of them looks: it compares notes with
;		the others, fetches what is its to keep, lets go of what is not
;	Run	they look by the clock again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/doc.inc")
(import "service/storage/app.inc")

(enums +select 0
	(enum main tree watch tip))

(enums +event 0
	(enum close max min)
	(enum demo put next gopher stop start cut heal fork step run pin tree))

(defq +width 860 +height 520 +columns 115 +rows 9 +wheel_line 4 +tick 700000 +grace 3000000
	+demo_ids ''("m-a" "m-b" "m-c" "m-d" "m-e")
	+hues ''(0xff2a7fff 0xffff7f2a 0xff2fb344 0xffc0399f 0xffd6a800 0xff20b2aa 0xffe0483e 0xff7a5cff)
	+ink 0xff202020 +dim 0xff909090 +paper 0xfff4f4f0 +none 0xffe2e2dc +trail 0xff000000)

(ui-window *window* ()
	(ui-title-bar _ "Storage" (+sym_close) +event_close)
	(ui-tool-bar *toolbar* (:font *env_body_font*)
		(each (lambda (label event) (. (ui-button _ (:text label :min_width 62)) :connect event))
			'("Demo" "Put" "Next" "Gopher" "Stop" "Start" "Cut" "Heal" "Fork" "Pin" "Tree" "Step" "Run")
			(list +event_demo +event_put +event_next +event_gopher +event_stop +event_start +event_cut +event_heal +event_fork
				+event_pin +event_tree +event_step +event_run)))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-canvas *canvas* +width +height 1))

(ui-tool-tips *toolbar* '("the store of this machine, or a demo of five" "put an object at a member"
	"show the next object" "send a gopher to the object shown" "switch a member off" "switch them all on"
	"cut a member off from the rest" "all can reach all again" "write one path at two members"
	"pin the object shown at a member" "put this whole folder in, and read it back"
	"look only when told, a member each press" "look by the clock again"))

(defun say (&rest text)
	(def *status* :text (apply (const cat) text))
	(.-> *status* :layout :dirty))

;;;;;;;;;;;;;;;;;;;;;;;
; the members of a demo
;;;;;;;;;;;;;;;;;;;;;;;

;a member is known to this by where it was started, not looked up by a
;name: a node whose only service of its own is forgotten does not say so,
;and its name is known on the others for ever. So a name is of the one
;start, never used again, and *svcs* is who is there, each (id netid)
(defun demo-name (id) (cat "@StorageDemo" *demo_tag* (str (++ *starts*)) "_" id))
(defun demo-image (id) (cat "obj/storage_demo_" id ".img"))

(defun demo-start-member (id)
	;little stores of little blocks, to see an object as many. For a tree
	;of files, room, and blocks of the length a store has when not told
	;each on a node of this machine, the next of them in turn, so that five
	;members are the work of five cores where there are five
	(defq nodes (lisp-nodes :t))
	(setq *svcs* (filter (# (nql (first %0) id)) *svcs*))
	(when (defq svc (storage-start (demo-image id) :size (if *big* 0x10000000 0x400000) :id "demo" :name (demo-name id)
		:block (if *big* 65536 4096) :node (elem-get nodes (% (ifn (find id +demo_ids) 0) (length nodes)))
		:group *group* :member id :tick (if *stepping* 0 +tick) :grace (if *stepping* 0 +grace)))
		(push *svcs* (list id svc))))

(defun demo-start ()
	;five members of a store of their own, new each time
	;of a name of its own: the names of the last, on other nodes, are
	;known here for a moment after they have gone
	(setq *demo_tag* (cat (str (pii-pid)) "_" (str (++ *demos*)) "_") *group* (cat "*StorageDemo" *demo_tag*) *demo* :t *count* 0 *trail* :nil
		*stepping* :nil *turn* 0 *pins* (list) *look* :nil *rows* (list) *at* 0 *want* :nil *svcs* (list))
	(each (# (pii-remove (demo-image %0))) +demo_ids)
	(each (const demo-start-member) +demo_ids)
	(mail-send *watch* (cat "group " *group* " 1"))
	(say "Five members, three keep a block. Put some objects."))

(defun demo-stop ()
	(when *tree* (mail-send *tree* "stop") (setq *tree* :nil))
	(when *demo*
		;all told at once, and none waited for: one that is busy is slow to say it has
		(each (# (storage-send (second %0) +storage_type_stop)) *svcs*)
		(setq *svcs* (list))
		;till they have gone, and not for ever: their files are theirs till then
		(defq t0 (pii-time))
		(while (and (nempty? (mail-enquire (cat *group* ","))) (< (- (pii-time) t0) 3000000))
			(task-sleep 50000))
		(task-sleep 200000)
		(each (# (pii-remove (demo-image %0))) +demo_ids)
		(setq *demo* :nil *look* :nil *rows* (list) *at* 0 *want* :nil)
		(mail-send *watch* (cat "group " +storage_group " 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; what there is to be seen
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw ()
	;what the watcher last said there is. The list of the objects is all
	;here, *rows*, and which is shown is this app's own, *at*: it is gone
	;up and down with nothing asked. What is said of the blocks of one,
	;*look*, comes after: the members, each (id where blocks cut), how
	;many blocks the object has with its manifest, which of them each
	;member keeps, the object that is of, and the fewest and most keepers.
	;Till it is of the one shown, the blocks of that are not drawn
	(bind '(members total has entry kept) (ifn *look* (list (list) 0 (list) (list) (list))))
	(defq count (length *rows*) shown_at (max 0 (min *at* (dec count)))
		row (if (> count 0) (elem-get *rows* shown_at))
		shown_name (if row (first row))
		fresh (and row (nempty? entry) (eql (first entry) shown_name))
		total (if fresh total 0) has (if fresh has (list)) kept (if fresh kept (list))
		from (max 0 (min (- shown_at (/ +rows 2)) (- count +rows)))
		rows (slice *rows* from (min count (+ from +rows)))
		items (list (cwb-shape (cwb-d-rect 0 0 +width +height) :fill +paper :stroke 0))
		all (if *demo* +demo_ids (map (const first) members))
		row_h 44 left 150 top 54
		cell (if (= total 0) 16 (max 6 (min 24 (/ (- +width left 24) total))))
		;no more columns than there is room for
		blocks (max 0 (dec total))
		cols (min total +columns (/ (- +width left 24) cell)))
	(defun text (words x y &optional size ink)
		(push items (cwb-text words x y :font "fonts/OpenSans-Regular.ctf" :font_size (ifn size 14) :fill (ifn ink +ink))))
	(text (cond
		((not *look*) "Looking.")
		((empty? members) (if *demo* "The members are starting." "No Storage service is running here. Demo starts five members of a store, on this machine."))
		((= count 0) "Nothing is stored yet.")
		((not fresh) (cat shown_name ", " (str (second row)) " bytes. Looking for its blocks."))
		(:t (cat (first entry) ", " (str (second entry)) " bytes in " (str blocks) (if (= blocks 1) " block. " " blocks. ")
				(if (> blocks (dec cols)) (cat "The first " (str (dec cols)) " of them, each a column.")
					"Each column a block, the first what the object is made of."))))
		16 30 15)
	;how many members keep a block of it, the fewest and the most of all
	;its blocks, those not drawn too. A block kept by none is a hole
	(when (nempty? kept)
		(bind '(fewest most) kept)
		(text (cond
			((= fewest 0) "A block of it is kept by no member that is there.")
			((= fewest most) (cat "Every block of it is kept by " (str fewest) "."))
			((cat "Each block of it is kept by " (str fewest) " to " (str most) ".")))
			16 48 12 (if (= fewest 0) 0xffc02020 +dim))
		;the first column is not a block of it, and is ruled off
		(push items (cwb-shape (cwb-d-rect (+ left cell 1) (+ top 4) (+ left cell 2) (+ top (* (length all) row_h) -10) 0)
			:fill +dim :stroke 0)))
	;a row for each member, and a square where it keeps a block
	(each (lambda (id)
		(defq y (+ top (* (!) row_h)) at (some (# (if (eql (first %0) id) (!))) members)
			member (if at (elem-get members at)) hue (elem-get +hues (% (!) (length +hues))))
		(push items (cwb-shape (cwb-d-rect 14 (+ y 6) 30 (+ y 22) 3) :fill (if member hue +none) :stroke 0))
		(text (slice id 0 (min 8 (length id))) 38 (+ y 20) 15 (if member +ink +dim))
		(text (if member
			(cat (str (third member)) " blocks"
				;cut off from most is cut off, from a few it is they that are
				(cond
					((> (* 2 (length (elem-get member 3))) (dec (length all))) ", cut off")
					((nempty? (elem-get member 3)) (cat ", no " (join (elem-get member 3) " ")))
					(""))
				(if (and shown_name (some (# (and (eql (first %0) id) (eql (second %0) shown_name))) *pins*)) ", pinned" "")
				(if (and *stepping* (= at (% *turn* (length members)))) ", looks next" ""))
			"not there")
			38 (+ y 36) 11 +dim)
		(when (and member (> cols 0))
			(defq held (if (< at (length has)) (elem-get has at) ""))
			(each (lambda (col)
				(defq x (+ left (* col cell) (if (> col 0) 6 0)))
				(push items (cwb-shape (cwb-d-rect x (+ y 6) (+ x (- cell 2)) (+ y 30) 2)
					:fill (if (and (< col (length held)) (eql (elem-get held col) "1")) hue +none) :stroke 0)))
				(range 0 cols))))
		all)
	;where a gopher went: a line from block to block through the members that had them
	(when (and *trail* (> cols 1))
		(defq points (list) stops (second *trail*))
		(each (lambda ((who at))
			(defq row (find who all) to (if (< (inc (!)) (length stops)) (second (elem-get stops (inc (!)))) (dec total)))
			(when row
				(each (lambda (block)
					(push points (+ left (* (inc block) cell) 6 (/ (- cell 2) 2)) (+ top (* row row_h) 18)))
					(range (min at (- cols 2)) (min (max (inc at) to) (dec cols))))))
			stops)
		(when (>= (length points) 4)
			(push items (cwb-shape (cwb-d-points points) :stroke +trail :width 2.0 :fill 0 :join :round))
			(push items (cwb-shape (cwb-d-ellipse (first points) (second points) 4) :fill +trail :stroke 0))))
	;the objects
	(defq y (+ top (* (max 1 (length all)) row_h) 26))
	(text "Objects, and which of the members have each. A dark edge, it has another of that name." 16 y 13 +dim)
	(if (> count (length rows))
		(text (cat (str (inc from)) " to " (str (+ from (length rows))) " of " (str count)) 690 y 13 +dim))
	(each (lambda ((name bytes writes writer who))
		(defq yy (+ y 22 (* (!) 20)))
		(if (= (+ from (!)) shown_at)
			(push items (cwb-shape (cwb-d-rect 10 (- yy 15) (- +width 10) (+ yy 5) 3) :fill 0xffdfe8f7 :stroke 0)))
		;a square for each member, its colour if it has the object
		(each (lambda (id)
			(defq x (+ 16 (* (!) 12)) at (some (# (if (eql (first %0) id) (!))) members)
				state (if (and at (< at (length who))) (elem-get who at) 0) hue (elem-get +hues (% (!) (length +hues))))
			(cond
				((= state 2)
					(push items (cwb-shape (cwb-d-rect x (- yy 10) (+ x 10) yy 2) :fill +ink :stroke 0))
					(push items (cwb-shape (cwb-d-rect (+ x 2) (- yy 8) (+ x 8) (- yy 2) 1) :fill hue :stroke 0)))
				((push items (cwb-shape (cwb-d-rect x (- yy 10) (+ x 10) yy 2) :fill (if (= state 1) hue +none) :stroke 0)))))
			all)
		(text (cat (str bytes) " bytes") 90 yy 13 +dim)
		(text name 190 yy 13)
		(text (cat "written " (str writes) (if (= writes 1) " time, by " " times, last by ") (slice writer 0 (min 8 (length writer))))
			470 yy 13 +dim))
		rows)
	(.-> *canvas* (:fill +paper))
	(cwb-draw-items *canvas* items :nil)
	(. *canvas* :swap +swap_write))

;;;;;;;;;;;;;;;;;
; what is done
;;;;;;;;;;;;;;;;;

(defun running ()
	;the members that are there, (id netid): the demo's, or of the store
	;this system is a member of, on whatever machines they are
	(if *demo*
		(filter (const identity) (map (lambda (id) (some (# (if (eql (first %0) id) %0)) *svcs*)) +demo_ids))
		(map (lambda ((id where &ignore)) (list id (hex-decode where))) (if *look* (first *look*) (list)))))

(defun shown-name ()
	;the path of the object shown, if there is one
	(if (< -1 *at* (length *rows*)) (first (elem-get *rows* *at*))))

(defun select-at (index)
	;that one of the list is the one shown, now: it is drawn at once, and
	;the watcher is told which to say the blocks of, which come after
	(when (nempty? *rows*)
		(setq *at* (max 0 (min index (dec (length *rows*)))) *trail* :nil)
		(mail-send *watch* (cat "show " (shown-name)))
		(draw)))

(defun want (name)
	;that one is the one shown, once it is known of
	(setq *want* name)
	(when (defq index (some (# (if (eql (first %0) name) (!))) *rows*))
		(setq *want* :nil)
		(select-at index)))

(defun listed (rows)
	;the list is not as it was. The one shown is still the one shown,
	;wherever in it that now is, and one that was wanted may be there
	(defq name (shown-name))
	(setq *rows* rows)
	(cond
		((and *want* (defq index (some (# (if (eql (first %0) *want*) (!))) rows)))
			(setq *want* :nil)
			(select-at index))
		((and name (defq index (some (# (if (eql (first %0) name) (!))) rows)))
			(setq *at* index))
		(:t (setq *at* (max 0 (min *at* (dec (length rows)))))
			(if (nempty? rows) (mail-send *watch* (cat "show " (shown-name)))))))

(defun bytes-of (count seed)
	;blocks of 4096, no two the same
	(apply (const cat) (map (# (cat (hex-encode (sha256 (str seed %0))) (pad "" 4032 "."))) (range 0 count))))

(defun action-put ()
	(when (nempty? (defq at (running)))
		(defq member (elem-get at (random (length at))) name (cat "docs/object-" (pad (str (++ *count*)) 2 "00"))
			result (storage-put name (bytes-of (+ 10 (random 22)) name) :nil (second member)))
		(setq *trail* :nil)
		(want name)
		(say name " put at " (first member) ": " (str (first result)) (if *stepping* ". The others have it as they look." ". The others have it when they next meet."))))

(defun action-gopher ()
	(when (nempty? (defq at (running)))
		(when (defq name (shown-name))
			(defq member (elem-get at (random (length at)))
				back (storage-walk name 0 '(lambda (sum data at count)
					(list (reduce (# (+ %0 (code data 1 %1))) (range 0 (length data) 64) sum) (inc at))) :nil 20 (second member)))
			(setq *trail* back)
			(say (if back (cat "A gopher from " (first member) " came back with " (str (first back)) ", by way of "
					(join (map (const first) (second back)) " ") ".")
				"The gopher did not come back.")))))

(defun action-stop ()
	(when (> (length (defq at (running))) 2)
		(defq member (elem-get at (random (length at))))
		(storage-stop (second member))
		;where a gopher went is of the members as they were
		(setq *trail* :nil *svcs* (filter (# (nql (first %0) (first member))) *svcs*))
		(say (first member) " is switched off. "
			(if *stepping* "As the others look, what it kept is kept by them." "In three seconds what it kept is kept by others."))))

(defun action-start ()
	(each (lambda (id) (unless (some (# (eql (first %0) id)) *svcs*) (demo-start-member id))) +demo_ids)
	(setq *trail* :nil)
	(say "All five are on."))

(defun action-cut ()
	(when (> (length (defq at (running))) 1)
		(defq member (elem-get at (random (length at))) others (filter (# (nql (first %0) (first member))) at))
		(storage-cut (map (const first) others) (second member))
		(each (# (storage-cut (list (first member)) (second %0))) others)
		(setq *cut* (first member) *trail* :nil)
		(say (first member) " can reach none of the others, nor they it. Fork writes a path on both sides.")))

(defun action-heal ()
	(each (# (storage-cut (list) (second %0))) (running))
	(setq *cut* :nil)
	(say "All can reach all. What was written apart is put together as they meet."))

(defun action-fork ()
	(when (> (length (defq at (running))) 1)
		(defq one (if *cut* (some (# (if (eql (first %0) *cut*) %0)) at) (first at))
			one (ifn one (first at))
			two (some (# (if (nql (first %0) (first one)) %0)) at))
		(storage-put "plan" (bytes-of 3 (cat "plan of " (first one) (str (random 100000)))) :nil (second one))
		(storage-put "plan" (bytes-of 5 (cat "plan of " (first two) (str (random 100000)))) :nil (second two))
		(say "plan written at " (first one) " and at " (first two)
			(if *cut* ". Neither knows of the other till they meet." ". One knew of the other, or soon will."))))

(defun action-pin ()
	;the object shown is pinned at one of the members, or, if it is at
	;any that is there, at none of them
	(when (nempty? (defq at (running)))
		(when (defq name (shown-name))
			(defq held (filter (lambda ((id path)) (and (eql path name) (some (# (eql (first %0) id)) at))) *pins*))
			(cond
				((nempty? held)
					(each (lambda ((id path)) (storage-pin name :t (second (some (# (if (eql (first %0) id) %0)) at)))) held)
					(setq *pins* (filter (# (not (find %0 held))) *pins*))
					(say name " is pinned nowhere. What is not a member's to keep, it lets go of."))
				(:t (defq member (elem-get at (random (length at))))
					(storage-pin name :nil (second member))
					(push *pins* (list (first member) name))
					(say name " is pinned at " (first member) ": it fetches all of it and keeps it. Send a gopher."))))))

(defun action-tree ()
	;the folder this was started in is put in the store, by a task of its
	;own that says how it goes: of a demo, a new one with room
	(when *tree* (mail-send *tree* "stop") (setq *tree* :nil))
	(when *demo*
		;five stores with room are some seconds in the making, said first
		(say "Making five stores with room, a few seconds.")
		(demo-stop)
		(setq *big* :t)
		(demo-start))
	;put at all the members there are, each in turn, by a farm of workers
	(when (nempty? (defq at (running)))
		(setq *tree* (open-child "apps/system/storage/tree.lisp" +kn_call_run))
		(mail-send *tree* (join (list (hex-encode (elem-get *select* +select_tree)) "." "tree/"
			(join (map (# (hex-encode (second %0))) at) " ")) (ascii-char 10)))
		(say (if *demo* "A store of five with room. " "") "Every file of this folder is being put, at all "
			(str (length at)) " members at once.")))

(defun tree-said (msg)
	;how the putting and the reading back are going
	(bind '(what &rest more) (split msg " "))
	(cond
		((eql what "done")
			(bind '(put same count bytes put_ms read_ms) (map (const str-as-num) more))
			(setq *tree* :nil)
			(want "tree/apps/media/films/data/raymarch.flm")
			(say (str put) " of " (str count) " files put in " (str (/ put_ms 1000)) "s. " (str same) " read back the same, "
				(str (/ bytes 1048576)) "MB, in " (str (/ read_ms 1000)) "s."))
		(:t (bind '(at count path) more)
			(if (eql what "put") (want path))
			(say (if (eql what "put") "Putting " "Reading back ") at " of " count ": " path))))

(defun action-step ()
	;the first press, they look only when told. After it, the next looks
	(when (nempty? (defq at (running)))
		(cond
			(*stepping*
				(defq member (elem-get at (% *turn* (length at))) did (storage-settle (second member) 10))
				(setq *turn* (% (inc *turn*) (length at)))
				(say (first member) (if did (cat " looked: new notes with " (str (first did)) ", " (str (second did))
						" blocks fetched, " (str (third did)) " let go of.")
					" did not say what it did.")))
			(:t (setq *stepping* :t *turn* 0)
				(each (# (storage-pace 0 0 (second %0))) at)
				(say "They look only when told. Each Step, the next of them looks. Run is by the clock again.")))))

(defun action-run ()
	(setq *stepping* :nil)
	(each (# (storage-pace +tick +grace (second %0))) (running))
	(say "They look by the clock."))

(defun main ()
	(defq id :t held 0 wheel 0 select (task-mboxes +select_size)
		*demo* :nil *demo_tag* "" *group* +storage_group *count* 0 *trail* :nil *cut* :nil
		*stepping* :nil *turn* 0 *pins* (list) *big* :nil *tree* :nil *select* select *look* :nil *rows* (list) *at* 0 *want* :nil *demos* 0 *starts* 0 *svcs* (list)
		*watch* (open-child "apps/system/storage/watch.lisp" +kn_call_run))
	(mail-send *watch* (hex-encode (elem-get select +select_watch)))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;the store of this system if it has one, and if not, one to look at
	(cond
		((storage-service) (say "The store this system is a member of. Put, Next, Gopher, Pin, Tree. Demo starts five members of another."))
		(:t (demo-start)
			(times 3 (action-put))
			(say "Five members of a store, each on a node of this machine, three keep a block. Put, Gopher, Stop, Cut, Fork.")))
	(draw)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close) (setq id :nil))
					((= id +event_demo)
						;from a demo to the store this system is a member of, if it
						;is of one, and from that, or from nothing, to a new demo
						(cond
							((and *demo* (storage-service))
								(demo-stop)
								(say "The store this system is a member of. Put, Next, Gopher, Pin, Tree. Demo is five members of another."))
							(:t (demo-stop) (setq *big* :nil) (demo-start))))
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse))
						;a press on a line of the list: that object is the one
						;shown. Only as the button goes down
						(defq buttons (getf msg +ev_msg_mouse_buttons))
						(when (and (= held 0) (/= buttons 0) (nempty? *rows*))
							(bind '(w h) (. *canvas* :get_size))
							(defq ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2))
								count (length *rows*)
								from (max 0 (min (- *at* (/ +rows 2)) (- count +rows)))
								all (if *demo* (length +demo_ids) (if *look* (length (first *look*)) 0))
								line (/ (- ry (+ 54 (* (max 1 all) 44) 26 7)) 20))
							(and (>= ry (+ 54 (* (max 1 all) 44) 26 7)) (< line +rows) (< (+ from line) count)
								(select-at (+ from line))))
						(setq held buttons))
					((= (getf msg +ev_msg_type) +ev_type_wheel)
						;the wheel goes up and down the list, a line for so much of
						;a turn: a trackpad sends a great many small ones
						(setq wheel (- wheel (getf msg +ev_msg_wheel_y)))
						(when (/= 0 (defq lines (/ wheel +wheel_line)))
							(setq wheel (- wheel (* lines +wheel_line)))
							(select-at (+ *at* lines))))
					((= id +event_put) (action-put))
					((= id +event_next) (if (nempty? *rows*) (select-at (% (inc *at*) (length *rows*)))))
					((= id +event_gopher) (action-gopher))
					((= id +event_pin) (action-pin))
					((= id +event_tree) (action-tree))
					;the rest are of a demo: a member of a store that is somebody's
					;is not switched off, nor cut off, from here
					((not *demo*) (. *window* :event msg))
					((= id +event_stop) (action-stop))
					((= id +event_start) (action-start))
					((= id +event_cut) (action-cut))
					((= id +event_heal) (action-heal))
					((= id +event_fork) (action-fork))
					((= id +event_step) (action-step))
					((= id +event_run) (action-run))
					((. *window* :event msg))))
			(+select_tip
				;what a button is for
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(+select_tree (tree-said msg))
			(+select_watch
				;all it has said, if this was kept from reading it: the list of
				;the objects when that changes, and what it sees at each look
				(while msg
					(when (list? (defq said (catch (first (read (string-stream msg))) :t)))
						(if (eql (first said) "list") (listed (second said)) (setq *look* (rest said))))
					(setq msg (if (mail-poll (list (elem-get select +select_watch)))
						(mail-read (elem-get select +select_watch)))))
				(draw))))
	;the window goes at once, what it started is put away after
	(gui-sub-rpc *window*)
	(mail-send *watch* "stop")
	(demo-stop))
