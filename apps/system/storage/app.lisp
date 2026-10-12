;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a store and its members, to look at
;
; The members of a store down the side, the blocks of one object across,
; and a square where a member keeps a block. Under it the objects, and how
; many of the members have each.
;
; It shows the store this system is a member of, if its Storage service is
; running. If not, and when Demo is pressed, it starts five members of a
; store of their own, here on this node, each with a little store in a file
; under obj/: all of what a store of five machines does, on the one. Then
;
;	Put	an object of some blocks is put at one of the members
;	Next	the next object is the one shown
;	Gopher	a gopher is sent to it, to add up its bytes: the line is
;		where it went, member to member, block by block
;	Stop	a member is switched off, Start, those that are, on
;	Cut	a member can reach none of the others, nor they it
;	Heal	all can reach all
;	Fork	a path is written at two members at once: with one cut off,
;		neither knows of the other
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
	(enum main timer))

(enums +event 0
	(enum close max min)
	(enum demo put next gopher stop start cut heal fork step run))

(defq +width 860 +height 520 +rate 500000 +rows 9 +tick 700000 +grace 3000000
	+demo_ids ''("m-a" "m-b" "m-c" "m-d" "m-e")
	+hues ''(0xff2a7fff 0xffff7f2a 0xff2fb344 0xffc0399f 0xffd6a800 0xff20b2aa 0xffe0483e 0xff7a5cff)
	+ink 0xff202020 +dim 0xff909090 +paper 0xfff4f4f0 +none 0xffe2e2dc +trail 0xff000000)

(ui-window *window* ()
	(ui-title-bar _ "Storage" (+sym_close) +event_close)
	(ui-tool-bar _ (:font *env_body_font*)
		(each (lambda (text event) (. (ui-button _ (:text text :min_width 72)) :connect event))
			'("Demo" "Put" "Next" "Gopher" "Stop" "Start" "Cut" "Heal" "Fork" "Step" "Run")
			(list +event_demo +event_put +event_next +event_gopher +event_stop +event_start +event_cut +event_heal +event_fork
				+event_step +event_run)))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-canvas *canvas* +width +height 1))

(defun say (&rest text)
	(def *status* :text (apply (const cat) text))
	(.-> *status* :layout :dirty))

;;;;;;;;;;;;;;;;;;;;;;;
; the members of a demo
;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-name (id) (cat "@StorageDemo" *demo_tag* id))
(defun demo-image (id) (cat "obj/storage_demo_" id ".img"))

(defun demo-start-member (id)
	(storage-start (demo-image id) :size 0x400000 :id "demo" :name (demo-name id) :block 4096
		:group *group* :member id :tick (if *stepping* 0 +tick) :grace (if *stepping* 0 +grace)))

(defun demo-start ()
	;five members of a store of their own, new each time
	(setq *demo_tag* (str (pii-pid)) *group* (cat "*StorageDemo" *demo_tag*) *demo* :t *count* 0 *trail* :nil
		*stepping* :nil *turn* 0)
	(each (# (pii-remove (demo-image %0))) +demo_ids)
	(each (const demo-start-member) +demo_ids)
	(say "Five members, three keep a block. Put some objects."))

(defun demo-stop ()
	(when *demo*
		(each (# (if (defq svc (storage-service (demo-name %0))) (storage-stop svc))) +demo_ids)
		(task-sleep 200000)
		(each (# (pii-remove (demo-image %0))) +demo_ids)
		(setq *demo* :nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; what there is to be seen
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun name-key (name)
	;a name and the number it ends with, so that 2 is before 10
	(defq at (length name))
	(while (and (> at 0) (<= (ascii-code "0") (code name 1 (dec at)) (ascii-code "9"))) (setq at (dec at)))
	(list (slice name 0 at) (if (< at (length name)) (str-as-num (slice name at -1)) -1)))

(defun name-cmp (one two)
	(defq one (name-key one) two (name-key two))
	(if (eql (first one) (first two)) (- (second one) (second two)) (cmp (first one) (first two))))

(defun look ()
	; (look) -> (members objects names)
	;the members that are there, each (id netid blocks cut), in order; the
	;objects, every one that any of them has; and the blocks of the one
	;shown. *who* is, for each object, what each member has of it: :nil,
	;:t, or :differs, it has the path and it is not the same
	(defq there (list) store_id :nil)
	(each (lambda (entry)
		(defq parts (split entry ",") about (split (last parts) " "))
		(if (= (length about) 2) (push there (list (second about) (hex-decode (second parts)) (first about)))))
		(mail-enquire (cat *group* ",")))
	;a store that is not a demo: only the members of this system's own
	(unless *demo*
		(and (defq mine (storage-service)) (defq info (storage-info mine))
			(setq there (filter (# (eql (third %0) (. info :find :id))) there))))
	(sort there (# (cmp (first %0) (first %1))))
	(defq members (map (lambda ((id where &ignore))
			(defq info (storage-info where) state (storage-state where))
			(list id where (if info (. info :find :blocks) 0) (if state (. state :find :cut) (list))))
		there)
		lists (map (lambda ((id where &ignore)) (ifn (storage-list "" where) (list))) members)
		objects (list) known (Fmap 31) names (list))
	(each (lambda (entries)
		(each (lambda (entry)
			(unless (. known :find (first entry))
				(. known :insert (first entry) :t)
				(push objects entry)))
			entries))
		lists)
	(sort objects (# (name-cmp (first %0) (first %1))))
	(setq *who* (map (lambda (entry)
		(map (lambda (entries)
			(if (defq theirs (some (# (if (eql (first %0) (first entry)) %0)) entries))
				(if (eql (elem-get theirs +store_entry_hash) (elem-get entry +store_entry_hash)) :t :differs)))
			lists))
		objects))
	(when (nempty? objects)
		;what was just put is the one shown, once it is known of here
		(and *want* (defq at (some (# (if (eql (first %0) *want*) (!))) objects))
			(setq *shown* at *want* :nil))
		(setq *shown* (% (max 0 *shown*) (length objects)))
		(defq entry (elem-get objects *shown*) hash (elem-get entry +store_entry_hash))
		;its manifest, from whichever member has it, then its blocks
		(and (defq text (some (lambda ((id where &ignore)) (storage-block-at hash where)) members))
			(defq lines (filter (const nempty?) (split text (ascii-char 10))))
			(setq names (cat (list hash) (map (const hex-decode) (slice lines 2 -1))))))
	(list members objects names))

(defun draw ()
	(bind '(members objects names) (look))
	(defq items (list (cwb-shape (cwb-d-rect 0 0 +width +height) :fill +paper :stroke 0))
		all (if *demo* +demo_ids (map (const first) members))
		row_h 44 left 150 top 54
		cell (if (empty? names) 16 (max 6 (min 24 (/ (- +width left 24) (length names))))))
	(defun text (words x y &optional size ink)
		(push items (cwb-text words x y :font "fonts/OpenSans-Regular.ctf" :font_size (ifn size 14) :fill (ifn ink +ink))))
	(text (cond
		((empty? members) (if *demo* "The members are starting." "No Storage service is running here. Demo starts five members of a store, on this machine."))
		((empty? objects) "Nothing is stored yet.")
		(:t (defq entry (elem-get objects *shown*))
			(cat (first entry) ", " (str (elem-get entry +store_entry_bytes)) " bytes in " (str (max 0 (dec (length names))))
				" blocks. Each column a block, the first what the object is made of.")))
		16 30 15)
	;a row for each member, and a square where it keeps a block
	(each (lambda (id)
		(defq y (+ top (* (!) row_h)) member (some (# (if (eql (first %0) id) %0)) members)
			hue (elem-get +hues (% (!) (length +hues))))
		(push items (cwb-shape (cwb-d-rect 14 (+ y 6) 30 (+ y 22) 3) :fill (if member hue +none) :stroke 0))
		(text (slice id 0 (min 8 (length id))) 38 (+ y 20) 15 (if member +ink +dim))
		(text (cond
			((not member) "not there")
			;cut off from most is cut off, from a few it is they that are
			((> (* 2 (length (elem-get member 3))) (dec (length all))) (cat (str (third member)) " blocks, cut off"))
			((nempty? (elem-get member 3)) (cat (str (third member)) " blocks, no " (join (elem-get member 3) " ")))
			((and *stepping* (eql member (elem-get members (% *turn* (length members)))))
				(cat (str (third member)) " blocks, looks next"))
			((cat (str (third member)) " blocks")))
			38 (+ y 36) 11 +dim)
		(when (and member (nempty? names))
			(defq has (ifn (storage-ask (second member) +storage_type_has (apply (const cat) (cat (list "") names))) ""))
			(each (lambda (at)
				(defq x (+ left (* at cell) (if (> at 0) 6 0)) held (and (< at (length has)) (eql (elem-get has at) "1")))
				(push items (cwb-shape (cwb-d-rect x (+ y 6) (+ x (- cell 2)) (+ y 30) 2)
					:fill (if held hue +none) :stroke 0)))
				(range 0 (length names)))))
		all)
	;where a gopher went: a line from block to block through the members that had them
	(when (and *trail* (nempty? names))
		(defq points (list) stops (second *trail*))
		(each (lambda ((who at))
			(defq row (find who all) to (if (< (inc (!)) (length stops)) (second (elem-get stops (inc (!)))) (dec (length names))))
			(when row
				(each (lambda (block)
					(push points (+ left (* (inc block) cell) 6 (/ (- cell 2) 2)) (+ top (* row row_h) 18)))
					(range at (max (inc at) to)))))
			stops)
		(when (>= (length points) 4)
			(push items (cwb-shape (cwb-d-points points) :stroke +trail :width 2.0 :fill 0 :join :round))
			(push items (cwb-shape (cwb-d-ellipse (first points) (second points) 4) :fill +trail :stroke 0))))
	;the objects
	(defq y (+ top (* (max 1 (length all)) row_h) 26))
	(text "Objects, and which of the members have each. A dark edge, it has another of that name." 16 y 13 +dim)
	;as many as there is room for, the one shown among them
	(defq from (max 0 (min (- *shown* (/ +rows 2)) (- (length objects) +rows))) to (min (length objects) (+ from +rows)))
	(if (> (length objects) +rows)
		(text (cat (str (inc from)) " to " (str to) " of " (str (length objects))) 760 y 13 +dim))
	(each (lambda (entry)
		(when (<= from (!) (dec to))
			(bind '(name hash bytes vv writer seen deleted) entry)
			(defq yy (+ y 22 (* (- (!) from) 20)) shown (= (!) *shown*))
			(if shown (push items (cwb-shape (cwb-d-rect 10 (- yy 15) (- +width 10) (+ yy 5) 3) :fill 0xffdfe8f7 :stroke 0)))
			;a square for each member, its colour if it has the object
			(defq has (elem-get *who* (!)))
			(each (lambda (id)
				(defq x (+ 16 (* (!) 12)) at (some (# (if (eql (first %0) id) (!))) members)
					state (and at (elem-get has at)) hue (elem-get +hues (% (!) (length +hues))))
				(cond
					((eql state :differs)
						(push items (cwb-shape (cwb-d-rect x (- yy 10) (+ x 10) yy 2) :fill +ink :stroke 0))
						(push items (cwb-shape (cwb-d-rect (+ x 2) (- yy 8) (+ x 8) (- yy 2) 1) :fill hue :stroke 0)))
					((push items (cwb-shape (cwb-d-rect x (- yy 10) (+ x 10) yy 2) :fill (if state hue +none) :stroke 0)))))
				all)
			(text (cat (str bytes) " bytes") 90 yy 13 +dim)
			(text name 190 yy 13)
			(text (cat "written " (str (store-vv-total vv)) (if (= (store-vv-total vv) 1) " time, by " " times, last by ") (slice writer 0 (min 8 (length writer))))
				470 yy 13 +dim)))
		objects)
	(.-> *canvas* (:fill +paper))
	(cwb-draw-items *canvas* items :nil)
	(. *canvas* :swap +swap_write))

;;;;;;;;;;;;;;;;;
; what is done
;;;;;;;;;;;;;;;;;

(defun running ()
	;the demo's members that are there, (id netid)
	(filter (const second) (map (# (list %0 (storage-service (demo-name %0)))) +demo_ids)))

(defun bytes-of (count seed)
	;blocks of 4096, no two the same
	(apply (const cat) (map (# (cat (hex-encode (sha256 (str seed %0))) (pad "" 4032 "."))) (range 0 count))))

(defun action-put ()
	(when (nempty? (defq at (running)))
		(defq member (elem-get at (random (length at))) name (cat "docs/object-" (str (++ *count*)))
			result (storage-put name (bytes-of (+ 10 (random 22)) name) :nil (second member)))
		(setq *trail* :nil *want* name)
		(say name " put at " (first member) ": " (str (first result)) (if *stepping* ". The others have it as they look." ". The others have it when they next meet."))))

(defun action-gopher ()
	(when (nempty? (defq at (running)))
		(bind '(members objects names) (look))
		(when (nempty? objects)
			(defq member (elem-get at (random (length at))) name (first (elem-get objects *shown*))
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
		(setq *trail* :nil)
		(say (first member) " is switched off. "
			(if *stepping* "As the others look, what it kept is kept by them." "In three seconds what it kept is kept by others."))))

(defun action-start ()
	(each (# (unless (storage-service (demo-name %0)) (demo-start-member %0))) +demo_ids)
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
	(defq id :t select (task-mboxes +select_size)
		*demo* :nil *demo_tag* "" *group* +storage_group *shown* 0 *count* 0 *trail* :nil *cut* :nil
		*stepping* :nil *turn* 0 *want* :nil *who* (list))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;the store of this system if it has one, and if not, one to look at
	(cond
		((storage-service) (say "The store this system is a member of. Demo starts five members of another, to try."))
		(:t (demo-start)
			(times 3 (action-put))
			(say "Five members of a store, on this machine, three keep a block. Put, Gopher, Stop, Cut, Fork.")))
	(draw)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close) (setq id :nil))
					((= id +event_demo) (demo-stop) (demo-start))
					((not *demo*) (. *window* :event msg))
					((= id +event_put) (action-put))
					((= id +event_next) (setq *shown* (inc *shown*) *trail* :nil *want* :nil))
					((= id +event_gopher) (action-gopher))
					((= id +event_stop) (action-stop))
					((= id +event_start) (action-start))
					((= id +event_cut) (action-cut))
					((= id +event_heal) (action-heal))
					((= id +event_fork) (action-fork))
					((= id +event_step) (action-step))
					((= id +event_run) (action-run))
					((. *window* :event msg))))
			(+select_timer
				(draw)
				(mail-timeout (elem-get select +select_timer) +rate 0))))
	(demo-stop)
	(gui-sub-rpc *window*))
