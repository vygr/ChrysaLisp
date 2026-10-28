;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/app.inc")

(defun say (text)
	(def *status* :text text)
	(.-> *status* :layout :dirty))

(defun bytes-said (n)
	;so many bytes as a person says them, to a tenth
	(defq units (list "B" "KB" "MB" "GB" "TB" "PB") u 0 tenths (* n 10))
	(while (and (>= tenths 10240) (< u 5))
		(setq tenths (/ tenths 1024))
		(++ u))
	(if (= u 0) (cat (str n) " B")
		(cat (str (/ tenths 10)) "." (str (% tenths 10)) " " (elem-get units u))))

;;;;;;;;;;;;;;;;;;;;
; what a side can be
;;;;;;;;;;;;;;;;;;;;

(defun sources-find ()
	;the files of this machine, its store if it has one, and the stores of
	;the other machines of the mesh, one member of each store, as the
	;group says who is there: the members of a store have the one tree
	(defq out (list (list "Files of this machine" :host :nil)) ids (list))
	(when (defq svc (storage-service))
		(defq id (. (ifn (storage-info svc) (Emap)) :find :id))
		(push ids id)
		(push out (list (cat "Store " id ", this machine") :store svc)))
	;the group's name, the service, its machine, and the store and member
	(each (lambda (entry)
			(defq parts (split entry ","))
			(when (> (length parts) 3)
				(bind '(id member &ignore) (split (elem-get parts 3) " "))
				(unless (find id ids)
					(push ids id)
					(push out (list (cat "Store " id ", of " (slice member 0 (min 8 (length member))))
						:store (hex-decode (second parts)))))))
		(mail-enquire (cat +storage_group ",")))
	out)

(defun side-source (side)
	;what a side is, (label kind svc)
	(. side :find :source))

(defun side-host? (side)
	(eql (second (side-source side)) :host))

(defun side-svc (side)
	(third (side-source side)))

(defun host-path (folder name)
	(if (ends-with "/" folder) (cat folder name) (cat folder "/" name)))

(defun host-bytes (file)
	(if (defq info (pii-fstat file)) (second info) 0))

(defun side-rows (side)
	;what is in the folder a side is at, a row each, (name folder bytes
	;info), folders first. A folder of a store is the start of the paths of
	;its objects up to a /: how many are under it, and how long they are.
	;An object, how long, how many are to keep it, and how many of the
	;members have it now
	(defq folder (. side :find :folder) folders (list) files (list))
	(cond
		((side-host? side)
			(bind '(names kinds) (dir-list folder))
			(each (lambda (name kind)
					(cond
						((starts-with "." name))
						((eql kind "d") (push folders (list name :t 0 "")))
						((push files (list name :nil (defq bytes (host-bytes (host-path folder name))) (bytes-said bytes))))))
				names kinds))
		(:t (defq svc (side-svc side) start (if (empty? folder) "" (cat folder "/"))
				members (ifn (storage-members :nil svc) (list))
				dial (ifn (. (ifn (storage-info svc) (Emap)) :find :copies) 0) sums (Fmap))
			(each (lambda (entry)
					(defq rest (slice (first entry) (length start) -1) bytes (elem-get entry +store_entry_bytes))
					(cond
						((defq at (find "/" rest))
							(defq name (slice rest 0 at))
							(cond
								((defq sum (. sums :find name))
									(elem-set sum 0 (inc (first sum)))
									(elem-set sum 1 (+ (second sum) bytes)))
								(:t (. sums :insert name (list 1 bytes))
									(push folders name))))
						(:t (defq copies (elem-get entry +store_entry_copies)
								seen (length (filter (# (find %0 members)) (elem-get entry +store_entry_seen))))
							(push files (list rest :nil bytes
								(cat (bytes-said bytes) "  x" (str (if (> copies 0) copies dial)) (if (> copies 0) "" "*")
									"  " (str seen) "/" (str (length members))))))))
				(ifn (storage-list start svc) (list)))
			(setq folders (map (lambda (name)
					(bind '(count bytes) (. sums :find name))
					(list name :t bytes (cat (str count) (if (= count 1) " object, " " objects, ") (bytes-said bytes))))
				folders))))
	(defq by_name (# (cmp (first %0) (first %1))))
	(cat (sort folders by_name) (sort files by_name)))

;;;;;;;;;;;;;;
; a side shown
;;;;;;;;;;;;;;

(defun row-event (s n part)
	;the event of a row of a side: its name, or the button to open it
	(+ +event_rows (* s +rows_max 2) (* n 2) part))

(defun side-note (s)
	(defq side (elem-get *sides* s) rows (. side :find :rows) taken (. side :find :taken)
		note (. side :find :note))
	(def note :text (cat (str (length rows)) " here"
		(if (> (length rows) +rows_max) (cat ", the first " (str +rows_max) " shown") "")
		(if (nempty? taken) (cat ", " (str (length taken)) " taken") "")
		(if (side-host? side) "" ".  Objects: size, copies kept (* the store's), members that have it")))
	(.-> note :layout :dirty))

(defun side-paint (s)
	;the rows taken lit, the others not
	(defq side (elem-get *sides* s) taken (. side :find :taken))
	(each (lambda (button (name &ignore))
			(def (. button :dirty) :paper (if (find name taken) (get :paper_highlight *env*) +argb_white)))
		(. side :find :buttons) (. side :find :rows))
	(side-note s))

(defun side-show (s)
	;the rows of a side made again, a row a thing, a line under it
	(defq side (elem-get *sides* s) rows (. side :find :rows) rows_flow (. side :find :list) buttons (list))
	(each (# (. %0 :sub)) (. rows_flow :children))
	(each! (lambda ((name folder bytes info))
			(defq row (Flow) button (Button) label (Label) rule (Backdrop) n (!))
			(def row :flow_flags +flow_left_fill)
			;from the right: the button to open a folder, a blank as tall for a
			;file so every row is as tall, what it is, its name
			(defq open (if folder (Button) (Label)))
			(def open :text (if folder (num-to-utf8 +sym_next) " ") :border 0 :paper +argb_white :ink +argb_black)
			(view-variant open :font :font_symbol)
			(if folder (. open :connect (row-event s n 1)))
			(. row :add_child open)
			(def label :text info :border 0 :paper +argb_white :ink +argb_grey8)
			(view-variant label :font :font_small)
			(def button :text (if folder (cat name "/") name) :border 0 :paper +argb_white :ink +argb_black
				:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))
			(view-variant button :font :font_button)
			(. button :connect (row-event s n 0))
			(push buttons button)
			(.-> row (:add_child label) (:add_child button))
			(def rule :paper +argb_grey14 :min_height 1)
			(.-> rows_flow (:add_child row) (:add_child rule)))
		(list rows) 0 (min (length rows) +rows_max))
	(. side :insert :buttons buttons)
	(when (nempty? rows)
		(defq row (second (. (first (. rows_flow :children)) :pref_size)))
		(def (. side :find :ruled) :spacing (inc row) :origin_y row))
	(.-> (. side :find :scroll) :layout :dirty_all)
	(side-paint s))

(defun go-to (s folder)
	;a side at a folder, nothing in it taken
	(defq side (elem-get *sides* s))
	(. side :insert :folder folder)
	(. side :insert :taken (list))
	(. side :insert :rows (side-rows side))
	(.-> (. side :find :path) (:set_text (if (side-host? side) folder (cat "/" folder))) :layout :dirty)
	(def (defq source (. side :find :source_button)) :text (first (side-source side)))
	(.-> source :layout :dirty)
	(side-show s))

(defun look-again ()
	;both sides as they are now, at the folders they were at
	(each (# (go-to %1 (. %0 :find :folder))) *sides* (list 0 1)))

(defun stores-start ()
	;the two sides: the files of this machine, and its store if it has one
	(setq *sources* (sources-find)
		*sides* (map (lambda (path source scroll ruled rows_flow note)
				(def scroll :rows rows_flow)
				(scatter (Emap) :path path :source_button source :scroll scroll :ruled ruled
					:list rows_flow :note note :folder "" :taken (list) :rows (list) :buttons (list) :source :nil))
			(list *path_0* *path_1*) (list *source_0* *source_1*) (list *scroll_0* *scroll_1*)
			(list *ruled_0* *ruled_1*) (list *list_0* *list_1*) (list *note_0* *note_1*)))
	(. (first *sides*) :insert :source (first *sources*))
	(. (second *sides*) :insert :source (elem-get *sources* (if (> (length *sources*) 1) 1 0)))
	(go-to 0 "/")
	(go-to 1 (if (side-host? (second *sides*)) "/" ""))
	(toolbar-states *rule_toolbar* (list :t :nil :nil))
	(say "Click names to take them, then copy them across."))

;;;;;;;;;;;;;;;;;;;;;;;
; what a side is asked
;;;;;;;;;;;;;;;;;;;;;;;

(defun action-row (s n part)
	;a name taken, or let go of; a folder opened
	(defq side (elem-get *sides* s) row (elem-get (. side :find :rows) n) taken (. side :find :taken))
	(setq *active* s *delete* :nil)
	(cond
		((= part 1)
			(go-to s (if (side-host? side) (host-path (. side :find :folder) (first row))
				(if (empty? (. side :find :folder)) (first row) (cat (. side :find :folder) "/" (first row))))))
		(:t (. side :insert :taken (if (find (first row) taken)
				(filter (# (nql %0 (first row))) taken)
				(push (cat taken) (first row))))
			(side-paint s))))

(defun action-source (s)
	;the next thing a side can be: files, then each store there is now
	(defq side (elem-get *sides* s) was (side-source side))
	(setq *sources* (sources-find) *active* s *delete* :nil)
	;where it was in what there is now, the stores there may have changed
	(defq at (ifn (some (# (and (eql (second %0) (second was)) (eql (third %0) (third was)) (!))) *sources*) -1))
	(. side :insert :source (elem-get *sources* (% (inc at) (length *sources*))))
	(go-to s (if (side-host? side) "/" "")))

(defun action-up (s)
	(defq side (elem-get *sides* s) folder (. side :find :folder))
	(setq *active* s *delete* :nil)
	(go-to s (if (side-host? side)
		(if (empty? (defq above (path-dir folder))) "/" above)
		(path-dir folder))))

(defun action-path (s)
	;the folder written: one of the host if it is there, of a store any
	(defq side (elem-get *sides* s) text (trim (. (. side :find :path) :get_text)))
	(setq *active* s *delete* :nil)
	(cond
		((side-host? side)
			(while (and (> (length text) 1) (ends-with "/" text)) (setq text (slice text 0 -2)))
			(if (and (defq info (pii-fstat text)) (folder-mode? (third info)))
				(go-to s text)
				(say (cat "No folder " text " here."))))
		(:t (go-to s (trim text "/")))))

(defun action-all (s)
	;all of a side taken, or none if all were
	(defq side (elem-get *sides* s) rows (. side :find :rows))
	(setq *active* s *delete* :nil)
	(. side :insert :taken (if (= (length (. side :find :taken)) (length rows)) (list) (map (const first) rows)))
	(side-paint s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what is done, by a task of its own
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun side-said (side)
	;a side as ./work.lisp is told it
	(if (side-host? side)
		(list :host (. side :find :folder))
		(list :store (hex-encode (side-svc side)) (. side :find :folder))))

(defun work-start (what from to &optional copies)
	;the task that does it, told what, and it is all it is told
	(defq rows (. from :find :rows)
		names (map (lambda (name) (list name (if (some (# (and (eql (first %0) name) (second %0))) rows) :t :nil)))
			(. from :find :taken)))
	(setq *work* (open-child "apps/system/stores/work.lisp" +kn_call_run) *doing* what *plan* (list 0 0) *faults* 0)
	(mail-send *work* (str (list (hex-encode *work_mbox*) what (side-said from) (side-said to) *rule* names (ifn copies 0))))
	(def *progress* :value 0)
	(.-> *progress* :dirty)
	(say "Looking at what there is to do ..."))

(defun work-heard (msg)
	;what the task said: what there is, how far it has got, what it could
	;not do, and that it is done
	(defq said (first (read (string-stream msg))))
	(case (first said)
		(:plan (setq *plan* (rest said)))
		(:at (bind '(bytes count name) (rest said))
			(bind '(all total) *plan*)
			(def *progress* :value (if (> total 0) (/ (* (min bytes total) 1000) total) 0))
			(.-> *progress* :dirty)
			(say (cat (str (inc count)) " of " (str all)
				(if (eql *doing* :copy) (cat ", " (bytes-said bytes) " of " (bytes-said total)) "") ": " name)))
		(:not (++ *faults*)
			(bind '(name why) (rest said))
			(print "Stores: not done " name ", " why))
		(:done (bind '(done skipped failed bytes ms stopped) (rest said))
			(setq *work* :nil)
			(def *progress* :value (if stopped 0 1000))
			(.-> *progress* :dirty)
			(look-again)
			(say (cat (if stopped "Stopped. " "Done. ") (str done) " done"
				(if (> skipped 0) (cat ", " (str skipped) " there already, left") "")
				(if (> failed 0) (cat ", " (str failed) " not done (the terminal says which)") "")
				(if (eql *doing* :copy) (cat ", " (bytes-said bytes)) "") " in " (str (/ ms 1000)) " s.")))))

;;;;;;;;;;;;;;;;;;;;;
; the toolbar actions
;;;;;;;;;;;;;;;;;;;;;

(defun action-copy (s)
	(defq from (elem-get *sides* s) to (elem-get *sides* (- 1 s)))
	(setq *delete* :nil)
	(cond
		(*work* (say "Busy: Cancel what is being done first."))
		((empty? (. from :find :taken)) (say "Click the names of what to copy first."))
		((and (side-host? from) (side-host? to)) (say "Files to files is not what this is for: one side is to be a store."))
		((work-start :copy from to))))

(defun action-to-right () (action-copy 0))
(defun action-to-left () (action-copy 1))

(defun action-delete ()
	;what is taken on the side last clicked, out of its store: the button
	;is pressed twice
	(defq side (elem-get *sides* *active*) taken (. side :find :taken))
	(cond
		(*work* (say "Busy: Cancel what is being done first."))
		((side-host? side) (say "Only what is in a store is deleted here, not files."))
		((empty? taken) (say "Click the names of what to delete first."))
		((eql *delete* (list *active* taken))
			(setq *delete* :nil)
			(work-start :delete side side))
		(:t (setq *delete* (list *active* taken))
			(say (cat "Delete " (str (length taken)) " taken, and all under a folder, from "
				(first (side-source side)) "? Delete again to do it.")))))

(defun action-copies ()
	;how many members are to keep what is taken, on the side last clicked
	(defq side (elem-get *sides* *active*) copies (str-as-num (trim (. *copies* :get_text))))
	(setq *delete* :nil)
	(cond
		(*work* (say "Busy: Cancel what is being done first."))
		((side-host? side) (say "Copies are of what is in a store."))
		((empty? (. side :find :taken)) (say "Click the names of what to set the copies of first."))
		((< copies 0) (say "Copies is a number, 0 for as many as the store says."))
		((work-start :copies side side copies))))

(defun action-cancel ()
	(if *work* (mail-send *work* "stop") (say "Nothing is being done.")))

(defun action-refresh ()
	(setq *sources* (sources-find) *delete* :nil)
	(look-again))

(defun action-rule (rule n)
	(setq *rule* rule)
	(toolbar-states *rule_toolbar* (map (# (= %0 n)) (list 0 1 2))))

(defun action-skip () (action-rule :skip 0))
(defun action-replace () (action-rule :replace 1))
(defun action-both () (action-rule :both 2))
(defun action-source-0 () (action-source 0))
(defun action-source-1 () (action-source 1))
(defun action-up-0 () (action-up 0))
(defun action-up-1 () (action-up 1))
(defun action-path-0 () (action-path 0))
(defun action-path-1 () (action-path 1))
(defun action-all-0 () (action-all 0))
(defun action-all-1 () (action-all 1))

(defun action-close ()
	(if *work* (mail-send *work* "stop"))
	(setq *running* :nil))

(defun action-minimise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y (* w 2) (* h 2)))
	(. *window* :change_dirty x y w h))
