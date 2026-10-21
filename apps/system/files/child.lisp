(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/collections/tree.inc")
(import "lib/text/searching.inc")
(import "service/lock/app.inc")

;The file picker, a drawer: a folder is open, what is in it listed,
;folders first and then the files of the kinds asked for, those whose
;names the filter, a regexp, finds, and the path of it above. A folder is
;gone into, a file taken, and taken again, or OK, it is the answer. To
;save, a name may be written of one not there, in a folder not there yet,
;which the host makes as the file is written, and one that is there is
;replaced only when asked again. A folder of the tree is from its top,
;one of the host from /.
;
;Down the side the places to go to: the tree, the user's folder, the
;docs, and those of the app that asked, where it last took a file from,
;newest first, kept for each user and app in the user's places.tre.
;
;It is mailed reply_mbox then title|root[|kinds[|mode[|app]]]: root a
;folder, or a file to start at, its name in the field, "." to start where
;the app last was; kinds the ends of names it lists, as ".cwb .md", * or
;none for all; mode open or save; app the name its places are kept by, -
;or none for none kept. No field is left empty, (split) leaves it out. It answers
;the once, the path taken or "" for none, to reply_mbox, and goes.
;apps/system/files/picker.inc.

;our UI widgets and events
(import "./widgets.inc")

(defq +places_file (cat (get :home *env*) "places.tre") +places_recent 50 +places_version 1)

(defun say (text)
	(def *note* :text text)
	(.-> *note* :layout :dirty))

(defun folder? (path)
	(and (defq info (pii-fstat path)) (= (logand (third info) 0xf000) 0x4000)))

(defun folder-of (path)
	;a folder written as it is kept: of the tree from its top, no ./, of
	;the host from /, and no / at the end
	(while (starts-with "./" path) (setq path (slice path 2 -1)))
	(while (and (> (length path) 1) (ends-with "/" path)) (setq path (slice path 0 -2)))
	(if (eql path "") "." path))

(defun folder-above (folder)
	;the folder a folder is in, the top of the tree or of the host no further
	(cond
		((or (eql folder ".") (eql folder "/")) folder)
		((defq at (rfind "/" folder)) (if (= at 1) "/" (slice folder 0 (dec at))))
		(".")))

(defun path-in (folder name)
	(cond
		((eql folder ".") name)
		((eql folder "/") (cat "/" name))
		((cat folder "/" name))))

(defun folder-said (folder)
	;a folder as the path field and a place say it
	(cond
		((eql folder ".") "./")
		((eql folder "/") "/")
		((cat folder "/"))))

(defun go-to (folder)
	;the drawer open at a folder: its folders, then its files of the kinds,
	;of those the filter finds
	(setq *folder* folder *replace* :nil)
	(defq folders (list) files (list) engine :nil meta :nil)
	;the filter as grep and the editor take one, (query), a regexp
	(if (nempty? *pattern*) (bind '(engine meta &ignore) (query *pattern* :nil :t :nil)))
	(each! (lambda (name type)
			(unless (starts-with "." name)
				(cond
					((eql type "4") (push folders (cat name "/")))
					((and (or (empty? *exts*) (some (# (ends-with %0 name)) *exts*))
							(or (not meta) (. engine :match? name meta)))
						(push files name)))))
		(unzip (split (ifn (pii-dirlist folder) "") ",") 2))
	(setq *items* (cat (sort folders) (sort files)))
	(each (# (. %0 :sub)) (. *list* :children))
	(each (lambda (name)
			;a row white, and a line under it, the lines of the sheet go on below
			(defq button (Button) rule (Backdrop))
			(def button :text name :border 0 :paper +argb_white :ink +argb_black)
			(view-variant button :font :font_button)
			(. button :connect (+ +event_item (!)))
			(def rule :paper +argb_grey14 :min_height 1)
			(.-> *list* (:add_child button) (:add_child rule)))
		*items*)
	(when (empty? *items*)
		(def (defq hint (Label)) :text (if *save* "Nothing here yet" "Nothing here") :ink +argb_grey8)
		(. *list* :add_child hint))
	;the sheet as long as the rows, and the scroll at least, ruled at the
	;height of a row
	(bind '(w h) (. *list* :pref_size))
	(bind '(sw sh) (. *scroll* :get_size))
	(defq w (max w (- sw 10) (- (get :min_width *scroll*) 10)) h (max h sh (get :min_height *scroll*) 40))
	;a row and its line, the sheet's lines from the bottom of the first
	(when (nempty? *items*)
		(defq row (second (. (first (. *list* :children)) :pref_size)))
		(def *ruled* :spacing (inc row) :origin_y row))
	(. *list* :change 0 0 w (second (. *list* :pref_size)) :t)
	(. *ruled* :change_dirty 0 0 w h :t)
	(.-> *scroll* :layout :dirty_all)
	(.-> *path* (:set_text (folder-said folder)) :layout :dirty)
	(if (and (nempty? *pattern*) (not meta)) (say "The filter is no regexp.") (say "")))

;;;;;;;;
; places
;;;;;;;;

(defun places-load ()
	;the user's places of every app, :nil if none kept
	(defq tree (if (/= (age +places_file) 0)
		(with-read-lock +places_file (tree-load (file-stream +places_file)))))
	(and tree (eql (. tree :find :version) +places_version) tree))

(defun places-of (app)
	; (places-of app) -> (last_file folder ...)
	(ifn (and app (defq tree (places-load)) (. tree :find (sym (cat ":" app)))) (list "")))

(defun places-keep (app path)
	;the folder of a path taken first of the app's places, and the path its
	;last, the rest after it, so many
	(when app
		(defq tree (ifn (places-load) (scatter (Emap) :version +places_version))
			key (sym (cat ":" app)) was (rest (ifn (. tree :find key) (list "")))
			folder (folder-above (if (starts-with "/" path) path (cat "./" path)))
			folder (folder-of folder))
		(defq was (filter (# (nql %0 folder)) was))
		(. tree :insert key (cat (list path folder) (slice was 0 (min (length was) (dec +places_recent)))))
		(with-write-lock +places_file
			(tree-save (file-stream +places_file +file_open_write) tree))))

(defun place-name (folder all)
	;a folder by its own name, and the folder above it too where another of
	;all has that name, so the side stays narrow, its path its tip
	(defq parts (split folder "/") name (last parts))
	(cond
		((not name) (folder-said folder))
		((> (length (filter (# (eql (last (split %0 "/")) name)) all)) 1)
			(cat (join (slice parts (max 0 (- (length parts) 2)) -1) "/") "/"))
		((cat name "/"))))

(defun places-show ()
	;the places down the side: the tree, the user's folder, the docs, then
	;those of the app, a button each, in a scroll of their own
	(defq home (folder-of (get :home *env*)))
	(setq *place_list* (cat (list "." home "docs")
		(filter! (# (and (nempty? %0) (not (find %0 (list "." home "docs"))))) *app_places* 1 -1)))
	(if (> (length *place_list*) +places_max) (setq *place_list* (slice *place_list* 0 +places_max)))
	(each (# (. %0 :sub)) (. *places* :children))
	(each (lambda (folder)
			(defq button (Button))
			(def button :text (cond ((= (!) 0) "The tree") ((= (!) 1) "Your folder")
					((place-name folder (slice *place_list* 3 -1))))
				:tip_text (folder-said folder))
			(view-variant button :font :font_button)
			(view-variant button :border :border_button)
			(. button :connect (+ +event_place (!)))
			(. *places* :add_child button))
		*place_list*)
	(bind '(w h) (. *places* :pref_size))
	(bind '(sw &ignore) (. *places_scroll* :get_size))
	(. *places* :change_dirty 0 0 (max w (- sw 16) 144) h :t)
	(.-> *places_scroll* :layout :dirty_all))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	;read parameters from parent
	(defq msg (mail-read (task-mbox)) reply_mbox (slice msg 0 +net_id_size))
	(bind '(title root &optional kinds mode app) (split (slice msg +net_id_size -1) "|"))
	(defq *running* :t *save* (eql mode "save") *exts* (if (eql kinds "*") (list) (split (ifn kinds "") " "))
		*app* (and app (nempty? app) (nql app "-") app) *app_places* (places-of *app*)
		*pattern* "" *place_list* (list)
		*folder* "." *items* (list) *replace* :nil root (folder-of (ifn root ".")))
	(def *window_title* :text title)
	(. *kinds* :set_text (join *exts* " "))
	(places-show)
	;no root said, where the app last took a file, that file's name taken
	(and (eql root ".") (nempty? (first *app_places*)) (second *app_places*)
		(setq root (first *app_places*)))
	;a root that is a file starts at its folder, with its name taken
	(cond
		((folder? root) (go-to root))
		((defq at (rfind "/" root))
			(defq folder (folder-above root))
			(go-to (if (folder? folder) folder "."))
			(. *name* :set_text (slice root at -1)))
		(:t (go-to ".") (. *name* :set_text root)))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;the places and the sheet sized again now the window has its size
	(places-show)
	(go-to *folder*)
	(defq select (task-mboxes 2))
	(def *window* :tip_mbox (elem-get select 1))
	(while *running*
		(defq idx (mail-select select) *msg* (mail-read (elem-get select idx)) id (getf *msg* +ev_msg_target_id))
		(cond
			((= idx 1)
				;what a place is, its path
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((eql *msg* "")
				(setq *running* :nil))
			;a row of the drawer, or a place
			((and (= (getf *msg* +ev_msg_type) +ev_type_action)
					(>= id +event_item) (< id (+ +event_item (length *items*))))
				(action-item (- id +event_item)))
			((and (= (getf *msg* +ev_msg_type) +ev_type_action)
					(>= id +event_place) (< id (+ +event_place (length *place_list*))))
				(go-to (elem-get *place_list* (- id +event_place))))
			;must be gui event to main mailbox
			((. *window* :dispatch *msg*))
			(:t ;gui event
				(. *window* :event *msg*))))
	(gui-sub-rpc *window*))
