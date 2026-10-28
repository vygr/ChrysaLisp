(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/collections/tree.inc")
(import "lib/text/searching.inc")
(import "service/lock/app.inc")
(import "lib/files/names.inc")
(import "./picker.inc")
(import "./preview.inc")

;The file picker, a drawer: a folder is open, what is in it listed,
;folders first and then the files of the kinds asked for, those whose
;names the filter, a regexp, finds, and the path of it above. A folder is
;gone into, a file taken, and taken again, or OK, it is the answer. To
;save, a name may be written of one not there, in a folder not there yet,
;which the host makes as the file is written, and one that is there is
;replaced only when asked again. A folder of the tree is from its top,
;one of the host from /, one of the store from store:, lib/files/names.inc.
;
;Down the side the places to go to: the tree, the user's folder, the
;app's own folders, the store if there is one and the app takes its
;objects, and those of the app that asked, where it last took a file from,
;newest first, kept for each user and app in the user's places.tre.
;
;It is mailed reply_mbox then title|root[|kinds[|mode[|app[|names[|places]]]]]: root a
;folder, or a file to start at, its name in the field, "." to start where
;the app last was; kinds the ends of names it lists, as ".cwb .md", * or
;none for all; mode open or save; app the name its places are kept by, -
;or none for none kept; names if the app takes objects of the store,
;store:path, - or none if only files; places the app's own folders, a
;tab between them, - or none for none. No field is left empty, (split) leaves it out. It answers
;the once, the path taken or "" for none, to reply_mbox, and goes.
;apps/system/files/picker.inc.

;our UI widgets and events
(import "./widgets.inc")

(defq +places_file (file-places-file) +places_recent 50
	;how many things are looked at, for all the folders of a listing, to
	;see if there is any wanted in each, before the rest are shown unlooked
	+filter_most 20000)

(defun say (text)
	(def *note* :text text)
	(.-> *note* :layout :dirty))

(defun folder? (path)
	(name-folder? path))

(defun folder-of (path)
	;a folder written as it is kept: of the tree from its top, no ./, of
	;the host from /, of the store from store:, and no / at the end
	(while (starts-with "./" path) (setq path (slice path 2 -1)))
	(while (and (> (length path) 1) (ends-with "/" path) (nql path +name_store)) (setq path (slice path 0 -2)))
	(if (eql path "") "." path))

(defun folder-above (folder)
	;the folder a folder is in, the top of the tree, of the host or of the
	;store no further
	(cond
		((find folder (list "." "/" +name_store)) folder)
		((defq at (rfind "/" folder)) (if (= at 1) "/" (slice folder 0 (dec at))))
		((name-in-store folder) +name_store)
		(".")))

(defun path-in (folder name)
	(cond
		((eql folder ".") name)
		((find folder (list "/" +name_store)) (cat folder name))
		((cat folder "/" name))))

(defun name-last (full)
	;the name of a path without the folder it is in
	(cond
		((defq at (rfind "/" full)) (slice full at -1))
		((defq name (name-in-store full)) name)
		(full)))

(defun folder-said (folder)
	;a folder as the path field and a place say it
	(cond
		((eql folder ".") "./")
		((find folder (list "/" +name_store)) folder)
		((cat folder "/"))))

(defun wanted? (name engine meta)
	;a file of the kinds, that the filter finds if there is one
	(and (or (empty? *exts*) (some (# (ends-with %0 name)) *exts*))
		(or (not meta) (. engine :match? name meta))))

(defun folder-has? (folder engine meta)
	;anything wanted in a folder, or a folder in it, and so on down: the
	;first found ends the look. Not hidden folders, nor what is built. Once
	;the listing has looked at so many, *seen*, a folder is said to have
	(defq stack (list folder) found :nil)
	(while (and (not found) (defq at (pop stack)))
		(each! (lambda (name type)
				(cond
					(found)
					((starts-with "." name))
					((> (++ *seen*) +filter_most) (setq found :t))
					((eql type "d") (unless (eql name "obj") (push stack (path-in at name))))
					((wanted? name engine meta) (setq found :t))))
			(name-list at)))
	found)

(defun go-to (folder)
	;the drawer open at a folder: its folders, then its files of the kinds,
	;of those the filter finds
	(setq *folder* folder *replace* :nil)
	(defq folders (list) files (list) engine :nil meta :nil *seen* 0)
	;the filter as grep and the editor take one, (query), a regexp
	(if (nempty? *pattern*) (bind '(engine meta &ignore) (query *pattern* :nil :t :nil)))
	(each! (lambda (name type)
			(unless (starts-with "." name)
				(cond
					;of kinds, or a filter, a folder only if there is something in it
					;of them, the filter finds
					((eql type "d") (if (or (and (not meta) (empty? *exts*)) (folder-has? (path-in folder name) engine meta))
						(push folders (cat name "/"))))
					((wanted? name engine meta) (push files name)))))
		(name-list folder))
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
	;a row and its line, the sheet's lines from the bottom of the first,
	;and the sheet made to fit, (fit-drawer)
	(when (nempty? *items*)
		(defq row (second (. (first (. *list* :children)) :pref_size)))
		(def *ruled* :spacing (inc row) :origin_y row))
	(.-> *scroll* :layout :dirty_all)
	(.-> *path* (:set_text (folder-said folder)) :layout :dirty)
	(if (and (nempty? *pattern*) (not meta)) (say "The filter is no regexp.") (say "")))

;;;;;;;;;
; preview
;;;;;;;;;

(defun preview-show (file)
	;what a file taken is, as it can be seen, by the previewers there are,
	;./preview.inc, as a chat shows them; how long it is if none can
	(each (# (. %0 :sub)) (. *preview* :children))
	(when (and file (nempty? file) (defq size (name-size file)))
		;a previewer that fails shows nothing, the picker goes on
		(defq shown :nil)
		(catch (setq shown (file-preview file 212 212)) (progn (prin _) (print) (setq shown :nil) :t))
		(unless shown
			(def (setq shown (Label)) :text (cat (str size) " bytes")))
		(. *preview* :add_child shown))
	(.-> *preview* (:constrain :t) :layout :dirty_all))

;;;;;;;;
; places
;;;;;;;;

(defun places-keep (app path)
	;the folder of a path taken first of the app's places, and the path its
	;last, the rest after it, so many
	(when app
		(defq tree (ifn (places-load) (scatter (Emap) :version +places_version))
			key (sym (cat ":" app)) was (rest (ifn (. tree :find key) (list "")))
			folder (folder-of (folder-above (if (or (starts-with "/" path) (name-in-store path)) path (cat "./" path)))))
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
	;the places down the side: the tree, the user's folder, the folders the
	;app gave that are there, the store if this machine has one and the app
	;takes its objects, then where the app last took things from, a button
	;each, in a scroll of their own
	(defq home (folder-of (get :home *env*))
		given (filter (# (and (folder? %0) (not (find %0 (list "." home))))) *given*)
		fixed (cat (list "." home) given (if (and *names* (storage-service)) (list +name_store) (list))))
	(setq *place_list* (cat fixed
		(filter! (# (and (nempty? %0) (not (find %0 fixed)))) *app_places* 1 -1)))
	(if (> (length *place_list*) +places_max) (setq *place_list* (slice *place_list* 0 +places_max)))
	;the tree, the user's folder and the store by what they are, the rest by
	;their names
	(defq named (slice *place_list* 2 -1)
		labels (map (lambda (folder)
				(cond
					((eql folder ".") "The tree")
					((eql folder home) "Your folder")
					((eql folder +name_store) "The store")
					((place-name folder named))))
			*place_list*))
	(each (# (. %0 :sub)) (. *places* :children))
	(each (lambda (folder label)
			(defq button (Button))
			(def button :text label :tip_text (folder-said folder))
			(view-variant button :font :font_button)
			(view-variant button :border :border_button)
			(. button :connect (+ +event_place (!)))
			(. *places* :add_child button))
		*place_list* labels)
	(.-> *places_scroll* :layout :dirty_all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what the two scrolls hold, made to fit them as they change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sheet-width (sheet)
	;the width of a scroll less its slider
	(bind '(w &ignore) (. sheet :get_size))
	(bind '(sw &ignore) (. (get :vslider sheet) :get_constraint))
	(max 0 (- w sw)))

(defun fit-drawer (sheet)
	;the rows the width of the scroll, the ruled sheet that too, and as
	;long as the rows or the scroll, whichever is longer
	(bind '(& h) (. sheet :get_size))
	(defq w (sheet-width sheet) rows (second (. *list* :pref_size)))
	(. *list* :change_dirty 0 0 w rows :t)
	(. *ruled* :change_dirty 0 0 w (max rows h)))

(defun fit-places (sheet)
	(. *places* :change_dirty 0 0 (sheet-width sheet) (second (. *places* :pref_size)) :t))

(def *scroll* :fit fit-drawer)
(def *places_scroll* :fit fit-places)

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	;read parameters from parent
	(defq msg (mail-read (task-mbox)) reply_mbox (slice msg 0 +net_id_size))
	(bind '(title root &optional kinds mode app names places) (split (slice msg +net_id_size -1) "|"))
	(defq *running* :t *save* (eql mode "save") *names* (eql names "names")
		*given* (if (and places (nql places "-")) (map (const folder-of) (split places "\t")) (list)) *exts* (if (eql kinds "*") (list) (split (ifn kinds "") " "))
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
		(:t (defq folder (folder-above root))
			(go-to (if (folder? folder) folder "."))
			(. *name* :set_text (name-last root))))
	(preview-show (path-in *folder* (trim (. *name* :get_text))))
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
