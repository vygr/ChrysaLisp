;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/tools/edit/ui.inc")

(defun history-note (file)
	;a document has been gone to, by the tree, a link or a search. It is
	;the newest of where the user has been, and what was ahead of where
	;they had gone back to is forgotten, as a browser does. Going to the
	;document they are on is not a move
	(unless (and (>= *history_idx* 0) (eql (elem-get *history* *history_idx*) file))
		(setq *history* (push (slice *history* 0 (inc *history_idx*)) file)
			*history_idx* (dec (length *history*)))))

(defun show-file (file &optional step)
	;go to a document. With a step it is a move back or forward through
	;where the user has been, and is not noted as a new place
	(setq *current_file* file)
	(if step
		(setq *history_idx* (+ *history_idx* step))
		(history-note file))
	(. *file_selector* :select_node file)
	(populate-page file))

(defun show-place (place)
	;bring a heading of the page to the top of it, by its name, the part
	;of a link after the #. The page is a run of Md widgets, with what is
	;not text between them, each knows its own headings. No such heading,
	;or no name, and the page is left where it is
	(when (and (nempty? place) (defq page (get :child *page_scroll*))
			(defq view (some (# (and (Md? %0) (. %0 :find_anchor place))) (. page :flatten))))
		(bind '(& y & &) (. page :get_relative view))
		(def (get :vslider *page_scroll*) :value y)
		(.-> *page_scroll* :layout :dirty_all)))

(defun action-back ()
	(if (> *history_idx* 0)
		(show-file (elem-get *history* (dec *history_idx*)) -1)))

(defun action-forward ()
	(if (< *history_idx* (dec (length *history*)))
		(show-file (elem-get *history* (inc *history_idx*)) 1)))

;overload Edit app ui.inc
(redefun action-file-leaf-action ()
	;load up the file selected
	(show-file (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))

(redefun action-close ()
	(setq *running* :nil))

(redefun action-minimise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(redefun action-maximise ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w (* h 2)))
	(. *window* :change_dirty x y w h))

(redefun action-scale-up ()
	;scale font up
	(defq old_size (get :zoom *window*)
		new_size (min 1.5 (+ old_size 0.125)))
	(when (/= old_size new_size)
		(def *window* :zoom new_size)
		(populate-page *current_file*)))

(redefun action-scale-down ()
	;scale font down
	(defq old_size (get :zoom *window*)
		new_size (max 0.75 (- old_size 0.125)))
	(when (/= old_size new_size)
		(def *window* :zoom new_size)
		(populate-page *current_file*)))

(defun action-link ()
	;a link of the page was pressed. It is followed if it is to one of the
	;documents of the app, those of the tree at the left, which is asked.
	;So not out of the folder they are under, +doc_root, whatever that is,
	;not to the web, a pdf or a picture, and not to a file that is not there.
	;What is after a # is a place in the document, a heading by its name,
	;and a link that is only that is to a place in the page that is up
	(when (and (defq view (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
			(defq target (def? :link view))
			(not (find ":" target)))
		(defq place (if (defq at (find "#" target)) (slice target (inc at) -1) "")
			file (if (starts-with "#" target) *current_file* (md-link-file *current_file* target)))
		(cond
			((eql file *current_file*)
				(show-place place))
			((and (some (# (ends-with %0 file)) +doc_types)
					(. *file_selector* :find_node file))
				(show-file file)
				(show-place place)))))
