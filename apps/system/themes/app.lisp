(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")

;The theme of the desktop, chosen. A row for each theme, its name and some
;of its symbols as it draws them. Press one and every window that is open
;is drawn in it, and it is what an app starts in from then on.

(enums +event +event_user
	(enum pick))

;some symbols to show a theme by, and a size that no toolbar uses, so a
;row keeps its own theme's font when the rest of the desktop changes
(defq +show (apply (const cat) (map (const num-to-utf8) (list
		+sym_undo +sym_redo +sym_cut +sym_copy +sym_paste +sym_save +sym_open
		+sym_find +sym_play +sym_grid +sym_zoom_in +sym_delete)))
	+show_size 30)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Themes" (+sym_close) +event_close)
	(ui-label *status* (:text "" :font *env_body_font*))
	(ui-grid *rows* (:grid_width 1)))

(defun show-current ()
	(defq text (cat "The theme is " (theme-current *env_home*)))
	(def *status* :text text)
	(.-> *status* :layout :dirty))

(defun main ()
	;a row is a button with the name, and the symbols beside it
	(each (lambda ((name symbols_file &ignore))
		(defq row (Flow) button (Button) strip (Label))
		(def row :flow_flags +flow_right_fill)
		(def button :text name :min_width 96 :font *env_button_font* :border *env_button_border*)
		(def strip :text +show :font (create-font symbols_file +show_size) :border 0)
		(. button :connect (+ +event_pick (!)))
		(.-> row (:add_child button) (:add_child strip))
		(. *rows* :add_child row))
		*themes*)
	(show-current)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(defq id :t)
	(while id
		(defq msg (mail-read (task-mbox)))
		(cond
			((= (setq id (getf msg +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((and (>= id +event_pick) (< id (+ +event_pick (length *themes*)))
					(= (getf msg +ev_msg_type) +ev_type_action))
				;it is kept, for what starts next, and the GUI tells what is open
				(defq name (first (elem-get *themes* (- id +event_pick))))
				(theme-save *env_home* name)
				(gui-theme-rpc name)
				(show-current))
			((. *window* :event msg))))
	(gui-sub-rpc *window*))
