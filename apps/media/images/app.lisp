(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main tip))

(defq +file_types ''(".cpm" ".flm" ".tga" ".svg" ".cwb") *canvas* :nil)

(defun win-refresh (file)
	(when (defq new_canvas (canvas-load file 0))
		(setq *canvas* new_canvas)
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width w :min_height h)
		(def *window_title* :text (cat "Images -> " (slice file (ifn (rfind "/" file) 0) -1)))
		(. *image_scroll* :add_child *canvas*)
		(. *window_title* :layout)
		(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
		(def *image_scroll* :min_width 128 :min_height 128)
		(. *file_selector* :select_node file)
		(. *window* :change_dirty x y w h)))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(. *file_selector* :populate "." +file_types 2)
	(bind '(x y w h) (apply view-locate
		(. (win-refresh (cat *app_root* "data/background.cpm")) :get_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((. *window* :dispatch *msg*))
			(:t ;gui event
				(. *window* :event *msg*))))
	(gui-sub-rpc *window*))