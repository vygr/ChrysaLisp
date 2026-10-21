(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "apps/system/files/picker.inc")
(import "lib/files/names.inc")
(import "lib/files/files.inc")

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main tip picker))

(defq +file_types ''(".cpm" ".png" ".jpg" ".jpeg" ".tga" ".svg" ".cwb" ".flm") *canvas* :nil *file* :nil *picker* :nil)

(defun win-refresh (file)
	;the image is only ever shown, so its pixmap is freed once it is a texture.
	;A file, or an object of the store, lib/files/names.inc
	(when (defq new_canvas (canvas-load file +load_flag_free :nil (name-whole-stream file)))
		(setq *canvas* new_canvas *file* file)
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width w :min_height h)
		(def *window_title* :text (cat "Images -> " (path-name file)))
		(. *image_scroll* :add_child *canvas*)
		(. *window_title* :layout)
		(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
		(def *image_scroll* :min_width 128 :min_height 128)
		(. *window* :change_dirty x y w h)))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
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
			((= idx +select_picker)
				;the picture taken, or "" none
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "") (win-refresh file)))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	;a picker still open goes with it
	(if *picker* (mail-send *picker* ""))
	(gui-sub-rpc *window*))
