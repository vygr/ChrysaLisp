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
		(def *image_scroll* :want_width w :want_height h)
		(. *window_title* :set_text (cat "Images -> " (path-name file)))
		(. *image_scroll* :add_child *canvas*)
		(. *window* :fit)))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(win-refresh (cat *app_root* "data/background.cpm"))
	(. *window* :open)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(. *window* :tip *msg*))
			((= idx +select_picker)
				;the picture taken, or "" none
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "") (win-refresh file)))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	;a picker still open goes with it
	(if *picker* (mail-send *picker* ""))
	(. *window* :close))
