;the Viewer and Hexview, each says first what makes it which: +app_name,
;+state_filename, +file_types, +text_types with no syntax colours, and
;+hex_width, the bytes in a line if a file is shown as hex

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")
(import "service/clipboard/app.inc")
(import "service/lock/app.inc")

;our UI widgets and events
(import "./widgets.inc")
(import "./state.inc")

(enums +select 0
	(enum main tip))

(enums +refresh_mode 0
	(enum visible hidden))

(bind '(+edit_font +edit_size) (font-info (get :font_editor *env*)))

(defq +margin 2)

(import "./display.inc")

(defun populate-buffer (file cx cy ax ay sx sy)
	;create new file buffer
	(defq files_map (. *meta_map* :find :files) key (str file) file_meta :nil buffer :nil
		flags (if (or +hex_width (some (# (ends-with %0 file)) +text_types)) 0 +buffer_flag_syntax))
	;clear all file buffers
	(. files_map :each (lambda (k v) (. v :erase :buffer)))
	;create new file meta data
	(. files_map :update key (# (setq file_meta (ifn %0 (scatter (Fmap)
						:cx cx :cy cy :ax ax :ay ay :sx sx :sy sy)))))
	;create new buffer
	(. file_meta :insert :buffer (setq buffer (Document flags *syntax*)))
	(when file
		(with-read-lock file
			(if +hex_width
				(. buffer :stream_load_hex (file-stream file) +hex_width)
				(. buffer :stream_load (file-stream file))))))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-buffer file 0 0 0 0 0 0)
	(defq meta (.-> *meta_map* (:find :files) (:find (str file))))
	(bind '(cx cy ax ay sx sy buffer)
		(gather meta :cx :cy :ax :ay :sx :sy :buffer))
	(setq *current_file* file)
	(bind '(cx cy ax ay &ignore) (. *edit* :clip_cursor cx cy ax ay))
	(bind '(sx sy &ignore) (. *edit* :clip_cursor sx sy))
	(bind '(fx fy fx1 fy1) '(0 0 0 0))
	(.-> *edit* (:set_buffer buffer)
		(:set_cursor cx cy ax ay)
		(:set_focus (nums fx fy fx1 fy1))
		(:set_scroll sx sy))
	(scatter meta :cx cx :cy cy :ax ax :ay ay :sx sx :sy sy)
	(update-find-toolbar)
	(. *title* :set_text (cat +app_name (if file (cat " -> " file) "")))
	(refresh))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(defq func (first action))
	(if (find func *find_actions*)
		(push action *whole_words* *regexp* *ignore_case* (. *find_text* :get_text)))
	(catch (eval action)
		(progn (prin _) (print) (setq *refresh_mode* (list +refresh_mode_visible)) :t)))

(defun main ()
	(defq select (task-mboxes +select_size)
		*running* :t *edit* (Viewer-edit) *regexp* :nil *ignore_case* :nil
		*syntax* (Syntax) *whole_words* :nil *refresh_mode* (list +refresh_mode_visible)
		*meta_map* (scatter (Fmap) :files (Fmap)) *current_file* :nil
		*cursor_stack* (list))
	(config-load)
	(.-> *edit* (:set_buffer (Document))
		(:set_ink_color +argb_white)
		(:set_select_color +argb_grey6)
		(:set_found_color +argb_grey4)
		(:set_region_color +argb_grey3))
	(def *edit* :vdu_width +vdu_min_width :vdu_height +vdu_min_height
		:font (create-font +edit_font (page-scale +edit_size)))
	(set *vdu_lines* :font (get :font *edit*))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *edit_flow* :add_back *edit*)
	(. *file_selector* :populate "." +file_types 2)
	(populate-vdu *current_file*)
	(action-minimise)
	(.-> *window* (:connect +event_layout) :open)
	(refresh)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(. *window* :tip *msg*))
			((= (getf *msg* +ev_msg_type) +ev_type_key_up)
				;key up event
			)
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*)))
		;update meta data
		(update-meta-data))
	(config-save)
	(. *window* :close))
