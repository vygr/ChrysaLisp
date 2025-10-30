(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")

; Events
(enums +event 0
	(enum close launch toggle))

; Mailbox selection enums
(enums +select 0
	(enum main tip))

; Default configuration if `launcher_state.tre` is missing
(defun get-default-config ()
	(scatter (Lmap)
		:columns 2
		:order '(System Accessories Media Communication Games Science Generic)
		:exclude '(launcher login wallpaper tui)
		:categories (scatter (Fmap 11)
			'System (scatter (Lmap) :collapsed :nil
				:apps '(terminal services debug profile netmon netspeed files logout))
			'Accessories (scatter (Lmap) :collapsed :nil
				:apps '(edit viewer docs fonts clock calculator))
			'Media (scatter (Lmap) :collapsed :nil
				:apps '(images films canvas raymarch))
			'Communication (scatter (Lmap) :collapsed :nil
				:apps '(chat whiteboard))
			'Games (scatter (Lmap) :collapsed :nil
				:apps '(boing freeball bubbles chess minefield))
			'Science (scatter (Lmap) :collapsed :nil
				:apps '(molecule pcb mandelbrot mesh))
			'Generic (scatter (Lmap) :collapsed :nil
				:apps '()))))

; Configuration state
(defq *config* :nil *config_file* (cat *env_home* "launcher_state.tre"))

(defun load-config ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream))
		(setq *config* (get-default-config))))

(defun save-config ()
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun get-app-name (folder)
	(defq folder (slice folder 5 -3))
	(sym (if (defq i (find "/" folder))
			(slice folder 0 i)
			folder)))

(defun scan-apps ()
	(defq exclude_list (. *config* :find :exclude) categories (. *config* :find :categories)
		disk_apps (filter (# (not (find %0 exclude_list)))
			(usort (map get-app-name (files-dirs (files-all "apps" '("app.lisp")))))))
	(each (lambda (app_name)
		(defq found :nil)
		(. categories :each (lambda (name cat_data)
			(if (find app_name (. cat_data :find :apps)) (setq found :t))))
		(unless found
			(. categories :update 'Generic (lambda (generic_cat)
				(unless generic_cat
					(setq generic_cat (scatter (Lmap) :collapsed :nil :apps '())))
				(. generic_cat :insert :apps (usort (push (. generic_cat :find :apps) app_name)))))))
		disk_apps))

(defun app-path (app_name)
	(cat "apps/" app_name "/app.lisp"))

(defun make-category (cat_data name columns)
	(defq apps (. cat_data :find :apps)
		collapsed (. cat_data :find :collapsed))
	(when (nempty? apps)
		(ui-root cat_flow (Flow) (:flow_flags +flow_down_fill :cat_name name)
			(ui-flow header (:flow_flags +flow_right_fill)
				(. (ui-button toggle (:text (if collapsed ">" "v")
						:color *env_title_col* :font *env_medium_terminal_font*))
					:connect +event_toggle)
				(ui-title title (:text name :color *env_title_col*)))
			(ui-grid app_grid (:grid_width columns)
				(each (lambda (app)
					(. (ui-button _ (:text app)) :connect +event_launch))
					apps)))
		(. app_grid :set_flags (if collapsed +view_flag_hidden 0) +view_flag_hidden)
		(. *main_flow* :add_child cat_flow)))

(defun build-ui ()
	(defq categories (. *config* :find :categories)
		columns (. *config* :find :columns))
	(each (lambda (cat_name)
		(when (defq cat_data (. categories :find cat_name))
			(make-category cat_data cat_name columns)))
		(. *config* :find :order)))

(defun setup-tooltips (select)
	(def *window* :tip_mbox (elem-get select +select_tip))
	; Example: (ui-tool-tips some_toolbar '("Tip 1" "Tip 2"))
)

; Main Window Layout
(ui-window *window* ()
	(ui-title-bar _ "Launcher" (0xea19) +event_close)
	(ui-flow *main_flow* (:flow_flags +flow_down_fill)))

(defun main ()
	; Initialization
	(defq select (task-mboxes +select_size) *running* :t)
	(setup-tooltips select)
	(load-config)
	(scan-apps)
	(build-ui)

	; Position and show window
	(bind '(x y w h) (apply view-locate (push (. *window* :pref_size) *env_launcher_position*)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; Main Event Loop
	(while *running*
		(defq idx (mail-select select)
			msg (mail-read (elem-get select idx)))

		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			((= idx +select_main)
				(defq id (getf msg +ev_msg_target_id))
				(cond
					((= id +event_close)
						(setq *running* :nil))

					((= id +event_launch)
						(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
						(open-child (app-path (get :text button)) +kn_call_open)
						(setq *running* :nil))

					((= id +event_toggle)
						(defq toggle (. *window* :find_id (getf msg +ev_msg_action_source_id))
							cat_flow (penv (penv toggle))
							grid (second (. cat_flow :children))
							cat_name (get :cat_name cat_flow)
							categories (. *config* :find :categories)
							cat_data (. categories :find cat_name)
							collapsed (not (. cat_data :find :collapsed)))
						(. cat_data :insert :collapsed collapsed)
						(def toggle :text (if collapsed ">" "v"))
						(. grid :set_flags (if collapsed +view_flag_hidden 0) +view_flag_hidden)

						; Recalculate size and update scrollbars after visibility change
						(bind '(x y) (. *window* :get_pos))
						(bind '(w h) (. *window* :pref_size))
						(bind '(x y w h) (view-fit x y w h))
						(. *window* :change_dirty x y w h))

					(:t (. *window* :event msg))))))

	(save-config)
	(gui-sub-rpc *window*))