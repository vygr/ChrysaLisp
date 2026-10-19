(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/files/files.inc")

; Events
(enums +event +event_user
	(enum launch toggle collapse expand))

; Mailbox selection enums
(enums +select 0
	(enum main tip))

; Configuration state
(defq *config* :nil +config_version 21 +config_file (cat (get :home *env*) "launcher.tre"))

; Default configuration if `launcher.tre`
; is missing, or version mismatch
(defun config-default ()
	(scatter (Emap)
		:version +config_version :columns 3
		:exclude '("launcher" "login" "wallpaper" "tui")
		:categories (scatter (Emap)
			'System (scatter (Emap) :collapsed :nil
				:apps '("terminal" "services" "debug" "profile" "netmon"
					"netmap" "netspeed" "storage" "keys" "files" "themes" "logout"))
			'Desktop (scatter (Emap) :collapsed :nil
				:apps '("docs" "todo" "calculator" "chat" "clock" "eyes" "weather" "crypto" "sunclock" "news" "lexicon" "rosetta"))
			'Tools (scatter (Emap) :collapsed :nil
				:apps '("edit" "viewer" "hexview" "fonts" "benchmark"))
			'Media (scatter (Emap) :collapsed :nil
				:apps '("images" "films" "whiteboard" "voice"))
			'Games (scatter (Emap) :collapsed :nil
				:apps '("chess" "minefield" "slider" "pairs" "solitaire" "onslaught"))
			'Demos (scatter (Emap) :collapsed :nil
				:apps '("boing" "freeball" "bubbles" "opcodes" "canvas" "raymarch" "surface" "planets" "model"))
			'Science (scatter (Emap) :collapsed :nil
				:apps '("molecule" "pcb" "mandelbrot" "mesh")))))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
		(tree-load (file-stream +config_file))))
	(if (or (not *config*) (/= (. *config* :find :version) +config_version))
		(setq *config* (config-default))))

(defun config-save ()
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun scan-apps ()
	(defq exclude_list (. *config* :find :exclude) categories (. *config* :find :categories)
		disk_apps (filter (lambda (name) (notany (# (ends-with %0 name)) exclude_list))
			(files-all "apps" '("app.lisp") 5 -10)))
	(each (lambda (app_path)
		(defq app_name (path-name app_path) found :nil)
		(. name_to_path_map :insert app_name app_path)
		(. categories :each (lambda (name cat_data)
			(if (find app_name (. cat_data :find :apps)) (setq found :t))))
		;one new since the layout was kept goes where the default has it, if
		;that is there, else with the others no category has
		(unless found
			(defq home (some (lambda ((name cat_data))
					(and (find app_name (. cat_data :find :apps)) (. categories :find name) name))
				(. (. (config-default) :find :categories) :tolist)))
			(when home
				(. (. categories :find home) :update :apps (# (push %0 app_name)))
				(setq found :t)))
		(unless found
			(. categories :update 'Generic (lambda (generic_cat)
				(if generic_cat
					(progn (. generic_cat :update :apps (# (sort (push %0 app_name)))) generic_cat)
					(scatter (Emap) :collapsed :nil :apps (list app_name)))))))
		disk_apps))

(defun app-path (app_path)
	(cat "apps/" app_path "/app.lisp"))

(defun make-category (cat_data app_name columns)
	(defq apps (. cat_data :find :apps)
		collapsed (. cat_data :find :collapsed))
	(when (nempty? apps)
		(ui-root cat_flow (Flow) (:flow_flags +flow_down_fill :cat_name app_name)
			(ui-flow header (:flow_flags +flow_right_fill)
				(. (ui-button toggle (:text (num-to-utf8 (if collapsed +sym_next +sym_up))
						:paper :paper_title :font :font_title_buttons))
					:connect +event_toggle)
				(ui-title title (:text app_name :paper :paper_title)))
			(ui-grid app_grid (:grid_width columns)
				(each (lambda (app)
					(. (ui-button _ (:text app)) :connect +event_launch))
					apps)))
		(. app_grid :set_flags (if collapsed +view_flag_hidden 0) +view_flag_hidden)
		(. *main_flow* :add_child cat_flow)))

(defun cat-set (cat_flow collapsed)
	; (cat-set cat_flow collapsed)
	;a category collapsed or not, its toggle and its grid, and kept so
	(bind '(header grid) (. cat_flow :children))
	(. (. (. *config* :find :categories) :find (get :cat_name cat_flow)) :insert :collapsed collapsed)
	(def (first (. header :children)) :text (num-to-utf8 (if collapsed +sym_next +sym_up)))
	(. grid :set_flags (if collapsed +view_flag_hidden 0) +view_flag_hidden))

(defun window-fit ()
	; (window-fit)
	;the window its size again after categories are shown or hidden, all
	;laid out again, a toggle is a new size and the row it is in is not
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h :t))

(defun build-ui ()
	(defq categories (. *config* :find :categories)
		columns (. *config* :find :columns))
	(. categories :each (lambda (cat_name cat_data)
		(make-category cat_data cat_name columns))))

; Main Window Layout
(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Launcher" (+sym_close) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (+sym_up +sym_down) +event_collapse))
	(ui-flow *main_flow* (:flow_flags +flow_down_fill)))

(ui-tool-tips *main_toolbar*
	'("collapse" "expand"))

(defun main ()
	; Initialization
	(defq select (task-mboxes +select_size) running :t name_to_path_map (Fmap 11))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(config-load)
	(scan-apps)
	(build-ui)

	; Position and show window
	(bind '(x y w h) (apply view-locate (push (. *window* :pref_size) (get :launcher_position *env*))))
	(gui-add-front-rpc (. *window* :change x y w h))

	; Main Event Loop
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			;must be +select_main !
			((= (defq id (getf msg +ev_msg_target_id)) +event_close)
				(setq running :nil))
			((= id +event_launch)
				(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
				(open-child (app-path (. name_to_path_map :find (get :text button))) +kn_call_pin)
				(setq running :nil))
			((= id +event_toggle)
				(defq cat_flow (penv (penv (. *window* :find_id (getf msg +ev_msg_action_source_id)))))
				(cat-set cat_flow (not (. (. (. *config* :find :categories) :find (get :cat_name cat_flow)) :find :collapsed)))
				(window-fit))
			((or (= id +event_collapse) (= id +event_expand))
				;every category the one way
				(each (# (cat-set %0 (= id +event_collapse))) (. *main_flow* :children))
				(window-fit))
			((. *window* :event msg))))

	(config-save)
	(gui-sub-rpc *window*))
