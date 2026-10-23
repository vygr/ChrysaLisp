(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")
(import "./reader.inc")
(import "./viewer.inc")
(import "service/lock/app.inc")
(import "apps/system/files/picker.inc")
(import "lib/files/names.inc")

(enums +event +event_user
	(enum open reset)
	(enum mode)
	(enum layer))

(enums +select 0
	(enum main reply prog tip picker))

;the board shown, opened by the file picker, which keeps where boards were
;taken from, and kept as the one to show next time
(defq *file* (cat *app_root* "data/test1.pcb") *picker* :nil
	canvas_scale 1 *mode* 0 *show* -1
	+max_zoom 15.0 +min_zoom 5.0 *config* :nil +config_version 1
	+config_file (cat (get :home *env*) "pcb.tre")
	*running* :t pcb :nil pcb_data :nil child :nil +tag_min_size 104)

(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_open +sym_route) +event_open)
			(ui-buttons (+sym_zoom_in +sym_zoom_out) +event_zoom_in))
		(. (ui-radio-bar *mode_toolbar* (+sym_pcb +sym_gerber)) :connect +event_mode)
		(. (ui-radio-bar *layer_toolbar* ("0" "1" "2" "3" "4")
				(:paper (const (get :paper_toolbar2 *env*))
				:font (const (create-font "fonts/OpenSans-Regular.ctf" 20))))
			:connect +event_layer))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label _ (:text "grid_res:" :min_width +tag_min_size :flow_flags +flow_flag_align_hright))
		(ui-spinner res_spinner (:value 1 :maximum 3 :minimum 1))
		(ui-label _ (:text "vias_cost:" :min_width +tag_min_size :flow_flags +flow_flag_align_hright))
		(ui-spinner vias_spinner (:value 0 :maximum 8 :minimum 0))
		(ui-label _ (:text "quant:" :min_width +tag_min_size :flow_flags +flow_flag_align_hright))
		(ui-spinner quant_spinner (:value 1 :maximum 16 :minimum 1)))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label _ (:text "flood_range:" :min_width +tag_min_size :flow_flags +flow_flag_align_hright))
		(ui-spinner flood_spinner (:value 2 :maximum 4 :minimum 1))
		(ui-label _ (:text "even_range:" :min_width +tag_min_size :flow_flags +flow_flag_align_hright))
		(ui-spinner even_spinner (:value 1 :maximum 2 :minimum 1))
		(ui-label _ (:text "odd_range:" :min_width +tag_min_size :flow_flags +flow_flag_align_hright))
		(ui-spinner odd_spinner (:value 1 :maximum 2 :minimum 1)))
	(ui-progress progress (:value 100 :maximum 100))
	(ui-scroll pcb_scroll +scroll_flag_both (:min_width 512 :min_height 256)))

;a PCB is drawn once and then only ever shown, so the pixmap of its
;canvas is freed once it is a texture. A file, or an object of the store,
;lib/files/names.inc
(defun win-load (file)
	(defq stream (name-whole-stream file))
	(setq pcb_data (read-blk stream (ifn (name-size file) 0)) pcb (pcb-read pcb_data) *file* file)
	(bind '(w h) (. (defq canvas (pcb-canvas pcb *mode* *show* (get :zoom *window*) canvas_scale)) :pref_size))
	(def pcb_scroll :min_width w :min_height h)
	(def *window_title* :text (cat "Pcb -> " (path-name file)))
	(. pcb_scroll :add_child (. canvas :swap +swap_flag_free))
	(. *window_title* :layout)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(def pcb_scroll :min_width 32 :min_height 32)
	(. *window* :change_dirty x y w h))

(defun win-zoom ()
	(bind '(w h) (. (defq canvas (pcb-canvas pcb *mode* *show* (get :zoom *window*) canvas_scale)) :pref_size))
	(def pcb_scroll :min_width w :min_height h)
	(. pcb_scroll :add_child (. canvas :swap +swap_flag_free))
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(def pcb_scroll :min_width 32 :min_height 32)
	(. *window* :change_dirty x y w h))

(defun win-show ()
	(.-> pcb_scroll (:add_child (. (pcb-canvas pcb *mode* *show* (get :zoom *window*) canvas_scale) :swap +swap_flag_free)) :layout))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	;a bar at a time, the two views and the layers are bars of their own
	(ui-tool-tips *main_toolbar*
		'("open" "route" "zoom in" "zoom out"))
	(ui-tool-tips *mode_toolbar*
		'("pcb" "gerber"))
	(ui-tool-tips *layer_toolbar*
		'("all layers" "layer 1" "layer 2" "layer 3" "layer 4")))

(defun stop-route ()
	(when child
		(mail-send child "")
		(elem-set select +select_reply (mail-mbox))
		(elem-set select +select_prog (mail-mbox))))

(defun route ()
	(stop-route)
	(mail-send (setq child (open-child (const (cat *app_root* "child.lisp")) +kn_call_run))
		(setf-> (cat (str-alloc +pcb_job_size) pcb_data)
			(+pcb_job_grid_res (get :value res_spinner))
			(+pcb_job_vias_cost (get :value vias_spinner))
			(+pcb_job_quant (get :value quant_spinner))
			(+pcb_job_flood_range (get :value flood_spinner))
			(+pcb_job_even_range (get :value even_spinner))
			(+pcb_job_odd_range (get :value odd_spinner))
			(+pcb_job_reply (elem-get select +select_reply))
			(+pcb_job_prog (elem-get select +select_prog)))))

;import actions and bindings
(import "./actions.inc")

(defun config-default ()
	(scatter (Emap) :version +config_version :zoom 1.0 :file *file*))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
		(tree-load (file-stream +config_file))))
	(if (or (not *config*) (/= (. *config* :find :version) +config_version))
		(setq *config* (config-default)))
	(def *window* :zoom (. *config* :find :zoom))
	;the board shown last, if it is still there
	(and (defq file (. *config* :find :file)) (name-size file) (setq *file* file)))

(defun config-save ()
	(scatter *config* :zoom (get :zoom *window*) :file *file*)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size))
	(tooltips)
	(config-load)
	(. *layer_toolbar* :set_selected 0)
	(. *mode_toolbar* :set_selected 0)
	(bind '(x y w h) (apply view-locate (. (win-load *file*) :get_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_reply)
				;child pcb data
				(setq pcb (pcb-read *msg*))
				(win-show))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_picker)
				;the board taken, or "" none
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "")
					(stop-route)
					(win-load file)))
			((= idx +select_prog)
				;progress report mail
				(set (. progress :dirty) :value (getf *msg* +pcb_progress_current)
					:maximum (getf *msg* +pcb_progress_total)))
			;must be gui event to main mailbox
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	;a picker still open goes with it
	(if *picker* (mail-send *picker* ""))
	(stop-route)
	(config-save)
	(gui-sub-rpc *window*))
