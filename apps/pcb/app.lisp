(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
;(import "lib/debug/frames.inc")
(import "./app.inc")
(import "./reader.inc")
(import "./viewer.inc")

(enums +event 0
	(enum close)
	(enum prev next reset zoom_out zoom_in mode_normal mode_gerber)
	(enum show_all show_1 show_2 show_3 show_4))

(enums +select 0
	(enum main reply prog tip))

(defun all-pcbs (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (and (eql m "8") (ends-with ".pcb" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq *pcbs* (all-pcbs "apps/pcb/data/")
	*index* (some (# (if (eql "apps/pcb/data/test1.pcb" %0) _)) *pcbs*)
	canvas_scale 1 *mode* 0 *show* -1
	+max_zoom 15.0 +min_zoom 5.0 *zoom* (/ (+ +min_zoom +max_zoom) 2.0) +eps 0.25
	*running* t pcb nil pcb_data nil child nil +tag_min_size 104)

(ui-window *window* ()
	(ui-title-bar window_title "" (0xea19) +event_close)
	(ui-tool-bar main_toolbar ()
		(ui-buttons (0xe91d 0xe91e 0xe972 0xea00 0xea01 0xe9ac 0xe9ad) +event_prev)
		(ui-buttons ("0" "1" "2" "3" "4") +event_show_all
			(:color (const *env_toolbar2_col*) :font (const (create-font "fonts/OpenSans-Regular.ctf" 20)))))
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

(defun win-load (_)
	(setq pcb_data (load (defq file (elem-get (setq *index* _) *pcbs*))) pcb (pcb-read pcb_data))
	(bind '(w h) (. (defq canvas (pcb-canvas pcb *mode* *show* *zoom* canvas_scale)) :pref_size))
	(def pcb_scroll :min_width w :min_height h)
	(def window_title :text (cat "Pcb -> " (slice (inc (find-rev "/" file)) -1 file)))
	(. pcb_scroll :add_child (. canvas :swap))
	(. window_title :layout)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(def pcb_scroll :min_width 32 :min_height 32)
	(. *window* :change_dirty x y w h))

(defun win-zoom ()
	(bind '(w h) (. (defq canvas (pcb-canvas pcb *mode* *show* *zoom* canvas_scale)) :pref_size))
	(def pcb_scroll :min_width w :min_height h)
	(. pcb_scroll :add_child (. canvas :swap))
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(def pcb_scroll :min_width 32 :min_height 32)
	(. *window* :change_dirty x y w h))

(defun win-show ()
	(.-> pcb_scroll (:add_child (. (pcb-canvas pcb *mode* *show* *zoom* canvas_scale) :swap)) :layout))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(each (# (def %0 :tip_text %1))
		(. main_toolbar :children)
		'("prev" "next" "route" "zoom out" "zoom in" "pcb" "gerber"
		"all layers" "layer 1" "layer 2" "layer 3" "layer 4")))

(defun stop-route ()
	(when child
		(mail-send child "")
		(mail-free-mbox (elem-get +select_reply select))
		(mail-free-mbox (elem-get +select_prog select))
		(elem-set +select_reply select (mail-alloc-mbox))
		(elem-set +select_prog select (mail-alloc-mbox))))

(defun route ()
	(stop-route)
	(mail-send (setq child (open-child "apps/pcb/child.lisp" +kn_call_child))
		(setf-> (cat (str-alloc +job_size) pcb_data)
			(+job_grid_res (get :value res_spinner))
			(+job_vias_cost (get :value vias_spinner))
			(+job_quant (get :value quant_spinner))
			(+job_flood_range (get :value flood_spinner))
			(+job_even_range (get :value even_spinner))
			(+job_odd_range (get :value odd_spinner))
			(+job_reply (elem-get +select_reply select))
			(+job_prog (elem-get +select_prog select)))))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size))
	(tooltips)
	(bind '(x y w h) (apply view-locate (. (win-load *index*) :get_size)))
	(gui-add-front (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_reply)
				;child pcb data
				(setq pcb (pcb-read *msg*))
				(win-show))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_prog)
				;progress report mail
				(set (. progress :dirty) :value (getf *msg* +progress_current)
					:maximum (getf *msg* +progress_total)))
			;must be gui event to main mailbox
			((defq *id* (getf *msg* +ev_msg_target_id) action (. event_map :find *id*))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id *id*)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_option +ev_key_mod_command))))
						;call bound control/command key action
						(when (defq action (. key_map_control :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. key_map_shift :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilda)
								;insert char etc ...
								(char key))))
					((defq action (. key_map :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilda)
						;insert char etc ...
						(char key))))
			(t (. *window* :event *msg*))))
	(stop-route)
	(free-select select)
	(gui-sub *window*))
