(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/text/syntax.inc")

(enums +event 0
	(enum close button))

(defq +margin_width (* 8 3) syntax (Syntax) handlers (emap) scroll_pos (xmap) button nil
	doc_list '("VP_VM" "VP_ASSIGNMENT" "VP_STRUCTURE" "VP_FUNCTIONS" "VP_CLASSES"
	"LISP" "ENVIRONMENT" "CONDITIONALS" "ITERATION" "MACROS" "CLASSES"
	"COMMS" "EVENT_LOOPS" "EVENT_DISPATCH" "UI_WIDGETS"
	"SYNTAX" "TERMINAL" "COMMANDS" "DIARY"
	"INTRO" "TAOS" "TODO"))

(defun handler-func (state)
	(unless (defq handler (. handlers :find state))
		(defq module (cat "apps/docs/" (slice 1 -1 state) ".inc"))
		(repl (file-stream module) module)
		(. handlers :insert state handler))
	handler)

(defun populate-page (file)
	(ui-root page_flow (Flow) (:flow_flags +flow_right_fill :font *env_window_font*
			:color (get :color *window*))
		(ui-label _ (:min_width +margin_width))
		(ui-flow page (:flow_flags +flow_down_fill :min_width 800))
		(ui-label _ (:min_width +margin_width)))
	(defq state :text)
	(each-line (lambda (line)
			(task-sleep 0)
			(setq state ((handler-func state) state page (trim-end line (ascii-char 13)))))
		(file-stream (cat "docs/" file ".md")))
	((handler-func state) state page "")
	(bind '(w h) (. page_flow :pref_size))
	(. page_flow :change 0 0 w h)
	(def page_scroll :min_width w)
	(def (get :vslider page_scroll) :value (if (defq pos (. scroll_pos :find file)) pos 0))
	(.-> page_scroll (:add_child page_flow) (:layout))
	(.-> doc_flow :layout :dirty_all))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Docs" (0xea19) +event_close)
	(ui-flow doc_flow (:flow_flags +flow_right_fill :font *env_window_font* :color *env_toolbar_col*)
		(ui-flow index (:flow_flags (logior +flow_flag_down +flow_flag_fillw))
			(each (lambda (p)
				(. (ui-button _
					(:text p :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hleft))) :connect +event_button)) doc_list))
		(ui-scroll page_scroll +scroll_flag_vertical (:min_height 900))))

(defun main ()
	(populate-page (defq file (elem 0 doc_list)))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(while (cond
		((= (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_close)
			nil)
		((= id +event_button)
			(if button (undef (. button :dirty) :color))
			(. scroll_pos :insert file (get :value (get :vslider page_scroll)))
			(setq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
			(def (. button :dirty) :color *env_radio_col*)
			(populate-page (setq file (get :text button))))
		(t (. *window* :event msg))))
	(gui-sub *window*))
