;imports
(import 'apps/login/pupa.inc)
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close 'win_button))

(defq space_width 8 tab_width (* space_width 4) margin_width (* space_width 3)
	doc_list '("VM" "ASSIGNMENT" "STRUCTURE" "COMMS" "FUNCTIONS"
	"LISP" "SYNTAX" "CLASSES" "INTRO" "TAOS" "TODO"))

(defun-bind normal-line ()
	(cond
		((starts-with "```" line_str)
			(setq state 'code))
		((starts-with "####" line_str)
			(def (setq line_widget (create-label)) 'border 0 'text (slice 4 -1 line_str)
				'font (create-font "fonts/OpenSans-Regular.ctf" 22)))
		((starts-with "###" line_str)
			(def (setq line_widget (create-label)) 'border 0 'text (slice 3 -1 line_str)
				'font (create-font "fonts/OpenSans-Regular.ctf" 26)))
		((starts-with "##" line_str)
			(def (setq line_widget (create-label)) 'border 0 'text (slice 2 -1 line_str)
				'font (create-font "fonts/OpenSans-Regular.ctf" 30)))
		((starts-with "#" line_str)
			(def (setq line_widget (create-label)) 'border 0 'text (slice 1 -1 line_str)
				'font (create-font "fonts/OpenSans-Regular.ctf" 34)))
		((find "`" line_str)
			(def (setq line_widget (create-flow)) 'flow_flags flow_right_fill)
			(defq word_lst (split (cat " " line_str " ") "`"))
			(each (lambda (word)
				(setq word_cnt (inc word_cnt))
				(unless (eql word " ")
					(cond
						((= 0 (logand word_cnt 1))
							(def (defq space_widget1 (create-label)) 'border 0 'min_width space_width)
							(def (defq word_widget (create-label)) 'border 0 'text word 'ink_color argb_blue)
							(def (defq space_widget2 (create-label)) 'border 0 'min_width space_width)
							(view-add-back line_widget space_widget1)
							(view-add-back line_widget word_widget)
							(view-add-back line_widget space_widget2))
						(t
							(def (defq word_widget (create-label)) 'border 0 'text word 'ink_color argb_black)
							(view-add-back line_widget word_widget))))) word_lst)
			(setq word_cnt (inc word_cnt)))
		(t
			(def (setq line_widget (create-label)) 'border 0 'text line_str))))

(defun-bind code-line ()
	(cond
		((starts-with "```" line_str)
			(setq state 'normal word_cnt 0))
		((defq tab_pos (find (ascii-char 9) line_str))
			(def (setq line_widget (create-flow)) 'flow_flags flow_right_fill)
			(def (defq tab_widget (create-label)) 'border 0 'min_width (* (inc tab_pos) tab_width))
			(def (defq code_widget (create-label)) 'border 0 'text (slice (inc tab_pos) -1 line_str)
				'font (create-font "fonts/Hack-Regular.ctf" 16) 'ink_color argb_blue)
			(view-add-back line_widget tab_widget)
			(view-add-back line_widget code_widget))
		(t
			(def (setq line_widget (create-label)) 'border 0 'text line_str
				'font (create-font "fonts/Hack-Regular.ctf" 16) 'ink_color argb_blue))))

(defun-bind populate-page (file)
	(ui-tree page_flow (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh)
			'font (create-font "fonts/OpenSans-Regular.ctf" 18) 'color argb_white)
		(ui-label _ ('min_width margin_width))
		(ui-flow page_widget ('flow_flags (logior flow_flag_down flow_flag_fillw)))
		(ui-label _ ('min_width margin_width)))
	(defq state 'normal word_cnt 0)
	(each-line (lambda (line_str)
		(defq line_str (trim-end line_str (ascii-char 13)) line_widget (while nil))
		(case state
			(normal (normal-line))
			(code (code-line)))
		(if line_widget (view-add-child page_widget line_widget))) (file-stream (cat "docs/" file ".md")))
	(apply view-change (cat (list page_flow 0 0) (view-pref-size page_flow)))
	(view-layout (view-add-child page_scroll page_flow))
	(view-dirty-all (view-layout doc_flow)))

(ui-window window ()
	(ui-flow _ ('flow_flags flow_down_fill)
		(ui-flow _ ('flow_flags flow_left_fill 'font (create-font "fonts/Entypo.ctf" 22) 'color *env_title_col*)
			(ui-buttons (0xea19) (const event_win_close))
			(ui-title _ ('text "Docs" 'font (create-font "fonts/OpenSans-Regular.ctf" 18))))
		(ui-flow doc_flow ('flow_flags flow_right_fill
				'font (create-font "fonts/OpenSans-Regular.ctf" 18) 'color *env_toolbar_col*)
			(ui-flow index ('flow_flags (logior flow_flag_down flow_flag_fillw))
				(each (lambda (path)
					(component-connect (ui-button _
						('text path 'flow_flags (logior flow_flag_align_vcenter flow_flag_align_hleft))) event_win_button)) doc_list))
			(ui-element page_scroll (create-scroll scroll_flag_vertical) ('min_width 848 'min_height 800 'color *env_slider_col*)))))

(defun-bind main ()
	(populate-page (elem 0 doc_list))
	(gui-add (apply view-change (cat (list window 280 64) (view-pref-size window))))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			nil)
		((= id event_win_button)
			(populate-page (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text)))
		(t (view-event window msg))))
	(view-hide window))
