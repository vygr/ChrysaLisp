;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_button))

(defq id t doc_list '("VM" "ASSIGNMENT" "STRUCTURE" "FUNCTIONS" "LISP" "SYNTAX" "CLASSES" "TODO"))

(defun-bind populate-page (file)
	(def (defq state 'normal page_widget (create-flow)) 'flow_flags (logior flow_flag_down flow_flag_fillw)
		'font (create-font "fonts/OpenSans-Regular.ttf" 18) 'color argb_white)
	(defq word_cnt 0)
	(each-line (lambda (line_str)
		(while nil)
		(defq line_str (trim-end line_str (ascii-char 13)) line_widget nil)
		(case state
			(normal
				(cond
					((starts-with "```" line_str)
						(setq state 'code))
					((starts-with "####" line_str)
						(def (setq line_widget (create-label)) 'text (slice 4 -1 line_str)
							'font (create-font "fonts/OpenSans-Regular.ttf" 22)))
					((starts-with "###" line_str)
						(def (setq line_widget (create-label)) 'text (slice 3 -1 line_str)
							'font (create-font "fonts/OpenSans-Regular.ttf" 26)))
					((starts-with "##" line_str)
						(def (setq line_widget (create-label)) 'text (slice 2 -1 line_str)
							'font (create-font "fonts/OpenSans-Regular.ttf" 30)))
					((starts-with "#" line_str)
						(def (setq line_widget (create-label)) 'text (slice 1 -1 line_str)
							'font (create-font "fonts/OpenSans-Regular.ttf" 34)))
					(t
						(cond
							((defq s (find "`" line_str))
								(def (setq line_widget (create-flow)) 'flow_flags (logior flow_flag_right flow_flag_fillh flow_flag_lastw))
								(defq word_lst (split (cat " " line_str " ") "`"))
								(each (lambda (word)
									(if (eq 0 (logand (setq word_cnt (inc word_cnt)) 1))
										(def (defq word_widget (create-label)) 'text (cat "`" word "`") 'ink_color argb_blue)
										(def (defq word_widget (create-label)) 'text word 'ink_color argb_black))
									(view-add-back line_widget word_widget)) word_lst)
								(setq word_cnt (inc word_cnt)))
							(t
								(def (setq line_widget (create-label)) 'text line_str))))))
			(code
				(cond
					((starts-with "```" line_str)
						(setq state 'normal word_cnt 0))
					(t
						(def (setq line_widget (create-label)) 'text line_str
							'font (create-font "fonts/Hack-Regular.ttf" 16) 'ink_color argb_blue)))))
		(if line_widget (view-add-child page_widget line_widget))) (cat "doc/" file ".md"))
	(bind '(w h) (view-pref-size page_widget))
	(view-layout (view-add-child page_scroll (view-change page_widget 0 0 w h)))
	(view-dirty-all (view-layout doc_flow)))

(ui-tree window (create-window window_flag_close) nil
	(ui-element doc_flow (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh flow_flag_lastw)
			'font (create-font "fonts/OpenSans-Regular.ttf" 18) 'color argb_yellow)
		(ui-element index (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw))
			(each (lambda (path)
				(button-connect-click (ui-element _ (create-button)
					('text path 'flow_flags (logior flow_flag_align_vcenter flow_flag_align_hleft))) event_win_button)) doc_list))
		(ui-element page_scroll (create-scroll scroll_flag_vertical) ('min_width 800 'min_height 800 'color argb_green))))

(populate-page (elem 0 doc_list))
(window-set-title window "Docs")
(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 300 64 w h))

(while id
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((eq id event_win_button)
			(populate-page (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text)))
		(t (view-event window msg))))
