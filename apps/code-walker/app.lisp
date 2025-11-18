;debug options
(case 2
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")

(enums +event 0
	(enum close max min)
	(enum process_btn clear_btn step_expand_btn export_btn toggle_diff_btn toggle_tree_btn toggle_compare_btn)
	(enum history_prev history_next clear_history)
	(enum save_session load_session)
	(enum input_field input_field2)
	(enum example1 example2 example3 example4))

(enums +select 0
	(enum main tip))

;;;;;;;;;;;;;;;;;;;;;
; Configuration
;;;;;;;;;;;;;;;;;;;;;

(defq +scroll_width 15
	+font_mono (create-font "fonts/Hack-Regular.ctf" 13)
	+font_title (create-font "fonts/Hack-Regular.ctf" 14)
	+bg_color (const (argb 255 32 32 40))
	+section_color (const (argb 255 48 48 56))
	+header_color (const (argb 255 64 64 72)))

;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;

(defq *show_diffs* :t *show_tree* :nil *compare_mode* :nil)
(defq *history* (list) *history_index* -1 *max_history* 50)

(defun compute-diff (old new)
	; Compute a simple diff between two pretty-printed forms
	; Returns a string showing changes
	(defq old_str (pretty-print old)
		  new_str (pretty-print new))

	(if (eql old_str new_str)
		"[No changes]"
		(progn
			; Split into lines for comparison
			(defq old_lines (split old_str "\n")
				  new_lines (split new_str "\n"))

			; Simple line-by-line diff
			(defq result (list))
			(defq max_lines (max (length old_lines) (length new_lines)))

			(each (lambda (idx)
				(defq old_line (if (< idx (length old_lines)) (elem-get old_lines idx) :nil)
					  new_line (if (< idx (length new_lines)) (elem-get new_lines idx) :nil))

				(cond
					((and old_line new_line (eql old_line new_line))
						; Line unchanged
						(push result (cat "  " old_line)))
					((and old_line (not new_line))
						; Line removed
						(push result (cat "- " old_line)))
					((and new_line (not old_line))
						; Line added
						(push result (cat "+ " new_line)))
					(:t
						; Line changed
						(push result (cat "- " old_line))
						(push result (cat "+ " new_line)))))
				(range 0 max_lines))

			(apply cat (map (lambda (line) (cat line "\n")) result)))))

(defun split (str delim)
	; Split string by delimiter
	(defq result (list) current "" delim_char (first delim))
	(each (lambda (ch)
		(if (= ch delim_char)
			(progn
				(push result current)
				(setq current ""))
			(setq current (cat current (str (char ch))))))
		str)
	(when (nempty? current) (push result current))
	result)

(defun tree-print (obj &optional prefix is_last)
	; Print AST as ASCII tree structure
	(setd prefix "" is_last :t)
	(defq result (list))

	(cond
		((list? obj)
			(if (= (length obj) 0)
				(push result (cat prefix (if is_last "└── " "├── ") "()"))
				(progn
					; Print opening paren with count
					(push result (cat prefix (if is_last "└── " "├── ")
						"( [" (str (length obj)) " items]"))

					; Print each child
					(defq new_prefix (cat prefix (if is_last "    " "│   ")))
					(each (lambda (item)
						(defq child_is_last (= (! ) (dec (length obj))))
						(each (lambda (line) (push result line))
							(tree-print item new_prefix child_is_last)))
						obj)

					; Print closing paren
					(push result (cat new_prefix "    )")))))

		((sym? obj)
			(push result (cat prefix (if is_last "└── " "├── ")
				"sym: " (str obj))))

		((num? obj)
			(push result (cat prefix (if is_last "└── " "├── ")
				"num: " (str obj))))

		((str? obj)
			(push result (cat prefix (if is_last "└── " "├── ")
				"str: \"" obj "\"")))

		((func? obj)
			(push result (cat prefix (if is_last "└── " "├── ")
				"func: " (str obj))))

		(:t
			(push result (cat prefix (if is_last "└── " "├── ")
				(str obj)))))

	result)

(defun safe-read (code_str)
	; Safely parse code from string
	(catch (progn
		(defq stream (string-stream code_str))
		(defq result (read stream))
		result)
		(cat "Parse Error: " (str _))))

(defun safe-macroexpand (form)
	; Safely expand macros
	(catch (macroexpand form)
		(cat "Expand Error: " (str _))))

(defun expand-one-level (form env)
	; Expand only the top-level macro, not recursively
	(if (and (list? form) (nempty? form))
		(progn
			(defq head (first form))
			(if (and (sym? head) (defq macro (. env :find head)))
				(if (and (lambda? macro) (get :macro macro))
					; It's a macro, expand it
					(catch (apply macro (rest form)) form)
					; Not a macro
					form)
				; Can't find symbol or not a symbol
				form))
		; Not a list
		form))

(defun safe-prebind (form)
	; Safely pre-bind symbols
	(catch (prebind form)
		(cat "Prebind Error: " (str _))))

(defun analyze-bindings (original bound)
	; Analyze what symbols were bound to addresses
	(defq bindings (list))
	(when (and (list? original) (list? bound))
		(when (and (nempty? original) (nempty? bound))
			(defq orig_head (first original)
				  bound_head (first bound))
			; Check if head symbol was bound
			(when (and (sym? orig_head) (func? bound_head))
				(push bindings (list (str orig_head) (str bound_head))))
			; Recursively check rest
			(each (lambda (o b)
				(when (and (list? o) (list? b))
					(each (lambda (binding) (push bindings binding))
						(analyze-bindings o b))))
				(rest original) (rest bound))))
	bindings)

(defun safe-eval (form)
	; Safely evaluate form
	(catch (eval form)
		(cat "Eval Error: " (str _))))

(defun pretty-print (obj &optional indent)
	; Pretty print with indentation for readability
	(setd indent 0)
	(defq prefix (apply cat (map (lambda (_) "  ") (range 0 indent))))
	(cond
		((str? obj) obj)
		((or (num? obj) (sym? obj)) (str obj))
		((list? obj)
			(if (= (length obj) 0) "()"
				(if (< (length obj) 4)
					; Short lists on one line
					(progn
						(defq s (string-stream ""))
						(print obj s)
						(str s))
					; Long lists with indentation
					(progn
						(defq parts (list "("))
						(each (lambda (item)
							(if (= (! ) 0)
								(push parts (pretty-print item (inc indent)))
								(push parts (cat "\n" prefix "  " (pretty-print item (inc indent))))))
							obj)
						(push parts ")")
						(apply cat parts)))))
		(:t (str obj))))

(defun save-to-history ()
	; Save current state to history
	(defq entry (list
		(get :text *input_field*)
		(get :text *input_field2*)
		*compare_mode*
		(get :text *read_output*)
		(get :text *expand_output*)
		(get :text *bind_output*)
		(get :text *eval_output*)))

	; If we're not at the end of history, truncate forward history
	(when (< *history_index* (dec (length *history*)))
		(setq *history* (slice *history* 0 (inc *history_index*))))

	; Add to history
	(push *history* entry)
	(setq *history_index* (dec (length *history*)))

	; Limit history size
	(when (> (length *history*) *max_history*)
		(setq *history* (slice *history* 1 (length *history*)))
		(setq *history_index* (dec (length *history*))))

	; Update history label
	(update-history-label))

(defun load-from-history (idx)
	; Load a history entry and display it
	(when (and (>= idx 0) (< idx (length *history*)))
		(defq entry (elem-get *history* idx))
		(setq *history_index* idx)

		; Restore state
		(set *input_field* :text (elem-get entry 0))
		(set *input_field2* :text (elem-get entry 1))
		(setq *compare_mode* (elem-get entry 2))
		(set *read_output* :text (elem-get entry 3))
		(set *expand_output* :text (elem-get entry 4))
		(set *bind_output* :text (elem-get entry 5))
		(set *eval_output* :text (elem-get entry 6))

		; Update UI visibility for compare mode
		(set *compare_toggle_btn* :text (if *compare_mode* "Compare: ON" "Compare: OFF"))
		(def *input2_section* :visible *compare_mode*)

		; Update layout
		(.-> *input_field* :layout :dirty)
		(.-> *input_field2* :layout :dirty)
		(.-> *compare_toggle_btn* :layout :dirty)
		(.-> *input2_section* :layout :dirty)
		(.-> *read_output* :layout :dirty)
		(.-> *expand_output* :layout :dirty)
		(.-> *bind_output* :layout :dirty)
		(.-> *eval_output* :layout :dirty)

		; Update history label
		(update-history-label)))

(defun history-prev ()
	; Navigate to previous history entry
	(when (> *history_index* 0)
		(load-from-history (dec *history_index*))))

(defun history-next ()
	; Navigate to next history entry
	(when (< *history_index* (dec (length *history*)))
		(load-from-history (inc *history_index*))))

(defun clear-history ()
	; Clear all history
	(setq *history* (list) *history_index* -1)
	(update-history-label))

(defun update-history-label ()
	; Update the history position label
	(if (empty? *history*)
		(set *history_label* :text "History: empty")
		(set *history_label* :text
			(cat "History: " (str (inc *history_index*)) "/" (str (length *history*)))))
	(.-> *history_label* :layout :dirty))

(defun save-session ()
	; Save complete session to file
	(defq timestamp (str (time))
		  filename (cat *env_home* "code-walker-session-" timestamp ".cws"))

	(when (defq stream (file-stream filename +file_open_write))
		; Write header
		(print "CODE_WALKER_SESSION_V1" stream)
		(print "" stream)

		; Write toggle states
		(print "[TOGGLES]" stream)
		(print (if *show_diffs* ":t" ":nil") stream)
		(print (if *show_tree* ":t" ":nil") stream)
		(print (if *compare_mode* ":t" ":nil") stream)
		(print "" stream)

		; Write current inputs
		(print "[CURRENT_INPUT]" stream)
		(print (get :text *input_field*) stream)
		(print (get :text *input_field2*) stream)
		(print "" stream)

		; Write current outputs
		(print "[CURRENT_OUTPUT]" stream)
		(print (get :text *read_output*) stream)
		(print "---PHASE---" stream)
		(print (get :text *expand_output*) stream)
		(print "---PHASE---" stream)
		(print (get :text *bind_output*) stream)
		(print "---PHASE---" stream)
		(print (get :text *eval_output*) stream)
		(print "" stream)

		; Write history
		(print "[HISTORY]" stream)
		(print (str (length *history*)) stream)
		(print (str *history_index*) stream)
		(each (lambda (entry)
			(print "---ENTRY---" stream)
			(each (lambda (item)
				(print item stream)
				(print "---FIELD---" stream))
				entry))
			*history*)

		(close stream)

		; Show success message
		(defq old_text (get :text *eval_output*))
		(set *eval_output* :text (cat "Session saved to: " filename "\n\n" old_text))
		(.-> *eval_output* :layout :dirty)))

(defun load-session ()
	; Load session from file - presents file browser
	; For now, use a simple filename prompt approach
	; In a full implementation, would use file browser UI
	(defq filename (cat *env_home* "code-walker-session-latest.cws"))

	(when (defq stream (file-stream filename +file_open_read))
		(catch
			(progn
				; Read header
				(defq header (read-line stream))
				(unless (eql header "CODE_WALKER_SESSION_V1")
					(throw "Invalid session file format"))

				(read-line stream) ; blank line

				; Read toggles
				(defq section (read-line stream))
				(unless (eql section "[TOGGLES]")
					(throw "Expected [TOGGLES] section"))
				(setq *show_diffs* (eql (read-line stream) ":t"))
				(setq *show_tree* (eql (read-line stream) ":t"))
				(setq *compare_mode* (eql (read-line stream) ":t"))
				(read-line stream) ; blank line

				; Update toggle buttons
				(set *diff_toggle_btn* :text (if *show_diffs* "Diffs: ON" "Diffs: OFF"))
				(set *tree_toggle_btn* :text (if *show_tree* "Tree: ON" "Tree: OFF"))
				(set *compare_toggle_btn* :text (if *compare_mode* "Compare: ON" "Compare: OFF"))

				; Read current input
				(setq section (read-line stream))
				(unless (eql section "[CURRENT_INPUT]")
					(throw "Expected [CURRENT_INPUT] section"))
				(set *input_field* :text (read-line stream))
				(set *input_field2* :text (read-line stream))
				(read-line stream) ; blank line

				; Read current output
				(setq section (read-line stream))
				(unless (eql section "[CURRENT_OUTPUT]")
					(throw "Expected [CURRENT_OUTPUT] section"))

				; Helper to read until separator
				(defq read-until-sep (lambda (sep)
					(defq lines (list))
					(defq line (read-line stream))
					(while (and line (not (eql line sep)))
						(push lines line)
						(setq line (read-line stream)))
					(apply cat (map (lambda (l) (cat l "\n")) lines))))

				(set *read_output* :text (read-until-sep "---PHASE---"))
				(set *expand_output* :text (read-until-sep "---PHASE---"))
				(set *bind_output* :text (read-until-sep "---PHASE---"))
				(set *eval_output* :text (read-until-sep ""))

				; Read history
				(setq section (read-line stream))
				(unless (eql section "[HISTORY]")
					(throw "Expected [HISTORY] section"))

				(defq hist_count (num (read-line stream)))
				(setq *history_index* (num (read-line stream)))
				(setq *history* (list))

				(each (lambda (_)
					(defq sep (read-line stream))
					(unless (eql sep "---ENTRY---")
						(throw "Expected ---ENTRY---"))

					(defq entry (list))
					(each (lambda (_)
						(defq lines (list))
						(defq line (read-line stream))
						(while (and line (not (eql line "---FIELD---")))
							(push lines line)
							(setq line (read-line stream)))
						(push entry (apply cat (map (lambda (l) (cat l "\n")) lines))))
						(range 0 7))
					(push *history* entry))
					(range 0 hist_count))

				(close stream)

				; Update UI
				(def *input2_section* :visible *compare_mode*)
				(update-history-label)
				(.-> *diff_toggle_btn* :layout :dirty)
				(.-> *tree_toggle_btn* :layout :dirty)
				(.-> *compare_toggle_btn* :layout :dirty)
				(.-> *input_field* :layout :dirty)
				(.-> *input_field2* :layout :dirty)
				(.-> *input2_section* :layout :dirty)
				(.-> *read_output* :layout :dirty)
				(.-> *expand_output* :layout :dirty)
				(.-> *bind_output* :layout :dirty)
				(.-> *eval_output* :layout :dirty)

				(set *eval_output* :text (cat "Session loaded from: " filename "\n\n" (get :text *eval_output*)))
				(.-> *eval_output* :layout :dirty))

			; Error handler
			(progn
				(close stream)
				(set *eval_output* :text (cat "Error loading session: " (str _)))
				(.-> *eval_output* :layout :dirty)))))

;;;;;;;;;;;;;;;;;;;;;
; UI Definition
;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* (:color +bg_color)
	(ui-title-bar _ "Code Walker / AST Explorer" (0xea19 0xea1b 0xea1a) +event_close)

	; Input section
	(ui-flow input_section (:flow_flags +flow_down_fill :color +section_color)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-label _ (:text "Expression A:" :font +font_title :color +header_color
				:border 0 :flow_flags +flow_flag_align_vleft))
			(ui-textfield *input_field* (:text "(defun add (a b) (+ a b))"
				:font +font_mono :color +argb_white :border 0 :flow_flags +flow_flag_align_vleft))
			(. *input_field* :connect +event_input_field))
		(ui-flow *input2_section* (:flow_flags +flow_right_fill :visible :nil)
			(ui-label _ (:text "Expression B:" :font +font_title :color +header_color
				:border 0 :flow_flags +flow_flag_align_vleft))
			(ui-textfield *input_field2* (:text "(defun mul (x y) (* x y))"
				:font +font_mono :color +argb_white :border 0 :flow_flags +flow_flag_align_vleft))
			(. *input_field2* :connect +event_input_field2)))

	; Buttons section
	(ui-flow button_section (:flow_flags +flow_right_fill :color +section_color)
		(ui-button process_btn (:text "Process All" :font +font_title))
		(ui-button step_btn (:text "Step Expand" :font +font_title))
		(ui-button export_btn (:text "Export" :font +font_title))
		(ui-button *diff_toggle_btn* (:text "Diffs: ON" :font +font_title))
		(ui-button *tree_toggle_btn* (:text "Tree: OFF" :font +font_title))
		(ui-button *compare_toggle_btn* (:text "Compare: OFF" :font +font_title))
		(ui-button clear_btn (:text "Clear" :font +font_title))
		(. process_btn :connect +event_process_btn)
		(. step_btn :connect +event_step_expand_btn)
		(. export_btn :connect +event_export_btn)
		(. *diff_toggle_btn* :connect +event_toggle_diff_btn)
		(. *tree_toggle_btn* :connect +event_toggle_tree_btn)
		(. *compare_toggle_btn* :connect +event_toggle_compare_btn)
		(. clear_btn :connect +event_clear_btn))

	; History navigation section
	(ui-flow history_section (:flow_flags +flow_right_fill :color +section_color)
		(ui-label *history_label* (:text "History: empty" :font +font_title :color +header_color :border 0))
		(ui-button history_prev_btn (:text "◄ Prev" :font +font_title))
		(ui-button history_next_btn (:text "Next ►" :font +font_title))
		(ui-button history_clear_btn (:text "Clear History" :font +font_title))
		(ui-button save_session_btn (:text "Save Session" :font +font_title))
		(ui-button load_session_btn (:text "Load Session" :font +font_title))
		(. history_prev_btn :connect +event_history_prev)
		(. history_next_btn :connect +event_history_next)
		(. history_clear_btn :connect +event_clear_history)
		(. save_session_btn :connect +event_save_session)
		(. load_session_btn :connect +event_load_session))

	; Example buttons section
	(ui-flow example_section (:flow_flags +flow_right_fill :color +section_color)
		(ui-label _ (:text "Examples:" :font +font_title :color +header_color :border 0))
		(ui-button ex1_btn (:text "defun" :font +font_mono))
		(ui-button ex2_btn (:text "let" :font +font_mono))
		(ui-button ex3_btn (:text "case" :font +font_mono))
		(ui-button ex4_btn (:text "ui-window" :font +font_mono))
		(. ex1_btn :connect +event_example1)
		(. ex2_btn :connect +event_example2)
		(. ex3_btn :connect +event_example3)
		(. ex4_btn :connect +event_example4))

	; Output sections
	(ui-scroll *output_scroll* +scroll_flag_vertical
		(:min_width 700 :min_height 500 :color +bg_color)
		(ui-flow output_flow (:flow_flags +flow_down_fill)

			; Phase 1: Read (Parse)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 1: READ (Parse to AST)"
					:font +font_title :color +argb_cyan :border 0))
				(ui-label *read_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *tree_read* (:text "" :font +font_mono
					:color +argb_cyan :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 2: Expand (Macro Expansion)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 2: EXPAND (Macro Expansion)"
					:font +font_title :color +argb_yellow :border 0))
				(ui-label *expand_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *diff_read_expand* (:text "" :font +font_mono
					:color +argb_grey6 :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *tree_expand* (:text "" :font +font_mono
					:color +argb_yellow :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 3: Bind (Pre-binding)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 3: BIND (Pre-binding)"
					:font +font_title :color +argb_green :border 0))
				(ui-label *bind_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *diff_expand_bind* (:text "" :font +font_mono
					:color +argb_grey6 :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *tree_bind* (:text "" :font +font_mono
					:color +argb_green :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 4: Eval (Optional - Result)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 4: EVAL (Result - Optional)"
					:font +font_title :color +argb_magenta :border 0))
				(ui-label *eval_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))))))

;;;;;;;;;;;;;;;;;;;;;
; Process Functions
;;;;;;;;;;;;;;;;;;;;;

(defq *expansion_steps* (list) *current_step* 0)
(defq *last_read* :nil *last_expand* :nil *last_bind* :nil)
(defq *last_read2* :nil *last_expand2* :nil *last_bind2* :nil)

(defun step-expand ()
	; Perform one step of macro expansion
	(defq code_str (get :text *input_field*))

	; If this is the first step, initialize
	(when (= *current_step* 0)
		(defq read_result (safe-read code_str))
		(if (str? read_result)
			(progn
				(set *read_output* :text read_result)
				(setq *expansion_steps* (list)))
			(progn
				(set *read_output* :text (pretty-print read_result))
				(setq *expansion_steps* (list read_result)))))

	; Try to expand one more level
	(when (nempty? *expansion_steps*)
		(defq current_form (last *expansion_steps*))
		(defq next_form (expand-one-level current_form (env)))

		; Check if expansion happened
		(if (eql current_form next_form)
			; No more expansion possible
			(set *expand_output* :text
				(cat (pretty-print next_form)
					 "\n\n[No more macros to expand - Step " (str *current_step*) "]"))
			; Expansion happened
			(progn
				(push *expansion_steps* next_form)
				(setq *current_step* (inc *current_step*))
				(set *expand_output* :text
					(cat (pretty-print next_form)
						 "\n\n[Expansion step " (str *current_step*) "]"))))

		; Update displays
		(.-> *read_output* :layout :dirty)
		(.-> *expand_output* :layout :dirty)))

(defun update-diffs ()
	; Update diff displays based on current state
	(when *show_diffs*
		; Diff: READ → EXPAND
		(when (and *last_read* *last_expand*)
			(set *diff_read_expand* :text
				(cat "\n[Diff: READ → EXPAND]\n"
					(compute-diff *last_read* *last_expand*)))
			(.-> *diff_read_expand* :layout :dirty))

		; Diff: EXPAND → BIND
		(when (and *last_expand* *last_bind*)
			(set *diff_expand_bind* :text
				(cat "\n[Diff: EXPAND → BIND]\n"
					(compute-diff *last_expand* *last_bind*)))
			(.-> *diff_expand_bind* :layout :dirty))))

(defun update-trees ()
	; Update tree visualizations based on current state
	(when *show_tree*
		; Tree for READ
		(when *last_read*
			(set *tree_read* :text
				(cat "\n[Tree Structure]\n"
					(apply cat (map (lambda (line) (cat line "\n"))
						(tree-print *last_read*)))))
			(.-> *tree_read* :layout :dirty))

		; Tree for EXPAND
		(when *last_expand*
			(set *tree_expand* :text
				(cat "\n[Tree Structure]\n"
					(apply cat (map (lambda (line) (cat line "\n"))
						(tree-print *last_expand*)))))
			(.-> *tree_expand* :layout :dirty))

		; Tree for BIND
		(when *last_bind*
			(set *tree_bind* :text
				(cat "\n[Tree Structure]\n"
					(apply cat (map (lambda (line) (cat line "\n"))
						(tree-print *last_bind*)))))
			(.-> *tree_bind* :layout :dirty))))

(defun process-code ()
	; Process the code through all phases
	(if *compare_mode*
		(process-compare)
		(process-single)))
(defun process-single ()
	; Process the code through all phases
	(defq code_str (get :text *input_field*))

	; Phase 1: Read
	(defq read_result (safe-read code_str))
	(if (str? read_result)
		; Error in reading
		(progn
			(set *read_output* :text read_result)
			(set *expand_output* :text "")
			(set *bind_output* :text "")
			(set *eval_output* :text "")
			(set *diff_read_expand* :text "")
			(set *diff_expand_bind* :text "")
			(setq *last_read* :nil *last_expand* :nil *last_bind* :nil))
		; Success - continue to next phases
		(progn
			(setq *last_read* read_result)
			(set *read_output* :text (pretty-print read_result))

			; Phase 2: Expand
			(defq expand_result (safe-macroexpand read_result))
			(if (str? expand_result)
				; Error in expansion
				(progn
					(set *expand_output* :text expand_result)
					(set *bind_output* :text "")
					(set *eval_output* :text "")
					(set *diff_read_expand* :text "")
					(set *diff_expand_bind* :text "")
					(setq *last_expand* :nil *last_bind* :nil))
				; Success - continue to next phases
				(progn
					(setq *last_expand* expand_result)
					(set *expand_output* :text (pretty-print expand_result))

					; Phase 3: Bind
					(defq bind_result (safe-prebind expand_result))
					(if (str? bind_result)
						; Error in binding
						(progn
							(set *bind_output* :text bind_result)
							(set *eval_output* :text "")
							(set *diff_expand_bind* :text "")
							(setq *last_bind* :nil))
						; Success - continue to eval
						(progn
							(setq *last_bind* bind_result)

							; Analyze what symbols were bound
							(defq bindings (analyze-bindings expand_result bind_result))
							(defq binding_info
								(if (nempty? bindings)
									(cat "\n\n[Pre-bound symbols (O(1) optimization):]\n"
										(apply cat (map (lambda (b)
											(cat "  " (first b) " -> " (second b) "\n"))
											bindings)))
									"\n\n[No symbols were pre-bound]"))

							(set *bind_output* :text
								(cat (pretty-print bind_result) binding_info))

							; Phase 4: Eval (optional - only for safe expressions)
							(defq eval_result (safe-eval bind_result))
							(set *eval_output* :text (pretty-print eval_result))))))))

	; Update diffs and trees if enabled
	(update-diffs)
	(update-trees)

	; Update layout
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty)
	(.-> *output_scroll* :layout :dirty)

	; Save to history
	(save-to-history))

(defun process-compare ()
	; Process two expressions for comparison
	(defq code_str1 (get :text *input_field*)
		  code_str2 (get :text *input_field2*))

	; Process expression A
	(defq read1 (safe-read code_str1)
		  expand1 (if (str? read1) :nil (safe-macroexpand read1))
		  bind1 (if (str? expand1) :nil (safe-prebind expand1)))

	; Process expression B
	(defq read2 (safe-read code_str2)
		  expand2 (if (str? read2) :nil (safe-macroexpand read2))
		  bind2 (if (str? expand2) :nil (safe-prebind expand2)))

	; Store for diffs/trees
	(setq *last_read* read1 *last_expand* expand1 *last_bind* bind1)
	(setq *last_read2* read2 *last_expand2* expand2 *last_bind2* bind2)

	; Format comparison output
	(set *read_output* :text
		(cat ">>> Expression A <<<\n"
			(if (str? read1) read1 (pretty-print read1))
			"\n\n>>> Expression B <<<\n"
			(if (str? read2) read2 (pretty-print read2))))

	(set *expand_output* :text
		(cat ">>> Expression A <<<\n"
			(if (str? expand1) expand1 (pretty-print expand1))
			"\n\n>>> Expression B <<<\n"
			(if (str? expand2) expand2 (pretty-print expand2))))

	(set *bind_output* :text
		(cat ">>> Expression A <<<\n"
			(if (str? bind1) bind1 (pretty-print bind1))
			"\n\n>>> Expression B <<<\n"
			(if (str? bind2) bind2 (pretty-print bind2))))

	; Evaluate both
	(defq eval1 (if bind1 (safe-eval bind1) :nil)
		  eval2 (if bind2 (safe-eval bind2) :nil))

	(set *eval_output* :text
		(cat ">>> Expression A <<<\n"
			(pretty-print eval1)
			"\n\n>>> Expression B <<<\n"
			(pretty-print eval2)))

	; Update diffs and trees if enabled
	(update-diffs)
	(update-trees)

	; Update layout
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty)
	(.-> *output_scroll* :layout :dirty)

	; Save to history
	(save-to-history))

(defun toggle-diffs ()
	; Toggle diff display on/off
	(setq *show_diffs* (not *show_diffs*))
	(set *diff_toggle_btn* :text (if *show_diffs* "Diffs: ON" "Diffs: OFF"))
	(.-> *diff_toggle_btn* :layout :dirty)

	(if *show_diffs*
		(update-diffs)
		(progn
			(set *diff_read_expand* :text "")
			(set *diff_expand_bind* :text "")
			(.-> *diff_read_expand* :layout :dirty)
			(.-> *diff_expand_bind* :layout :dirty))))

(defun toggle-tree ()
	; Toggle tree visualization on/off
	(setq *show_tree* (not *show_tree*))
	(set *tree_toggle_btn* :text (if *show_tree* "Tree: ON" "Tree: OFF"))
	(.-> *tree_toggle_btn* :layout :dirty)

	(if *show_tree*
		(update-trees)
		(progn
			(set *tree_read* :text "")
			(set *tree_expand* :text "")
			(set *tree_bind* :text "")
			(.-> *tree_read* :layout :dirty)
			(.-> *tree_expand* :layout :dirty)
			(.-> *tree_bind* :layout :dirty))))

(defun toggle-compare ()
	; Toggle comparison mode on/off
	(setq *compare_mode* (not *compare_mode*))
	(set *compare_toggle_btn* :text (if *compare_mode* "Compare: ON" "Compare: OFF"))
	(.-> *compare_toggle_btn* :layout :dirty)

	; Show/hide second input field
	(def *input2_section* :visible *compare_mode*)
	(.-> *input2_section* :layout :dirty)

	; Reprocess with new mode
	(process-code))

(defun clear-all ()
	; Clear all output fields and history
	(set *input_field* :text "")
	(set *input_field2* :text "")
	(set *read_output* :text "")
	(set *expand_output* :text "")
	(set *bind_output* :text "")
	(set *eval_output* :text "")
	(set *diff_read_expand* :text "")
	(set *diff_expand_bind* :text "")
	(set *tree_read* :text "")
	(set *tree_expand* :text "")
	(set *tree_bind* :text "")
	(setq *expansion_steps* (list) *current_step* 0)
	(setq *last_read* :nil *last_expand* :nil *last_bind* :nil)
	(setq *last_read2* :nil *last_expand2* :nil *last_bind2* :nil)
	(clear-history)
	(.-> *input_field* :layout :dirty)
	(.-> *input_field2* :layout :dirty)
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty)
	(.-> *diff_read_expand* :layout :dirty)
	(.-> *diff_expand_bind* :layout :dirty)
	(.-> *tree_read* :layout :dirty)
	(.-> *tree_expand* :layout :dirty)
	(.-> *tree_bind* :layout :dirty))

(defun load-example (code)
	; Load example code into input field and process it
	(setq *expansion_steps* (list) *current_step* 0)
	(set *input_field* :text code)
	(.-> *input_field* :layout :dirty)
	(process-code))

(defun export-results ()
	; Export all phase results to a file
	(defq timestamp (str (time))
		  filename (cat *env_home* "code-walker-" timestamp ".txt"))
	(when (defq stream (file-stream filename +file_open_write))
		(print "Code Walker / AST Explorer Export" stream)
		(print (cat "Timestamp: " timestamp) stream)
		(print "=" stream)
		(print "" stream)
		(print "Input Code:" stream)
		(print (get :text *input_field*) stream)
		(print "" stream)
		(print "Phase 1: READ (Parse to AST)" stream)
		(print "----------------------------" stream)
		(print (get :text *read_output*) stream)
		(print "" stream)
		(print "Phase 2: EXPAND (Macro Expansion)" stream)
		(print "----------------------------------" stream)
		(print (get :text *expand_output*) stream)
		(print "" stream)
		(print "Phase 3: BIND (Pre-binding)" stream)
		(print "----------------------------" stream)
		(print (get :text *bind_output*) stream)
		(print "" stream)
		(print "Phase 4: EVAL (Result)" stream)
		(print "----------------------" stream)
		(print (get :text *eval_output*) stream)
		(print "" stream)
		(close stream)
		; Show success message in eval output temporarily
		(defq old_text (get :text *eval_output*))
		(set *eval_output* :text (cat "Exported to: " filename "\n\n" old_text))
		(.-> *eval_output* :layout :dirty)))

;;;;;;;;;;;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	(defq select (task-mboxes +select_size) running :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; Process initial code on startup
	(process-code)

	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			  id (getf msg +ev_msg_target_id))

		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			(+select_main
				(cond
					((= id +event_close) (setq running :nil))
					((= id +event_min)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))
					((= id +event_max)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) '(800 600))))
						(. *window* :change_dirty x y w h))
					((= id +event_process_btn) (process-code))
					((= id +event_step_expand_btn) (step-expand))
					((= id +event_export_btn) (export-results))
					((= id +event_toggle_diff_btn) (toggle-diffs))
					((= id +event_toggle_tree_btn) (toggle-tree))
					((= id +event_toggle_compare_btn) (toggle-compare))
					((= id +event_clear_btn) (clear-all))
					((= id +event_history_prev) (history-prev))
					((= id +event_history_next) (history-next))
					((= id +event_clear_history) (clear-history))
					((= id +event_save_session) (save-session))
					((= id +event_load_session) (load-session))
					((= id +event_example1)
						(load-example "(defun add (a b) (+ a b))"))
					((= id +event_example2)
						(load-example "(let ((x 10) (y 20)) (+ x y))"))
					((= id +event_example3)
						(load-example "(case 2 (0 :zero) (1 :one) (2 :two) (:t :other))"))
					((= id +event_example4)
						(load-example "(ui-window w () (ui-label _ (:text \"Hello\")))"))
					((and (= id +event_input_field)
						  (= (getf msg +ev_msg_type) +ev_type_key_down)
						  (= (getf msg +ev_msg_key_key) +char_lf))
						; Process on Enter key in input field
						(process-code))
					(:t (. *window* :event msg))))))

	(gui-sub-rpc *window*)
	(profile-report "Code Walker"))
