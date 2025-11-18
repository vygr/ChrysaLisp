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
	(enum process_btn clear_btn step_expand_btn export_btn toggle_diff_btn)
	(enum input_field)
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

(defq *show_diffs* :t)

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

;;;;;;;;;;;;;;;;;;;;;
; UI Definition
;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* (:color +bg_color)
	(ui-title-bar _ "Code Walker / AST Explorer" (0xea19 0xea1b 0xea1a) +event_close)

	; Input section
	(ui-flow input_section (:flow_flags +flow_right_fill :color +section_color)
		(ui-label _ (:text "Enter Lisp Code:" :font +font_title :color +header_color
			:border 0 :flow_flags +flow_flag_align_vleft))
		(ui-textfield *input_field* (:text "(defun add (a b) (+ a b))"
			:font +font_mono :color +argb_white :border 0 :flow_flags +flow_flag_align_vleft))
		(. *input_field* :connect +event_input_field))

	; Buttons section
	(ui-flow button_section (:flow_flags +flow_right_fill :color +section_color)
		(ui-button process_btn (:text "Process All" :font +font_title))
		(ui-button step_btn (:text "Step Expand" :font +font_title))
		(ui-button export_btn (:text "Export" :font +font_title))
		(ui-button *diff_toggle_btn* (:text "Diffs: ON" :font +font_title))
		(ui-button clear_btn (:text "Clear" :font +font_title))
		(. process_btn :connect +event_process_btn)
		(. step_btn :connect +event_step_expand_btn)
		(. export_btn :connect +event_export_btn)
		(. *diff_toggle_btn* :connect +event_toggle_diff_btn)
		(. clear_btn :connect +event_clear_btn))

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
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 2: Expand (Macro Expansion)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 2: EXPAND (Macro Expansion)"
					:font +font_title :color +argb_yellow :border 0))
				(ui-label *expand_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *diff_read_expand* (:text "" :font +font_mono
					:color +argb_grey6 :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 3: Bind (Pre-binding)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 3: BIND (Pre-binding)"
					:font +font_title :color +argb_green :border 0))
				(ui-label *bind_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))
				(ui-label *diff_expand_bind* (:text "" :font +font_mono
					:color +argb_grey6 :border 0 :flow_flags +flow_flag_wrap_text)))

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

(defun process-code ()
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

	; Update diffs if enabled
	(update-diffs)

	; Update layout
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty)
	(.-> *output_scroll* :layout :dirty))

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

(defun clear-all ()
	; Clear all output fields
	(set *input_field* :text "")
	(set *read_output* :text "")
	(set *expand_output* :text "")
	(set *bind_output* :text "")
	(set *eval_output* :text "")
	(set *diff_read_expand* :text "")
	(set *diff_expand_bind* :text "")
	(setq *expansion_steps* (list) *current_step* 0)
	(setq *last_read* :nil *last_expand* :nil *last_bind* :nil)
	(.-> *input_field* :layout :dirty)
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty)
	(.-> *diff_read_expand* :layout :dirty)
	(.-> *diff_expand_bind* :layout :dirty))

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
					((= id +event_clear_btn) (clear-all))
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
