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
	(enum process_btn clear_btn)
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

(defun safe-prebind (form)
	; Safely pre-bind symbols
	(catch (prebind form)
		(cat "Prebind Error: " (str _))))

(defun safe-eval (form)
	; Safely evaluate form
	(catch (eval form)
		(cat "Eval Error: " (str _))))

(defun pretty-print (obj)
	; Pretty print any Lisp object by converting to string
	(defq s (string-stream ""))
	(print obj s)
	(str s))

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
		(ui-button process_btn (:text "Process" :font +font_title))
		(ui-button clear_btn (:text "Clear" :font +font_title))
		(. process_btn :connect +event_process_btn)
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
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 3: Bind (Pre-binding)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 3: BIND (Pre-binding)"
					:font +font_title :color +argb_green :border 0))
				(ui-label *bind_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text)))

			; Phase 4: Eval (Optional - Result)
			(ui-flow _ (:flow_flags +flow_down_fill :color +section_color :border 5)
				(ui-label _ (:text "Phase 4: EVAL (Result - Optional)"
					:font +font_title :color +argb_magenta :border 0))
				(ui-label *eval_output* (:text "" :font +font_mono
					:color +argb_white :border 0 :flow_flags +flow_flag_wrap_text))))))

;;;;;;;;;;;;;;;;;;;;;
; Process Functions
;;;;;;;;;;;;;;;;;;;;;

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
			(set *eval_output* :text ""))
		; Success - continue to next phases
		(progn
			(set *read_output* :text (pretty-print read_result))

			; Phase 2: Expand
			(defq expand_result (safe-macroexpand read_result))
			(if (str? expand_result)
				; Error in expansion
				(progn
					(set *expand_output* :text expand_result)
					(set *bind_output* :text "")
					(set *eval_output* :text ""))
				; Success - continue to next phases
				(progn
					(set *expand_output* :text (pretty-print expand_result))

					; Phase 3: Bind
					(defq bind_result (safe-prebind expand_result))
					(if (str? bind_result)
						; Error in binding
						(progn
							(set *bind_output* :text bind_result)
							(set *eval_output* :text ""))
						; Success - continue to eval
						(progn
							; For bind phase, we need to show that functions are now pointers
							; We'll show a representation that indicates binding happened
							(set *bind_output* :text
								(cat (pretty-print bind_result)
									"\n\n[Functions and symbols are now pre-bound to memory addresses]"))

							; Phase 4: Eval (optional - only for safe expressions)
							(defq eval_result (safe-eval bind_result))
							(set *eval_output* :text (pretty-print eval_result))))))))

	; Update layout
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty)
	(.-> *output_scroll* :layout :dirty))

(defun clear-all ()
	; Clear all output fields
	(set *input_field* :text "")
	(set *read_output* :text "")
	(set *expand_output* :text "")
	(set *bind_output* :text "")
	(set *eval_output* :text "")
	(.-> *input_field* :layout :dirty)
	(.-> *read_output* :layout :dirty)
	(.-> *expand_output* :layout :dirty)
	(.-> *bind_output* :layout :dirty)
	(.-> *eval_output* :layout :dirty))

(defun load-example (code)
	; Load example code into input field and process it
	(set *input_field* :text code)
	(.-> *input_field* :layout :dirty)
	(process-code))

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
