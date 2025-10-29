(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/consts/scodes.inc")

(enums +event 0
	(enum close max min)
	(enum base_change)
	(enum button))

; Enums for the mail select loop
(enums +select 0
	(enum main tip))

; Use enums to define named indices for our state list for clarity.
(enums +state 0
	(enum accum num base lastop error_state new_entry))

; Pre-define lists of buttons to make UI updates easier.
(defq hex_buttons (list))
(defq other_base_buttons (list))
(defq +operators ''("=" "+" "-" "*" "/" "%" "AND" "OR" "XOR"))
(defq +disabled_color +argb_grey4)
(defq +disabled_ink_color *env_hint_col*)
(defq +digit_list (static-q (map identity "0123456789ABCDEF")))

; A map for direct keyboard scancode to operation mapping.
(defq key_map (scatter (Fmap)
	+sc_0 "0" +sc_1 "1" +sc_2 "2" +sc_3 "3" +sc_4 "4"
	+sc_5 "5" +sc_6 "6" +sc_7 "7" +sc_8 "8" +sc_9 "9"
	+sc_a "A" +sc_b "B" +sc_c "C" +sc_d "D" +sc_e "E" +sc_f "F"
	+sc_kp_0 "0" +sc_kp_1 "1" +sc_kp_2 "2" +sc_kp_3 "3" +sc_kp_4 "4"
	+sc_kp_5 "5" +sc_kp_6 "6" +sc_kp_7 "7" +sc_kp_8 "8" +sc_kp_9 "9"
	+sc_kp_divide "/" +sc_kp_multiply "*" +sc_kp_minus "-" +sc_kp_plus "+"
	+sc_kp_enter "="
	+sc_slash "/" +sc_minus "-" +sc_equals "="
	+sc_return "=" +sc_escape "AC" +sc_backspace "BACK" +sc_delete "CE"))

; A map for shifted keyboard scancodes.
(defq shift_key_map (scatter (Fmap)
	+sc_equals "+"      ; Shift + =
	+sc_8 "*"           ; Shift + 8
    +sc_5 "%"           ; Shift + 5
	+sc_7 "AND"         ; Shift + 7 (&)
	+sc_6 "XOR"         ; Shift + 6 (^)
	+sc_backslash "OR"  ; Shift + \ (|) - Varies by layout
	+sc_grave "NOT"     ; Shift + ` (~) - Varies by layout
	+sc_comma "<<"      ; Shift + , (<)
	+sc_period ">>"))   ; Shift + . (>)

(ui-window *window* ()
	(ui-title-bar _ "Calculator" (0xea19 0xea1b 0xea1a) +event_close)
	(. (ui-radio-bar base_bar ("DEC" "HEX" "BIN" "OCT")
			(:color (const *env_toolbar2_col*)
			 :font (const *env_window_font*)))
		:connect +event_base_change)
	(ui-label *display* (:text "0" :color +argb_white :flow_flags +flow_flag_align_hright
		:font (create-font "fonts/OpenSans-Regular.ctf" 24)))
	(ui-grid button_grid (:grid_width 4 :grid_height 8 :color *env_toolbar_col*
			:font (create-font "fonts/OpenSans-Regular.ctf" 28))
		(each (lambda (text)
			(defq button (ui-button _ (:text text)))
			(. button :connect +event_button)
			; Group buttons for easy enabling/disabling
			(cond
				((find text '("A" "B" "C" "D" "E" "F")) (push hex_buttons button))
				((find text '("2" "3" "4" "5" "6" "7" "8" "9")) (push other_base_buttons button))))
			'("AND" "OR"  "XOR" "NOT"
			  ">>>" ">>"  "<<"  "NEG"
			  "D"   "E"   "F"   "%"
			  "A"   "B"   "C"   "/"
			  "7"   "8"   "9"   "*"
			  "4"   "5"   "6"   "-"
			  "1"   "2"   "3"   "+"
			  "0"  "BACK" "AC"  "=" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-calculator-state (base)
	; State list: '(accum num base lastop error_state new_entry)
	(list 0 0 base :nil :nil :t))

(defun num-to-base-str (n base chars)
	(if (= n 0) "0"
		(progn
			(defq result "" temp "")
			(when (< n 0) (setq result "-" n (abs n)))
			(while (> n 0)
				(setq temp (cat (elem-get chars (% n base)) temp)
					  n (/ n base)))
			(cat result temp))))

(defun format-number (num base)
	(case base
		(16 (num-to-base-str num 16 "0123456789ABCDEF"))
		(2  (num-to-base-str num 2 "01"))
		(8  (num-to-base-str num 8 "01234567"))
		(:t (str num))))

(defun do_op (accum num op)
	(case op
		("+" (+ accum num))
		("-" (- accum num))
		("*" (* accum num))
		("/" (if (/= num 0) (/ accum num) :error))
		("%" (if (/= num 0) (% accum num) :error))
		("AND" (logand accum num))
		("OR"  (logior accum num))
		("XOR" (logxor accum num))
		(:t num)))

(defun update-display (state)
    (bind '(accum num base _ _ new_entry) state)
    (defq display_num (if new_entry accum num))
	(if (elem-get state +state_error_state)
		(set *display* :text "Error")
		(set *display* :text (format-number display_num base)))
	(.-> *display* :layout :dirty))

(defun update-button-states (base)
    ; Hex-specific buttons (A-F) are only enabled for HEX base.
	(each (lambda (button)
		(defq is_enabled (= base 16))
		(def (. button :dirty) :disabled (not is_enabled)
			:color (if is_enabled *env_toolbar_col* +disabled_color)
			:ink_color (if is_enabled *env_ink_col* +disabled_ink_color)))
		hex_buttons)

    ; Digit buttons (2-9) are enabled if their value is less than the current base.
	(each (lambda (button)
		(defq button_val (str-as-num (get :text button)))
		(defq is_enabled (< button_val base))
		(def (. button :dirty) :disabled (not is_enabled)
			:color (if is_enabled *env_toolbar_col* +disabled_color)
			:ink_color (if is_enabled *env_ink_col* +disabled_ink_color)))
		other_base_buttons))

(defun key-to-op (key mod base)
	(defq op
		(if (bits? mod +ev_key_mod_shift)
			(. shift_key_map :find key)
			(. key_map :find key)))
	; Validate the operation, especially digits, against the current base
	(if op
		(if (defq digit (find op +digit_list))
			(if (< digit base) op)
			op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Core Logic Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-input (state op)
	(bind '(accum num base lastop error_state new_entry) state)
	(if (and error_state (not (eql op "AC")))
		state ; If in error state, only AC can do something
		(progn
			(defq digit (find op "0123456789ABCDEF"))
			(cond
				((eql op "AC")   (create-calculator-state base))
				((eql op "CE")   (list accum 0 base lastop error_state :t)) ; This is now unused but kept for key mapping
				((eql op "NOT")  (list accum (lognot num) base lastop error_state new_entry))
				((eql op "BACK") (list accum (if (< num 0) (neg (/ (abs num) base)) (/ num base)) base lastop error_state new_entry))
				((eql op "NEG")  (list accum (neg num) base lastop error_state new_entry))
				((eql op "<<")   (list accum (<< num 1) base lastop error_state new_entry))
				((eql op ">>")   (list accum (>> num 1) base lastop error_state new_entry))
				((eql op ">>>")  (list accum (>>> num 1) base lastop error_state new_entry))
				((find op +operators)
					(if (eql op "=")
                        ; Handle equals
                        (let ((result (do_op accum num (ifn lastop "+"))))
                            (if (eql result :error)
                                (list accum num base lastop :t :t)
                                (list result num base lastop :nil :t)))
                        ; Handle other operators
                        (if new_entry
                            (list accum num base op :nil :t) ; e.g. 5 * + -> changes op to +
                            (let ((result (do_op accum num lastop)))
                                (if (eql result :error)
                                    (list accum num base lastop :t :t)
                                    (list result result base op :nil :t))))))
				((and digit (< digit base))
					(if new_entry
						(list accum digit base lastop :nil :nil)
						(list accum (+ (* num base) digit) base lastop :nil :nil)))
				(:t state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(defq select (task-mboxes +select_size)
		  state (create-calculator-state 10)
		  running :t)

	; Setup tooltips
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips base_bar '("decimal" "hexadecimal" "binary" "octal"))

	(update-button-states (elem-get state +state_base))
	(. base_bar :set_selected 0)

	(while running
		(defq old_state state ; Keep a reference to the state before processing
			  msg (mail-read (elem-get select (defq idx (mail-select select))))
			  id (getf msg +ev_msg_target_id)
			  op :nil)

		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			(+select_main
				; Centralized event-to-op mapping
				(cond
					((and id (>= id +event_button)) ; GUI button click
						(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
						(unless (get :disabled button)
							(setq op (get :text button))))
					((= (getf msg +ev_msg_type) +ev_type_key_down)
						(setq op (key-to-op (getf msg +ev_msg_key_scode)
											(getf msg +ev_msg_key_mod)
											(elem-get state +state_base)))))

				; Process events and update state
				(cond
					((= id +event_close) (setq running :nil))
					((= id +event_min)
						(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))
					((= id +event_max)
						(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) '(512 512))))
						(. *window* :change_dirty x y w h))
					((= id +event_base_change)
						(bind '(accum num _ lastop error_state new_entry) state)
						(defq new_base (elem-get '(10 16 2 8) (. base_bar :get_selected)))
						(setq state (list accum num new_base lastop error_state new_entry)))
					(op (setq state (handle-input state op)))
					(:t (. *window* :event msg)))))

		; Only update UI if the state has actually changed.
		(unless (every (const eql) state old_state)
			(update-display state)
			; Only update button enabled/disabled states if the base changed.
			(unless (= (elem-get state +state_base) (elem-get old_state +state_base))
				(update-button-states (elem-get state +state_base)))))
	(gui-sub-rpc *window*))