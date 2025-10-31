;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/calculator/app.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;
(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/consts/scodes.inc")

(enums +event 0
	(enum close max min)
	(enum base_change)
	(enum button))

(enums +select 0
	(enum main tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; State and Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Enums for our state list for clarity. The operand_stack holds numbers,
; and the operator_stack holds operators, forming the basis of our expression evaluation.
(enums +state 0
	(enum operand_stack operator_stack current_number base memory error_state new_entry))

; Operator precedence map. Higher numbers are evaluated first.
(defq +precedence (scatter (Fmap)
    "*" 2 "/" 2 "%" 2
    "+" 1 "-" 1
    "AND" 1 "OR" 1 "XOR" 1))

; Pre-define lists of buttons to make UI updates easier.
(defq hex_buttons (list))
(defq other_base_buttons (list))
(defq +operators ''("=" "+" "-" "*" "/" "%" "AND" "OR" "XOR"))
(defq +disabled_color +argb_grey4)
(defq +disabled_ink_color *env_hint_col*)
(defq +digit_list (static-q (map identity "0123456789ABCDEF")))
(defq +calculator_font (create-font "fonts/Hack-Regular.ctf" 22))

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
			(:color (const *env_toolbar2_col*) :font +calculator_font))
		:connect +event_base_change)
	(ui-label *display* (:text "0" :color +argb_white :flow_flags +flow_flag_align_hright
		:font +calculator_font))
	(ui-grid button_grid (:grid_width 4 :grid_height 0 :color *env_toolbar_col*
			:font +calculator_font)
		(each (lambda (text)
			(defq button (ui-button _ (:text text)))
			(. button :connect +event_button)
			; Group buttons for easy enabling/disabling
			(cond
				((find text '("A" "B" "C" "D" "E" "F")) (push hex_buttons button))
				((find text '("2" "3" "4" "5" "6" "7" "8" "9")) (push other_base_buttons button))))
			'("MC" "MR"  "M-" "M+"
			"AND" "OR"  "XOR" "NOT"
			">>>" ">>"  "<<"  "NEG"
			"D"   "E"   "F"   "%"
			"A"   "B"   "C"   "/"
			"7"   "8"   "9"   "*"
			"4"   "5"   "6"   "-"
			"1"   "2"   "3"   "+"
			"0"  "CE" "AC"  "=" ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-calculator-state (base &optional error new_entry_val memory_val)
	; operand_stack operator_stack current_number base memory error_state new_entry
	(list (list) (list) "0" base (ifn memory_val 0) (ifn error :nil) (ifn new_entry_val :t)))

(defun num-to-base-str (n base chars)
	(if (= n 0) "0"
		(progn
			(defq result "" temp "")
			(when (< n 0) (setq result "-" n (abs n)))
			(while (> n 0)
				(setq temp (cat (elem-get chars (% n base)) temp)
					  n (/ n base)))
			(cat result temp))))

(defun str-to-num-base (s base)
    (defq n 0 neg :nil valid :t)
    (if (starts-with "-" s) (setq neg :t s (rest s)))
    (each (lambda (c)
            (defq val (find c +digit_list))
            (if (and val (< val base))
                (setq n (+ (* n base) val))
                (setq valid :nil))) s)
    (if valid (if neg (neg n) n) :error))


(defun format-number (num base)
	(case base
		(16 (num-to-base-str num 16 "0123456789ABCDEF"))
		(2  (num-to-base-str num 2 "01"))
		(8  (num-to-base-str num 8 "01234567"))
		(:t (str num))))

(defun do_op (op v2 v1)
	(case op
		("+" (+ v1 v2))
		("-" (- v1 v2))
		("*" (* v1 v2))
		("/" (if (/= v2 0) (/ v1 v2) :error))
		("%" (if (/= v2 0) (% v1 v2) :error))
		("AND" (logand v1 v2))
		("OR"  (logior v1 v2))
		("XOR" (logxor v1 v2))
		(:t v2)))

(defun apply-op (operands operators)
    (if (< (length operands) 2) :error
        (progn
            (defq op (pop operators)
                  v2 (pop operands)
                  v1 (pop operands))
            (defq result (do_op op v2 v1))
            (if (eql result :error)
                :error
                (push operands result)))))

(defun update-display (state)
    (bind '(operands _ current_number base _ error_state new_entry) state)
	(if error_state
		(set *display* :text "Error")
        (if new_entry
            (if (nempty? operands)
                (set *display* :text (format-number (first operands) base))
                (set *display* :text "0"))
            (set *display* :text (format-number (str-to-num-base current_number base) base))))
	(.-> *display* :layout :dirty))

(defun update-button-states (base)
	(each (lambda (button)
		(defq is_enabled (= base 16))
		(def (. button :dirty) :disabled (not is_enabled)
			:color (if is_enabled *env_toolbar_col* +disabled_color)
			:ink_color (if is_enabled *env_ink_col* +disabled_ink_color)))
		hex_buttons)

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
	(if op
		(if (defq digit (find op +digit_list))
			(if (< digit base) op)
			op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Core Logic Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-input (state op)
	(bind '(operands operators current_number base memory error_state new_entry) state)
	(if (and error_state (not (eql op "AC")))
		state
		(progn
			(defq digit (find op "0123456789ABCDEF"))
			(cond
                ; --- CLEAR and UNARY ---
				((eql op "AC") (list (list) (list) "0" base memory :nil :t))
				((eql op "CE") (list operands operators "0" base memory :nil new_entry))
				((eql op "BACK")
                    (defq new_num (if (> (length current_number) 1) (most current_number) "0"))
                    (list operands operators new_num base memory :nil new_entry))
                ((eql op "NEG")
                    (defq num (str-to-num-base current_number base))
                    (list operands operators (str (neg num)) base memory :nil new_entry))
                ((eql op "NOT")
                    (defq num (str-to-num-base current_number base))
                    (list operands operators (str (lognot num)) base memory :nil new_entry))
                ((eql op "<<")
                    (defq num (str-to-num-base current_number base))
                    (list operands operators (str (<< num 1)) base memory :nil new_entry))
                ((eql op ">>")
                    (defq num (str-to-num-base current_number base))
                    (list operands operators (str (>> num 1)) base memory :nil new_entry))
                ((eql op ">>>")
                    (defq num (str-to-num-base current_number base))
                    (list operands operators (str (>>> num 1)) base memory :nil new_entry))

                ; --- MEMORY ---
                ((eql op "M+") (list operands operators current_number base (+ memory (str-to-num-base current_number base)) :nil :t))
                ((eql op "M-") (list operands operators current_number base (- memory (str-to-num-base current_number base)) :nil :t))
                ((eql op "MR") (list operands operators (str memory) base memory :nil :nil))
                ((eql op "MC") (list operands operators current_number base 0 :nil new_entry))

                ; --- DIGITS ---
				((and digit (< digit base))
					(defq new_num (if (or new_entry (eql current_number "0")) op (cat current_number op)))
					(list operands operators new_num base memory :nil :nil))

                ; --- OPERATORS and EQUALS ---
				((or (find op +operators) (eql op "="))
                    (progn
                        (unless new_entry
                            (push operands (str-to-num-base current_number base)))
                        (defq error_flag :nil)

                        (if (eql op "=")
                            (progn
                                (while (and (not error_flag) (nempty? operators))
                                    (if (eql (apply-op operands operators) :error) (setq error_flag :t)))
                                (if error_flag
                                    (create-calculator-state base :error :t memory)
                                    (progn
                                        (defq result (first operands))
                                        (list (list (ifn result 0)) (list) (str (ifn result 0)) base memory :nil :t))))
                            (progn
                                (while (and (not error_flag) (nempty? operators)
                                            (defq top_op_prec (. +precedence :find (first operators)))
                                            (defq new_op_prec (. +precedence :find op))
                                            (and top_op_prec new_op_prec (>= top_op_prec new_op_prec)))
                                     (if (eql (apply-op operands operators) :error) (setq error_flag :t)))
                                (if error_flag
                                    (create-calculator-state base :error :t memory)
                                    (progn
                                        (push operators op)
                                        (list operands operators current_number base memory :nil :t)))))))
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
		(defq old_state state
			  msg (mail-read (elem-get select (defq idx (mail-select select))))
			  id (getf msg +ev_msg_target_id)
			  op :nil)

		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			(+select_main
				(cond
					((and id (>= id +event_button))
						(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
						(unless (get :disabled button) (setq op (get :text button))))
					((= (getf msg +ev_msg_type) +ev_type_key_down)
						(setq op (key-to-op (getf msg +ev_msg_key_scode)
											(getf msg +ev_msg_key_mod)
											(elem-get state +state_base)))))
				(cond
					((= id +event_close) (setq running :nil))
					((= id +event_min)
						(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))
					((= id +event_max)
						(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) '(512 512))))
						(. *window* :change_dirty x y w h))
					((= id +event_base_change)
						(bind '(_1 _2 current_num _4 memory _6 _7) state)
                        (defq new_base (elem-get '(10 16 2 8) (. base_bar :get_selected)))
                        (if (eql (str-to-num-base current_num new_base) :error)
                            (setq current_num "0"))
                        (setq state (list _1 _2 current_num new_base memory _6 _7)))
					(op (setq state (handle-input state op)))
					(:t (. *window* :event msg)))))

		(unless (and (= (length state) (length old_state)) (every eql state old_state))
			(update-display state)
			(unless (= (elem-get state +state_base) (elem-get old_state +state_base))
				(update-button-states (elem-get state +state_base)))))
	(gui-sub-rpc *window*))