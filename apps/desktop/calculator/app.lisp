(import "usr/env.inc")
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
; Constants & App State
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*config* :nil
	*config_version* 1
	*config_file* (cat *env_home* "calculator.tre")

	; App State
	*operands* (list)
	*operators* (list)
	*current_number* "0"
	*base* 10
	*memory* 0
	*error_state* :nil
	*new_entry* :t

	; Component grouping
	*hex_buttons* (list)
	*other_base_buttons* (list)

	; Static references
	+operators ''("=" "+" "-" "*" "/" "%" "AND" "OR" "XOR")
	+disabled_color +argb_grey4
	+disabled_ink_color *env_hint_col*
	+digit_list (static-q (map identity "0123456789ABCDEF"))
	+calculator_font (create-font "fonts/Hack-Regular.ctf" 22)

	; Evaluation Presedence
	+precedence (scatter (Fmap)
		"*" 2 "/" 2 "%" 2
		"+" 1 "-" 1
		"AND" 1 "OR" 1 "XOR" 1)

	; Scancode mappings
	key_map (scatter (Fmap)
		+sc_0 "0" +sc_1 "1" +sc_2 "2" +sc_3 "3" +sc_4 "4"
		+sc_5 "5" +sc_6 "6" +sc_7 "7" +sc_8 "8" +sc_9 "9"
		+sc_a "A" +sc_b "B" +sc_c "C" +sc_d "D" +sc_e "E" +sc_f "F"
		+sc_kp_0 "0" +sc_kp_1 "1" +sc_kp_2 "2" +sc_kp_3 "3" +sc_kp_4 "4"
		+sc_kp_5 "5" +sc_kp_6 "6" +sc_kp_7 "7" +sc_kp_8 "8" +sc_kp_9 "9"
		+sc_kp_divide "/" +sc_kp_multiply "*" +sc_kp_minus "-" +sc_kp_plus "+"
		+sc_kp_enter "=" +sc_slash "/" +sc_minus "-" +sc_equals "="
		+sc_return "=" +sc_escape "AC" +sc_backspace "BACK" +sc_delete "CE")

	shift_key_map (scatter (Fmap)
		+sc_equals "+"
		+sc_8 "*"
		+sc_5 "%"
		+sc_7 "AND"
		+sc_6 "XOR"
		+sc_backslash "OR"
		+sc_grave "NOT"
		+sc_comma "<<"
		+sc_period ">>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Configuration Management
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:base 10
		:memory 0
		:operands (list)
		:operators (list)
		:current_number "0"))

(defun config-load ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (config-default)))
	(bind '(*base* *memory* *operands* *operators* *current_number*)
		(gather *config* :base :memory :operands :operators :current_number))
	(setq *error_state* :nil *new_entry* :t))

(defun config-save ()
	(scatter *config*
		:base *base*
		:memory *memory*
		:operands *operands*
		:operators *operators*
		:current_number *current_number*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; UI Construction
;;;;;;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* ()
	(ui-title-bar _ "Calculator" (0xea19 0xea1b 0xea1a) +event_close)
	(. (ui-radio-bar base_bar ("dec" "hex" "bin" "oct")
			(:color (const *env_toolbar2_col*) :font +calculator_font))
		:connect +event_base_change)
	(ui-label *display* (:text "0" :color +argb_white :flow_flags +flow_flag_align_hright :font +calculator_font))
	(ui-grid button_grid (:grid_width 4 :grid_height 0 :color *env_toolbar_col* :font +calculator_font)
		(each (lambda (text)
			(defq button (ui-button _ (:text text)))
			(. button :connect +event_button)
			(cond
				((find text '("A" "B" "C" "D" "E" "F")) (push *hex_buttons* button))
				((find text '("2" "3" "4" "5" "6" "7" "8" "9")) (push *other_base_buttons* button)))
			(. button_grid :add_child button))
			'("MC" "MR"  "M-" "M+"
			  "AND" "OR"  "XOR" "NOT"
			  ">>>" ">>"  "<<"  "NEG"
			  "D"   "E"   "F"   "%"
			  "A"   "B"   "C"   "/"
			  "7"   "8"   "9"   "*"
			  "4"   "5"   "6"   "-"
			  "1"   "2"   "3"   "+"
			  "0"   "CE"  "AC"  "=" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Math and Display Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-to-base-str (n base chars)
	(if (= n 0) "0"
		(progn
			(defq result "" temp "")
			(when (< n 0) (setq result "-" n (abs n)))
			(while (> n 0)
				(setq temp (cat (elem-get chars (% n base)) temp) n (/ n base)))
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

(defun apply-op ()
	(if (< (length *operands*) 2) :error
		(progn
			(defq op (pop *operators*)
				v2 (pop *operands*) v1 (pop *operands*)
				result (do_op op v2 v1))
			(if (eql result :error)
				:error
				(push *operands* result)))))

(defun key-to-op (key mod base)
	(defq op
		(if (bits? mod +ev_key_mod_shift)
			(. shift_key_map :find key)
			(. key_map :find key)))
	(if op
		(if (defq digit (find op +digit_list))
			(if (< digit base) op)
			op)))

(defun update-display ()
	(if *error_state*
		(set *display* :text "Error")
		(if *new_entry*
			(if (nempty? *operands*)
				(set *display* :text (format-number (first *operands*) *base*))
				(set *display* :text "0"))
			(set *display* :text (format-number (str-to-num-base *current_number* *base*) *base*))))
	(.-> *display* :layout :dirty))

(defun update-button-states ()
	(defq is_hex (= *base* 16))
	(each (lambda (button)
		(def (. button :dirty) :disabled (not is_hex)
			:color (if is_hex *env_toolbar_col* +disabled_color)
			:ink_color (if is_hex *env_ink_col* +disabled_ink_color)))
		*hex_buttons*)

	(each (lambda (button)
		(defq button_val (str-as-num (get :text button))
			  is_enabled (< button_val *base*))
		(def (. button :dirty) :disabled (not is_enabled)
			:color (if is_enabled *env_toolbar_col* +disabled_color)
			:ink_color (if is_enabled *env_ink_col* +disabled_ink_color)))
		*other_base_buttons*))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Input Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-input (op)
	(if (and *error_state* (nql op "AC"))
		:nil
		(progn
			(defq digit (find op +digit_list))
			(cond
				((eql op "AC")
					(setq *operands* (list) *operators* (list) *current_number* "0" *error_state* :nil *new_entry* :t))
				((eql op "CE")
					(setq *current_number* "0" *error_state* :nil *new_entry* :t))
				((eql op "BACK")
					(setq *current_number* (if (> (length *current_number*) 1) (most *current_number*) "0")))
				((eql op "NEG")
					(setq *current_number* (format-number (neg (str-to-num-base *current_number* *base*)) *base*)))
				((eql op "NOT")
					(setq *current_number* (format-number (lognot (str-to-num-base *current_number* *base*)) *base*)))
				((eql op "<<")
					(setq *current_number* (format-number (<< (str-to-num-base *current_number* *base*) 1) *base*)))
				((eql op ">>")
					(setq *current_number* (format-number (>> (str-to-num-base *current_number* *base*) 1) *base*)))
				((eql op ">>>")
					(setq *current_number* (format-number (>>> (str-to-num-base *current_number* *base*) 1) *base*)))
				((eql op "M+")
					(++ *memory* (str-to-num-base *current_number* *base*))
					(setq *new_entry* :t))
				((eql op "M-")
					(-- *memory* (str-to-num-base *current_number* *base*))
					(setq *new_entry* :t))
				((eql op "MR")
					(setq *current_number* (format-number *memory* *base*) *new_entry* :nil))
				((eql op "MC")
					(setq *memory* 0 *new_entry* :t))
				((and digit (< digit *base*))
					(setq *current_number* (if (or *new_entry* (eql *current_number* "0")) op (cat *current_number* op))
						  *new_entry* :nil))
				((or (find op +operators) (eql op "="))
					(unless *new_entry*
						(push *operands* (str-to-num-base *current_number* *base*)))
					(if (eql op "=")
						(progn
							(while (and (not *error_state*) (nempty? *operators*))
								(if (eql (apply-op) :error) (setq *error_state* :t)))
							(unless *error_state*
								(defq result (if (nempty? *operands*) (first *operands*) 0))
								(setq *operands* (list result) *operators* (list) *current_number* (format-number result *base*) *new_entry* :t)))
						(progn
							(while (and (not *error_state*) (nempty? *operators*)
										(defq top_prec (. +precedence :find (first *operators*)))
										(defq new_prec (. +precedence :find op))
										(and top_prec new_prec (>= top_prec new_prec)))
								 (if (eql (apply-op) :error) (setq *error_state* :t)))
							(unless *error_state*
								(push *operators* op)
								(setq *new_entry* :t))))))
			(update-display))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(config-load)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips base_bar '("decimal" "hexadecimal" "binary" "octal"))
	(. base_bar :set_selected (find *base* '(10 16 2 8)))
	(update-display)
	(update-button-states)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			  id (getf msg +ev_msg_target_id))
		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(+select_main
				(cond
					((= id +event_close)
						(setq *running* :nil))
					((= id +event_min)
						(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))
					((= id +event_max)
						(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) '(512 512))))
						(. *window* :change_dirty x y w h))
					((= id +event_base_change)
						(defq new_base (elem-get '(10 16 2 8) (. base_bar :get_selected)))
						(when (/= new_base *base*)
							(defq num_value (str-to-num-base *current_number* *base*))
							(when (nql num_value :error)
								(setq *current_number* (format-number num_value new_base)
									  *base* new_base)
								(update-button-states)
								(update-display))))
					((and id (>= id +event_button))
						(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
						(unless (get :disabled button)
							(action-input (get :text button))))
					((= (getf msg +ev_msg_type) +ev_type_key_down)
						(defq op (key-to-op (getf msg +ev_msg_key_scode) (getf msg +ev_msg_key_mod) *base*))
						(if op
							(action-input op)
							(. *window* :event msg)))
					(:t (. *window* :event msg))))))
	(config-save)
	(gui-sub-rpc *window*))
