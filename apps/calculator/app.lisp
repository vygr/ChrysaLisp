(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")

(enums +event 0
	(enum close max min)
	(enum base_change)
	(enum button))

; Use enums to define named indices for our state list.
(enums +state 0
    (enum accum num base lastop error_state new_entry))

(defq hex_buttons (list))
(defq dec_buttons (list))
(defq other_base_buttons (list))
(defq +operators ''("=" "+" "-" "*" "/" "AND" "OR" "XOR"))
(defq +disabled_color +argb_grey4)
(defq +disabled_ink_color *env_hint_col*)

(ui-window *window* ()
	(ui-title-bar _ "Calculator" (0xea19 0xea1b 0xea1a) +event_close)
	(. (ui-radio-bar base_bar ("DEC" "HEX" "BIN" "OCT")
			(:color (const *env_toolbar2_col*)
			 :font (const *env_window_font*)))
		:connect +event_base_change)
	(ui-label *display* (:text "0" :color +argb_white :flow_flags +flow_flag_align_hright
		:font (create-font "fonts/OpenSans-Regular.ctf" 24)))
	(ui-grid _ (:grid_width 4 :grid_height 7 :color *env_toolbar_col*
			:font (create-font "fonts/OpenSans-Regular.ctf" 28))
		(each (lambda (text)
			(defq button (ui-button _ (:text text)))
			(. button :connect +event_button)
			; Group buttons for easy enabling/disabling
			(cond
				((find text '("A" "B" "C" "D" "E" "F")) (push hex_buttons button))
				((find text '("2" "3" "4" "5" "6" "7" "8" "9")) (push other_base_buttons button))
				((find text '("AND" "OR" "XOR" "NOT")) (push dec_buttons button))))
			
			; CHANGED: The "DEF" and "ABC" rows have been swapped here.
			'("AND" "OR"  "XOR" "NOT"
			  "D"   "E"   "F"   "AC"
			  "A"   "B"   "C"   "/"
			  "7"   "8"   "9"   "*"
			  "4"   "5"   "6"   "-"
			  "1"   "2"   "3"   "+"
			  "0"   "CE"  "BACK" "="))))

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
        ("/" (if (/= num 0) (/ accum num) :error)) ; Return :error on division by zero
        ("AND" (logand accum num))
        ("OR"  (logior accum num))
        ("XOR" (logxor accum num))
        (:t num)))

(defun update-display (state)
    (if (elem-get state +state_error_state)
        (set *display* :text "Error")
        (set *display* :text (format-number (elem-get state +state_num) (elem-get state +state_base))))
    (.-> *display* :layout :dirty))

(defun update-button-states (base)
    (each (lambda (button)
        (def (. button :dirty) :disabled (/= base 16)
            :color (if (= base 16) *env_toolbar_col* +disabled_color)
            :ink_color (if (= base 16) *env_ink_col* +disabled_ink_color)))
        hex_buttons)
    (each (lambda (button)
         (def (. button :dirty) :disabled (= base 10)
            :color (if (/= base 10) *env_toolbar_col* +disabled_color)
            :ink_color (if (/= base 10) *env_ink_col* +disabled_ink_color)))
        dec_buttons)
    (each (lambda (button)
        (defq button_val (str-as-num (get :text button)))
        (def (. button :dirty) :disabled (>= button_val base)
            :color (if (< button_val base) *env_toolbar_col* +disabled_color)
            :ink_color (if (< button_val base) *env_ink_col* +disabled_ink_color)))
        other_base_buttons))

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
                ((eql op "CE")   (list accum 0 base lastop error_state :t))
                ((eql op "NOT")  (list accum (lognot num) base lastop error_state new_entry))
                ((eql op "BACK") (list accum (/ num base) base lastop error_state new_entry))
                ((find op +operators)
                    (if new_entry
                        (list num num base (if (eql op "=") :nil op) :nil :t)
                        (let ((result (do_op accum num lastop)))
                             (if (eql result :error)
                                 (list accum num base lastop :t new_entry)
                                 (list result result base (if (eql op "=") :nil op) :nil :t)))))
                ((and digit (< digit base))
                    (if new_entry
                        (list accum digit base (if lastop lastop :t) :nil :nil)
                        (list accum (+ (* num base) digit) base lastop :nil :nil)))
                (:t state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(defq state (create-calculator-state 10) running :t)
	(update-button-states (elem-get state +state_base))
	(. base_bar :set_selected 0)

	(while running
		(defq msg (mail-read (task-mbox))
		      id (getf msg +ev_msg_target_id)
		      op :nil)

		; Centralized event-to-op mapping
		(when (and id (>= id +event_button))
			(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id)))
			(unless (get :disabled button)
				(setq op (get :text button))))

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
                (setq state (list accum num new_base lastop error_state new_entry))
				(update-button-states new_base))
			(op (setq state (handle-input state op)))
			(:t (. *window* :event msg)))

        ; Update the display based on the new state
        (update-display state))
	(gui-sub-rpc *window*))