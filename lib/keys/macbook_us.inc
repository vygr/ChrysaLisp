;module
(env-push)

(import "././consts/scodes.inc")
(import "././consts/chars.inc")

(defq
*normal_map* (scatter (Fmap)
	+sc_a (ascii-code "a") +sc_b (ascii-code "b") +sc_c (ascii-code "c")
	+sc_d (ascii-code "d") +sc_e (ascii-code "e") +sc_f (ascii-code "f")
	+sc_g (ascii-code "g") +sc_h (ascii-code "h") +sc_i (ascii-code "i")
	+sc_j (ascii-code "j") +sc_k (ascii-code "k") +sc_l (ascii-code "l")
	+sc_m (ascii-code "m") +sc_n (ascii-code "n") +sc_o (ascii-code "o")
	+sc_p (ascii-code "p") +sc_q (ascii-code "q") +sc_r (ascii-code "r")
	+sc_s (ascii-code "s") +sc_t (ascii-code "t") +sc_u (ascii-code "u")
	+sc_v (ascii-code "v") +sc_w (ascii-code "w") +sc_x (ascii-code "x")
	+sc_y (ascii-code "y") +sc_z (ascii-code "z")
	+sc_0 (ascii-code "0") +sc_1 (ascii-code "1") +sc_2 (ascii-code "2")
	+sc_3 (ascii-code "3") +sc_4 (ascii-code "4") +sc_5 (ascii-code "5")
	+sc_6 (ascii-code "6") +sc_7 (ascii-code "7") +sc_8 (ascii-code "8")
	+sc_9 (ascii-code "9")
	+sc_return +char_lf +sc_escape +char_esc +sc_backspace +char_backspace
	+sc_space +char_space +sc_tab +char_tab +sc_minus +char_minus
	+sc_equals +char_equal +sc_grave +char_tick +sc_period +char_dot
	+sc_slash +char_divide +sc_backslash +char_backslash +sc_nonusbackslash +char_tick
	+sc_leftbracket +char_lsb +sc_rightbracket +char_rsb +sc_semicolon +char_semi
	+sc_apostrophe +char_quote +sc_comma +char_comma +sc_delete +char_delete
	+sc_right 0x4000004f +sc_left 0x40000050
	+sc_down 0x40000051 +sc_up 0x40000052
	+sc_home 0x4000004A +sc_end 0x4000004D)
*shift_map* (scatter (Fmap)
	+sc_a (ascii-code "A") +sc_b (ascii-code "B") +sc_c (ascii-code "C")
	+sc_d (ascii-code "D") +sc_e (ascii-code "E") +sc_f (ascii-code "F")
	+sc_g (ascii-code "G") +sc_h (ascii-code "H") +sc_i (ascii-code "I")
	+sc_j (ascii-code "J") +sc_k (ascii-code "K") +sc_l (ascii-code "L")
	+sc_m (ascii-code "M") +sc_n (ascii-code "N") +sc_o (ascii-code "O")
	+sc_p (ascii-code "P") +sc_q (ascii-code "Q") +sc_r (ascii-code "R")
	+sc_s (ascii-code "S") +sc_t (ascii-code "T") +sc_u (ascii-code "U")
	+sc_v (ascii-code "V") +sc_w (ascii-code "W") +sc_x (ascii-code "X")
	+sc_y (ascii-code "Y") +sc_z (ascii-code "Z")
	+sc_0 (ascii-code ")") +sc_1 (ascii-code "!") +sc_2 (ascii-code "@")
	+sc_3 (ascii-code "#") +sc_4 (ascii-code "$") +sc_5 (ascii-code "%")
	+sc_6 (ascii-code "^") +sc_7 (ascii-code "&") +sc_8 (ascii-code "*")
	+sc_9 (ascii-code "(")
	+sc_minus +char_underscore +sc_equals +char_plus
	+sc_period +char_gt +sc_slash +char_question +sc_backslash +char_pipe
	+sc_nonusbackslash +char_tilde +sc_leftbracket +char_lcb
	+sc_rightbracket +char_rcb +sc_semicolon +char_colon +sc_apostrophe +char_double_quote
	+sc_comma +char_lt +sc_grave +char_tilde)
)

;module
(export-symbols '(*normal_map* *shift_map*))
(env-pop)
