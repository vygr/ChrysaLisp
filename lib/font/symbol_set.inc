;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/font/symbol_set.inc - the symbols
;
; Each is a name and what it is drawn with, on a grid of 24, by the kit
; of lib/font/symbols.inc. The code of a symbol is where it comes in this
; list, from 0xe000, and its name is +sym_ and what it is called here,
; lib/consts/symbols.inc. So a new one goes on the end, or every code
; after it moves, and one is never taken out of the middle.
;
; To change how a symbol looks, change it here and run the symbols
; command. To change how they all look, the weight of a stroke, its ends
; and its corners are the theme, in cmd/symbols.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./symbols.inc")

;the parts that more than one symbol is made of
(defq s_cross (cat (s-line 6 6 18 18) (s-line 18 6 6 18))
	s_undo (cat (s-line 3 7 3 13 9 13) (s-arc 12 17 9 0 154))
	s_doc (cat (s-loop 6 2 14 2 20 8 20 20 18 22 6 22 4 20 4 4) (s-line 14 2 14 8 20 8))
	s_doc_x (cat s_doc (s-line 9.5 12.5 14.5 17.5) (s-line 14.5 12.5 9.5 17.5))
	s_folder (s-loop 2 5 4 3 8 3 9.6 3.9 10.5 5.2 12 6 20 6 22 8 22 18 20 20 4 20 2 18)
	s_floppy (cat (s-loop 3 5 5 3 16 3 21 8 21 19 19 21 5 21 3 19)
		(s-line 7 21 7 13 17 13 17 21) (s-line 7 3 7 8 15 8))
	s_lens (cat (s-ring 11 11 7) (s-line 16.2 16.2 21 21))
	s_tray (s-line 4 14 4 19 6 21 18 21 20 19 20 14)
	s_tri (s-loop 7 4 19 12 7 20)
	s_pause (cat (s-line 9 5 9 19) (s-line 15 5 15 19))
	s_step (cat (s-arc-arrow 12 15 8 170 10) (s-disc 12 20 1.8))
	s_fwd (cat (s-loop 3 6 11 12 3 18) (s-loop 13 6 21 12 13 18))
	s_swap (cat (s-arrow 4 8 19 8) (s-arrow 20 16 5 16))
	s_funnel (s-loop 3 4 21 4 14 12 14 19 10 21 10 12)
	s_stop (cat (s-ring 12 12 8) (s-disc 12 12 3.6))
	s_stop_x (cat (s-ring 12 12 8) (s-line 8.8 8.8 15.2 15.2) (s-line 15.2 8.8 8.8 15.2))
	s_big_a (cat (s-line 3 19 8 5 13 19) (s-line 4.8 14.5 11.2 14.5))
	s_small_a (cat (s-ring 18 15.5 3.2) (s-line 21.2 11.5 21.2 19))
	s_left (cat (s-line 4 3 4 21) (s-rrect 7 5 20 10 1) (s-rrect 7 14 15 19 1))
	s_top (s-turn s_left)
	s_centre (cat (s-line 12 3 12 21) (s-rrect 4 5 20 10 1) (s-rrect 7 14 17 19 1))
	s_paren (cat (s-arc 14 12 9 125 235) (s-arrow 21 12 12 12))
	s_lines (cat (s-line 4 5 20 5) (s-line 11 10 20 10) (s-line 11 14 20 14) (s-line 4 19 20 19))
	s_slash (cat (s-line 5 19 19 5) (s-line 11 5 19 5 19 13)))

(defq *symbols* (list
	;the window
	(list "close" s_cross)
	(list "min" (s-line 6 18 18 18))
	(list "max" (s-rrect 5 5 19 19 2))
	;editing
	(list "undo" s_undo)
	(list "redo" (s-mirror s_undo))
	(list "rewind" (cat (s-line 11 6 5 12 11 18) (s-line 19 6 13 12 19 18)))
	(list "cut" (cat (s-ring 6 6 3) (s-ring 6 18 3)
		(s-line 8.12 8.12 12 12) (s-line 20 4 8.12 15.88) (s-line 14.8 14.8 20 20)))
	(list "copy" (cat (s-rrect 8 8 22 22 2) (s-line 4 16 2.6 15.4 2 14 2 4 2.6 2.6 4 2 14 2 15.4 2.6 16 4)))
	(list "paste" (cat (s-rrect 5 4 19 22 2) (s-rrect 9 2 15 6 1)))
	(list "delete" (cat (s-line 3 6 21 6) (s-line 5 6 6 20 8 22 16 22 18 20 19 6)
		(s-line 8 6 8 4 10 2 14 2 16 4 16 6) (s-line 10 11 10 17) (s-line 14 11 14 17)))
	(list "clear" (cat (s-loop 9 20 4 15 14 5 21 12 13 20) (s-line 9 10 16 17) (s-line 9 20 21 20)))
	(list "clear_all" (cat (s-ring 12 12 8.5) (s-line 6 6 18 18)))
	(list "split" (cat (s-line 12 3 12 21) (s-line 8 8 4 12 8 16) (s-line 16 8 20 12 16 16)))
	(list "reflow" (cat (s-line 4 6 20 6) (s-line 4 12 17 12 19 13 20 15 19 17 17 18 12 18)
		(s-head 12 18 -1 0 3) (s-line 4 18 8 18)))
	(list "comment" (cat (s-disc 12 7.5 2.3) (s-disc 12 15 2.3) (s-line 13.2 15.5 10.2 21)))
	(list "upper_case" (cat s_big_a (s-arrow 19 19 19 5)))
	(list "lower_case" (cat (s-ring 8 15.5 3.6) (s-line 11.6 11.5 11.6 19) (s-arrow 19 5 19 19)))
	(list "sort" (cat (s-line 4 7 9 7) (s-line 4 12 12 12) (s-line 4 17 15 17) (s-arrow 19 5 19 19)))
	(list "unique" (cat (s-line 4 7 10 7) (s-line 4 17 10 17) (s-line 10 7 15 12 10 17) (s-line 15 12 20 12)))
	(list "reverse" (cat (s-arrow 8 19 8 5) (s-arrow 16 5 16 19)))
	(list "indent" (cat s_lines (s-line 4 9.5 7.5 12 4 14.5)))
	(list "outdent" (cat s_lines (s-line 7.5 9.5 4 12 7.5 14.5)))
	(list "select_paragraph" (cat (s-corners) (s-line 7 9 17 9) (s-line 7 12 17 12) (s-line 7 15 13 15)))
	(list "select_region" (cat (s-corners 3 21 5) (s-disc 12 12 1.6)))
	(list "select_form" (cat (s-corners) (s-arc 17.5 12 9 150 210) (s-arc 6.5 12 9 -30 30)))
	(list "start_form" s_paren)
	(list "end_form" (s-mirror s_paren))
	;files
	(list "new" (cat s_doc (s-line 9 15 15 15) (s-line 12 12 12 18)))
	(list "open" s_folder)
	(list "save" s_floppy)
	(list "save_all" (s-all s_floppy))
	(list "close_file" s_doc_x)
	(list "close_all" (s-all s_doc_x))
	(list "load" (cat s_tray (s-arrow 12 3 12 15)))
	(list "load_all" (cat s_tray (s-arrow 8 3 8 14) (s-arrow 16 3 16 14)))
	(list "scratchpad" (cat (s-rrect 3 6 16 21 2) (s-line 6.5 11 12.5 11) (s-line 6.5 15 10.5 15)
		(s-loop 18.5 3 21.5 6 15 12.5 11.5 13 12 9.5)))
	;finding
	(list "find" s_lens)
	(list "find_global" (s-global s_lens))
	(list "find_down" (s-arrow 12 4 12 20 5))
	(list "find_up" (s-arrow 12 20 12 4 5))
	(list "whole_words" (cat (s-line 3 15 3 20 21 20 21 15) (s-ring 7.5 11 2.8) (s-line 10.3 7.5 10.3 13.8)
		(s-line 14 4 14 13.8) (s-ring 16.8 11 2.8)))
	(list "regexp" (cat (s-disc 6 18 1.9) (s-line 15 4 15 16) (s-line 9.8 7 20.2 13) (s-line 20.2 7 9.8 13)))
	(list "ignore_case" (cat s_big_a s_small_a))
	(list "replace" s_swap)
	(list "replace_all" (s-all s_swap))
	(list "replace_global" (s-global s_swap))
	(list "collect" s_funnel)
	(list "collect_global" (s-global s_funnel))
	;running, a macro or a debugger
	(list "play" s_tri)
	(list "play_all" (s-all s_tri))
	(list "play_global" (s-global s_tri))
	(list "play_end" (cat (s-loop 5 4 16 12 5 20) (s-line 20 4 20 20)))
	(list "pause" s_pause)
	(list "pause_all" (s-all s_pause))
	(list "step" s_step)
	(list "step_all" (s-all s_step))
	(list "forward" s_fwd)
	(list "forward_all" (s-all s_fwd))
	(list "record" (s-disc 12 12 7))
	(list "breakpoint" s_stop)
	(list "breakpoint_remove" s_stop_x)
	(list "remove_all" (s-all s_stop_x))
	(list "enable_all" (s-all s_stop))
	(list "disable_all" (s-all (s-ring 12 12 8)))
	(list "save_debug" (cat (s-small s_floppy) (s-fill 15 14 22 18 15 22)))
	;layout
	(list "align_left" s_left)
	(list "align_right" (s-mirror s_left))
	(list "align_top" s_top)
	(list "align_bottom" (s-flip s_top))
	(list "align_hcenter" s_centre)
	(list "align_vcenter" (s-turn s_centre))
	(list "group" (cat (s-corners) (s-rrect 7 7 13 13 1) (s-rrect 11 11 17 17 1)))
	(list "ungroup" (cat (s-rrect 3 3 11 11 1.5) (s-rrect 13 13 21 21 1.5)))
	;drawing
	(list "select" (s-loop 5 3 5 18 9 14.5 11.5 20 13.8 19 11.3 13.5 16.5 13.5))
	(list "move" (cat (s-line 12 3 12 21) (s-line 3 12 21 12) (s-line 9 6 12 3 15 6)
		(s-line 9 18 12 21 15 18) (s-line 6 9 3 12 6 15) (s-line 18 9 21 12 18 15)))
	(list "pen" (cat (s-loop 17 3 21 7 8 20 3 21 4 16) (s-line 14.5 5.5 18.5 9.5)))
	(list "brush" (cat (s-loop 20 3 21 4 12 14 10 12) (s-loop 9 14 11 16 10 19 7 21 3 21 5 19 6 16)))
	(list "line" (s-line 5 19 19 5))
	(list "arrow" s_slash)
	(list "double_arrow" (cat s_slash (s-line 5 11 5 19 13 19)))
	(list "rect" (s-rrect 4 6 20 18 1.5))
	(list "circle" (s-ring 12 12 8))
	(list "filled_rect" (s-box 4 6 20 18))
	(list "filled_circle" (s-disc 12 12 8.5))
	(list "small" (s-disc 12 12 2.6))
	(list "medium" (s-disc 12 12 4.8))
	(list "large" (s-disc 12 12 7.4))
	;the view
	(list "zoom_in" (cat s_lens (s-line 8 11 14 11) (s-line 11 8 11 14)))
	(list "zoom_out" (cat s_lens (s-line 8 11 14 11)))
	(list "plain" (s-rrect 4 4 20 20 2))
	(list "grid" (cat (s-rrect 4 4 20 20 2) (s-line 9.3 4 9.3 20) (s-line 14.7 4 14.7 20)
		(s-line 4 9.3 20 9.3) (s-line 4 14.7 20 14.7)))
	(list "lines" (cat (s-rrect 4 4 20 20 2) (s-line 4 9.3 20 9.3) (s-line 4 14.7 20 14.7)))
	(list "axis" (cat (s-line 5 3 5 19) (s-line 5 19 21 19) (s-line 2.5 5.5 5 3 7.5 5.5)
		(s-line 18.5 16.5 21 19 18.5 21.5) (s-line 8 15 11 10 14 13 18 7)))
	(list "mode" (cat (s-loop 12 3 20 7.5 20 16.5 12 21 4 16.5 4 7.5)
		(s-line 12 12 12 21) (s-line 12 12 4 7.5) (s-line 12 12 20 7.5)))
	(list "auto" (cat (s-arc-arrow 12 12 8 160 20) (s-arc-arrow 12 12 8 340 200)))
	(list "image" (cat (s-rrect 3 4 21 20 2) (s-ring 8.5 9.5 1.6) (s-line 3.5 18 9 13 13 16.5 16.5 13 20.5 17)))
	(list "layers" (cat (s-loop 12 3 21 8 12 13 3 8) (s-line 3 12.5 12 17.5 21 12.5) (s-line 3 17 12 22 21 17)))
	;going about
	(list "prev" (s-line 15 6 9 12 15 18))
	(list "next" (s-line 9 6 15 12 9 18))
	(list "up" (s-line 6 15 12 9 18 15))
	(list "down" (s-line 6 9 12 15 18 9))
	;a game, a list of things to do, a call
	(list "reset" (s-arc-arrow 12 12 8 60 -250))
	(list "scramble" (cat (s-line 3 7 8 7 16 17 21 17) (s-line 3 17 8 17 16 7 21 7)
		(s-line 18 4 21 7 18 10) (s-line 18 14 21 17 18 20)))
	(list "solve" (cat (s-arc 12 10 6.5 -50 230) (s-line 16.2 15 14.5 17.5) (s-line 7.8 15 9.5 17.5)
		(s-line 9.5 17.5 14.5 17.5) (s-line 10 21 14 21)))
	(list "peek" (cat (s-loop 2 12 5 8.5 9 6.5 12 6 15 6.5 19 8.5 22 12 19 15.5 15 17.5 12 18 9 17.5 5 15.5)
		(s-ring 12 12 3)))
	(list "done" (s-line 4 12.5 9.5 18 20 6))
	(list "dial" (cat (s-line 6.5 6 6.5 9.5 8.5 13.5 10.5 15.5 14.5 17.5 18 17.5)
		(s-disc 6.2 5.6 2.5) (s-disc 18.4 17.8 2.5)))
	(list "hangup" (cat (s-arc 12 19 9.5 25 155) (s-disc 3.4 15 2.4) (s-disc 20.6 15 2.4) (s-line 5 5 19 5)))
	(list "warning" (cat (s-loop 12 3 22 20 2 20) (s-line 12 9 12 14) (s-disc 12 17 1.3)))
	;a circuit board
	(list "route" (cat (s-ring 5 18 2.4) (s-ring 19 6 2.4) (s-line 7.4 18 11 18 15 6 16.6 6)))
	(list "pcb" (cat (s-rrect 3 5 21 19 2) (s-disc 7.5 9.5 1.5) (s-disc 16.5 14.5 1.5)
		(s-line 7.5 9.5 12 9.5 16.5 14.5) (s-disc 7.5 15 1.5) (s-disc 16.5 9 1.5)))
	(list "gerber" (cat (s-rrect 3 5 21 19 2) (s-ring 8 12 2.4) (s-ring 16 12 2.4) (s-line 10.4 12 13.6 12)))
	;where you were, and where you came back from. Not prev and next, which
	;are the one before and the one after in a list
	(list "go_back" (s-arrow 19 12 5 12))
	(list "go_forward" (s-arrow 5 12 19 12))
	;a whiteboard: what rubs out, the instruments, and what is done to
	;what is selected
	(list "eraser" (cat (s-loop 13 4 20 11 11 20 7 20 3 16) (s-line 8 9 15 16) (s-line 11 20 21 20)))
	(list "ruler" (cat (s-loop 3 17 17 3 21 7 7 21) (s-line 7 13 8.6 14.6) (s-line 10 10 12.2 12.2)
		(s-line 13 7 14.6 8.6)))
	(list "protractor" (cat (s-arc 12 17 9 0 180) (s-line 3 17 21 17) (s-line 12 17 12 13.5)
		(s-line 12 8 12 10) (s-line 5.6 10.6 7 12) (s-line 18.4 10.6 17 12)))
	(list "set_square" (cat (s-loop 4 20 4 4 20 20) (s-loop 7.5 16.5 7.5 11.5 12.5 16.5)))
	(list "snap" (cat (s-line 12 3 12 7) (s-line 12 17 12 21) (s-line 3 12 7 12) (s-line 17 12 21 12)
		(s-disc 12 12 3.4) (s-corners)))
	(list "snap_angle" (cat (s-line 20 19 4 19 15 5) (s-arc 4 19 9 0 52)))
	(list "to_front" (cat (s-rrect 9 9 20 20 1.5) (s-box 4 4 13 13)))
	(list "to_back" (cat (s-box 11 11 20 20) (s-rrect 4 4 15 15 1.5)))
	(list "duplicate" (cat (s-rrect 8 8 20 20 1.5) (s-line 4 15 4 5 5 4 15 4) (s-line 14 11.5 14 16.5)
		(s-line 11.5 14 16.5 14)))
	(list "arc" (s-arc 12 17 9 20 160))
	(list "pie" (cat (s-arc 11 14 8 0 270) (s-line 19 14 11 14 11 22)))
	(list "text" (cat (s-line 5 5 19 5) (s-line 12 5 12 20) (s-line 9 20 15 20)))))

;;;;;;;;;;;;
; the themes
;;;;;;;;;;;;

;a theme is a font of the same symbols: the file, half the weight of a
;stroke on the grid of 24, how two strokes join and how one ends
(defq *sym_themes* (list
	(list "fonts/Symbols.ctf" 1.15 +join_round +cap_round)
	(list "fonts/Symbols-Light.ctf" 0.8 +join_round +cap_round)
	(list "fonts/Symbols-Bold.ctf" 1.5 +join_round +cap_round)
	(list "fonts/Symbols-Sharp.ctf" 1.15 +join_miter +cap_square)))

(defun sym-code-hex (code)
	;a code as four hex digits, the high byte first
	(to-lower (hex-encode (cat (char (>> code 8)) (char (logand code 0xff))))))

(defun sym-names-text ()
	;lib/consts/symbols.inc, the name of each symbol and its code
	(defq out (list
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The symbols of the symbol font, fonts/Symbols.ctf, by name.
;
; A toolbar is written with these, (ui-buttons (+sym_undo +sym_redo) ...),
; so it reads as what it means, and each meaning has a symbol of its own.
; A name with _all is the symbol with three dots, for all of them, and
; one with _global is the symbol with a ring, for everywhere.
;
; This is made by the symbols command, from lib/font/symbol_set.inc, with
; the fonts. Do not edit it by hand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq") line "")
	(each (lambda ((name &ignore))
		(defq item (cat "+sym_" name " 0x" (sym-code-hex (+ +sf_base (!)))
			(if (= (!) (dec (length *symbols*))) ")" " ")))
		(when (> (+ (length line) (length item)) 76)
			(push out (cat (ascii-char 10) (ascii-char 9) (trim-end line)))
			(setq line ""))
		(setq line (cat line item)))
		*symbols*)
	(push out (cat (ascii-char 10) (ascii-char 9) (trim-end line) (ascii-char 10)))
	;the header above is a string of this file, and a checkout on Windows
	;may have a return before each of its ends of line
	(join (split (apply (const cat) out) (ascii-char 13)) ""))
