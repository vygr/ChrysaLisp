;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defq
	+sym_close 0xe000 +sym_min 0xe001 +sym_max 0xe002 +sym_undo 0xe003
	+sym_redo 0xe004 +sym_rewind 0xe005 +sym_cut 0xe006 +sym_copy 0xe007
	+sym_paste 0xe008 +sym_delete 0xe009 +sym_clear 0xe00a
	+sym_clear_all 0xe00b +sym_split 0xe00c +sym_reflow 0xe00d
	+sym_comment 0xe00e +sym_upper_case 0xe00f +sym_lower_case 0xe010
	+sym_sort 0xe011 +sym_unique 0xe012 +sym_reverse 0xe013 +sym_indent 0xe014
	+sym_outdent 0xe015 +sym_select_paragraph 0xe016 +sym_select_region 0xe017
	+sym_select_form 0xe018 +sym_start_form 0xe019 +sym_end_form 0xe01a
	+sym_new 0xe01b +sym_open 0xe01c +sym_save 0xe01d +sym_save_all 0xe01e
	+sym_close_file 0xe01f +sym_close_all 0xe020 +sym_load 0xe021
	+sym_load_all 0xe022 +sym_scratchpad 0xe023 +sym_find 0xe024
	+sym_find_global 0xe025 +sym_find_down 0xe026 +sym_find_up 0xe027
	+sym_whole_words 0xe028 +sym_regexp 0xe029 +sym_ignore_case 0xe02a
	+sym_replace 0xe02b +sym_replace_all 0xe02c +sym_replace_global 0xe02d
	+sym_collect 0xe02e +sym_collect_global 0xe02f +sym_play 0xe030
	+sym_play_all 0xe031 +sym_play_global 0xe032 +sym_play_end 0xe033
	+sym_pause 0xe034 +sym_pause_all 0xe035 +sym_step 0xe036
	+sym_step_all 0xe037 +sym_forward 0xe038 +sym_forward_all 0xe039
	+sym_record 0xe03a +sym_breakpoint 0xe03b +sym_breakpoint_remove 0xe03c
	+sym_remove_all 0xe03d +sym_enable_all 0xe03e +sym_disable_all 0xe03f
	+sym_save_debug 0xe040 +sym_align_left 0xe041 +sym_align_right 0xe042
	+sym_align_top 0xe043 +sym_align_bottom 0xe044 +sym_align_hcenter 0xe045
	+sym_align_vcenter 0xe046 +sym_group 0xe047 +sym_ungroup 0xe048
	+sym_select 0xe049 +sym_move 0xe04a +sym_pen 0xe04b +sym_brush 0xe04c
	+sym_line 0xe04d +sym_arrow 0xe04e +sym_double_arrow 0xe04f
	+sym_rect 0xe050 +sym_circle 0xe051 +sym_filled_rect 0xe052
	+sym_filled_circle 0xe053 +sym_small 0xe054 +sym_medium 0xe055
	+sym_large 0xe056 +sym_zoom_in 0xe057 +sym_zoom_out 0xe058
	+sym_plain 0xe059 +sym_grid 0xe05a +sym_lines 0xe05b +sym_axis 0xe05c
	+sym_mode 0xe05d +sym_auto 0xe05e +sym_image 0xe05f +sym_layers 0xe060
	+sym_prev 0xe061 +sym_next 0xe062 +sym_up 0xe063 +sym_down 0xe064
	+sym_reset 0xe065 +sym_scramble 0xe066 +sym_solve 0xe067 +sym_peek 0xe068
	+sym_done 0xe069 +sym_dial 0xe06a +sym_hangup 0xe06b +sym_warning 0xe06c
	+sym_route 0xe06d +sym_pcb 0xe06e +sym_gerber 0xe06f)
