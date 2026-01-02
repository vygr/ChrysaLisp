(import "usr/env.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum reset undo)
	(enum click))

(enums +select 0
	(enum main tip))

(defq +grid_w 7
	  +grid_h 7
	  +tile_count (* +grid_w +grid_h)
	  ; Board States: -1 = Invalid (Corner), 0 = Empty, 1 = Peg
	  +state_invalid -1
	  +state_empty 0
	  +state_peg 1)

; Configuration and State
(defq *config_file* (cat *env_home* "solitaire.tre")
	  *board* (list)
	  *selected* :nil	  ; Index of selected peg
	  *undo_stack* (list)
	  *pegs_left* 0
	  *game_over* :nil
	  *font* (create-font "fonts/OpenSans-Bold.ctf" 24)
	  *running* :t)

; Helper function defined before UI creation for use in the layout loop
(defun is-valid-pos (i)
	(defq x (% i +grid_w) y (/ i +grid_w))
	; English Cross layout: Valid if x in [2,4] OR y in [2,4]
	(or (and (>= x 2) (<= x 4))
		(and (>= y 2) (<= y 4))))

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Peg Solitaire" (0xea19) +event_close)
	(ui-tool-bar *toolbar* ()
		(ui-buttons (0xe972 0xe9fe) +event_reset))
	
	; Stack layout: Backdrop is behind, Grid is in front
	(ui-flow *board_view* (:flow_flags +flow_stack_fill)
		(ui-grid *grid* (:grid_width +grid_w :grid_height +grid_h 
						 :color 0 :font *font*)
			(each (lambda (i)
				(if (is-valid-pos i)
					; Valid position: Interactive button
					(. (ui-button _ (:min_width 40 :min_height 40)) 
					   :connect (+ +event_click i))
					; Invalid position: Solid backdrop (visual only)
					(ui-backdrop _ (:color +argb_grey3 :min_width 40 :min_height 40))))
				(range 0 +tile_count)))
		(ui-backdrop _ (:color +argb_grey3)))
				
	(ui-label *status* (:text "Pegs: 32" :flow_flags +flow_flag_align_hcenter)))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *toolbar* '("reset game" "undo move")))

(defun config-default ()
	; Create standard English board (33 holes)
	(defq b (list))
	(each (lambda (i)
		(if (is-valid-pos i)
			(push b +state_peg)
			(push b +state_invalid))) (range 0 +tile_count))
	; Clear center
	(elem-set b (+ 3 (* 3 +grid_w)) +state_empty)
	(scatter (Emap)
		:board b
		:selected :nil
		:undo (list)))

(defun update-view ()
	(defq children (. *grid* :children)
		  i 0)
	(while (< i +tile_count)
		(defq val (elem-get *board* i))
		; Only update widgets corresponding to valid game positions (Buttons)
		(when (/= val +state_invalid)
			(defq btn (elem-get children i))
			(cond
				((= val +state_empty)
					(def btn :text "" :color +argb_grey2 :ink_color +argb_black))
				((= val +state_peg)
					(def btn :text "O" :ink_color +argb_white)
					(if (eql i *selected*)
						(def btn :color +argb_yellow :ink_color +argb_black)
						(def btn :color +argb_grey6))))
			(.-> btn (:constrain :t) :dirty))
		(++ i))
	(if *game_over*
		(def *status* :text (if (and (= *pegs_left* 1) (= (elem-get *board* 24) +state_peg)) 
								"Perfect! (Center)" "Finished!") 
					 :color +argb_green)
		(def *status* :text (cat "Pegs: " (str *pegs_left*)) :color *env_window_col*))
	(.-> *status* :layout :dirty))

(defun update-gamestate ()
	(setq *pegs_left* (length (filter (# (= %0 +state_peg)) *board*))
		  *game_over* (if (= *pegs_left* 1) :t :nil)))

(defun init-game ()
	(defq defaults (config-default))
	(setq *board* (. defaults :find :board)
		  *undo_stack* (list)
		  *selected* :nil
		  *game_over* :nil)
	(update-gamestate)
	(update-view))

(defun config-load ()
	(if (and (defq data (if (defq stream (file-stream *config_file*)) (tree-load stream)))
			 (= (length (. data :find :board)) +tile_count))
		(setq *board* (. data :find :board)
			  *undo_stack* (ifn (defq u (. data :find :undo)) (list) u)
			  *selected* (. data :find :selected))
		(init-game))
	(update-gamestate)
	(update-view))

(defun config-save ()
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream (scatter (Emap)
			:board *board*
			:selected *selected*
			:undo *undo_stack*))))

(defun try-move (to_idx)
	(defq from_idx *selected*)
	(if (eql from_idx to_idx)
		; Clicked self -> deselect
		(progn (setq *selected* :nil) (update-view))
		; Else try logic
		(progn
			(defq x1 (% from_idx +grid_w) y1 (/ from_idx +grid_w)
				  x2 (% to_idx +grid_w) y2 (/ to_idx +grid_w)
				  dx (- x2 x1) dy (- y2 y1))
			
			; Valid jump? Distance must be 2 in one axis, 0 in other
			(if (or (and (= (abs dx) 2) (= dy 0))
					(and (= (abs dy) 2) (= dx 0)))
				(progn
					; Calculate middle index
					(defq mid_idx (+ x1 (/ dx 2) (* (+ y1 (/ dy 2)) +grid_w)))
					
					; Check: To is empty, Mid has peg
					(if (and (= (elem-get *board* to_idx) +state_empty)
							 (= (elem-get *board* mid_idx) +state_peg))
						(progn
							; Save state for Undo
							(push *undo_stack* (list *selected* (copy *board*)))
							; Execute Move
							(elem-set *board* from_idx +state_empty) ; Remove from origin
							(elem-set *board* mid_idx +state_empty)  ; Remove jumped
							(elem-set *board* to_idx +state_peg)	 ; Place at dest
							; Update
							(setq *selected* :nil)
							(update-gamestate)
							(update-view))
						; Invalid logic target
						(progn (setq *selected* :nil) (update-view))))
				; Invalid distance
				(progn (setq *selected* :nil) (update-view))))))

(defun handle-click (index)
	(defq val (elem-get *board* index))
	(cond
		(*game_over* :nil)
		((= val +state_peg)
			; Toggle selection: if clicking already selected, deselect it
			(setq *selected* (if (eql *selected* index) :nil index))
			(update-view))
		((= val +state_empty)
			; Clicked empty slot - try to move selected there
			(if *selected*
				(try-move index)
				(update-view))) ; Deselect if clicked empty with nothing selected
		(:t :nil))) ; Invalid slot

(defun do-undo ()
	(when (nempty? *undo_stack*)
		(defq state (pop *undo_stack*))
		(setq *selected* (first state)
			  *board* (second state))
		(update-gamestate)
		(update-view)))

(defun dispatch-action (id)
	(cond
		((= id +event_close)
			(setq *running* :nil))
		((= id +event_reset)
			(init-game))
		((= id +event_undo)
			(do-undo))
		((>= id +event_click)
			(handle-click (- id +event_click)))))

(defun main ()
	(defq select (task-mboxes +select_size))
	(config-load)
	(tooltips)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			((= (getf msg +ev_msg_type) +ev_type_action)
				(dispatch-action (getf msg +ev_msg_target_id)))
			(:t (. *window* :event msg))))
	
	(config-save)
	(gui-sub-rpc *window*))