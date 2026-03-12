(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/colors.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close)
	(enum button))

(enums +select 0
	(enum main task reply timer))

(defq vdu_width 38 vdu_height 12 text_buf :nil
	flicker_rate (/ 1000000 8) timer_rate (/ 1000000 1) max_move_time 10000000 id :t
	brd "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr"
	history (list brd) color +white start_time (pii-time) last_move_time 0 replys (list) next_seq 0
	human_color +white selected_idx :nil legal_boards (list))

(ui-window *window* (:color +argb_black)
	(ui-title-bar _ "Chess" (0xea19) +event_close)
	(ui-grid chess_grid (:grid_width 8 :grid_height 8 :font (create-font "fonts/Chess.ctf" 42)
			:border 1 :text " ")
		(each (lambda (i)
			(. (ui-button _ (:color 0 :ink_color 0)) :connect (+ +event_button i))) (range 0 64)))
	(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color +argb_cyan)))

(defun valid-dests (selected_idx)
	(defq dests (list))
	(each (lambda (lbrd)
		(when (eql (elem-get lbrd selected_idx) " ")
			(defq dest :nil)
			(each (lambda (i)
				(when (and (/= i selected_idx) (nql (elem-get lbrd i) (elem-get brd i)))
					(setq dest i))) (range 0 64))
			(if dest (push dests dest))))
		legal_boards)
	(unique dests))

(defun display-board (board)
	(defq dests (if selected_idx (valid-dests selected_idx) (list)))
	(each (lambda (square piece)
		(defq i (!))
		(def square :text (elem-get
			(if (= (logand (+ i (>> i 3)) 1) 0) "wltvmoqkrbnp " "qkrbnpwltvmo ")
			(find piece "QKRBNPqkrbnp ")))
		(if (= (logand (+ i (>> i 3)) 1) 0)
			(defq paper +argb_white ink +argb_black)
			(defq paper +argb_black ink +argb_white))
		(if (eql i selected_idx)
			(setq paper +argb_yellow ink +argb_black))
		(if (find i dests)
			(setq paper +argb_green ink +argb_black))
		(def square :color paper :ink_color ink)
		(. square :constrain :t)) (. chess_grid :children) board)
	(. chess_grid :dirty_all))

(defun vdu-print (vdu buf s)
	(each (lambda (c)
		(cond
			((eql c (ascii-char 10))
				;line feed and truncate
				(if (> (length (push buf "")) (const vdu_height))
					(setq buf (slice buf (const (dec (neg vdu_height))) -1))))
			(:t ;char
				(elem-set buf -2 (cat (last buf) c))))) s)
	(. vdu :load buf 0 0 (length (last buf)) (dec (length buf))) buf)

(defun dispatch-job (key val)
	;send job to child
	(def val :timestamp (pii-time))
	(defq job_type (if (= color human_color) +job_type_get_moves +job_type_move))
	(mail-send (get :child val)
		(setf-> (cat (str-alloc +job_size) brd (apply (const cat) history))
			(+job_reply (elem-get select +select_reply))
			(+job_move_time max_move_time)
			(+job_type job_type)
			(+job_color color)))
	;update display
	(setq text_buf (vdu-print vdu (list "")
		(cat (LF) "Last Move: " (time-in-seconds last_move_time) " s" (LF)
			(if (= color +white) "White to move:" "Black to move:") (LF))))
	;reset reply sequence
	(clear replys)
	(setq next_seq 0))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task (const (cat *app_root* "child.lisp")) (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(elem-set select +select_reply (mail-mbox)))

(defun main ()
	(display-board brd)
	(defq select (task-mboxes +select_size) farm (Farm create destroy 1))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_main)
				;main mailbox
				(defq target_id (getf msg +ev_msg_target_id))
				(cond
					((= target_id +event_close)
						;close button
						(setq id :nil))
					((>= target_id +event_button)
						;board click
						(defq board_idx (- target_id +event_button))
						(if (= color human_color)
							(cond
								((eql selected_idx board_idx)
									;deselect
									(setq selected_idx :nil)
									(display-board brd))
								((find board_idx (if selected_idx (valid-dests selected_idx) (list)))
									;make move
									;find the matching board in legal_boards
									(defq target_board :nil)
									(each (lambda (lbrd)
										(when (and (eql (elem-get lbrd selected_idx) " ")
												(nql (elem-get lbrd board_idx) (elem-get brd board_idx)))
											(unless target_board (setq target_board lbrd))))
										legal_boards)
									(setq selected_idx :nil legal_boards (list))
									(push history (setq color (neg color) brd target_board))
									(display-board brd)
									;start engine farm
									(setq last_move_time (- (pii-time) start_time) start_time (pii-time) farm (Farm create destroy 1)))
								(:t ;select piece if it belongs to human
									(defq piece (elem-get brd board_idx) is_black (= color +black))
									(when (and (nql piece " ") (eql (< (code piece) (ascii-code "Z")) is_black))
										(setq selected_idx board_idx)
										(display-board brd)))))
						(. *window* :event msg))
					(:t (. *window* :event msg))))
			((= idx +select_task)
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (and farm (defq val (. farm :find key)))
					(def val :child child)
					(dispatch-job key val)))
			((= idx +select_reply)
				;child reply, process in sequence order
				(sort (push replys msg) (# (- (getf %1 +reply_seq) (getf %0 +reply_seq))))
				(while (and (/= (length replys) 0)
							(= (getf (last replys) +reply_seq) next_seq))
					(setq msg (pop replys) next_seq (inc next_seq))
					(defq data_type (getf msg +reply_type) data (slice msg +reply_data -1))
					(cond
						;move
						((= data_type (ascii-code "b"))
							(times 3
								(display-board brd)
								(task-sleep flicker_rate)
								(display-board data)
								(task-sleep flicker_rate))
							(push history (setq color (neg color) brd data))
							(if farm (. farm :close))
							(setq last_move_time (- (pii-time) start_time) start_time (pii-time) farm (Farm create destroy 1)))
						;moves for human
						((= data_type (ascii-code "m"))
							(setq legal_boards (partition data 64))
							(if farm (. farm :close))
							(setq farm :nil))
						;end
						((= data_type (ascii-code "e"))
							(setq text_buf (vdu-print vdu (list "") data))
							(if farm (. farm :close))
							(setq farm :nil))
						;status
						((= data_type (ascii-code "s"))
							(setq text_buf (vdu-print vdu text_buf data))))))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) timer_rate 0)
				(if farm (. farm :refresh (+ max_move_time 2000000))))))
	;close window and children
	(if farm (. farm :close))
	(gui-sub-rpc *window*))