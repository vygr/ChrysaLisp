;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/consts/colors.inc")
(import "lib/task/farm.inc")
(import "apps/chess/app.inc")

(structure '+event 0
	(byte 'close+)
	(byte 'button+))

(structure '+select 0
	(byte 'main+ 'task+ 'reply+ 'timer+))

(defq vdu_width 38 vdu_height 12 text_buf nil
	flicker_rate (/ 1000000 8) timer_rate (/ 1000000 1) max_move_time 10000000 id t
	select (list (task-mailbox) (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
	brd "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr"
	history (list brd) color +white+ start_time (pii-time) replys (list) next_seq 0)

(ui-window mywindow (:color +argb_black+)
	(ui-flow _ (:flow_flags +flow_down_fill+)
		(ui-title-bar _ "Chess" (0xea19) +event_close+)
		(ui-grid chess_grid (:grid_width 8 :grid_height 8 :font (create-font "fonts/Chess.ctf" 42)
				:border 1 :text " ")
			(each (lambda (i)
				(if (= (logand (+ i (>> i 3)) 1) 0)
					(defq paper +argb_white+ ink +argb_black+)
					(defq paper +argb_black+ ink +argb_white+))
				(ui-button _ (:color paper :ink_color ink))) (range 0 64)))
		(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color +argb_cyan+))))

(defun display-board (board)
	(each (lambda (square piece)
		(def square :text (elem (find piece "QKRBNPqkrbnp ")
			(if (= (logand (+ _ (>> _ 3)) 1) 0) "wltvmoqkrbnp " "qkrbnpwltvmo ")))
		(. square :layout)) (. chess_grid :children) board)
	(. chess_grid :dirty_all))

(defun vdu-print (vdu buf s)
	(each (lambda (c)
		(cond
			((eql c (ascii-char 10))
				;line feed and truncate
				(if (> (length (push buf "")) (const vdu_height))
					(setq buf (slice (const (dec (neg vdu_height))) -1 buf))))
			(t	;char
				(elem-set -2 buf (cat (elem -2 buf) c))))) s)
	(. vdu :load buf 0 0 (length (elem -2 buf)) (dec (length buf))) buf)

(defun time-in-seconds (_)
	(str (/ _ 1000000) "." (pad (% _ 1000000) 6 "00000")))

(defun dispatch-job (key val)
	;send job to child
	(. val :insert :timestamp (pii-time))
	(mail-send (. val :find :child) (cat
		(elem +select_reply+ select)
		(char max_move_time +long_size+)
		(char color)
		brd (apply cat history)))
	;update display
	(setq text_buf (vdu-print vdu (list "")
		(cat (LF) "Elapsed Time: " (time-in-seconds (- (pii-time) start_time)) (LF)
			(if (= color +white+) "White to move:" "Black to move:") (LF))))
	;reset reply sequence
	(clear replys)
	(setq next_seq 0))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/chess/child.lisp" (elem (random (length nodes)) nodes)
		kn_call_child key (elem +select_task+ select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. val :find :child)) (mail-send child ""))
	(mail-free-mbox (elem +select_reply+ select))
	(elem-set +select_reply+ select (mail-alloc-mbox)))

(defun main ()
	(display-board brd)
	(defq farm (Farm create destroy 1))
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(mail-timeout (elem +select_timer+ select) timer_rate)
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main+)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) +event_close+)
						;close button
						(setq id nil))
					(t (. mywindow :event msg))))
			((= idx +select_task+)
				;child launch responce
				(defq key (get-long msg 0) child (slice +long_size+ (const (+ +long_size+ net_id_size)) msg))
				(when (defq val (. farm :find key))
					(. val :insert :child child)
					(dispatch-job key val)))
			((= idx +select_reply+)
				;child reply, process in sequence order
				(sort (# (- (get-long %1 0) (get-long %0 0))) (push replys msg))
				(while (and (/= (length replys) 0)
							(= (get-long (elem -2 replys) 0) next_seq))
					(setq msg (pop replys) next_seq (inc next_seq))
					(defq data_type (elem +long_size+ msg)
						data (slice (const (inc +long_size+)) -1 msg))
					(cond
						;move
						((eql data_type "b")
							(each (lambda (_)
								(display-board brd)
								(task-sleep flicker_rate)
								(display-board data)
								(task-sleep flicker_rate)) (range 0 2))
							(push history (setq color (neg color) brd data))
							(. farm :close)
							(setq farm (Farm create destroy 1)))
						;end
						((eql data_type "e")
							(setq text_buf (vdu-print vdu (list "") data))
							(. farm :close))
						;status
						((eql data_type "s")
							(vdu-print vdu text_buf data)))))
			(t	;timer event
				(mail-timeout (elem +select_timer+ select) timer_rate)
				(. farm :refresh (+ max_move_time 1000000)))))
	;close window and children
	(. farm :close)
	(each mail-free-mbox (slice 1 -1 select))
	(. mywindow :hide))
