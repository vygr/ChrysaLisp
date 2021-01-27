;jit compile apps native functions if needed
(import "lib/asm/asm.inc")
(bind '(_ *cpu* *abi*) (split (load-path) "/"))
(make '("apps/chess/lisp.vp") *abi* *cpu*)

;imports
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
	flicker_rate (/ 1000000 10) timer_rate (/ 1000000 1) max_move_time 10000000 id t
	select (list (task-mailbox) (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
	brd "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr"
	history (list brd) color white start_time (pii-time))

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

(defun dispatch-job (child)
	;send job to child
	(defq val (. farm :find child))
	(. val :insert :timestamp (pii-time))
	(mail-send child (cat
		(elem +select_reply+ select)
		(char max_move_time long_size)
		(char color long_size)
		brd (apply cat history)))
	;update display
	(setq text_buf (vdu-print vdu (list "")
		(cat (LF) "Elapsed Time: " (time-in-seconds (- (pii-time) start_time)) (LF))))
	(if (= color (const white))
		(vdu-print vdu text_buf (cat "White to move:" (LF)))
		(vdu-print vdu text_buf (cat "Black to move:" (LF)))))

(defun create (nodes)
	; (create nodes)
	;function called when entry is created
	(open-task "apps/chess/child.lisp" (elem (random (length nodes)) nodes)
		kn_call_child (elem +select_task+ select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(mail-send key "")
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
				(defq child (slice (const long_size) (const (+ long_size net_id_size)) msg))
				(. farm :insert child (emap))
				(dispatch-job child))
			((= idx +select_reply+)
				;child reply
				(cond
					;move
					((eql (defq id (elem 0 msg)) "b")
						(defq new_brd (slice 1 -1 msg))
						(each (lambda (_)
							(display-board brd)
							(task-sleep flicker_rate)
							(display-board new_brd)
							(task-sleep flicker_rate)) (range 0 2))
						(setq color (neg color) brd new_brd)
						(push history brd)
						(. farm :close)
						(setq farm (Farm create destroy 1)))
					;end
					((eql id "e")
						(setq text_buf (vdu-print vdu (list "") (slice 1 -1 msg)))
						(. farm :close))
					;status
					((eql id "s")
						(vdu-print vdu text_buf (slice 1 -1 msg)))))
			(t	;timer event
				(mail-timeout (elem +select_timer+ select) timer_rate)
				(. farm :refresh (+ max_move_time 1000000)))))
	;close window and children
	(. farm :close)
	(each mail-free-mbox (slice 1 -1 select))
	(. mywindow :hide))
