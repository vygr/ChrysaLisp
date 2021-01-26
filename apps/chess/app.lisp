;jit compile apps native functions if needed
(import "lib/asm/asm.inc")
(bind '(_ *cpu* *abi*) (split (load-path) "/"))
(make '("apps/chess/lisp.vp") *abi* *cpu*)

;imports
(import "gui/lisp.inc")
(import "lib/consts/colors.inc")
(import "apps/chess/app.inc")

(structure '+event 0
	(byte 'close+)
	(byte 'button+))

(structure '+select 0
	(byte 'main+ 'reply+))

;create child and send args etc
(defq squares (list) vdu_width 38 vdu_height 12 text_buf nil flicker 100000
	select (list (task-mailbox) (mail-alloc-mbox)) id t child_mbox nil
	brd "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr"
	history (list brd) white 1 black -1 color white start_time (pii-time))

(ui-window mywindow (:color +argb_black+)
	(ui-flow _ (:flow_flags +flow_down_fill+)
		(ui-title-bar _ "Chess" (0xea19) +event_close+)
		(ui-grid chess_grid (:grid_width 8 :grid_height 8 :font (create-font "fonts/Chess.ctf" 42) :border 1 :text " ")
			(each (lambda (i)
				(if (= (logand (+ i (>> i 3)) 1) 0)
					(defq paper +argb_white+ ink +argb_black+)
					(defq paper +argb_black+ ink +argb_white+))
				(push squares (ui-button _ (:color paper :ink_color ink)))) (range 0 64)))
		(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color +argb_cyan+))))

(defun display-board (board)
	(each (lambda (square piece)
		(def square :text (elem (find piece "QKRBNPqkrbnp ")
			(if (= (logand (+ _ (>> _ 3)) 1) 0) "wltvmoqkrbnp " "qkrbnpwltvmo ")))
		(. square :layout)) squares board)
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

(defun next_move ()
	(setq text_buf (list ""))
	(vdu-print vdu text_buf (cat (LF) "Elapsed Time: " (time-in-seconds (- (pii-time) start_time)) (LF)))
	(if (= color (const white))
		(vdu-print vdu text_buf (cat "White to move:" (LF)))
		(vdu-print vdu text_buf (cat "Black to move:" (LF))))
	(mail-send (setq child_mbox (open-child "apps/chess/child.lisp" kn_call_child))
		(cat
			(elem +select_reply+ select)
			(char 10000000 long_size)
			(char color long_size)
			brd (apply cat history))))

(defun main ()
	(display-board brd)
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(next_move)
	;main event loop
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main+)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) +event_close+)
						(setq id nil))
					(t (. mywindow :event msg))))
			((= idx +select_reply+)
				;child reply
				(cond
					((eql (defq id (elem 0 msg)) "b")
						(defq new_brd (slice 1 -1 msg))
						(each (lambda (_)
							(display-board brd)
							(task-sleep flicker)
							(display-board new_brd)
							(task-sleep flicker)) (range 0 2))
						(setq color (neg color) brd new_brd)
						(push history brd)
						(next_move))
					((eql id "s")
						(setq text_buf (vdu-print vdu text_buf (slice 1 -1 msg))))))))
	;close child and window
	(each mail-free-mbox (slice 1 -1 select))
	(mail-send child_mbox "")
	(. mywindow :hide))
