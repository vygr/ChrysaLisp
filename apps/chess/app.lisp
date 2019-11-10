;jit compile apps native functions if needed
(let ()
	(import 'cmd/asm.inc)
	(make 'apps/chess/lisp.vp))

;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_brd 'win_str 'win_stop)
	(byte 'win_close))

(structure 'child_msg 0
	(long 'id 'seq)
	(offset 'data))

(ui-tree window (create-window window_flag_close) ('color argb_black)
	(ui-element vdu (create-vdu) ('vdu_width 39 'vdu_height 40 'ink_color argb_cyan
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(gui-add (apply view-change (cat (list window 512 128)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Chess")))))

(defun-bind display-board (board)
	(defq d (range 0 8))
	(vdu-print vdu (const (str (ascii-char 128) (ascii-char 10) "    a   b   c   d   e   f   g   h" (ascii-char 10))))
	(vdu-print vdu (str "  +---+---+---+---+---+---+---+---+" (ascii-char 10)))
	(each (lambda (row)
		(vdu-print vdu (str "  " (apply cat (map (lambda (col)
			(cat "| " (elem (+ (* 8 row) col) board) " ")) d)) "| " (- 8 row) (ascii-char 10)))
		(if (/= row 7)
			(vdu-print vdu (str "  |---+---+---+---+---+---+---+---|" (ascii-char 10))))) d)
	(vdu-print vdu (str "  +---+---+---+---+---+---+---+---+" (ascii-char 10))))

;create child and send args
(mail-send (array (task-mailbox) 2000000)
	(defq mbox (open-child "apps/chess/child.lisp" kn_call_child)))

(defq id t msg_seq 0 msg_que (list))

(defun-bind get-child-msg (msg)
	(when msg
		(push msg_que msg)
		(sort (lambda (x y)
			(- (get-long x child_msg_seq) (get-long y child_msg_seq))) msg_que))
	(when (and (/= (length msg_que) 0) (= (get-long (elem -2 msg_que) child_msg_seq) msg_seq))
		(setq msg_seq (inc msg_seq))
		(pop msg_que)))

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((<= event_win_brd id event_win_stop)
			;read sequenced msg que
			(while (setq msg (get-child-msg msg))
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) event_win_brd)
						(display-board (slice child_msg_data -1 msg)))
					((= id event_win_str)
						(vdu-print vdu (slice child_msg_data -1 msg))))
				(setq msg nil)))
		(t (view-event window msg))))

;close child and window, wait for child to close
(mail-send "" mbox)
(view-hide window)
(until id
	(when (<= event_win_brd (get-long (setq msg (mail-read (task-mailbox))) ev_msg_target_id) event_win_stop)
		;read sequenced msg que
		(while (setq msg (get-child-msg msg))
			(if (= (get-long msg ev_msg_target_id) event_win_stop)
				(setq id t)
				(setq msg nil)))))
