;jit compile apps native functions if needed
(let ()
	(import 'cmd/asm.inc)
	(make 'apps/chess/lisp.vp))

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_button))

;create child and send args etc
(defq id t squares (list) next_char (const (ascii-code " "))
	data_in (msg-in-stream) select (array (task-mailbox) (msg-in-mbox data_in)))
(mail-send (array (msg-in-mbox data_in) 10000000)
	(defq child_mbox (open-child "apps/chess/child.lisp" kn_call_child)))

(ui-tree window (create-window window_flag_close) ('color argb_black)
	(ui-element vdu (create-vdu) ('vdu_width 38 'vdu_height 12 'ink_color argb_cyan
		'font (create-font "fonts/Hack-Regular.ttf" 16)))
	(ui-element chess_grid (create-grid) ('grid_width 8 'grid_height 8
			'font (create-font "fonts/Chess.ttf" 42) 'border 1 'text " ")
		(each (lambda (i)
			(if (= (logand (+ i (>> i 3)) 1) 0)
				(defq paper argb_white ink argb_black)
				(defq paper argb_black ink argb_white))
			(push squares (ui-element _ (create-button)
				('color paper 'ink_color ink)))) (range 0 64))))

(gui-add (apply view-change (cat (list window 512 128)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Chess")))))

(defun-bind display-board (board)
	(each! 0 -1 (lambda (square piece)
		(def square 'text (elem (find piece "QKRBNPqkrbnp ")
			(if (= (logand (+ _ (>> _ 3)) 1) 0) "wltvmoqkrbnp " "qkrbnpwltvmo ")))
		(view-layout square)) (list squares board))
	(view-dirty-all chess_grid))

;main event loop
(while id
	(defq idx (mail-select select))
	(cond
		((= idx 0)
			;GUI event from main mailbox
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
					(setq id nil))
				(t (view-event window msg))))
		(t	;from child stream
			(bind '(data next_char) (read data_in next_char))
			(cond
				((eql (setq id (elem 0 data)) "b")
					(display-board (slice 1 -1 data)))
				((eql id "s")
					(vdu-print vdu (slice 1 -1 data)))))))

;close child and window, wait for child stream to close
(mail-send "" child_mbox)
(view-hide window)
(until id
	(defq idx (mail-select select))
	(cond
		((= idx 0)
			;GUI event from main mailbox
			(mail-read (task-mailbox)))
		(t	;from child stream
			(bind '(data next_char) (read data_in next_char))
			(setq id (= next_char -1)))))
