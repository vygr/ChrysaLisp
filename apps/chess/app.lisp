;jit compile apps native functions if needed
(let ()
	(import 'cmd/asm.inc)
	(make 'apps/chess/lisp.vp))

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

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

;create child and send args etc
(defq id t msg_in (msg-in-stream) select (array (task-mailbox) (msg-in-mbox msg_in)))
(mail-send (array (msg-in-mbox msg_in) 2000000)
	(defq child_mbox (open-child "apps/chess/child.lisp" kn_call_child)))

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
			(bind '(data _) (read msg_in (const (ascii-code " "))))
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
			(bind '(data _) (read msg_in (const (ascii-code " "))))
			(setq id (eql data "")))))
