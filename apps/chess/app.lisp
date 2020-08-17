;jit compile apps native functions if needed
(import 'lib/asm/asm.inc)
(make 'apps/chess/lisp.vp)

;imports
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close)
	(byte 'button))

;create child and send args etc
(defq squares (list) next_char (ascii-code " ")
	data_in (in-stream) select (array (task-mailbox) (in-mbox data_in))
	vdu_width 38 vdu_height 12 text_buf (list ""))

(ui-window window (:color argb_black)
	(ui-flow _ (:flow_flags flow_down_fill)
		(ui-title-bar _ "Chess" (0xea19) (const event_close))
		(ui-grid chess_grid (:grid_width 8 :grid_height 8 :font (create-font "fonts/Chess.ctf" 42) :border 1 :text " ")
			(each (lambda (i)
				(if (= (logand (+ i (>> i 3)) 1) 0)
					(defq paper argb_white ink argb_black)
					(defq paper argb_black ink argb_white))
				(push squares (ui-button _ (:color paper :ink_color ink)))) (range 0 64)))
		(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color argb_cyan))))

(defun-bind display-board (board)
	(each (lambda (square piece)
		(def square :text (elem (find piece "QKRBNPqkrbnp ")
			(if (= (logand (+ _ (>> _ 3)) 1) 0) "wltvmoqkrbnp " "qkrbnpwltvmo ")))
		(view-layout square)) squares board)
	(view-dirty-all chess_grid))

(defun-bind vdu-print (vdu buf s)
	(each (lambda (c)
		(cond
			((eql c (ascii-char 10))
				;line feed and truncate
				(if (> (length (push buf "")) (const vdu_height))
					(setq buf (slice (const (dec (neg vdu_height))) -1 buf))))
			(t	;char
				(elem-set -2 buf (cat (elem -2 buf) c))))) s)
	(vdu-load vdu buf 0 0 (length (elem -2 buf)) (dec (length buf))) buf)

(defun-bind main ()
	(mail-send (array (in-mbox data_in) 10000000)
		(defq child_mbox (open-child "apps/chess/child.lisp" kn_call_child)))
	(bind '(x y w h) (apply view-locate (view-pref-size window)))
	(gui-add (view-change window x y w h))
	;main event loop
	(while (cond
		((= (mail-select select) 0)
			;GUI event from main mailbox
			(cond
				((= (get-long (defq msg (mail-read (elem 0 select))) ev_msg_target_id) event_close)
					nil)
				(t (view-event window msg))))
		(t	;from child stream
			(bind '(data next_char) (read data_in next_char))
			(cond
				((eql (defq id (elem 0 data)) "b")
					(display-board (slice 1 -1 data)))
				((eql id "c")
					(setq text_buf (list ""))
					(vdu-print vdu text_buf (slice 1 -1 data)))
				((eql id "s")
					(setq text_buf (vdu-print vdu text_buf (slice 1 -1 data))))))))
	;close child and window, wait for child stream to close
	(mail-send "" child_mbox)
	(view-hide window)
	(until id
		(setq id (mail-select select))
		(cond
			((= id 0)
				;GUI event from main mailbox
				(mail-read (elem 0 select)))
			(t	;from child stream
				(bind '(data next_char) (read data_in next_char))
				(setq id (= next_char -1))))))
