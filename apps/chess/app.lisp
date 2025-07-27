(import "././login/env.inc")
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
	history (list brd) color +white start_time (pii-time) replys (list) next_seq 0)

(ui-window *window* (:color +argb_black)
	(ui-title-bar _ "Chess" (0xea19) +event_close)
	(ui-grid chess_grid (:grid_width 8 :grid_height 8 :font (create-font "fonts/Chess.ctf" 42)
			:border 1 :text " ")
		(each (lambda (i)
			(if (= (logand (+ i (>> i 3)) 1) 0)
				(defq paper +argb_white ink +argb_black)
				(defq paper +argb_black ink +argb_white))
			(ui-button _ (:color paper :ink_color ink))) (range 0 64)))
	(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color +argb_cyan)))

(defun display-board (board)
	(each (lambda (square piece)
		(def square :text (elem-get
			(if (= (logand (+ (!) (>> (!) 3)) 1) 0) "wltvmoqkrbnp " "qkrbnpwltvmo ")
			(find piece "QKRBNPqkrbnp ")))
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
	(mail-send (get :child val)
		(setf-> (cat (str-alloc +job_size) brd (apply (const cat) history))
			(+job_reply (elem-get select +select_reply))
			(+job_move_time max_move_time)
			(+job_color color)))
	;update display
	(setq text_buf (vdu-print vdu (list "")
		(cat (LF) "Elapsed Time: " (time-in-seconds (- (pii-time) start_time)) (LF)
			(if (= color +white) "White to move:" "Black to move:") (LF))))
	;reset reply sequence
	(clear replys)
	(setq next_seq 0))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/chess/child.lisp" (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(mail-free-mbox (elem-get select +select_reply))
	(elem-set select +select_reply (mail-alloc-mbox)))

(defun main ()
	(display-board brd)
	(defq select (alloc-select +select_size) farm (Farm create destroy 1))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_main)
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					(:t (. *window* :event msg))))
			((= idx +select_task)
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
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
							(. farm :close)
							(setq farm (Farm create destroy 1)))
						;end
						((= data_type (ascii-code "e"))
							(setq text_buf (vdu-print vdu (list "") data))
							(. farm :close))
						;status
						((= data_type (ascii-code "s"))
							(vdu-print vdu text_buf data)))))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) timer_rate 0)
				(. farm :refresh (+ max_move_time 1000000)))))
	;close window and children
	(. farm :close)
	(free-select select)
	(gui-sub-rpc *window*))
