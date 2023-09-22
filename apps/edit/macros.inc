;;;;;;;;;;;;;;;
; macro actions
;;;;;;;;;;;;;;;

(defq +macro_set (Fmap-kv "0" (list)))

(defun macro-playback (actions)
	(some! 0 -1 :t (# (defq func (elem-get 0 %0))
			(cond
				((or (eql func (const action-find-up))
						(eql func (const action-find-down)))
					(setq running (eval %0)))
				(:t (eval %0) :t)))
		(list actions)))

(defun macro-record (action)
	(push (. +macro_set :find "0") action))

(defun action-macro-save (slot)
	(. +macro_set :insert slot
		(cat (. +macro_set :find "0"))))

(defun action-macro-record ()
	(if (setq *macro_record* (not *macro_record*))
		(clear (. +macro_set :find "0")))
	(radio-select macro_toolbar (list :nil :nil :nil *macro_record*))
	(refresh))

(defun action-macro-playback (&optional slot)
	(setd slot "0")
	(when (and (defq actions (. +macro_set :find slot))
			(> (length actions) 0))
		(defq buffer (. *edit* :get_buffer) running :t)
		(push *refresh_mode* 1)
		(undoable (macro-playback actions))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-to-eof (&optional slot)
	(setd slot "0")
	(when (and (defq actions (. +macro_set :find slot))
			(> (length actions) 0))
		(defq buffer (. *edit* :get_buffer) running :t)
		(push *refresh_mode* 1)
		(undoable
			(bind '(cx cy) (. buffer :get_cursor))
			(bind '(eof_gap_x eof_gap_y) (eof-gaps))
			(defq old_eof_gap_x +max_int  old_eof_gap_y +max_int)
			(while (and running (> eof_gap_y 0)
						(or (< eof_gap_y old_eof_gap_y)
							(and (= eof_gap_y old_eof_gap_y) (< eof_gap_x old_eof_gap_x)))
						(not (some! cy -1 :t (const blank-line?)
							(list (. buffer :get_text_lines)))))
				(macro-playback actions)
				(when running
					(setq old_eof_gap_x eof_gap_x old_eof_gap_y eof_gap_y)
					(bind '(cx cy) (. buffer :get_cursor))
					(bind '(eof_gap_x eof_gap_y) (eof-gaps)))))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-global (&optional slot)
	(for-all-buffers
		(.-> *edit* (:set_cursor 0 0) (:set_anchor 0 0) (:set_find 0 0 0 0))
		(action-macro-to-eof slot)))

(defun action-macro-playback-1 () (action-macro-playback "1"))
(defun action-macro-playback-2 () (action-macro-playback "2"))
(defun action-macro-playback-3 () (action-macro-playback "3"))
(defun action-macro-playback-4 () (action-macro-playback "4"))
(defun action-macro-playback-5 () (action-macro-playback "5"))
(defun action-macro-playback-6 () (action-macro-playback "6"))
(defun action-macro-playback-7 () (action-macro-playback "7"))
(defun action-macro-playback-8 () (action-macro-playback "8"))
(defun action-macro-playback-9 () (action-macro-playback "9"))
(defun action-macro-save-1 () (action-macro-save "1"))
(defun action-macro-save-2 () (action-macro-save "2"))
(defun action-macro-save-3 () (action-macro-save "3"))
(defun action-macro-save-4 () (action-macro-save "4"))
(defun action-macro-save-5 () (action-macro-save "5"))
(defun action-macro-save-6 () (action-macro-save "6"))
(defun action-macro-save-7 () (action-macro-save "7"))
(defun action-macro-save-8 () (action-macro-save "8"))
(defun action-macro-save-9 () (action-macro-save "9"))
