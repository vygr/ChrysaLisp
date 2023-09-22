;;;;;;;;;;;;;;;
; macro actions
;;;;;;;;;;;;;;;

(defq +macro_map (Lmap-kv 0 (list) 1 (list) 2 (list) 3 (list) 4 (list)
	5 (list) 6 (list) 7 (list) 8 (list) 9 (list)))

(defun macro-playback (actions)
	(some! 0 -1 :t (# (cond
			((or (eql (defq func (first %0)) (const action-find-up))
					(eql func (const action-find-down)))
				(setq running (eval %0)))
			(:t (eval %0) :t)))
		(list actions)))

(defun macro-record (action)
	(push (. +macro_map :find 0) action))

(defun action-macro-playback (&optional slot)
	(setd slot 0)
	(when (and (defq actions (. +macro_map :find slot))
			(> (length actions) 0))
		(defq buffer (. *edit* :get_buffer) running :t)
		(push *refresh_mode* 1)
		(undoable (macro-playback actions))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-to-eof (&optional slot)
	(setd slot 0)
	(when (and (defq actions (. +macro_map :find slot))
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

(each (# (def (penv (penv))
		(sym (str "action-macro-playback-" %0))
			`(,lambda () (,action-macro-playback ,%0))))
	+range_0_10)

(defq +playback_funcs `'(,action-macro-playback-0 ,action-macro-playback-1
	,action-macro-playback-2 ,action-macro-playback-3 ,action-macro-playback-4
	,action-macro-playback-5 ,action-macro-playback-6 ,action-macro-playback-7
	,action-macro-playback-8 ,action-macro-playback-9))

(defun is-playback-func? (f)
	(some (# (if (eql (first f) %0) _)) +playback_funcs))

(defun action-macro-save (slot)
	;check if recursive !
	(. +macro_map :insert slot (cat (. +macro_map :find 0)))
	(defq stack (list slot) cnt 10000)
	(while (and (> (setq cnt (dec cnt)) 0) (defq s (pop stack)))
		(setq stack (cat stack (filter (const num?)
			(map (const is-playback-func?) (. +macro_map :find s))))))
	(when (= cnt 0)
		(.-> +macro_map (:insert 0 (list)) (:insert slot (list)))
		(throw "Edit macro recursive error !" slot)))

(each (# (def (penv (penv))
		(sym (str "action-macro-save-" %0))
			`(,lambda () (,action-macro-save ,%0))))
	+range_0_10)

(defun action-macro-record ()
	(if (setq *macro_record* (not *macro_record*))
		(clear (. +macro_map :find 0)))
	(radio-select macro_toolbar (list :nil :nil :nil *macro_record*))
	(refresh))

(defun encode-macro (n)
	(if (empty? (defq m (reduce
			(lambda (m (f &rest p))
				(if (eql f action-insert)
					(elem-set 0 p (id-encode (elem-get 0 p))))
				(if (some (# (eql %0 f)) *find_actions*)
					(elem-set 2 p (id-encode (elem-get 2 p))))
				(if (some (# (eql %0 f)) *replace_actions*)
					(elem-set 3 p (id-encode (elem-get 3 p))))
				(push m (str (cat (list (find f *recorded_actions*)) p))))
			(. +macro_map :find n) (list))))
		"" (join m "|")))

(defun decode-macro (n e)
	(. +macro_map :insert n (map
			(# (bind '(f &rest p) (first (read (string-stream %0))))
				(setq f (elem-get f *recorded_actions*))
				(if (eql f action-insert)
					(elem-set 0 p (id-decode (elem-get 0 p))))
				(if (some (# (eql %0 f)) *find_actions*)
					(elem-set 2 p (id-decode (elem-get 2 p))))
				(if (some (# (eql %0 f)) *replace_actions*)
					(elem-set 3 p (id-decode (elem-get 3 p))))
				(cat (list f) p))
		(split e "|"))))
