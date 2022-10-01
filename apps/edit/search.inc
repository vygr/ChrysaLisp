;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(radio-select find_toolbar (if *whole_words* 2 -1)))

(defun filter-whole-words (found pattern)
	(cond
		(*whole_words*
			(defq pattern_len (length pattern) out (cap (length found) (list)))
			(each (lambda (line xs)
					(defq out_xs (list))
					(each (lambda (x)
						(defq x1 (+ x pattern_len))
						(if (cond
								((eql pattern line))
								((= x 0) (not-whole-word-char? (elem-get x1 line)))
								((= x1 (length line)) (not-whole-word-char? (elem-get (dec x) line)))
								((and (not-whole-word-char? (elem-get (dec x) line)))
									(not-whole-word-char? (elem-get x1 line))))
							(push out_xs x))) xs)
					(push out out_xs))
				(.-> *edit* :get_buffer :get_text_lines) found)
			  out)
		(found)))

(defun action-find-down ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (filter-whole-words (. buffer :find pattern) pattern))) 0)
			(bind '(cx cy) (. *edit* :get_cursor))
			(when (defq next (some! cy -1 :nil (lambda (xs)
					(defq y _)
					(some (lambda (x) (cond
						((> y cy) (list x y))
						((>= x cx) (list x y)))) xs)) (list found)))
				(bind '(x y) next)
				(. *edit* :set_anchor x y)
				(. buffer :set_cursor (+ x (length pattern)) y)
				(refresh)))))

(defun action-find-up ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (filter-whole-words (. buffer :find pattern) pattern))) 0)
			(bind '(cx cy) (. *edit* :get_cursor))
			(when (defq next (some! (inc cy) 0 :nil (lambda (xs)
					(defq y _)
					(some-rev (lambda (x) (cond
						((< y cy) (list x y))
						((< x cx) (list x y)))) xs)) (list found)))
				(bind '(x y) next)
				(. *edit* :set_anchor (+ x (length pattern)) y)
				(. buffer :set_cursor x y)
				(refresh)))))

(defun action-set-find-text ()
	(when (selection?)
		(bind '(ax ay) (. *edit* :get_anchor))
		(when (> (length (defq text
				(split (.-> *edit* :get_buffer (:copy ax ay)) (ascii-char +char_lf)))) 0)
			(setq text (elem-get 0 text))
			(set *find_text* :clear_text text :cursor (length text) :anchor 0)
			(.-> *find_text* :layout :dirty))))

(defun action-replace ()
	(unless (or (eql (defq pattern (get :clear_text *find_text*)) "")
				(eql (defq replace (get :clear_text *replace_text*)) pattern))
		(bind '(ax ay) (. *edit* :get_anchor))
		(when (eql (.-> *edit* :get_buffer (:copy ax ay)) pattern)
			(undoable :nil
				(. buffer :cut ax ay)
				(. buffer :insert replace))
			(refresh))
		(action-find-down)))

(defun action-replace-all ()
	(unless (or (eql (defq pattern (get :clear_text *find_text*)) "")
				(eql (defq replace (get :clear_text *replace_text*)) pattern))
		(when (> (length (defq found (filter-whole-words (.-> *edit* :get_buffer (:find pattern)) pattern))) 0)
			(undoable :nil
				(each-rev (lambda (xs)
					(defq y _)
					(each-rev (lambda (x)
						(.-> buffer (:set_cursor x y) (:cut (+ x (length pattern)) y))
						(unless (eql replace "") (. buffer :insert replace))) xs)) found))
			(bind '(cx cy) (. *edit* :get_cursor))
			(.-> *edit* (:set_cursor cx cy) (:set_anchor cx cy))
			(refresh))))
