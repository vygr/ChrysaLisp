;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(radio-select find_toolbar (list :nil *whole_words* *regexp* :nil :nil)))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(radio-select find_toolbar (list :nil *whole_words* *regexp* :nil :nil)))

(defun action-find-down (whole_words regexp pattern)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (. buffer :find pattern whole_words regexp))) 0)
			(bind '(cx cy) (. *edit* :get_cursor))
			(when (defq next (some! cy -1 :nil (lambda ((indexes substrs))
					(defq y _)
					(some (lambda ((x x1)) (cond
						((> y cy) (list x x1 y))
						((>= x cx) (list x x1 y)))) indexes)) (list found)))
				(bind '(x x1 y) next)
				(. *edit* :set_anchor x y)
				(. buffer :set_cursor x1 y)
				(refresh)))))

(defun action-find-up (whole_words regexp pattern)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (. buffer :find pattern whole_words regexp))) 0)
			(bind '(cx cy) (. *edit* :get_cursor))
			(when (defq next (some! (inc cy) 0 :nil (lambda ((indexes substrs))
					(defq y _)
					(some-rev (lambda ((x x1)) (cond
						((< y cy) (list x x1 y))
						((< x cx) (list x x1 y)))) indexes)) (list found)))
				(bind '(x x1 y) next)
				(. *edit* :set_anchor x1 y)
				(. buffer :set_cursor x y)
				(refresh)))))

(defun action-set-find-text ()
	(when (selection?)
		(bind '(ax ay) (. *edit* :get_anchor))
		(when (> (length (defq text
				(split (.-> *edit* :get_buffer (:copy ax ay)) (ascii-char +char_lf)))) 0)
			(setq text (elem-get 0 text))
			(.-> *find_text* (:set_text text) :layout :dirty))))

(defun build-replace (found rep_text rep_matchs cx cy ax)
	(when (< cy (length found))
		(if (< ax cx) (defq tx cx cx ax ax tx))
		(bind '(indexs substrs) (elem-get cy found))
		(when (defq subs (some (lambda ((x x1)) (and (= x cx) (= x1 ax) _)) indexs))
			(when regexp
				(setq subs (if (>= subs (length substrs)) '() (elem-get subs substrs)))
				(each! -1 0 (lambda ((x x1) (s))
						(setq rep_text (replace rep_text x x1
							(if (< (defq s (str-to-num s)) (length subs))
								(elem-get s subs) ""))))
					rep_matchs))
			rep_text)))

(defun action-replace (whole_words regexp pattern rep_text)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer)
			found (. buffer :find pattern whole_words regexp))
		(bind '(cx cy) (. *edit* :get_cursor))
		(bind '(ax ay) (. *edit* :get_anchor))
		(when (and (= cy ay) (defq
				rep_matchs (matches rep_text "\$([0-9]+)")
				rep_text (build-replace found rep_text rep_matchs cx cy ax)))
			(undoable
				(. buffer :cut ax ay)
				(. buffer :insert rep_text))
			(refresh))
		(action-find-down whole_words regexp pattern)))

(defun action-replace-all (whole_words regexp pattern rep_text)
	(unless (eql pattern "")
		(when (> (length (defq buffer (. *edit* :get_buffer)
				found (. buffer :find pattern whole_words regexp))) 0)
			(defq rep_matchs (matches rep_text "\$([0-9]+)")
				found (cat found))
			(undoable
				(each (lambda ((indexes substrs))
					(defq y _)
					(each-rev (lambda ((x x1))
						(when (defq rep (build-replace found rep_text rep_matchs x y x1))
							(.-> buffer (:set_cursor x y) (:cut x1 y))
							(. buffer :insert rep))) indexes)) found))
			(bind '(cx cy) (. *edit* :get_cursor))
			(.-> *edit* (:set_cursor cx cy) (:set_anchor cx cy))
			(update-meta-data)
			(refresh))))

(defun action-replace-global (whole_words regexp pattern rep_text)
	(when (defq exit :nil here *current_file*)
		(until exit
		(action-replace-all whole_words regexp pattern rep_text)
		(action-next-buffer)
		(setq exit (eql *current_file* here)))))

(defun action-find-global (whole_words regexp pattern)
	;find files match but not dictionaries !
	(defq files (sort cmp (all-files "." +file_types 2)))
	(if (defq i (find "lib/text/english.txt" files))
		(setq files (erase files i (inc i))))
	(setq files (sort cmp (file-matches files pattern whole_words regexp)))
	;paste into scratch buffer
	(switch-file :nil)
	(defq buffer (. *edit* :get_buffer))
	(undoable
		(each (# (.-> buffer (:insert %0) :break)) files))
	(refresh))
