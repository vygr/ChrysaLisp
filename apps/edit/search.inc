;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(radio-select find_toolbar (list :nil *whole_words* *regexp* :nil :nil)))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(radio-select find_toolbar (list :nil *whole_words* *regexp* :nil :nil)))

(defun filter-whole-words (whole_words found)
	(cond
		(whole_words
			(defq out (cap (length found) (list)))
			(each (lambda (line (indexes substrs))
					(defq out_indexes (list) out_substrs (list))
					(each (lambda ((x x1))
						(when (cond
								((and (= x 0) (= x1 (length line))))
								((= x 0) (not-whole-word-char? (elem-get x1 line)))
								((= x1 (length line)) (not-whole-word-char? (elem-get (dec x) line)))
								((and (not-whole-word-char? (elem-get (dec x) line)))
									(not-whole-word-char? (elem-get x1 line))))
							(push out_indexes (list x x1))
							(when (< _ (length substrs))
								(push out_substrs (elem-get _ substrs))))) indexes)
					(push out (list out_indexes out_substrs)))
				(.-> *edit* :get_buffer :get_text_lines) found)
			out)
		((cat found))))

(defun action-find-down (whole_words regexp pattern)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (filter-whole-words whole_words
					(. buffer :find pattern regexp)))) 0)
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
		(when (> (length (defq found (filter-whole-words whole_words
					(. buffer :find pattern regexp)))) 0)
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
			found (filter-whole-words whole_words (. buffer :find pattern regexp)))
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
					found (filter-whole-words whole_words (. buffer :find pattern regexp)))) 0)
			(defq rep_matchs (matches rep_text "\$([0-9]+)"))
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

(defun file-match? (file search pattern meta)
	(when (defq result :nil stream (file-stream file))
		(while (and (not result) (defq line (read-line stream)))
			(task-slice)
			(setq result (. search :match? line pattern meta)))
		result))

(defun action-find-global (whole_words regexp pattern)
	(when (defq search (if regexp +*regexp* +*substr*)
			pattern (map (const progn) pattern)
			meta (. search :compile pattern))
		;find files match but not dictionaries !
		(defq files (sort cmp (all-files "." +file_types 2)))
		(if (defq i (find "lib/text/english.txt" files))
			(setq files (erase files i (inc i))))
		(setq files (filter (# (file-match? %0 search pattern meta)) files))
		;paste into scratch buffer
		(switch-file :nil)
		(defq buffer (. *edit* :get_buffer))
		(undoable
			(each (# (.-> buffer (:insert %0) :break)) files))
		(refresh)))
