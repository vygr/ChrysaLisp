;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(update-find-toolbar)
	(refresh-display))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(update-find-toolbar)
	(refresh-display))

(defun action-region ()
	(bind '(sy sy1) (select-lines))
	(bind '(fx fy fx1 fy1) (. *edit* :get_find))
	(if (> fy1 fy) (setq fy 0 fy1 0) (setq fy sy fy1 sy1))
	(. *edit* :set_find fx fy fx1 fy1)
	(update-find-toolbar)
	(refresh-display))

(defun action-find-down (whole_words regexp pattern)
	;return :nil if nothing found !
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (. buffer :find pattern whole_words regexp))) 0)
			(bind '(cx cy) (. *edit* :get_cursor))
			(defq sy 0 sy1 (length found))
			(bind '(_ fy _ fy1) (. *edit* :get_find))
			(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1)))
			(if (< cy sy) (setq cy sy cx 0))
			(when (and (< cy sy1) (defq next (some! cy sy1 :nil
				(lambda ((indexes substrs))
					(defq y _)
					(some (lambda ((x x1)) (cond
						((> y cy) (list x x1 y))
						((>= x cx) (list x x1 y)))) indexes))
					(list found))))
				(bind '(x x1 y) next)
				(. *edit* :set_anchor x y)
				(. buffer :set_cursor x1 y)
				(refresh) :t))))

(defun action-find-up (whole_words regexp pattern)
	;return :nil if nothing found !
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer))
		(when (> (length (defq found (. buffer :find pattern whole_words regexp))) 0)
			(bind '(cx cy) (. *edit* :get_cursor))
			(defq sy 0 sy1 (length found))
			(bind '(_ fy _ fy1) (. *edit* :get_find))
			(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1)))
			(if (>= cy sy1) (setq cy (dec sy1) cx +max_int))
			(when (and (>= cy sy) (defq next (some! (inc cy) sy :nil
				(lambda ((indexes substrs))
					(defq y _)
					(some-rev (lambda ((x x1)) (cond
						((< y cy) (list x x1 y))
						((< x cx) (list x x1 y)))) indexes))
					(list found))))
				(bind '(x x1 y) next)
				(. *edit* :set_anchor x1 y)
				(. buffer :set_cursor x y)
				(refresh) :t))))

(defun action-set-find-text ()
	(when (selection?)
		(bind '(ax ay) (. *edit* :get_anchor))
		(when (> (length (defq text
				(split (.-> *edit* :get_buffer (:copy ax ay)) (ascii-char +char_lf)))) 0)
			(setq text (elem-get 0 text))
			(.-> *find_text* (:set_text text) :layout :dirty)
			(refresh))))

(defun build-replace (found rep_text rep_matchs sx sy sx1)
	(when (< sy (length found))
		(bind '(indexs substrs) (elem-get sy found))
		(when (defq subs (some (lambda ((x x1)) (and (= x sx) (= x1 sx1) _)) indexs))
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
		(bind '(sx sy sx1 sy1) (sort-selection))
		(when (and (= sy sy1) (defq
				rep_matchs (matches rep_text "\$(\d+)")
				rep_text (build-replace found rep_text rep_matchs sx sy sx1)))
			(undoable
				(.-> buffer
					(:set_cursor sx sy)
					(:delete (- sx1 sx))
					(:insert rep_text)))
			(refresh))
		(action-find-down whole_words regexp pattern)))

(defun action-replace-all (whole_words regexp pattern rep_text &optional clip)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer)
			found (cat (. buffer :find pattern whole_words regexp))
			rep_matchs (matches rep_text "\$(\d+)"))
		(undoable
			(defq sy 0 sy1 (length found))
			(unless clip
				(bind '(_ fy _ fy1) (. *edit* :get_find))
				(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1))))
			(each! sy sy1 (lambda ((indexes substrs))
				(defq y _)
				(each-rev (lambda ((x x1))
					(when (defq rep (build-replace found rep_text rep_matchs x y x1))
						(.-> buffer
							(:set_cursor x y)
							(:delete (- x1 x))
							(:insert rep)))) indexes)) (list found)))
		(bind '(cx cy) (. *edit* :get_cursor))
		(. *edit* :set_anchor cx cy)
		(refresh)))

(defun action-replace-global (whole_words regexp pattern rep_text)
	(unless (eql pattern "")
		(for-all-buffers
			(action-replace-all whole_words regexp pattern rep_text :t))))

(defun action-find-global (whole_words regexp pattern)
	;find files match but not dictionaries !
	(defq files (sort cmp (file-matches
		(filter (lambda (f) (notany (# (eql f %0)) +dictionaries))
			(all-files "." +file_types 2)) pattern whole_words regexp)))
	;paste into scratch buffer
	(switch-file :nil)
	(defq buffer (. *edit* :get_buffer))
	(undoable
		(each (# (.-> buffer (:insert %0) :break)) files))
	(refresh))
