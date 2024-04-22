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
	(cond
		((unless (eql pattern "")
			(defq buffer (. *edit* :get_buffer))
			(when (> (length (defq found (. buffer :find pattern whole_words regexp))) 0)
				(bind '(cx cy) (. *edit* :get_cursor))
				(defq sy 0 sy1 (length found))
				(bind '(_ fy _ fy1) (. *edit* :get_find))
				(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1)))
				(if (< cy sy) (setq cy sy cx 0))
				(when (and (< cy sy1) (defq next (some! cy sy1 :nil
						(lambda (match)
							(defq y _)
							(some (lambda (((x x1) &ignore)) (cond
								((> y cy) (list x x1 y))
								((>= x cx) (list x x1 y)))) match))
						(list found))))
					(bind '(x x1 y) next)
					(. *edit* :set_anchor x y)
					(. buffer :set_cursor x1 y)
					(refresh) :t))))
		(:t (refresh) :nil)))

(defun action-find-up (whole_words regexp pattern)
	;return :nil if nothing found !
	(cond
		((unless (eql pattern "")
			(defq buffer (. *edit* :get_buffer))
			(when (> (length (defq found (. buffer :find pattern whole_words regexp))) 0)
				(bind '(cx cy) (. *edit* :get_cursor))
				(defq sy 0 sy1 (length found))
				(bind '(_ fy _ fy1) (. *edit* :get_find))
				(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1)))
				(if (>= cy sy1) (setq cy (dec sy1) cx +max_int))
				(when (and (>= cy sy) (defq next (some! (inc cy) sy :nil
						(lambda (match)
							(defq y _)
							(some-rev (lambda (((x x1) &ignore)) (cond
								((< y cy) (list x x1 y))
								((< x cx) (list x x1 y)))) match))
						(list found))))
					(bind '(x x1 y) next)
					(. *edit* :set_anchor x1 y)
					(. buffer :set_cursor x y)
					(refresh) :t))))
		(:t (refresh) :nil)))

(defun action-set-find-text ()
	(defq text "")
	(when (selection?)
		(bind '(ax ay) (. *edit* :get_anchor))
		(when (defq lines (split (.-> *edit* :get_buffer (:copy ax ay)) (ascii-char +char_lf)))
			(setq text (first lines))))
	(if *regexp* (setq text (escape text)))
	(.-> *find_text* (:set_text text) :layout :dirty)
	(refresh))

(defun build-replace (lines found rep_text rep_matches sx sy sx1)
	(when (< sy (length found))
		(defq match (elem-get found sy) line (elem-get lines sy))
		(when (defq mi (some (lambda (((x x1) &ignore))
					(and (= x sx) (= x1 sx1) _)) match))
			(when regexp
				(setq match (elem-get match mi))
				(each-rev (lambda (((x x1) (x2 x3)))
						(setq rep_text (replace rep_text x x1
							(cond
								((< (defq s (str-to-num (slice rep_text x2 x3)))
										(length match))
									(bind '(x x1) (elem-get match s))
									(slice line x x1))
								("")))))
					rep_matches))
			rep_text)))

(defun action-replace (whole_words regexp pattern rep_text)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer)
			lines (. buffer :get_text_lines)
			found (. buffer :find pattern whole_words regexp))
		(bind '(sx sy sx1 sy1) (sort-selection))
		(when (and (= sy sy1) (defq
				rep_matches (matches rep_text "\$(\d+)")
				rep_text (build-replace lines found rep_text rep_matches sx sy sx1)))
			(undoable
				(.-> buffer
					(:set_cursor sx sy)
					(:delete (- sx1 sx))
					(:insert rep_text)))
			(refresh))
		(action-find-down whole_words regexp pattern)))

(defun action-replace-all (whole_words regexp pattern rep_text &optional noclip)
	(unless (eql pattern "")
		(defq buffer (. *edit* :get_buffer)
			lines (. buffer :get_text_lines)
			found (cat (. buffer :find pattern whole_words regexp))
			rep_matches (matches rep_text "\$(\d+)"))
		(undoable
			(defq sy 0 sy1 (length found))
			(unless noclip
				(bind '(_ fy _ fy1) (. *edit* :get_find))
				(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1))))
			(each! sy sy1 (lambda (match)
				(defq y _)
				(each-rev (lambda (((x x1) &ignore))
					(when (defq rep (build-replace lines found rep_text rep_matches x y x1))
						(.-> buffer
							(:set_cursor x y)
							(:delete (- x1 x))
							(:insert rep)))) match)) (list found)))
		(bind '(cx cy) (. *edit* :get_cursor))
		(. *edit* :set_anchor cx cy)
		(refresh)))

(defun action-replace-global (whole_words regexp pattern rep_text)
	(unless (eql pattern "")
		(for-all-buffers
			(action-replace-all whole_words regexp pattern rep_text :t))))

(defun action-find-global (whole_words regexp pattern)
	(unless (eql pattern "")
		;flush buffers to filesystem
		(action-save-all)
		;find files match but not dictionaries !
		(defq cmd (cat "grep -c -f "
				(if whole_words "-w " "") (if regexp "-r " "")
				(id-encode pattern) " ")
			results (apply (const cat) (sort (const cmp) (map (const second)
				(pipe-farm (map (# (cat cmd %0))
					(filter-array (lambda (f) (notany (# (eql f %0)) +dictionaries))
						(files-all "." +file_types 2))) 20000000)))))
		;paste into scratch buffer
		(action-push)
		(switch-file :nil)
		(.-> *edit* :bottom :get_buffer (:paste results))
		(refresh)))

(defun collect (&optional noclip)
	(defq buffer (. *edit* :get_buffer)
		found (. buffer :find pattern whole_words regexp)
		lines (. buffer :get_text_lines)
		sy 0 sy1 (length found))
	(unless noclip
		(bind '(_ fy _ fy1) (. *edit* :get_find))
		(if (> fy1 fy) (setq sy (max sy fy) sy1 (min sy1 fy1))))
	(each! sy sy1 (lambda (line found)
			(if found (each (lambda (((x x1) &ignore))
				(push results (slice line x x1))) found)))
		(list lines found)))

(defun action-collect (whole_words regexp pattern)
	(unless (eql pattern "")
		(defq results (list))
		(collect)
		(when (nempty? results)
			(setq results (apply cat (join results (const `'(,(char 10))) 2)))
			;paste into scratch buffer
			(action-push)
			(switch-file :nil)
			(.-> *edit* :bottom :get_buffer (:paste results))
			(refresh))))

(defun action-collect-global (whole_words regexp pattern)
	(unless (eql pattern "")
		(defq results (list))
		(for-all-buffers (collect :t))
		(when (nempty? results)
			(setq results (apply cat (join results (const `'(,(char 10))) 2)))
			;paste into scratch buffer
			(action-push)
			(switch-file :nil)
			(.-> *edit* :bottom :get_buffer (:paste results))
			(refresh))))

(defun action-find-function ()
	(action-select-word)
	(bind '(ax ay) (. *edit* :get_anchor))
	(unless (eql (defq name (.-> *edit* :get_buffer (:copy ax ay))) "")
		(action-save-all)
		(defq pos :nil)
		(some (lambda (file)
			(each-line (lambda (line)
				(and (> (length line) 10) (eql "(" (first line))
					(>= (length (defq s (split line (const (char-class " ()\r\t"))))) 2)
					(find (first s) '("defun" "defmacro" "redefun" "redefmacro" "defclass" "ffi"))
					(eql (second s) name) (setq pos (list file 0 _))))
				(file-stream file)) pos)
			(sort (const cmp) (cat *open_files*)))
		(when pos
			(action-push)
			(push *cursor_stack* pos)
			(action-pop))))
