;;;;;;;;;;;;;;;
; debug actions
;;;;;;;;;;;;;;;

(defun num-to-hex-str (_)
	(cat "0x" (int-to-hex-str _)))

(defun action-debug-launch ()
	;save files
	(action-save-all)
	;launch debugger
	(open-child "apps/debug/app.lisp" +kn_call_open)
	;can we start a GUI app
	(when (starts-with "apps/" *current_file*)
		(unless (find (defq app (slice *current_file*
					(defq i (inc (find "/" *current_file*)))
					(find "/" *current_file* (inc i))))
				'("login" "wallpaper" "tui"))
			(open-child (cat "apps/" app "/app.lisp") +kn_call_open))))

(defun action-debug-toggle ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(cond
		((match? (defq line (. buffer :get_text_line cy)) "^(;; )?\(debug-brk \q.+\q\)$")
			;have either an enabled or disabled breakpoint
			(undoable
				(.-> *edit*
					(:set_cursor 0 cy)
					(:set_anchor 0 cy))
				(if (eql (first line) ";")
					(. buffer :delete 3)
					(.-> *edit*
						(:insert ";; ")
						(:set_cursor 0 cy)
						(:set_anchor 0 cy)))))
		(:t ;not got any breakpoint on line
			(undoable
				(.-> *edit*
					(:set_cursor 0 cy)
					(:set_anchor 0 cy)
					(:insert (cat {(debug-brk "} (num-to-hex-str (random +max_int)) {")}))
					:break
					(:set_cursor 0 cy)
					(:set_anchor 0 cy)))))
	(refresh))

(defun action-debug-remove ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(when (match? (. buffer :get_text_line cy) "^(;; )?\(debug-brk \q0x\x+\q\)$")
		;got auto breakpoint on line
		(undoable
			(.-> *edit*
				(:set_cursor 0 cy)
				(:set_anchor 0 (inc cy))
				:delete))
		(refresh)))

(defun action-debug-remove-all ()
	;scan for auto debug lines
	(defq buffer (. *edit* :get_buffer))
	(when (nempty? (defq autos (reduce (# (if (match? %1 "^(;; )?\(debug-brk \q0x\x+\q\)$")
					(push %0 (!)) %0)) (. buffer :get_text_lines) (list))))
		(bind '(cx cy) (. *edit* :get_cursor))
		(undoable
			(reach (# (.-> *edit*
				(:set_cursor 0 %0)
				(:set_anchor 0 (inc %0))
				:delete)
				(if (> cy %0) (-- cy))) autos)
			(.-> *edit*
				(:set_cursor cx cy)
				(:set_anchor cx cy)))
		(refresh)))

(defun action-debug-enable-all ()
	;scan for auto debug lines
	(defq buffer (. *edit* :get_buffer))
	(when (nempty? (defq autos (reduce (# (if (match? %1 "^(;; )?\(debug-brk \q.+\q\)$")
					(push %0 (!)) %0)) (. buffer :get_text_lines) (list))))
		(bind '(cx cy) (. *edit* :get_cursor))
		(undoable
			(reach (#
				;have either an enabled or disabled breakpoint
				(when (eql (first (. buffer :get_text_line %0)) ";")
					(.-> *edit*
						(:set_cursor 0 %0)
						(:set_anchor 0 %0))
					(. buffer :delete 3))) autos)
			(.-> *edit*
				(:set_cursor cx cy)
				(:set_anchor cx cy)))
		(refresh)))

(defun action-debug-disable-all ()
	;scan for auto debug lines
	(defq buffer (. *edit* :get_buffer))
	(when (nempty? (defq autos (reduce (# (if (match? %1 "^(;; )?\(debug-brk \q.+\q\)$")
					(push %0 (!)) %0)) (. buffer :get_text_lines) (list))))
		(bind '(cx cy) (. *edit* :get_cursor))
		(undoable
			(reach (#
				;have either an enabled or disabled breakpoint
				(unless (eql (first (. buffer :get_text_line %0)) ";")
					(.-> *edit*
						(:set_cursor 0 %0)
						(:set_anchor 0 %0)
						(:insert ";; ")))) autos)
			(.-> *edit*
				(:set_cursor cx cy)
				(:set_anchor cx cy)))
		(refresh)))
