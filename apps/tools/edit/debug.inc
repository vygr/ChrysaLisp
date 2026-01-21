;;;;;;;;;;;;;;;
; debug actions
;;;;;;;;;;;;;;;

(defun num-to-hex-str (%0)
	(cat "0x" (int-to-hex-str %0)))

(defq +disabled_prefix ";;; "
	+auto_pattern (cat "^(" +disabled_prefix ")?\(debug-brk \q0x\x+\q\)\n$")
	+user_pattern (cat "^(" +disabled_prefix ")?\(debug-brk \q.+\q\)\n$"))

(defun file-app (file)
	(defq app :nil)
	(when (starts-with "apps/" file)
		(defq file (slice file 5 (dec (ifn (rfind "/" file) 0))))
		(unless (some (# (ends-with %0 file))
				'("login" "logout" "wallpaper" "tui"
				"edit" "debug" "profile"))
			(setq app (cat "apps/" file "/app.lisp"))))
	app)

(defun action-debug-launch ()
	;save files
	(action-save-all)
	;launch debugger
	(open-child "apps/system/debug/app.lisp" +kn_call_open)
	;all potential apps based on the open files
	(if (nempty? (defq apps (filter (# (find %0 *open_files*))
					(reduce (# (if (defq app (file-app %1)) (push %0 app) %0))
						*open_files* (list)))))
		;does the current file come from one of them ?
		(if (and (defq app (file-app *current_file*)) (find app apps))
			(open-child app +kn_call_open)
			(open-child (first apps) +kn_call_open))))

(defun action-debug-toggle ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy &ignore) (. *edit* :get_cursor))
	(cond
		((match? (defq line (. buffer :get_text_line cy)) +user_pattern)
			;have either an enabled or disabled breakpoint
			(undoable buffer
				(if (starts-with +disabled_prefix line)
					(.-> *edit*
						(:set_cursor 0 cy (const (length +disabled_prefix)) cy)
						:delete)
					(.-> *edit*
						(:set_cursor 0 cy)
						(:insert +disabled_prefix)
						(:set_cursor 0 cy)))))
		(:t ;not got any breakpoint on line
			(undoable buffer
				(.-> *edit*
					(:set_cursor 0 cy)
					(:insert (cat {(debug-brk "} (num-to-hex-str (random +max_int)) {")}
						+char_class_lf))
					(:set_cursor 0 cy)))))
	(refresh))

(defun action-debug-remove ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy &ignore) (. *edit* :get_cursor))
	(when (match? (. buffer :get_text_line cy) +auto_pattern)
		;got auto breakpoint on line
		(undoable buffer
			(.-> *edit*
				(:set_cursor 0 cy 0 (inc cy))
				:delete))
		(refresh)))

(defun action-debug-remove-all ()
	;scan for auto debug lines
	(defq buffer (. *edit* :get_buffer))
	(when (nempty? (defq autos (reduce (# (if (match? %1 +auto_pattern)
					(push %0 (!)) %0)) (. buffer :get_buffer_lines) (list))))
		(bind '(cx cy &ignore) (. *edit* :get_cursor))
		(undoable buffer
			(reach (# (.-> *edit*
				(:set_cursor 0 %0 0 (inc %0))
				:delete)
				(if (> cy %0) (-- cy))) autos)
			(.-> *edit*
				(:set_cursor cx cy)))
		(refresh)))

(defun action-debug-enable-all ()
	;scan for all debug lines
	(defq buffer (. *edit* :get_buffer))
	(when (nempty? (defq autos (reduce (# (if (match? %1 +user_pattern)
					(push %0 (!)) %0)) (. buffer :get_buffer_lines) (list))))
		(bind '(cx cy &ignore) (. *edit* :get_cursor))
		(undoable buffer
			(reach (#
				;have either an enabled or disabled breakpoint
				(when (starts-with +disabled_prefix (. buffer :get_text_line %0))
					(.-> *edit*
						(:set_cursor 0 %0 (const (length +disabled_prefix)) %0)
						:delete))) autos)
			(.-> *edit*
				(:set_cursor cx cy)))
		(refresh)))

(defun action-debug-disable-all ()
	;scan for all debug lines
	(defq buffer (. *edit* :get_buffer))
	(when (nempty? (defq autos (reduce (# (if (match? %1 +user_pattern)
					(push %0 (!)) %0)) (. buffer :get_buffer_lines) (list))))
		(bind '(cx cy &ignore) (. *edit* :get_cursor))
		(undoable buffer
			(reach (#
				;have either an enabled or disabled breakpoint
				(unless (starts-with +disabled_prefix (. buffer :get_text_line %0))
					(.-> *edit*
						(:set_cursor 0 %0)
						(:insert +disabled_prefix)))) autos)
			(.-> *edit*
				(:set_cursor cx cy)))
		(refresh)))
