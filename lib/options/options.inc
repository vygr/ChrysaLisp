;;;;;;;;;;;;;;;;;;;;
; options processing
;;;;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

;module
(env-push)

(defun options-print (&rest _)
	; (options-print &rest _)
	(defq stream (io-stream 'stdout))
	(write-line stream (apply (const str) (cat _)))
	(stream-flush stream))

(defun options-find (optlist arg)
	; (options-find optlist arg) -> :nil | opt_entry
	(some (# (if (find arg (first %0)) %0)) optlist))

(defq +split_class_combo (char-class " \r\f\v\n\t|!"))

(defun options-split (args)
	; (options-split args) -> (a0 [a1] ...)
	(defq out (list) i 0 state :space)
	(while (< i (length args))
		(case state
			(:space
				(when (< (setq i (bskip +char_class_white_space args i)) (length args))
					(if (eql (elem-get args i) "\q")
						(setq i (inc i) state :quote)
						(if (bfind (elem-get args i) +split_class_combo)
							(push out (slice args i (setq i (inc i))))
							(setq state :normal)))))
			(:normal
				(push out (slice args i (setq state :space i
					(bskipn +split_class_combo args i)))))
			(:quote
				(push out (slice args i (setq i (bskipn "\q" args i))))
				(setq i (inc i) state :space)))) out)

(defun options (stdio optlist)
	; (options stdio optlist) -> :nil | args
	;scan the stdio args and process according to the optlist
	(defq args (options-split (stdio-get-args stdio)) out_args (list))
	(while (nempty? args)
		;look for "-", pass through if not
		(bind '(arg &rest args) args)
		(if (starts-with "-" arg)
			(if (defq opt_entry (or
					(options-find optlist arg)
					(options-find optlist "-h")))
				;found an entry, else help
				(cond
					((str? (second opt_entry))
						;just print it and quit
						(options-print (second opt_entry))
						(setq args '() out_args :nil))
					(:t ;else call arg handler
						(setq args (callback (second opt_entry) (penv) args arg))))
				;error, so quit
				(setq args '() out_args :nil))
			;arg to output
			(push out_args arg))) out_args)

(defun opt-flag (opt_var)
	; (opt-flag 'opt_var) -> args
	(static-qq (lambda (args arg)
		(setq ,opt_var :t) args)))

(defun opt-toggle (opt_var)
	; (opt-toggle 'opt_var) -> args
	(static-qq (lambda (args arg)
		(setq ,opt_var (not ,opt_var)) args)))

(defun opt-num (opt_var)
	; (opt-num 'opt_var) -> args
	(static-qq (lambda (args arg)
		(setq ,opt_var (str-as-num (first args))) (rest args))))

(defun opt-str (opt_var)
	; (opt-str 'opt_var) -> args
	(static-qq (lambda (args arg)
		(setq ,opt_var (first args)) (rest args))))

(defun opt-pair (opt_var sep)
	; (opt-pair 'opt_var sep) -> args
	;the arg split at the first sep, (key value), so the value may hold
	;one, text== is ("text" "="). With no sep in it, (key)
	(static-qq (lambda (args arg)
		(defq pair (first args))
		(setq ,opt_var (if (defq at (find ,sep pair))
			(list (slice pair 0 at) (slice pair (inc at) -1)) (list pair)))
		(rest args))))

(defun opt-nums (cnt opt_var)
	; (opt-nums cnt 'opt_var) -> args
	(static-qq (lambda (args arg)
		(push ,opt_var (map! (const str-as-num) (list args) 0 ,cnt))
		(slice args ,cnt -1))))

(defun options-input (options_args options_fn)
	; (options-input args fn) -> :nil | (fn stream)
	;fn of the file named after the command, read under a lock, or of
	;stdin if none is named. The file is let go before the lock is. The
	;names here are long, fn is called with them about it
	(cond
		((> (length options_args) 1)
			(with-read-lock (second options_args)
				(when (defq options_stream (file-stream (second options_args)))
					(defq options_out (options_fn options_stream))
					(setq options_stream :nil)
					options_out)))
		((options_fn (io-stream 'stdin)))))

;module
(export-symbols '(options options-input opt-flag opt-toggle opt-str opt-num opt-nums opt-pair))
(env-pop)
