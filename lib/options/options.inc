;;;;;;;;;;;;;;;;;;;;
; options prosessing
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun options-print (&rest _)
	; (options-print &rest _)
	(defq stream (io-stream 'stdout))
	(write stream (apply str (cat _ (list (ascii-char 10)))))
	(stream-flush stream))

(defun options-find (optlist arg)
	; (options-find optlist arg) -> :nil | opt_entry
	(some (# (if (find arg (first %0)) %0)) optlist))

(defun options-split (args)
	; (options-split args) -> (a0 [a1] ...)
	(defq out (list) i 0 state :space)
	(while (< i (length args))
		(case state
			(:space
				(when (< (setq i (bskip +char_class_white_space args i)) (length args))
					(if (eql (elem-get args i) (ascii-char 34))
						(setq i (inc i) state :quote)
						(setq state :normal))))
			(:normal
				(push out (slice args i (setq state :space i
					(bskipn +char_class_white_space args i)))))
			(:quote
				(push out (slice args i (setq i (bskipn (ascii-char 34) args i))))
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

(defun opt-num (opt_var)
	; (opt-num 'opt_var) -> args
	(static-qq (lambda (args arg)
		(setq ,opt_var (str-as-num (first args))) (rest args))))

(defun opt-str (opt_var)
	; (opt-str 'opt_var) -> args
	(static-qq (lambda (args arg)
		(setq ,opt_var (first args))) (rest args)))

;module
(export-symbols '(options opt-flag opt-str opt-num))
(env-pop)
