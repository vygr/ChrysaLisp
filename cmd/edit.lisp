(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/edit.inc")

(defq usage `(
(("-h" "--help")
{Usage: edit [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.
		-c --cmd '...': commands to execute.
		-s --script path: file containing command to execute.

	Command line text editor.

	The `edit-script` is compiled and executed in a custom environment
	populated with editing primitives.

	With the -c option your script commands will be auto wrapped into an
	`(defun edit-script () ...)` lambda, before execution.

	With the -s option your script is assumed to use advanced ChrysaLisp features,
	such as macros, and as such a simple wrapping will not suffice.
	So the assumption is that you will provide the `(defun edit-script () ...)`
	within the script file !

	If you specify both -c and -s, the -c option is compiled as if it was
	in front of the -s script ! So it could be used to set configuration or
	provide specific functions that the main script binds to etc.

		Available Commands:

	

		Search:	 (edit-find pattern [:w :r]) -> :nil | buffer_found

					(edit-find-next) (edit-find-prev)

	

		Cursors:	(edit-cursors) (edit-add-cursors) (edit-primary)

					(edit-find-add-next)

	

		Selection:  (edit-select-all) (edit-select-line)

	

	
				(edit-select-word) (edit-select-block)
				(edit-select-form) (edit-select-paragraph)
				(edit-select-ws-left) (edit-select-ws-right)
				(edit-select-bracket-left) (edit-select-bracket-right)
				(edit-select-home) (edit-select-end)
				(edit-select-top) (edit-select-bottom)
				(edit-select-left [cnt]) (edit-select-right [cnt])
				(edit-select-up [cnt]) (edit-select-down [cnt])

	Navigation:	(edit-top) (edit-bottom) (edit-home) (edit-end)
				(edit-bracket-left) (edit-bracket-right)
				(edit-ws-left) (edit-ws-right)
				(edit-up [cnt]) (edit-down [cnt])
				(edit-left [cnt]) (edit-right [cnt])

	Mutation:	(edit-insert txt) (edit-paste txt) (edit-replace pattern)
				(edit-delete [cnt]) (edit-backspace [cnt])
				(edit-trim) (edit-sort) (edit-unique) (edit-upper)
				(edit-lower) (edit-reflow) (edit-split) (edit-comment)
				(edit-indent) (edit-outdent) (edit-cut)

	Properties:	(edit-copy) -> txt
				(edit-get-text) -> txt
				(edit-get-filename) -> txt

	Utilities:	(edit-split-text txt [cls]) -> (txt ...)
				(edit-join-text (txt ...) [cls]) -> txt
				(edit-eof?) -> :t | :nil
				(edit-cx) -> cx
				(edit-cy) -> cy

	Example - Numbering lines:

	edit -c
		"(until (edit-eof?)
			(edit-insert (str (inc (edit-cy)) \q: \q))
			(edit-down)
			(edit-home))"
		file.txt}
	)
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--cmd") ,(opt-str 'opt_c))
(("-s" "--script") ,(opt-str 'opt_s))
))

(defun work (*file* *fnc*)
	; *edit* and *file* are bound here, visible to *fnc*
	; because *fnc* executes in this scope
	(when *fnc*
		(defq *edit* (Document (if (notany (# (ends-with %0 *file*))
				'(".md" ".txt")) +buffer_flag_syntax 0)))
		(catch
			(progn
				(. *edit* :stream_load (file-stream *file*))
				(*fnc*)
				(if (. *edit* :get_modified)
					(. *edit* :stream_save (file-stream *file* +file_open_write)))
				(print "Edited: " *file*))
			(print "Error editing " *file* ": " _))))

(defun main ()
	; Initialize options
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 opt_c :nil opt_s :nil args (options stdio usage)))
		; file list (args or stdin)
		(if (empty? (defq jobs (rest args)))
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		; prepare the script stream, opt_s can use prior opt_c option
		; maybe as config details etc !
		(defq script_stream (memory-stream))
		(if opt_c (write-blk script_stream opt_c))
		(if opt_s (write-blk script_stream (load opt_s)))
		(when (and (>= (stream-seek script_stream 0 0) 0) (nempty? jobs))
			(if (<= (length jobs) opt_j)
				(progn
					(cond
						(opt_s
							; compile user script
							(env-push)
							(repl script_stream "edit-script")
							(def (penv) 'fnc (def? 'edit-script))
							(env-pop))
						(opt_c
							; compile and wrap user commands
							(defq body (list) form :t next (ascii-code " "))
							(while form
								(bind '(form next) (read script_stream next))
								(if form (push body form)))
							(setq body (macrobind body))
							(defq fnc `(lambda () ~body))))
					(each (# (work %0 fnc)) jobs))
				; distribute to farm
				(each (lambda ((job result)) (prin result))
					(pipe-farm (map (# (str (first args)
							" -j " opt_j
							(if opt_c (cat " -c \q" opt_c "\q") "")
							(if opt_s (cat " -s " opt_s) "")
							" " (slice (str %0) 1 -2)))
						(partition jobs opt_j))))))))
