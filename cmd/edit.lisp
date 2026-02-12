(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/document.inc")

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

	Search:		(edit-find pattern [:w :r]) -> :t | :nil

	Cursors:	(edit-cursors) (edit-add-cursors)

	Selection:	(edit-select-all) (edit-select-line)
				(edit-select-word) (edit-select-block)
				(edit-select-form) (edit-select-paragraph)
				(edit-select-home) (edit-select-end)
				(edit-select-top) (edit-select-bottom)
				(edit-select-left [cnt]) (edit-select-right [cnt])
				(edit-select-up [cnt]) (edit-select-down [cnt])

	Navigation:	(edit-top) (edit-bottom) (edit-home) (edit-end)
				(edit-bracket-left) (edit-bracket-right)
				(edit-ws-left) (edit-ws-right)
				(edit-up [cnt]) (edit-down [cnt])
				(edit-left [cnt]) (edit-right [cnt])

	Mutation:	(edit-insert txt) (edit-paste txt)
				(edit-delete [cnt]) (edit-backspace [cnt])
				(edit-trim) (edit-sort) (edit-unique) (edit-upper)
				(edit-lower) (edit-reflow) (edit-split) (edit-comment)
				(edit-indent) (edit-outdent) (edit-cut)

	Properties:	(edit-copy) -> txt
				(edit-get-text) -> txt
				(edit-get-filename) -> txt

	Utilities:	(edit-split-text txt [cls]) -> (txt ...)
				(edit-join-text (txt ...) [cls]) -> txt

	Example - Numbering lines:

	edit -s my_script file.txt

	my_script
		"(defmacro for-each-line (&rest body)
			`(progn
				(edit-top)
				(defq cy 0)
				(while (/= cy (last (. *doc* :get_size)))
					~body
					(bind '(& cy &ignore) (. *doc* :get_cursor)))))
		(defun edit-script ()
			(defq line_num 0)
			(for-each-line
				(edit-insert (str (++ line_num) \": \"))
				(edit-down)
				(edit-home)))"}
	)
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--cmd") ,(opt-str 'opt_c))
(("-s" "--script") ,(opt-str 'opt_s))
))

; define proxy commands
(redefmacro gen-edit (n m) `(defun ,(sym (str "edit-" n)) () (. *doc* ,m)))
(gen-edit top :top) (gen-edit bottom :bottom) (gen-edit home :home) (gen-edit end :end)
(gen-edit bracket-left :left_bracket) (gen-edit bracket-right :right_bracket)
(gen-edit ws-left :left_white_space) (gen-edit ws-right :right_white_space)
(gen-edit select-all :select_all) (gen-edit select-line :select_line)
(gen-edit select-word :select_word) (gen-edit select-block :select_block)
(gen-edit select-form :select_form) (gen-edit select-paragraph :select_paragraph)
(gen-edit select-home :home_select) (gen-edit select-end :end_select)
(gen-edit select-top :top_select) (gen-edit select-bottom :bottom_select)
(gen-edit trim :trim) (gen-edit sort :sort) (gen-edit unique :unique)
(gen-edit upper :to_upper) (gen-edit lower :to_lower)
(gen-edit reflow :reflow) (gen-edit split :split) (gen-edit comment :comment)
(gen-edit cut :cut) (gen-edit copy :copy)

; define proxy commands with optional repeat
(redefmacro gen-edit (n m) `(defun ,(sym (str "edit-" n)) (&optional c) (times (ifn c 1) (. *doc* ,m))))
(gen-edit up :up) (gen-edit down :down) (gen-edit left :left) (gen-edit right :right)
(gen-edit select-left :left_select) (gen-edit select-right :right_select)
(gen-edit select-up :up_select) (gen-edit select-down :down_select)
(gen-edit delete :delete) (gen-edit backspace :backspace)
(gen-edit indent :right_tab) (gen-edit outdent :left_tab)

; more complex commands
(defun edit-get-filename () *file*)
(defun edit-insert (txt) (. *doc* :insert txt))
(defun edit-paste (txt) (. *doc* :paste txt))
(defun edit-find (pattern &rest flags) (. *doc* :find pattern (find :w flags) (find :r flags)))
(defun edit-cursors () (. *doc* :set_found_cursors (. *doc* :get_buffer_found)))
(defun edit-add-cursors () (. *doc* :add_found_cursors (. *doc* :get_buffer_found)))
(defun edit-split-text (txt &optional cls) (split txt (ifn cls "\n\f")))
(defun edit-join-text (txts &optional cls) (join txts (ifn cls "\n")))
(defun edit-get-text () (edit-join-text (edit-split-text (. *doc* :copy))))
(defun edit-print (&rest args) (apply (const print) (if (nempty? args) args (list (edit-get-text)))))

(defun work (*file* *fnc*)
	; *doc* and *file* are bound here, visible to *fnc*
	; because *fnc* executes in this scope
	(when *fnc*
		(defq *doc* (Document (if (notany (# (ends-with %0 *file*))
				'(".md" ".txt")) +buffer_flag_syntax 0)))
		(catch
			(progn
				(. *doc* :stream_load (file-stream *file*))
				(*fnc*)
				(if (. *doc* :get_modified)
					(. *doc* :stream_save (file-stream *file* +file_open_write)))
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
