(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/document.inc")

(defq usage `(
(("-h" "--help")
"Usage: edit [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.
		-c --cmd '...': command commands to execute.
		-s --script path: file containing commands to execute.

	Command line text editor.
	The script is compiled into a function and executed in a
	custom environment populated with editing primitives.

	Available Commands:

	Search:  (edit-find pattern [:w] [:r])
	Cursors:	(edit-cursors) (edit-add-cursors)

	Selection:  (edit-select-all) (edit-select-line)
				(edit-select-word) (edit-select-block)
				(edit-select-form) (edit-select-para)
				(edit-select-left) (edit-select-right)
				(edit-select-up) (edit-select-down)
				(edit-select-home) (edit-select-end)
				(edit-select-top) (edit-select-bottom)

	Navigation: (edit-top) (edit-bottom) (edit-up) (edit-down)
				(edit-left) (edit-right) (edit-home) (edit-end)
				(edit-bracket-left) (edit-bracket-right)
				(edit-ws-left) (edit-ws-right)

	Mutation:   (edit-insert text) (edit-delete) (edit-backspace)
				(edit-trim) (edit-sort) (edit-unique) (edit-upper)
				(edit-lower) (edit-reflow) (edit-split) (edit-comment)
				(edit-indent) (edit-outdent)

	Properties: (edit-get-text) (edit-get-filename)

	Example - Numbering lines:

	edit -c \q(edit-top) (defq i 0)
		(while (edit-down)
			(edit-home) (edit-insert (str (++ i) ': '))) \q
		file.txt"
	)
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--cmd") ,(opt-str 'opt_c))
(("-s" "--script") ,(opt-str 'opt_s))
))

; define simple proxy commands
(defmacro gen-edit (n m) `(defun ,(sym (str "edit-" n)) () (. *doc* ,m)))
(gen-edit top :top) (gen-edit bottom :bottom) (gen-edit up :up) (gen-edit down :down)
(gen-edit left :left) (gen-edit right :right) (gen-edit home :home) (gen-edit end :end)
(gen-edit bracket-left :left_bracket) (gen-edit bracket-right :right_bracket)
(gen-edit ws-left :left_white_space) (gen-edit ws-right :right_white_space)
(gen-edit select-all :select_all) (gen-edit select-line :select_line)
(gen-edit select-word :select_word) (gen-edit select-block :select_block)
(gen-edit select-form :select_form) (gen-edit select-para :select_paragraph)
(gen-edit select-left :left_select) (gen-edit select-right :right_select)
(gen-edit select-up :up_select) (gen-edit select-down :down_select)
(gen-edit select-home :home_select) (gen-edit select-end :end_select)
(gen-edit select-top :top_select) (gen-edit select-bottom :bottom_select)
(gen-edit delete :delete) (gen-edit backspace :backspace) (gen-edit trim :trim)
(gen-edit sort :sort) (gen-edit unique :unique) (gen-edit upper :to_upper) (gen-edit lower :to_lower)
(gen-edit reflow :reflow) (gen-edit split :split) (gen-edit comment :comment)
(gen-edit indent :right_tab) (gen-edit outdent :left_tab)

; more complex commands
(defun edit-insert (txt) (. *doc* :insert txt))
(defun edit-find (pattern &rest flags)
	(. *doc* :find pattern (find :w flags) (find :r flags)))
(defun edit-cursors ()
	(. *doc* :set_found_cursors (. *doc* :get_buffer_found)))
(defun edit-add-cursors ()
	(. *doc* :add_found_cursors (. *doc* :get_buffer_found)))
(defun edit-get-text () (. *doc* :get_select))
(defun edit-get-filename () *file*)
(defun edit-print (&rest args)
	(apply print (if args args (list (. *doc* :get_select)))))

(defun work (file)
	; *doc* and *file* are bound here, visible to edit-script
	; because edit-script executes in this scope
	(defq *doc* (Document) *file* file)
	(catch
		(progn
			(. *doc* :stream_load (file-stream file))
			(edit-script)
			(. *doc* :stream_save (file-stream file +file_open_write))
			(print "Edited: " file))
		(print "Error editing " file ": " _)))

(defun main ()
	; Initialize options
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 opt_c :nil opt_s :nil args (options stdio usage)))
		; file list (args or stdin)
		(if (empty? (defq jobs (rest args)))
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		; prepare the script stream
		(defq script_stream (memory-stream))
		(if opt_c (write-blk script_stream opt_c))
		(if opt_s (write-blk script_stream (load opt_s)))
		(when (and (>= (stream-seek script_stream 0 0) 0) (nempty? jobs))
			(if (<= (length jobs) opt_j)
				(progn
					; parse user script(s)
					(defq forms (list) next (ascii-code " ") form :t)
					(while form
						(bind '(form next) (read script_stream next))
						(push forms form))
					; compile user script into a function 'edit-script'
					(defq edit-script (macrobind `(lambda () ~forms)))
					; run locally
					(each (const work) jobs))
				; distribute to farm
				(each (lambda ((job result)) (prin result))
					(pipe-farm (map (# (str (first args)
							" -j " opt_j
							(if opt_c (cat " -c \q" opt_c "\q") "")
							(if opt_s (cat " -s " opt_s) "")
							" " (slice (str %0) 1 -2)))
						(partition jobs opt_j))))))))
