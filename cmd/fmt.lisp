(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/format.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: fmt [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 4.
        -w --write: overwrite files in-place, default :nil.
        -c --check: list the files that need formatting.
        -l --limit num: line length that brings pressure to
            break, default 80. VP assembler lines get half as
            much again. 0 keeps your line breaks, and only
            indents and tidies.

    Formats ChrysaLisp source code. Only white space between
    tokens is ever changed, so what the reader sees, and so
    what gets built, is the same before and after.

    The layout is made from the code alone. The line breaks
    inside a form are not kept, the form is laid out afresh.

    Indentation, with tabs, from the structure alone. A line
    is one tab in from the line its enclosing form opened on.
    Where several forms open on the one line, each is a tab
    further in than the one around it, so the indent shows
    which form owns a line. VP block forms, (vpif)
    (loop-start) and so on, indent the lines between them.

    A form is one line if it fits. A definition always has its
    body on lines of its own, as does a (cond) or (case) each
    of its clauses. A (when) (while) and the like is one line
    only with a single body form. An (if) keeps its test and
    then form on the opening line, and its else form too if
    there is just the one, else each else form has a line.

    The limit is pressure to break, not an order to. A line
    over it is broken where the structure has a place for it,
    at the outermost form that can be. A form with a body
    then has each body form on a line of its own, bindings
    break before a name, and are filled. A line with no such
    place is left, until it is half as long again, and is
    then broken between arguments.

    Tidying. One space between tokens, no trailing white
    space, no runs of blank lines, no line starts with a close
    bracket, and the file ends with one newline.

    What is kept from the source. Strings, comments and the
    lines they are on, blank lines, and the text of a
    (defq usage ...) form.

    The source scanners and the doc builder read a line at a
    time. A form they look for, (def-method) (dec-method)
    (import) (ffi) a VP instruction and the like, starts a
    line if and only if it did in the source, and is never
    broken, nor is the opening line of a definition. Comments
    right under such a line stay right under it, and the lines
    of a key map are kept.

    As a guard, the result must read as the same forms as the
    source, or the file is left alone.

    Restricts targets to unique .lisp, .inc, and .vp files. If
    no paths are given on the command line, paths are read from
    stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-w" "--write") ,(opt-flag 'opt_w))
(("-c" "--check") ,(opt-flag 'opt_c))
(("-l" "--limit") ,(opt-num 'opt_l))
))

(defq +file_types ''(".lisp" ".inc" ".vp"))

;;;;;;;;;;;;;
; file worker
;;;;;;;;;;;;;

(defun forms (text)
	; (forms text) -> str
	;the forms the Lisp reader gives for the text
	(defq out (list) stream (string-stream text) next (ascii-code " "))
	(while (/= next -1)
		(bind '(form next) (read stream next))
		(unless (and (= next -1) (not form)) (push out form)))
	(str out))

(defun squash (text)
	; (squash text) -> str
	;the text with all white space taken out
	(apply (const cat) (split text (const (char-class (cat " \t\r" (ascii-char 10)))))))

(defun work (file)
	; (work file) -> :nil | str
	;format a file, :nil if it is already as it should be. The result is
	;not trusted, it must read as the same forms, and differ only in white
	;space, or the file is left alone.
	(defq data :nil)
	(with-read-lock file (setq data (load file)))
	(when data
		(defq formatted (format-lisp data opt_l (ends-with ".vp" file)))
		(when (nql data formatted)
			(cond
				((eql (defq before (catch (forms data) :t)) :t)
					(print "Left alone, it does not read: " file)
					:nil)
				((and (eql before (catch (forms formatted) :t))
					(eql (squash data) (squash formatted)))
					formatted)
				(:t
					(print "Left alone, fmt would have changed the code: " file)
					:nil)))))

(defun write-file (file data)
	(with-write-lock file
		(when (defq out (file-stream file +file_open_write))
			(write-blk out data)
			(stream-flush out)
			(setq out :nil)))
	(print "Formatted: " file))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_j 4 opt_w :nil opt_c :nil opt_l 80
				args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files) (lines! (# (push files %0) :nil) (io-stream 'stdin)))
		(setq files
				(usort (filter (lambda (file)
								(some (# (ends-with %0 file)) +file_types))
						files)))
		(cond
			((<= (length files) opt_j)
				;do them here
				(each (lambda (file) (defq formatted (work file))
						(cond
							(opt_c
								(if formatted
									(print "Needs formatting: " file)))
							(opt_w (if formatted (write-file file formatted)))
							(:t (prin (ifn formatted (load file))))))
					files))
			((or opt_w opt_c)
				;farm out the checking, then do any writing here, with no
				;task still to start that might import a half written file
				(defq needs (list))
				(each (lambda ((job result))
						(each (# (if (starts-with "Needs formatting: " %0)
									(push needs (slice %0 18 -1))
									(print %0)))
							(split result (ascii-char 10))))
					(pipe-farm (map (# (str (first args) " -c -j " opt_j " -l " opt_l " " (slice (str %0) 1 -2)))
								(partition files opt_j))))
				(each (lambda (file)
						(if opt_c (print "Needs formatting: " file)
							(if (defq formatted (work file))
								(write-file file formatted))))
					(sort needs)))
			(:t ;farm out the formatting, and print the results
				(each (lambda ((job result)) (prin result))
					(pipe-farm (map (# (str (first args) " -j " opt_j " -l " opt_l " " (slice (str %0) 1 -2)))
								(partition files opt_j))))))))
