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
            break, default 120.

    Lays out ChrysaLisp source code in the house style. Only
    white space between tokens is ever changed, so what the
    reader sees, and so what gets built, is the same before
    and after. The layout is made from the forms and the
    comments alone: however a file was laid out, the same
    forms give the same text.

    What is kept of the source. Comments, a comment on a line
    of its own before what it was before, one on the end of a
    line after what it was after. A blank line between two
    forms, one at most. Strings as they are, the text of a
    (defq usage ...) form, and the line a ;check off comment
    is on, or the form after one on a line of its own.

    Indentation, with tabs. A line is one tab in from the line
    its form opened on, and a tab more for each form that
    opened on that line before it, so the indent shows which
    form owns a line. VP block forms, (vpif) (loop-start) and
    so on, indent the forms between them. A (switch) is flat,
    its forms sit where it does till a case, (vpcase)
    (default) (else), which sits there too, and the forms after
    a case are indented.

    Line breaks. A form that fits on the line is on the line,
    but one with a test or a body has a width of its own,
    however much room the line has, so that the left edge
    shows the blocks of the code: a (when) (while) (until)
    (times) is one line only if it is no wider than half the
    limit, an (unless) or a (lambda) two thirds, an (if)
    (and) (or) (each) three quarters. A call, a binding or
    a list has the whole line. The test of a block that is
    over more than one line, if it is an (and) or an (or)
    wider than half the limit, has a condition to a line.

    A definition has its body on lines of its own, but one
    among others that fits is a line, the form before or
    after it a definition too with no blank line between,
    comments aside: a table of one line definitions. A (cond)
    or a (case) has each of its clauses on a line, and the
    clause of :t has its first form beside it. A (when)
    (while) (lambda) and the like is one line only with a
    single body form, a (#) with one or two. An (if) that is
    not a line has its then and else forms each on a line. An
    (and) of nothing but (defq)s has each on a line, under
    the (and). A name and its value are never parted, nor a
    keyword and its value in a call. A form within three
    eighths of the limit is never broken.

    A form that must be over more than one line, a (cond)
    say, starts a line of its own. One that is only too long,
    in a call or a list, hangs on the line it is on if it has
    half the line and its first argument fits there. If
    another argument will be over more than one line too,
    each starts a line. A (lambda) hangs, but two of a form
    each start a line. What comes after a
    form over more than one line starts a line. The first
    argument of a call starts a line only if it is a form
    that then fits, with more to come after it, or if all
    the arguments then fit. The limit is pressure, a line far
    in still has half of it past its indent.

    Tables. A quoted list of atoms alone or of lists, an
    (enum) or a (bit), or a form of eight or more atoms
    alone, is laid out in rows of its cells: a name and its
    value in bindings or properties, a form in a list of
    lists, else an atom. A row is a power of two of cells,
    the most that fit, the same in each, for each part of the
    table between its comments and blank lines, and a part
    that fits in one row is one row. The rows start on a line
    of their own if more cells then fit in each, or if it is
    a column, one cell to a row. A (scatter (Fmap) ...) of
    eight or more names and values is a table of them. A table
    of lists takes as many as fit.

    A map of keys and the functions they are for, a (scatter)
    into a map it makes where each value is a name, an event
    map or a key map, is a key and its action to a line,
    however much room there is, starting on the line of its
    name, as the doc builder reads a key map.

    The source scanners take a ( at the very start of a line
    to open a form at the top of the file. Each form at the
    top starts its line, and every line of what is in one is
    at least a tab in, so they find what they did. A
    declaration, (def-method) (dec-method) (import) (ffi) and
    the like, is never broken, nor is the opening line of a
    definition. The includes command reads a line at a time,
    and takes one that starts with (call (entry (exit (jump
    or the like to name a class the file uses: such a form,
    in another, always starts a line, and no line starts
    with such a word that is not one.

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
	(apply (const cat) (split text (const (char-class " \t\r\n")))))

(defun work (file)
	; (work file) -> :nil | str
	;format a file, :nil if it is already as it should be. The result is
	;not trusted, it must read as the same forms, and differ only in white
	;space, or the file is left alone.
	(defq data :nil)
	(with-read-lock file (setq data (load file)))
	(when data
		(defq formatted (format-lisp data opt_l))
		(when (nql data formatted)
			(cond
				((eql (defq before (catch (forms data) :t)) :t)
					(print "Left alone, it does not read: " file)
					:nil)
				((and (eql before (catch (forms formatted) :t)) (eql (squash data) (squash formatted))) formatted)
				(:t (print "Left alone, fmt would have changed the code: " file)
					:nil)))))

(defun write-file (file data)
	(with-write-lock file
		(when (defq out (file-stream file +file_open_write))
			(write-blk out data)
			(stream-flush out)
			(setq out :nil)))
	(print "Formatted: " file))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 4 opt_w :nil opt_c :nil opt_l 120 args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files) (lines! (# (push files %0) :nil) (io-stream 'stdin)))
		(setq files (usort (filter (lambda (file) (some (# (ends-with %0 file)) +file_types)) files)))
		(cond
			((<= (length files) opt_j)
				;do them here
				(each (lambda (file)
						(defq formatted (work file))
						(cond
							(opt_c (if formatted (print "Needs formatting: " file)))
							(opt_w (if formatted (write-file file formatted)))
							(:t (prin (ifn formatted (load file))))))
					files))
			((or opt_w opt_c)
				;farm out the checking, then do any writing here, with no
				;task still to start that might import a half written file
				(defq needs (list))
				(each (lambda ((job result))
						(each (# (if (starts-with "Needs formatting: " %0) (push needs (slice %0 18 -1)) (print %0)))
							(split result "\n")))
					(pipe-farm (map (# (str (first args) " -c -j " opt_j " -l " opt_l " " (slice (str %0) 1 -2)))
							(partition files opt_j))))
				(each (lambda (file)
						(if opt_c
							(print "Needs formatting: " file)
							(if (defq formatted (work file)) (write-file file formatted))))
					(sort needs)))
			(:t ;farm out the formatting, and print the results
				(each (lambda ((job result)) (prin result))
					(pipe-farm (map (# (str (first args) " -j " opt_j " -l " opt_l " " (slice (str %0) 1 -2)))
							(partition files opt_j))))))))
