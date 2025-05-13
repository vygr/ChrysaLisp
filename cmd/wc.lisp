(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: wc [options] [path] ...

	options:
		-h --help: this help info.
		-wc: count words.
		-lc: count lines.
		-pc: count paragraphs.

	If no count options are given, defaults to all (words, lines, paragraphs).

	If no paths given on command line
	then paths are read from stdin.")
(("-wc")
	,(lambda (args arg) (setq wc_flag :t) args))
(("-lc")
	,(lambda (args arg) (setq lc_flag :t) args))
(("-pc")
	,(lambda (args arg) (setq pc_flag :t) args))
))

;do the work on a file
(defun work (file)
	(defq word_count 0 line_count 0 paragraph_count 0 in_paragraph_flag :nil)

	(each-line (lambda (line)
		(task-slice) ; Good practice for long files
		(setq line_count (inc line_count))

		; Word count for the current line
		(defq words_on_line (split line)) ; Store the result of split
		(setq word_count (+ word_count (length words_on_line)))

		; Paragraph count logic
		(if (empty? words_on_line) ; Check if the list of words is empty
			(setq in_paragraph_flag :nil)
			(when (not in_paragraph_flag)
				(setq paragraph_count (inc paragraph_count))
				(setq in_paragraph_flag :t))))
		(file-stream file))

	; Construct output string
	(defq output_parts (list file))
	(if (or wc_flag default_all_flag) (push output_parts (str word_count)))
	(if (or lc_flag default_all_flag) (push output_parts (str line_count)))
	(if (or pc_flag default_all_flag) (push output_parts (str paragraph_count)))
	(print (apply (const cat) (join output_parts '(", ")))))

(defun main ()
	;initialize flags for options
	(defq wc_flag :nil lc_flag :nil pc_flag :nil)

	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))

		; Determine if we need to default to all counts
		(defq default_all_flag (not (or wc_flag lc_flag pc_flag)))

		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(each-line (# (push jobs %0)) (io-stream 'stdin)))

		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(if (nempty? jobs) (work (pop jobs))) ; Handle case of no input at all
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (cat
									(first args) ; This will be "wc"
									(if wc_flag " -wc" "")
									(if lc_flag " -lc" "")
									(if pc_flag " -pc" "")
									" "
									%0)) jobs))))))