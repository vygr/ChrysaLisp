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

	If no count options are given, defaults
	to all (words, lines, paragraphs).

	If no paths given on command line
	then paths are read from stdin.")
(("-wc") ,(opt-flag 'opt_wc))
(("-lc") ,(opt-flag 'opt_lc))
(("-pc") ,(opt-flag 'opt_pc))
))

;do the work on a file
(defun work (file)
	(defq word_count 0 line_count 0 paragraph_count 0 in_paragraph_flag :nil)
	(lines! (lambda (line)
		;good practice for long files
		(task-slice)
		(setq line_count (inc line_count))
		;word count for the current line
		(defq words_on_line (split line)
			word_count (+ word_count (length words_on_line)))
		;paragraph count logic
		(if (empty? words_on_line)
			(setq in_paragraph_flag :nil)
			(if (not in_paragraph_flag)
				(setq paragraph_count (inc paragraph_count) in_paragraph_flag :t))))
		(file-stream file))
	;construct output string
	(defq output_parts (list file))
	(if (or opt_wc default_all_flag) (push output_parts (str word_count)))
	(if (or opt_lc default_all_flag) (push output_parts (str line_count)))
	(if (or opt_pc default_all_flag) (push output_parts (str paragraph_count)))
	(print (join output_parts ", ")))

(defun main ()
	;initialize flags for options
	(defq opt_wc :nil opt_lc :nil opt_pc :nil)
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;determine if we need to default to all counts
		(defq default_all_flag (not (or opt_wc opt_lc opt_pc)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(if (nempty? jobs) (work (pop jobs)))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (cat
					(first args)
					(if opt_wc " -wc" "")
					(if opt_lc " -lc" "")
					(if opt_pc " -pc" "")
					" " %0)) jobs))))))
