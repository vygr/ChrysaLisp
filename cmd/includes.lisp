(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: includes [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.
		-s --super super: inheritance map, default :nil.

	Scan for needed includes in .vp files, optionally
	edits the file rewriting the include block.

	If no paths given on command line
	then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-s" "--super") ,(opt-str 'opt_s))
))

(defq +split_class (char-class " :()'\t\r\q"))

;do the work on a file
(defun work (file)
	(when (ends-with ".vp" file)
		(defq includes (list) requires (list "lib/asm/func.inc"))
		(lines! (lambda (line)
				(defq line_split (split line +split_class))
				(case (first line_split)
					("include"
						(merge includes (list (second line_split))))
					(("call" "entry" "exit" "def-method")
						(merge requires (list (. defs_map :find (second line_split)))))
					)
				(when (some (# (starts-with "+char_" %0)) line_split)
					(merge requires (list "lib/consts/chars.inc")))
				)
			(file-stream file))
		(defq includes (sort (map (# (abs-path %0 file)) includes))
			requires (sort (map (# (abs-path %0 file)) requires)))
		(when (or (/= (length includes) (length requires))
				(notany (const eql) includes requires))
			(print "File: " file)
			(print "Includes:")
			(each (const print) includes)
			(print "Requires:")
			(each (const print) requires)
			;rewrite the file
			(defq requires (cat (list "lib/asm/func.inc")
				(filter (# (not (eql %0 "lib/asm/func.inc"))) requires)))
			(defq no_includes (memory-stream))
			(lines! (lambda (line)
					(defq line_split (split line +split_class))
					(unless (starts-with "include" (first line_split))
						(write-line no_includes line)))
				(file-stream file))
			(defq stream (file-stream file +file_open_write))
			(each (# (write-line stream (cat "(" {include "} %0 {"} ")"))) requires)
			(stream-seek no_includes 0 0)
			(lines! (# (write-line stream %0)) no_includes))
		))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 opt_s :nil args (options stdio usage)))
		(defq super_map (Fmap 11) defs_map (Fmap 11))
		(cond
			(opt_s
				;we are given a super map
				(each (# (bind '(key val file) (split %0 ":"))
						(. super_map :insert key val)
						(. defs_map :insert key file))
					(split opt_s "[]")))
			(:t	;must build a super map
				(each (lambda (file)
						(lines! (# (defq line_split (split %0 +split_class))
								(when (eql (first line_split) "def-class")
									(. super_map :insert (second line_split) (third line_split))
									(. defs_map :insert (second line_split) file)))
							(file-stream file)))
					(files-all "." '("class.inc") 2))
				(setq opt_s (list))
				(. super_map :each (# (push opt_s (cat "[" %0 ":" %1 ":" (. defs_map :find %0 )"]"))))
				(setq opt_s (apply (const cat) opt_s))))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if opt_s (cat " -s " opt_s) "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))
		;output results
		))
