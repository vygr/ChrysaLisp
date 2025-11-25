(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: includes [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 8.
		-s --super super: inheritance map, default :nil.
		-w --write: write new file, default :nil.

	Scan for needed includes in .vp files, optionally
	edits the file rewriting the include block.

	If no paths given on command line
	then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-s" "--super") ,(opt-str 'opt_s))
(("-w" "--write") ,(opt-flag 'opt_w))
))

(defq +split_class (char-class " :()'\t\r\q{@}<>")
	+implicit_file "lib/asm/func.inc")

;lookup the file the class was defined in
;else just use one they all need
(defun find-file (cls)
	(if (defq file (. defs_map :find cls)) file +implicit_file))

;do the work on a file
(defun work (file)
	(defq includes (list) classes (list) requires (list +implicit_file))
	(lines! (lambda (line)
			(case (first (defq line_split (split line +split_class)))
				(("include")
					(merge includes (list (second line_split))))
				(("gen-type")
					(merge classes (list "list" "sym")))
				(("call" "entry" "exit" "def-method" "gen-vtable" "gen-create"
					"to-array" "jump" "f-bind")
					(merge classes (list (second line_split))))
				(("host-os-call" "host-gui-call" "host-audio-call")
					(merge requires (list "sys/statics/class.inc"))
					(merge classes (list (second line_split))))
				(("signature")
					(merge classes (rest line_split))))
			(each (lambda (token)
				(cond
					((eql "f-path" token) (merge classes (list (elem-get line_split (inc (!))))))
					((eql "vec-set" token) (merge classes '("sys_math")))
					((starts-with "view_" token) (merge classes '("view")))
					((starts-with "rect_" token) (merge classes '("region")))
					((starts-with "pixmap_" token) (merge classes '("pixmap")))
					((starts-with "lk_" token) (merge classes '("sys_link")))
					((starts-with "ld_" token) (merge classes '("sys_load")))
					((starts-with "stream_mail_state_" token) (merge classes '("out")))
					((starts-with "static_sym_" token) (merge classes '("sym")))
					((starts-with "+char_" token) (merge requires '("lib/consts/chars.inc")))
					((starts-with "+argb_" token) (merge requires '("lib/consts/colors.inc")))
					((starts-with "sys/statics/statics" token) (merge requires '("sys/statics/class.inc")))))
				line_split))
		(file-stream file))
	;don't include classes where we include one of their subclasses
	(defq ancestors (reduce (lambda (anc cls)
		(while (and (setq cls (. super_map :find cls))
				(not (eql cls "nil"))
				(not (find cls anc)))
			(push anc cls)) anc) classes (list))
		classes (filter (# (not (find %0 ancestors))) classes))
	;convert to the files we need
	(merge requires (map (const find-file) classes))
	(defq includes (map (# (path-to-absolute %0 file)) includes)
		requires (map (# (path-to-absolute %0 file)) requires))
	;keep any apps/ include files ! And remove any redundant
	(merge requires (filter (# (starts-with "apps/" %0)) includes))
	(defq requires (push (filter (# (not (find %0 redundant))) requires) +implicit_file))
	(sort includes) (sort requires)
	(when (or (/= (length includes) (length requires))
			(notevery (const eql) includes requires))
		(print "File: " file)
		(print "Includes:")
		(each (const print) includes)
		(print "Requires:")
		(each (const print) requires)
		;rewrite the file ?
		(when opt_w
			(defq requires (cat (list +implicit_file)
					(reverse (sort (map (# (abs-relative %0 file))
						(filter (# (not (eql %0 +implicit_file))) requires)))))
				no_includes (memory-stream))
			(lines! (lambda (line)
					(defq line_split (split line +split_class))
					(unless (starts-with "include" (first line_split))
						(write-line no_includes line)))
				(file-stream file))
			(defq stream (file-stream file +file_open_write))
			(each (# (write-line stream (cat "(" {include "} %0 {"} ")"))) requires)
			(stream-seek no_includes 0 0)
			(lines! (# (write-line stream %0)) no_includes))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_s :nil opt_w :nil args (options stdio usage)))
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
		;all depends of the lib/asm/func.inc file !
		;we will filter these out
		(defq redundant (files-all-depends (list +implicit_file)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		;only .vp files
		(setq jobs (filter (# (ends-with ".vp" %0)) jobs))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if opt_s (cat " -s " opt_s) "")
						(if opt_w " -w " "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
