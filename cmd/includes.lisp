(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/tokenize.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: includes [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -d --defs defs: class definitions map, default :nil.
        -w --write: write new file, default :nil.

    Scan for needed includes in .vp files, optionally
    edits the file rewriting the include block.

    A file is needed for the classes whose methods are called,
    for the constants that are used, and for the inline functions
    and macros of a class.inc that are used. What such an inline
    needs in turn, its class.inc must include for itself.

    Another .inc a file includes, lib/trans/vpb_trans.inc say,
    is text put in the file: it is needed if the file uses a
    name it defines, and what it uses is then needed too.

    If no paths given on command line
    then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-d" "--defs") ,(opt-str 'opt_d))
(("-w" "--write") ,(opt-flag 'opt_w))
))

;a name that is spliced in, ,lk_node_size, is that name
(defq +split_class (char-class " ()'\t\r\q{@}<>,`")
	+implicit_file "lib/asm/func.inc"
	+class_prefixes ''(
		(":fixeds" "+fixeds_") (":fixed" "+fixed_")
		(":nums" "+nums_") (":num" "+num_")
		(":out" "+stream_mail_" "+out_") (":stream" "+stream_")
		(":lisp" "+lisp_") (":str" "+str_")
		(":seq" "+seq_") (":array" "+array_") (":list" "+list_")
		(":dim" "+dim_") (":sys_math" "+math_")
		(":hset" "+hset_") (":hmap" "+hmap_") (":fstream" "+fstream_") (":lstream" "+lstream_")
		(":sstream" "+sstream_") (":mstream" "+mstream_") (":in" "+in_")
		(":netid" "+netid_") (":nodeid" "+nodeid_") (":stdio" "+stdio_")
		(":error" "+error_") (":func" "+func_")
		(":sys_task" "+tk_") (":pset" "+pset_") (":pmap" "+pmap_")
		(":host_os" "+stat_" "+file_open_" "+mmap_")
		(":host_audio" "+audio_") (":host_clipboard" "+clip_")
		(":view" "+view_") (":ctx" "+ctx_") (":path" "+path_") (":canvas" "+canvas_")
		(":pixmap" "+pixmap_") (":vdu" "+vdu_") (":texture" "+texture_")
		(":font" "+font_") (":sys_heap" "+hp_") (":sys_mem" "+mem_")
		(":region" "rect_") (":sys_link" "lk_") (":sys_load" "ld_")
		(":sym" "static_sym_") (":host_gui" "+ev_"))
	+require_prefixes ''(
		("sys/statics/class.inc" "statics_" "sys/statics/statics")
		("lib/consts/chars.inc" "+char_")
		("lib/consts/colors.inc" "+argb_")
		("lib/consts/scodes.inc" "+sc_")))
(defq +known_files (list quote (cat (list +implicit_file) (map (const first) +require_prefixes))))

;lookup the file the class was defined in
;else just use one they all need
(defun find-file (cls)
	(if (defq file (. defs_map :find cls)) file +implicit_file))

;what a file includes, its (include) lines alone: an (import) is made
;once, in the environment that first asks for it, so what it defines
;need not be in the one this file is made in
(defun includes-of (file)
	(defq files (list))
	(when (defq stream (file-stream file))
		(lines! (# (and (starts-with "(include \q" %0)
					(push files (path-to-absolute
						(slice %0 (defq i (inc (find "\q" %0))) (find "\q" %0 i)) file)))
				:nil)
			stream))
	files)

;the kept includes in an order each comes after those whose names it
;uses, as it is read where it is put: vpb_trans.inc after vpb_ops.inc
(defun include-order (kept uses defines)
	(defq out (list) left (range 0 (length kept)))
	(until (empty? left)
		(defq i (or (some (lambda (i)
					(when (notany (lambda (j)
							(and (/= i j) (some (# (. (elem-get uses i) :find %0)) (elem-get defines j))))
							left) i))
				left) (first left)))
		(push out (elem-get kept i))
		(setq left (filter (# (/= %0 i)) left)))
	out)

;all a file includes, and all those include
(defun depends-of (file)
	(defq files (list file))
	(each-mergeable (# (merge files (includes-of %0))) files)
	files)

;the names a file that is not a class.inc defines at the start of a line,
;none if it defines them some other way, a struct say
(defun defines-of (file)
	(defq names (list))
	(with-read-lock file
		(when (defq in_s (file-stream file))
			(lines! (# (defq input (split %0 +split_class))
					(and (starts-with "(" %0) (second input)
						(find (first input) '("defun" "defmacro" "defq" "defcvar"))
						(push names (second input)))
					:nil)
				in_s)))
	names)

;read a file for what it includes, the classes and the files its words
;need, and its words
(defun scan (file includes classes requires words)
	;The file is read as the highlighter reads it, a token at a time, so
	;what is in a comment is not looked at, on a line of its own or on
	;the end of one. The words of a line are its tokens, each taken apart
	;at what is not of a name: a string in braces has names in it
	(with-read-lock file
		(defq tok_state :text)
		(lines! (lambda (line)
			(task-slice)
			(bind '(toks states tok_next) (tokenize-line (trim-end line "\r") tok_state))
			(setq tok_state tok_next)
			(defq input (list))
			(each (lambda (tok state)
				(unless (eql state :comment)
					(each (# (push input %0)) (split tok +split_class)))) toks states)
			(case (first input)
				(("include")
					(merge includes (list (second input))))
				(("gen-type")
					(merge classes (list ":list" ":sym")))
				(("call" "entry" "exit" "def-method" "gen-vtable" "gen-create"
					"to-array" "jump" "f-bind")
					(merge classes (list (second input))))
				(("save-fields" "load-fields" "assign-fields"
						"save-net-id" "load-net-id" "assign-net-id")
					(merge requires (list "class/obj/class.inc")))
				(("host-os-call" "gen-host-call" "host-gui-call" "host-audio-call")
					(merge requires (list "sys/statics/class.inc"))
					(merge classes (list (second input))))
				(("signature")
					(merge classes (map! (const to-lower) (list input) 1))))
			(each (lambda (token)
				(. words :insert token)
				(cond
					((eql "f-path" token) (merge classes (list (elem-get input (inc (!))))))
					((eql "vec-set" token) (merge classes '(":sys_math")))
					((some (lambda (item)
						(when (some! (# (starts-with %0 token)) (list item) :nil 1)
							(merge classes (list (first item))) :t)) +class_prefixes))
					((some (lambda (item)
						(when (some! (# (starts-with %0 token)) (list item) :nil 1)
							(merge requires (list (first item))) :t)) +require_prefixes))
					;an inline function or macro that a class.inc defines, the
					;file that defines it is needed, (hmap-search) say
					((and (not (starts-with ":" token)) (defq inline_file (. defs_map :find token)))
						(merge requires (list inline_file)))))
				input) :nil) (file-stream file))))

;do the work on a file
(defun work (file)
	(defq includes (list) classes (list) requires (list +implicit_file) words (Fset 101))
	(scan file includes classes requires words)
	(setq includes (map (# (path-to-absolute %0 file)) includes))
	;an include not a class.inc, nor of the constants it knows, is text put
	;in the file: it is needed when the file uses a name it defines, or if
	;it defines none that can be seen, and then what it uses the file uses.
	;An apps/ one is kept as it is, it has the app's Lisp in it too
	(defq kept (list) uses (list) defines (list) more :t)
	(while more
		(setq more :nil)
		(each (lambda (incl)
				(unless (or (starts-with "apps/" incl) (ends-with "class.inc" incl)
						(find incl +known_files) (find incl kept))
					(defq defs (defines-of incl))
					(when (or (empty? defs) (some (# (. words :find %0)) defs))
						(defq own (Fset 101))
						(scan incl (list) classes requires own)
						(. own :each (# (. words :insert %0)))
						(push kept incl) (push uses own) (push defines defs)
						(setq more :t))))
			includes))
	;convert to the files we need, keep any apps/ include files !
	(merge requires (map (const find-file) classes))
	(setq requires (map (# (path-to-absolute %0 file)) requires))
	(merge requires kept)
	(merge requires (filter (# (starts-with "apps/" %0)) includes))
	;remove any redundant files !
	(defq depends_set (Fset 101))
	(each (lambda (file)
		(each (# (. depends_set :insert %0))
			(. depends_cache :update file (# (ifn %0
				(filter (# (nql %0 file)) (depends-of file))))))) requires)
	(setq requires (filter (# (not (. depends_set :find %0))) requires))
	;sort and see if we need to report a difference and maybe rewrite
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
			;the classes and constants, then the kept includes in their order
			(defq requires (cat (list +implicit_file)
					(reverse (sort (map (# (path-to-relative %0 file))
						(filter (# (not (or (eql %0 +implicit_file) (find %0 kept)))) requires))))
					(map (# (path-to-relative %0 file))
						(filter (# (find %0 requires)) (include-order kept uses defines))))
				no_includes (memory-stream))
			(with-write-lock file
				(when (defq in_s (file-stream file))
					(lines! (lambda (line)
							(defq input (split line +split_class))
							(unless (starts-with "include" (first input))
								(write-line no_includes line))
							:nil)
						in_s)
					(setq in_s :nil))
				(when (defq stream (file-stream file +file_open_write))
					(each (# (write-line stream (cat "(include \q" %0 "\q)"))) requires)
					(stream-seek no_includes 0 0)
					(lines! (# (write-line stream %0) :nil) no_includes)
					(stream-flush stream)
					(setq stream :nil))))))


(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_d :nil opt_w :nil args (options stdio usage)))
		(defq defs_map (Fmap 11) depends_cache (Fmap 101))
		(cond
			(opt_d
				;we are given a defs map
				(each (# (bind '(cls file) (split %0 "|"))
						(. defs_map :insert cls file))
					(split opt_d "[]")))
			(:t ;must build a defs map
				(each (lambda (file)
						(with-read-lock file
							(when (defq in_s (file-stream file))
								(lines! (# (defq input (split %0 +split_class))
										(cond
											((eql (first input) "def-class")
												(. defs_map :insert (second input) file))
											;the inline functions and macros it defines, but
											;not those of the assembler, every file has them
											((and (not (starts-with "./lib/asm/" file)) (not (starts-with "lib/asm/" file))
													(or (starts-with "(defun " %0) (starts-with "(defmacro " %0)))
												(. defs_map :insert (second input) file)))
										:nil)
									in_s)
								(setq in_s :nil))))
					(files-all "." '("class.inc") 2))

				(setq opt_d (list))
				(. defs_map :each (# (push opt_d (cat "[" %0 "|" %1 "]"))))
				(setq opt_d (apply (const cat) opt_d))))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		;only .vp files
		(setq jobs (filter (# (ends-with ".vp" %0)) jobs))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if opt_d (cat " -d \q" opt_d "\q") "")
						(if opt_w " -w " "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
