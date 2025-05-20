(import "lib/asm/asm.inc")
(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: make [options] [all] [boot] [platforms] [doc] [it] [test]

	options:
		-h --help: this help info.

	all:        include all .vp files.
	boot:       create a boot image.
	platforms:  for all platforms not just the host.
	docs:       scan source files and create documentation.
	it:         all of the above !
	test:       test make timings.")
))

(defq +LF (ascii-char 10))

(defun parent? (info)
	(some! (# (if (bfind %0 +char_class_upper) %0))
		(list info) :nil 2))

(defun information (stream info)
	(when (nempty? info)
		(write-line stream "```code")
		(write-line stream (first info))
		(setq info (rest info))
		(when (nempty? info)
			(write-line stream "")
			(each (# (write-line stream %0)) info))
		(write-line stream (cat "```" +LF))))

(defun sanitize (_)
	(defq out (cap (length _) (list)))
	(. (reduce (# (. %0 :insert %1)) _ (Fset 31)) :each
		(# (unless (some (lambda (_) (starts-with _ %0))
			'("lib/asm/" "lib/trans/" "lib/keys/"))
			(push out %0)))) out)

(defun make-docs ()
	(defq *abi* (abi) *cpu* (cpu))
	(defun chop (_)
		(when (defq e (rfind (char 0x22) _))
			(setq _ (slice _ 0 e))
			(slice _ (inc (rfind (char 0x22) _)) -1)))
	(print "Scanning source files...")

	;scan for VP classes info
	(defq *imports* (all-vp-files) classes (list) functions (list)
		docs (list) syntax (list) state :x)
	(within-compile-env (lambda ()
		(include "lib/asm/func.inc")
		(each include (all-class-files))
		(each-mergeable (lambda (file)
			(each-line (lambda (line)
				(when (eql state :y)
					(defq s (split line +char_class_space))
					(if (and (> (length s) 0) (starts-with ";" (first s)))
						(push (last docs) (slice line (inc (find ";" line)) -1))
						(setq state :x)))
				(when (and (eql state :x) (>= (length line) 9))
					(defq s (split line (const (char-class " ()'\t\r\q"))) _ (first s))
					(cond
						((eql _ "include")
							(make-merge *imports* (list (abs-path (second s) file))))
						((eql _ "def-class")
							(push classes (list (second s) (third s))))
						((eql _ "dec-method")
							(push (last classes) (list (second s) (sym (third s)))))
						((eql _ "def-method")
							(setq state :y)
							(push docs (list))
							(push functions (f-path (sym (second s)) (sym (third s)))))
						((or (eql _ "def-func") (eql _ "defun"))
							(setq state :y)
							(push docs (list))
							(push functions (sym (second s))))
						((and (or (eql _ "call") (eql _ "jump")) (eql (third s) ":repl_error"))
							(when (and (setq line (chop line)) (not (find line syntax)))
								(push syntax line)
								(push (last docs) "lisp binding" line)))))) (file-stream file))) *imports*)))

	;create VP classes docs
	(sort classes (# (cmp (first %0) (first %1))))
	(each (lambda ((cls super &rest methds))
		(defq stream (file-stream (cat "docs/reference/vp_classes/" cls ".md") +file_open_write))
		(write-line stream (cat "# " cls +LF))
		(unless (eql ":nil" super)
			(write-line stream (cat "## " super +LF)))
		(sort methds (# (cmp (first %0) (first %1))))
		(defq lisp_methds (filter-array (# (starts-with ":lisp_" (first %0))) methds)
			methds (filter-array (# (not (starts-with ":lisp_" (first %0)))) methds))
		(when (nempty? lisp_methds)
			(write-line stream (cat "## Lisp Bindings" +LF))
			(each (lambda ((methd function))
				(when (and (defq i (find function functions))
						(/= 0 (length (defq info (elem-get docs i)))))
					(when (defq i (find "lisp binding" info))
						(write-line stream (cat "### " (elem-get info (inc i)) +LF))
						(setq info (slice info 0 i))))) lisp_methds))
		(when (nempty? methds)
			(write-line stream (cat "## VP methods" +LF))
			(each (lambda ((methd function))
				(write-line stream (cat "### " methd " -> " function +LF))
				(when (and (defq i (find function functions))
						(/= 0 (length (defq info (elem-get docs i)))))
					(write-line stream "```code")
					(each (# (write-line stream %0)) info)
					(write-line stream (const (str "```" +LF))))) methds))
		(print (cat "-> docs/reference/vp_classes/" cls ".md"))) classes)

	;scan for Lisp functions and classes info
	(defq classes (list) functions (list) macros (list) keys (list))
	(each (lambda (file)
		(defq state :nil info :nil methods :nil)
		(each-line (lambda (line) (while line
			(case state
				((:function :class :method)
					(cond
						((and (nempty? (defq s (split line +char_class_space)))
								(starts-with ";" (first s)))
							(push info (trim (slice line (inc (find ";" line)) -1)))
							(setq line :nil))
						((setq state :nil))))
				(:keys
					(cond
						((nempty? (defq s (split line +char_class_space)))
							(push info (trim-start (trim-end line ")") (ascii-char 9)))
							(setq line :nil))
						((setq state :nil))))
				(:t (defq words (split line (const (char-class " ()'\t\r"))) line :nil)
					(when (>= (length words) 2)
						(defq type (first words) name (second words))
						(unless (or (some (# (eql name %0))
										'(":type_of" ",predn" ",n"
										"_structure" "defun" "defmacro"))
									(starts-with "action-" name))
							(case type
								(("*key_map*" "*key_map_shift*" "*key_map_control*")
									(push keys (list file type (setq info (list))))
									(setq state :keys))
								("defun"
									(push functions (list name (setq info (list))))
									(setq state :function))
								("defmacro"
									(push macros (list name (setq info (list))))
									(setq state :function))
								("defclass"
									(push classes (list name (parent? words)
										(setq methods (list)) (setq info (list))))
									(setq state :class))
								(("defmethod" "deffimethod" "defabstractmethod")
									(push methods (list name (setq info (list))))
									(setq state :method))
								("defgetmethod"
									(push methods (list (cat ":get_" name) (setq info (list))))
									(setq state :nil))
								("defsetmethod"
									(push methods (list (cat ":set_" name) (setq info (list))))
									(setq state :nil)))))))))
			(file-stream file)))
		(sanitize (cat
			(files-all "." '("lisp.inc" "actions.inc") 2)
			(files-all "./lib" '(".inc") 2)
			'("class/lisp/root.inc" "class/lisp/task.inc"))))

	;create classes docs
	(each (lambda ((name pname methods info))
			(defq document (cat "docs/reference/classes/" name ".md")
				stream (file-stream document +file_open_write))
			(write-line stream (cat "# " name +LF))
			(if pname (write-line stream (cat "## " pname +LF)))
			(information stream info)
			(each (lambda ((name info))
					(write-line stream (cat "### " name +LF))
					(information stream info))
				(sort methods (# (cmp (first %0) (first %1)))))
			(print "-> " document))
		(sort classes (# (cmp (first %0) (first %1)))))

	;create key bindings docs
	(defq document "docs/reference/keys.md" current_file ""
		stream (file-stream document +file_open_write))
	(write-line stream (cat "# Key Bindings" +LF))
	(each (lambda ((file name info))
			(unless (eql file current_file)
				(write-line stream (cat "## " file +LF))
				(setq current_file file))
			(when (nempty? info)
				(write-line stream (cat "### " name +LF))
				(write-line stream "```code")
				(each (# (write-line stream %0)) info)
				(write-line stream (cat "```" +LF))))
		(sort keys (# (if (/= 0 (defq _ (cmp (first %0) (first %1))))
			_ (cmp (second %0) (second %1))))))
	(print "-> " document)

	;create functions docs
	(defq document "docs/reference/functions.md"
		stream (file-stream document +file_open_write))
	(write-line stream (cat "# Functions" +LF))
	(each (lambda ((name info))
			(when (nempty? info)
				(write-line stream (cat "### " name +LF))
				(information stream info)))
		(sort functions (# (cmp (first %0) (first %1)))))
	(print "-> " document)

	;create macros docs
	(defq document "docs/reference/macros.md"
		stream (file-stream document +file_open_write))
	(write-line stream (cat "# Macros" +LF))
	(each (lambda ((name info))
			(when (nempty? info)
				(write-line stream (cat "### " name +LF))
				(information stream info)))
		(sort macros (# (cmp (first %0) (first %1)))))
	(print "-> " document)

	;create commands docs
	(defq document "docs/reference/commands.md"
		stream (file-stream document +file_open_write))
	(each (lambda ((job result))
			(write-line stream (cat "## " (slice job 0 -4)))
			(write-line stream "```code")
			(write stream result)
			(write-line stream "```"))
		(sort (pipe-farm (map (# (cat %0 " -h"))
					(files-all "cmd" '(".lisp") 4 -6)))
			(# (cmp (first %0) (first %1)))))
	(print "-> " document))

(defun make-ai ()
	(defq folders (Lmap)cmds (list))
	(each (lambda (file)
			(defq folder "host")
			(if (defq i (find "/" file)) (setq folder (slice file 0 i)))
			(. folders :update folder (# (if %0 (push %0 file) (list file)))))
		(files-all "." '("Makefile" ".vp" ".inc" ".lisp" ".c" ".cpp" ".h" ".sh" ".ps1" ".bat") 2))
	(. folders :each
		(# (push cmds (cat "cat -f " (apply cat (join %1 '(" "))) " | tee ai/" %0 ".txt | null"))))
	(pipe-farm cmds))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq all (rfind "all" args) boot (rfind "boot" args)
			platforms (rfind "platforms" args) docs (rfind "docs" args)
			it (rfind "it" args) test (rfind "test" args)
			ai (rfind "ai" args))
		(cond
			(test (make-test))
			(it (make-docs) (remake-all-platforms))
			((and boot all platforms) (remake-all-platforms))
			((and boot all) (remake-all))
			((and boot platforms) (remake-platforms))
			((and all platforms) (make-all-platforms))
			(all (make-all))
			(platforms (make-platforms))
			(boot (remake))
			(docs (make-docs))
			(ai (make-ai))
			(:t (make)))))
