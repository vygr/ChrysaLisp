(import "lib/asm/asm.inc")
(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "lib/task/cmd.inc")
(import "lib/task/pipe.inc")

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

(defun chop (_)
	(when (defq i (find (char 0x22) _))
		(slice _ (inc i) (find (char 0x22) _ (inc i)))))

(defun make-docs ()
	(defq *abi* (abi) *cpu* (cpu))
	(print "Scanning source files...")

	;scan for Lisp functions, macros, classes, ffi and keys info
	(defq docs_map (string-stream ""))
	(pipe-run (cat "docs -j 8 " (join (sanitize (cat
			(files-all "." '("lisp.inc" "actions.inc") 2)
			(files-all "./lib" '(".inc") 2)
			'("class/lisp/root.inc" "class/lisp/task.inc"))) " "))
		(# (write docs_map %0)))
	(setq docs_map (tree-load (string-stream (str docs_map))))

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
		(sort (. docs_map :find :classes) (# (cmp (first %0) (first %1)))))

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
		(sort (. docs_map :find :keys) (# (if (/= 0 (defq _ (cmp (first %0) (first %1))))
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
		(sort (. docs_map :find :functions) (# (cmp (first %0) (first %1)))))
	(print "-> " document)

	;create macros docs
	(defq document "docs/reference/macros.md"
		stream (file-stream document +file_open_write))
	(write-line stream (cat "# Macros" +LF))
	(each (lambda ((name info))
			(when (nempty? info)
				(write-line stream (cat "### " name +LF))
				(information stream info)))
		(sort (. docs_map :find :macros) (# (cmp (first %0) (first %1)))))
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
	(print "-> " document)

	;scan for VP classes info
	(defq *imports* (all-vp-files) classes (list) functions (list) docs (list)
		state :nil ffi_list (. docs_map :find :ffis))
	(within-compile-env (lambda ()
		(include "lib/asm/func.inc")
		(each include (all-class-files))
		(each-mergeable (lambda (file)
			(lines! (lambda (line)
				(when (eql state :info)
					(if (starts-with ";" (defq line_trim (trim line +char_class_space)))
						(push (last docs) (trim-start line_trim " ;"))
						(setq state :nil)))
				(when (eql state :nil)
					(defq line_split (split line (const (char-class " ()'\t\r\q")))
						type (sym (first line_split)) name (second line_split))
					(case type
						(include
							(make-merge *imports* (list (abs-path name file))))
						(def-class
							(push classes (list name (third line_split))))
						(dec-method
							(push (last classes) (list name (sym (third line_split)))))
						(def-method
							(setq state :info)
							(push docs (list))
							(push functions (f-path (sym name) (sym (third line_split)))))
						((def-func defun)
							(setq state :info)
							(push docs (list))
							(push functions (sym name)))
						((call jump)
							(and (eql (third line_split) ":repl_error")
								(setq line (chop line))
								(push ffi_list (list (last functions) (list line))))))))
			(file-stream file))) *imports*)))

	;create VP classes docs
	(sort classes (# (cmp (first %0) (first %1))))
	(each (lambda ((cls super &rest mthds))
		(defq stream (file-stream (cat "docs/reference/vp_classes/" cls ".md") +file_open_write))
		(write-line stream (cat "# " cls +LF))
		(unless (eql ":nil" super)
			(write-line stream (cat "## " super +LF)))
		(sort mthds (# (cmp (first %0) (first %1))))
		(defq lisp_mthds (filter (# (starts-with ":lisp_" (first %0))) mthds)
			mthds (filter (# (not (starts-with ":lisp_" (first %0)))) mthds))
		(when (nempty? lisp_mthds)
			(write-line stream (cat "## Lisp Bindings" +LF))
			(each (lambda ((mthd function))
				(when (and (defq i (some (# (if (eql function (first %0)) (!))) ffi_list))
						(defq info (first (second (elem-get ffi_list i)))))
					(write-line stream (cat "### " info +LF)))) lisp_mthds))
		(when (nempty? mthds)
			(write-line stream (cat "## VP methods" +LF))
			(each (lambda ((mthd function))
				(write-line stream (cat "### " mthd " -> " function +LF))
				(when (and (defq i (find function functions))
						(/= 0 (length (defq info (elem-get docs i)))))
					(write-line stream "```code")
					(each (# (write-line stream %0)) info)
					(write-line stream (const (str "```" +LF))))) mthds))
		(print (cat "-> docs/reference/vp_classes/" cls ".md"))) classes))

(defun make-ai ()
	(defq folders (Lmap) cmds (list))
	(each (lambda (file)
			(defq folder "host")
			(if (defq i (find "/" file)) (setq folder (slice file 0 i)))
			(. folders :update folder (# (if %0 (push %0 file) (list file)))))
		(files-all "." '("Makefile" ".vp" ".inc" ".lisp" ".c" ".cpp" ".h" ".sh" ".ps1" ".bat") 2))
	(. folders :each (# (push cmds (cat "cat -f " (join %1 " ") " | tee ai/" %0 ".txt | null"))))
	(pipe-farm cmds))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(each (# (def (penv) (sym %0) (find %0 args)))
			'("all" "platforms" "boot" "docs" "it" "test" "ai"))
		(cond
			(test (make-test))
			(it (remake-all-platforms) (make-docs))
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
