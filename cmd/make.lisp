(import "lib/asm/asm.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/task/pipe.inc")

(defq usage `(
(("-h" "--help")
"Usage: make [options] [all] [boot] [platforms] [doc] [it] [apps]
    [release] [debug] [vp] [test] [fmt]

    options:
        -h --help: this help info.
        -v --verbosity num: how much info, default 0.

    all:        include all .vp files.
    boot:       create a boot image.
    platforms:  for all platforms not just the host.
    docs:       scan source files and create documentation.
    vp:         the VP64 and obj/vp/ outputs.
    it:         all of the above, and then a check of the include
                and import lists, what includes and imports would
                change is printed.
    apps:       only the apps !
    release:    it/apps release mode.
    debug:      it/apps debug mode.
    validate:   it/apps validate mode.
    test:       test make timings.
    fmt:        format all the source files, with the fmt command.")
(("-v" "--verbosity") ,(opt-num 'opt_v))
))

(defq +LF "\n"
	+ai_excluded_files
		''(ai/ deps/ docs/ fonts/ usr/ obj/ apps/demos/boing/ apps/demos/bubbles/
		apps/demos/canvas/ apps/demos/freeball/ apps/desktop/calculator/
		apps/desktop/chat/ apps/desktop/clock/ apps/desktop/eyes/ app/games/
		apps/media/films/ apps/media/images/ apps/science/molecule/ apps/system/files/
		apps/system/netspeed/ apps/system/services/ apps/system/wallpaper/
		apps/tools/benchmark/ apps/tools/fonts/ cmd/cat.lisp cmd/cp.lisp cmd/diff.lisp
		cmd/dump.lisp cmd/echo.lisp cmd/files.lisp cmd/gui.lisp cmd/hbook.lisp
		cmd/head.lisp cmd/huff.lisp cmd/link.lisp cmd/lz4.lisp cmd/mv.lisp
		cmd/nodes.lisp cmd/null.lisp cmd/patch.lisp cmd/repeat.lisp cmd/rle.lisp
		cmd/rm.lisp cmd/save.lisp cmd/sdir.lisp cmd/shuffle.lisp cmd/slice.lisp
		cmd/sort.lisp cmd/split.lisp cmd/stats.lisp cmd/tail.lisp cmd/time.lisp
		cmd/tocpm.lisp cmd/toflm.lisp cmd/unhuff.lisp cmd/unique.lisp cmd/unlz4.lisp
		cmd/unrle.lisp cmd/vpstats.lisp cmd/wc.lisp)
	+ai_excluded_files_onslaught
		''(apps/games/onslaught/sfx/ apps/games/onslaught/image/ apps/games/onslaught/data/))

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

(defun chop (%0)
	(when (defq i (find (char 0x22) %0))
		(slice %0 (inc i) (find (char 0x22) %0 (inc i)))))

(defun vp-fields (file name)
	; (vp-fields file name) -> (str ...)
	;the fields of a VP class, each as its type and its name, from the
	;structure of that name in the struct.inc beside its class.inc
	(defq out (list) state :nil at (rfind "/" file)
		stream (file-stream (cat (slice file 0 (ifn at 0)) "struct.inc")))
	(when stream
		(lines! (lambda (line)
			(cond
				((not state)
					(if (or (starts-with (cat "(structure +" name " ") line) (starts-with (cat "(def-struct +" name " ") line))
						(setq state :in)))
				((eql state :in)
					(defq words (split line (const (char-class " ()\t\r"))))
					(cond
						((empty? words) (setq state :done))
						((find (first words) '("offset" "align")))
						((eql (first words) "struct") (push out (cat "struct " (second words))))
						(:t (each (# (push out (cat (first words) " " %0))) (rest words))))
					(if (ends-with "))" (trim line (const (char-class " \t\r")))) (setq state :done))))
			:nil) stream))
	out)

(defun chain-of (name supers)
	; (chain-of name supers) -> (name ...)
	;what a class comes of, from the first down to its parent, by a map
	;of each class to its parent
	(defq out (list) seen 0)
	(while (and (< (setq seen (inc seen)) 32) (defq parent (. supers :find name)))
		(push out parent)
		(setq name parent))
	(reverse out))

(defun diagram-save (doc file)
	(cwb-save doc (file-stream file +file_open_write))
	(if (> *build_verb* 0) (print "-> " file)))

(defun make-docs ()
	(print "Scanning source files...")
	;the diagrams of the reference are made here too, lib/cwb/diagram.inc.
	;It is brought in when docs are made and not as this file is read,
	;a build of the system has no use for it
	(import "lib/cwb/diagram.inc")

	;scan for Lisp functions, macros, classes, ffi and keys info
	(defq docs_map (string-stream ""))
	(pipe-run (cat "docs -j 8 " (join (sanitize (cat
			(files-all "." '("lisp.inc" "actions.inc") 2)
			(files-all "./lib" '(".inc") 2)
			'("class/lisp/root.inc" "class/lisp/task.inc"))) " "))
		(# (write-blk docs_map %0)))
	(setq docs_map (tree-load (string-stream (str docs_map))))

	;create classes docs, each with its diagram: what it comes of, its
	;methods, and what comes of it
	(defq lisp_supers (Fmap 31) lisp_nodes (list))
	(each (lambda ((name pname &ignore))
		(push lisp_nodes (list name pname))
		(if pname (. lisp_supers :insert name pname)))
		(. docs_map :find :classes))
	(each (lambda ((name pname methods info))
			(defq document (cat "docs/reference/classes/" name ".md")
				stream (file-stream document +file_open_write))
			(write-line stream (cat "# " name +LF))
			(write-line stream (cat "```image" +LF "docs/reference/classes/" name ".cwb" +LF "```" +LF))
			(diagram-save (dia-class name (if (nempty? info) (first info) "")
					(chain-of name lisp_supers)
					(sort (map (const first) (filter (# (eql (second %0) name)) lisp_nodes)) (const cmp))
					(list (list "methods" (sort (map (const first) methods) (const cmp)))))
				(cat "docs/reference/classes/" name ".cwb"))
			(if pname (write-line stream (cat "## " pname +LF)))
			(information stream info)
			(each (lambda ((name info))
					(write-line stream (cat "### " name +LF))
					(information stream info))
				(sort methods (# (cmp (first %0) (first %1)))))
			(if (> *build_verb* 0) (print "-> " document)))
		(sort (. docs_map :find :classes) (# (cmp (first %0) (first %1)))))
	;and the tree of them all
	(defq document "docs/reference/class_hierarchy.md" stream (file-stream document +file_open_write))
	(write-line stream (cat "# The Lisp classes" +LF))
	(write-line stream (cat "Every class of the Lisp side of the system, each to the right of the" +LF
		"class it comes of. Each has a page of its own under `classes/`, with" +LF
		"its methods. Made from the source by `make docs`." +LF))
	(write-line stream (cat "```image" +LF "docs/reference/class_hierarchy.cwb" +LF "```" +LF))
	(diagram-save (dia-hierarchy lisp_nodes "And the classes that come of no other, and have none come of them")
		"docs/reference/class_hierarchy.cwb")
	(if (> *build_verb* 0) (print "-> " document))

	;the diagrams that are thought out and not worked out, each a script
	;in docs/diagrams/ that makes a scene and saves it beside itself by
	;its own name, (diagram name doc). Made again each time, as the rest
	(defun diagram (name doc)
		(diagram-save doc (cat "docs/diagrams/" name ".cwb")))
	(each (# (catch (repl (file-stream %0) %0) (progn (print "Diagram " %0 ": " _) :t)))
		(sort (files-all "docs/diagrams" '(".lisp") 0)))

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
	(if (> *build_verb* 0) (print "-> " document))

	;create functions docs
	(defq document "docs/reference/functions.md"
		stream (file-stream document +file_open_write))
	(write-line stream (cat "# Functions" +LF))
	(each (lambda ((name info))
			(when (nempty? info)
				(write-line stream (cat "### " name +LF))
				(information stream info)))
		(sort (. docs_map :find :functions) (# (cmp (first %0) (first %1)))))
	(if (> *build_verb* 0) (print "-> " document))

	;create macros docs
	(defq document "docs/reference/macros.md"
		stream (file-stream document +file_open_write))
	(write-line stream (cat "# Macros" +LF))
	(each (lambda ((name info))
			(when (nempty? info)
				(write-line stream (cat "### " name +LF))
				(information stream info)))
		(sort (. docs_map :find :macros) (# (cmp (first %0) (first %1)))))
	(if (> *build_verb* 0) (print "-> " document))

	;create commands docs
	(defq document "docs/reference/commands.md"
		stream (file-stream document +file_open_write))
	(each (lambda ((job result))
			(write-line stream (cat "## " (slice job 0 -4)))
			(write-line stream "```code")
			(write-blk stream result)
			(write-line stream "```"))
		(sort (pipe-farm (map (# (cat %0 " -h")) (files-all "cmd" '(".lisp") 4 -6)))
			(# (cmp (first %0) (first %1)))))
	(if (> *build_verb* 0) (print "-> " document))

	;scan for VP classes info
	(defq *abi* (abi) *cpu* (cpu) *imports* (all-vp-files) classes (list) class_files (Fmap 31)
		functions (list) docs (list) state :nil ffi_list (. docs_map :find :ffis))
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
							(merge *imports* (list (path-to-absolute name file))))
						(def-class
							(. class_files :insert (sym name) file)
							(push classes (list (sym name) (third line_split))))
						(dec-method
							;its name, the function that is it, and what kind it is,
							;:static, :override, :final or :virtual, a virtual one if not said
							(push (last classes) (list (sym name) (sym (third line_split))
								(if (and (> (length line_split) 3)
										(find (elem-get line_split 3) '(":static" ":override" ":final" ":virtual")))
									(sym (elem-get line_split 3)) :virtual))))
						(def-method
							(setq state :info)
							(push docs (list))
							(push functions (f-path (sym name) (sym (third line_split)))))
						((gen-create gen-type)
							;generated functions, documented at the (gen-xxx :class) call
							(when (starts-with ":" name)
								(setq state :info)
								(push docs (list))
								(push functions (f-path (sym name) (cond
									((eql type 'gen-type) :type)
									((> (length line_split) 2) (sym (cat ":create_" (third line_split))))
									(:t :create))))))
						((def-func defun)
							(setq state :info)
							(push docs (list))
							(defq func_name (if (eql name "path-to-absolute")
									(sym (path-to-absolute (third line_split) file))
									(sym name)))
							(push functions func_name))
						((call jump)
							(and (eql (third line_split) ":repl_error")
								(setq line (chop line))
								(push ffi_list (list (last functions) (list line)))))))
				:nil)
			(file-stream file))) *imports*)))

	;create VP classes docs, each with its diagram: what it comes of, its
	;fields, its methods by kind, and what comes of it
	(sort classes (# (cmp (first %0) (first %1))))
	(defq vp_supers (Fmap 31) vp_nodes (list))
	(each (lambda ((cls super &rest mthds))
		(push vp_nodes (list (str cls) (if (eql ":nil" super) :nil super)))
		(unless (eql ":nil" super) (. vp_supers :insert (str cls) super)))
		classes)
	(defq document "docs/reference/vp_hierarchy.md" stream (file-stream document +file_open_write))
	(write-line stream (cat "# The VP classes" +LF))
	(write-line stream (cat "Every class of the VP side of the system, each to the right of the" +LF
		"class it comes of. Each has a page of its own under `vp_classes/`, with" +LF
		"its fields and its methods. Made from the source by `make docs`." +LF))
	(write-line stream (cat "```image" +LF "docs/reference/vp_hierarchy.cwb" +LF "```" +LF))
	(diagram-save (dia-hierarchy vp_nodes "And the classes that are only functions, they come of no other and have none come of them")
		"docs/reference/vp_hierarchy.cwb")
	(if (> *build_verb* 0) (print "-> " document))
	(each (lambda ((cls super &rest mthds))
		(defq stream (file-stream (cat "docs/reference/vp_classes/" (rest cls) ".md") +file_open_write))
		(write-line stream (cat "# " cls +LF))
		(write-line stream (cat "```image" +LF "docs/reference/vp_classes/" (rest cls) ".cwb" +LF "```" +LF))
		(defq file (. class_files :find cls) named (lambda (kinds)
				(sort (map (# (str (first %0))) (filter (# (and (find (third %0) kinds)
					(not (starts-with ":lisp_" (first %0))) (not (eql (first %0) :vtable)))) mthds)) (const cmp))))
		(diagram-save (dia-class (str cls) (ifn file "")
				(chain-of (str cls) vp_supers)
				(sort (map (const first) (filter (# (eql (second %0) (str cls))) vp_nodes)) (const cmp))
				(filter (# (nempty? (second %0))) (list
					(list "fields" (if file (vp-fields file (rest cls)) (list)))
					(list "virtual methods" (named '(:virtual :final)))
					(list "overrides" (named '(:override)))
					(list "static methods" (named '(:static)))
					;by the name Lisp calls each by, where the function says it
					(list "Lisp bindings" (sort (map (lambda ((mthd function &ignore))
							(defq i (some (# (if (eql function (first %0)) (!))) ffi_list)
								info (if i (first (second (elem-get ffi_list i)))))
							(if (and info (starts-with "(" info))
								(first (split (rest info) (const (char-class " )"))))
								(rest (str mthd))))
						(filter (# (starts-with ":lisp_" (first %0))) mthds)) (const cmp))))))
			(cat "docs/reference/vp_classes/" (rest cls) ".cwb"))
		(unless (eql ":nil" super)
			(write-line stream (cat "## " super +LF)))
		(sort mthds (# (cmp (first %0) (first %1))))
		(defq lisp_mthds (filter (# (starts-with ":lisp_" (first %0))) mthds)
			mthds (filter (# (not (starts-with ":lisp_" (first %0)))) mthds))
		(when (nempty? lisp_mthds)
			(write-line stream (cat "## Lisp Bindings" +LF))
			(each (lambda ((mthd function &ignore))
				(when (and (defq i (some (# (if (eql function (first %0)) (!))) ffi_list))
						(defq info (first (second (elem-get ffi_list i)))))
					(write-line stream (cat "### " info +LF)))) lisp_mthds))
		(when (nempty? mthds)
			(write-line stream (cat "## VP methods" +LF))
			(each (lambda ((mthd function &ignore))
				(write-line stream (cat "### " mthd " -> " function +LF))
				(when (and (defq i (find function functions))
						(/= 0 (length (defq info (elem-get docs i)))))
					(write-line stream "```code")
					(each (# (write-line stream %0)) info)
					(write-line stream (const (str "```" +LF))))) mthds))
		(if (> *build_verb* 0) (print (cat "-> docs/reference/vp_classes/" (rest cls) ".md")))) classes)
	(print "Done"))

(defun make-ai ()
	(defq folders (Lmap) cmds (list))
	(each (lambda (file)
			(defq folder "host")
			(if (defq i (find "/" file)) (setq folder (slice file 0 i)))
			(. folders :update folder (# (if %0 (push %0 file) (list file)))))
		(filter (lambda (file) (notany (# (starts-with %0 file)) +ai_excluded_files))
			(files-all "." '("Makefile" "Makefile.mingw" ".vp" ".inc" ".lisp" ".c" ".cpp" ".h" ".sh" ".ps1" ".bat") 2)))
	(. folders :each (# (push cmds (cat "cat -f " (join %1 " ") " | save ai/" %0 ".txt"))))
	(pipe-farm cmds)
	(defq folders (Lmap) cmds (list))
	(each (lambda (file)
			(defq folder "onslaught")
			(. folders :update folder (# (if %0 (push %0 file) (list file)))))
		(filter (lambda (file) (notany (# (starts-with %0 file)) +ai_excluded_files_onslaught))
			(files-all "apps/games/onslaught" '(".vp" ".inc" ".lisp" ".cpp"))))
	(. folders :each (# (push cmds (cat "cat -f " (join %1 " ") " | save ai/" %0 ".txt"))))
	(pipe-farm cmds))

(defun make-check ()
	;the include list of every .vp file and the import paths of every source
	;file, as (includes) and (imports) would have them. What they would
	;change is printed, nothing is written, -w on either does that
	(pipe-run "files | includes")
	(pipe-run "files | imports"))

(defun make-fmt ()
	;format every source file, only those that need it are written
	(pipe-run "files . | fmt -w"))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v 0 args (options stdio usage)))
		(each (# (def (penv) (sym %0) (find %0 args)))
			'("all" "platforms" "boot" "docs" "it" "apps"
				"release" "debug" "validate" "test" "ai" "vp" "fmt"))
		(defq mode (or (if validate 2) (if debug 1) (if release 0))
			*build_verb* opt_v)
		(cond
			(fmt (make-fmt))
			(test (make-test))
			(vp (remake-all-vp 1))
			(it (remake-all-platforms mode) (make-docs) (make-check))
			(apps (make-app-platforms mode))
			((and boot all platforms) (remake-all-platforms))
			((and boot all) (remake-all))
			((and boot platforms) (remake-platforms))
			((and all platforms) (make-all-platforms))
			(all (make-all))
			(platforms (make-platforms))
			(boot (remake))
			(docs (make-docs))
			(ai (make-ai))
			((make)))))
