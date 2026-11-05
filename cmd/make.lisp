(import "lib/asm/asm.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/task/pipe.inc")
(import "lib/files/info.inc")
(import "lib/text/tree.inc")

(defq usage `(
(("-h" "--help")
"Usage: make [options] [all] [boot] [platforms] [doc] [it] [apps]
    [vp] [trace] [validate] [test] [fmt]

    options:
        -h --help: this help info.
        -v --verbosity num: how much info, default 0.

    all:        include all .vp files.
    boot:       create a boot image.
    platforms:  for all platforms not just the host.
    docs:       scan source files and create documentation.
    vp:         only the VP64.
    trace:      only the VP of each file, the apps' too, no
                code of any platform: what trace reads.
    it:         all of the above, and then a check of the include
                and import lists, what includes and imports would
                change is printed.
    apps:       only the apps !
    validate:   validate mode, more checks as it runs.
    test:       test make timings.
    fmt:        format all the source files, with the fmt command.

    validate goes with any build, and all that is built is
    built so, without it all is built in the normal mode.")
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

(defq +field_types ''("byte" "ubyte" "short" "ushort" "int" "uint" "long" "ulong" "ptr" "pptr"
	"pbyte" "pubyte" "pshort" "pushort" "pint" "puint" "plong" "pulong" "netid" "nodeid" "fixed" "real"))

(defun struct-of (file name)
	; (struct-of file name) -> :nil | nodes
	;what is in the structure of a VP class, (structure +name ...), in the
	;struct.inc beside its class.inc, read as a tree of those heads
	(when (defq stream (file-stream (cat (path-dir file) (if (find "/" file) "/" "") "struct.inc")))
		(some (lambda (node)
				(and (defq kids (filter (# (not (find (first %0) '(:comment :blank)))) (third node)))
					(> (length kids) 1) (eql (third (second kids)) (cat "+" name))
					kids))
			(first (tree-read stream :nil '("structure" "def-struct"))))))

(defun vp-fields (file name)
	; (vp-fields file name) -> (str ...)
	;the fields of a VP class, each as its type and its name, from its
	;structure: each form in it of a type and the names of that type, and
	;a struct's name, the size after it may be worked out
	(defq out (list))
	(each (lambda (part)
			(and (defq words (tree-form? part)) (nempty? words)
				(eql (first (first words)) :symbol)
				(defq type (third (first words)))
				(cond
					((eql type "struct")
						(if (> (length words) 1) (push out (cat "struct " (third (second words))))))
					((find type +field_types)
						(each! (lambda (word) (if (eql (first word) :symbol) (push out (cat type " " (third word)))))
							(list words) 1)))))
		(ifn (struct-of file name) '()))
	out)

(defun vp-over (file name)
	; (vp-over file name) -> :nil | str
	;a VP class whose fields do not go after those of the class it comes
	;of, but begin at one of them and lie over it, :hmap is one: that
	;field, as "elems of :array". From what its structure begins at
	(and (defq kids (struct-of file name)) (> (length kids) 2)
		(eql (first (defq at (third kids))) :symbol)
		(starts-with "+" (defq from (third at)))
		(defq from (rest from) cut (find "_" from))
		(not (ends-with "_size" from))
		(cat (slice from (inc cut) -1) " of :" (slice from 0 cut))))

(defun chain-of (name supers)
	; (chain-of name supers) -> (name ...)
	;what a class comes of, from the first down to its parent, by a map
	;of each class to its parent
	(defq out (list) seen 0)
	(while (and (< (setq seen (inc seen)) 32) (defq parent (. supers :find name)))
		(push out parent)
		(setq name parent))
	(reverse out))

(defun class-props (files)
	; (class-props files) -> fmap
	;the properties of each Lisp class, by its name: those it gives itself,
	;and those it looks for. Found by reading each file as the reader does,
	;forms and not text, and going through each (defclass) for what is
	;done to this: (def this :k v ...) and (set this ...) and (lower :k)
	;give, (get :k this), (def? :k this) and (raise :k) look. A list of
	;what is left to look at, no function here calls itself
	(defq out (Fmap 31) key? (# (and (sym? %0) (starts-with ":" %0)))
		this? (lambda (form)
			;is it this, or a form with this in it, (. this :method ...) gives this back
			(defq todo (list form) found :nil)
			(while (and (not found) (defq node (pop todo)))
				(cond
					((eql node 'this) (setq found :t))
					((list?? node) (each (# (push todo %0)) node))))
			found))
	(each (lambda (file)
		(catch (progn
			(defq stream (file-stream file) next (ascii-code " "))
			(while (and stream (/= next -1))
				(bind '(form next) (read stream next))
				(when (and (list?? form) (> (length form) 2) (eql (first form) 'defclass))
					(defq gives (Fset 11) looks (Fset 11) todo (list form))
					(while (defq node (pop todo))
						(when (and (list?? node) (nempty? node))
							(defq op (first node) n (length node))
							(cond
								((and (find op '(def set)) (> n 3) (this? (second node)))
									(each! (# (and (= 0 (logand (!) 1)) (key? %0) (. gives :insert %0))) (list node) 2))
								((and (find op '(get def?)) (= n 3) (key? (second node)) (eql (third node) 'this))
									(. looks :insert (second node)))
								((eql op 'raise)
									(each! (# (if (key? %0) (. looks :insert %0))) (list node) 1))
								((eql op 'lower)
									(each! (# (cond ((key? %0) (. gives :insert %0))
										((and (list?? %0) (nempty? %0) (key? (first %0))) (. gives :insert (first %0)))))
										(list node) 1))
								((and (eql op 'defgetmethod) (> n 1) (key? (second node))) (. looks :insert (second node)))
								((and (eql op 'defsetmethod) (> n 1) (key? (second node))) (. gives :insert (second node))))
							(each (# (if (list?? %0) (push todo %0))) node)))
					(defq given (list) looked (list))
					(. gives :each (# (push given (str %0))))
					(. looks :each (# (unless (. gives :find %0) (push looked (str %0)))))
					(. out :insert (str (second form)) (list (sort given (const cmp)) (sort looked (const cmp))))))
			:t) :t))
		files)
	out)

(defun diagram-job (spec file)
	;a diagram to be made, (kind args ...), :class the args of (dia-class),
	;:tree of (dia-hierarchy), and saved to file, by (farm-diagrams)
	(push dia_jobs (cat (list (first spec) file) (rest spec)))
	(if (> *build_verb* 0) (print "-> " file)))

(defun farm-diagrams (jobs)
	;the diagrams made by a herd of children, lib/cwb/diagram_child.lisp,
	;one on each node of this machine, a few to a job. They are each a
	;layout and a save of their own, and they were most of the time of make
	;docs, one after another. What went wrong with any is printed
	(when (nempty? jobs)
		(defq units (partition jobs 4))
		(jobs-farm "lib/cwb/diagram_child.lisp"
			(map (# (cat (const (str-alloc +job_size)) (str %0))) units)
			(list (length units) 1 1) (task-timeout 60)
			(lambda (msg herd)
				(if (nempty? (defq said (slice msg +job_reply_size -1))) (print said))
				(. herd :answered msg))
			;a child that died has its job again, and not for ever
			(lambda (tries)
				(if (>= tries 3) (print "Error: no answer from the diagram makers"))))))

(defun make-docs ()
	(print "Scanning source files...")
	;the diagrams of the reference are made here too, by children that
	;each have lib/cwb/diagram.inc, (farm-diagrams), each one as it is come
	;to is put on the list
	(defq dia_jobs (list))

	;scan for Lisp functions, macros, classes, ffi and keys info
	(defq docs_map (string-stream ""))
	(defq lisp_files (sanitize (cat
			(files-all "." '("lisp.inc" "actions.inc") 2)
			(files-all "./lib" '(".inc") 2)
			;each service's calls for those that use it
			(files-all "./service" '("app.inc") 2)
			'("class/lisp/root.inc" "class/lisp/task.inc"))))
	(pipe-run (cat "docs -j 8 " (join lisp_files " "))
		(# (write-blk docs_map %0)))
	(setq docs_map (tree-load (string-stream (str docs_map))))

	;create classes docs, each with its diagram: what it comes of, its
	;methods, and what comes of it
	(defq lisp_supers (Fmap 31) lisp_nodes (list) lisp_props (class-props lisp_files))
	(each (lambda ((name pname &ignore))
		(push lisp_nodes (list name pname))
		(if pname (. lisp_supers :insert name pname)))
		(. docs_map :find :classes))
	(each (lambda ((name pname methods info))
			(defq document (cat "docs/reference/classes/" name ".md")
				stream (file-stream document +file_open_write))
			(write-line stream (cat "# " name +LF))
			(write-line stream (cat "```image" +LF "docs/reference/classes/" name ".cwb" +LF "```" +LF))
			(diagram-job (list :class name (if (nempty? info) (first info) "")
					(chain-of name lisp_supers)
					(sort (map (const first) (filter (# (eql (second %0) name)) lisp_nodes)) (const cmp))
					;what each class it comes of gives itself, from the first, then
					;what it gives itself, what it looks for that none of those
					;gave, and its methods
					(filter (# (nempty? (second %0))) (cat
						(map (lambda (parent)
								(defq given (if (defq pprops (. lisp_props :find (str parent))) (first pprops) (list)))
								(each (# (. had :insert %0)) given)
								(list (cat "properties of " parent) given))
							(progn (defq had (Fset 11)) (chain-of name lisp_supers)))
						(list
						(list "properties it gives itself" (if (defq props (. lisp_props :find (str name))) (first props) (list)))
						(list "properties it looks for" (if props (filter (# (not (. had :find %0))) (second props)) (list)))
						(list "methods" (sort (map (const first) methods) (const cmp)))))))
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
	(diagram-job (list :tree lisp_nodes "And the classes that come of no other, and have none come of them")
		"docs/reference/class_hierarchy.cwb")
	(if (> *build_verb* 0) (print "-> " document))

	;the diagrams that are thought out and not worked out, each a script
	;in docs/diagrams/ that makes a scene and saves it beside itself by
	;its own name, (diagram name doc). Made again each time, as the rest
	(each (# (push dia_jobs (list :script %0))) (sort (files-all "docs/diagrams" '(".lisp") 0)))

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
		;two of the one name, a function of an app and one of another, are in
		;the order of what they say, or they change places from build to build
		(sort (. docs_map :find :functions) (# (if (eql (first %0) (first %1))
			(cmp (str (second %0)) (str (second %1))) (cmp (first %0) (first %1))))))
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

	;the VP classes as the class database has them, lib/files/info.inc, read
	;from the source as trace reads it: each class declared, what it comes
	;of and the methods it declares, its file, and each function's comment
	;and what Lisp calls it with
	(defq class_db (files-classes-info) func_db (files-function-info class_db)
		classes (list) class_files (Fmap 31) ffi_list (. docs_map :find :ffis))
	(. class_db :each (lambda (cls entry)
		(when (defq file (. entry :find :file))
			(. class_files :insert cls file)
			(push classes (defq cls_info (list cls (str (. entry :find :parent)))))
			(. (. entry :find :methods) :each (lambda (mthd m_entry)
				(if (. m_entry :find :dec_file)
					(push cls_info (list mthd (sym (. m_entry :find :function)) (. m_entry :find :type)))))))))
	(. func_db :each (lambda (function f_entry)
		(if (defq sig (. f_entry :find :sig)) (push ffi_list (list (sym function) (list sig))))))

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
	(diagram-job (list :tree vp_nodes "And the classes that are only functions, they come of no other and have none come of them")
		"docs/reference/vp_hierarchy.cwb")
	(if (> *build_verb* 0) (print "-> " document))
	(each (lambda ((cls super &rest mthds))
		(defq stream (file-stream (cat "docs/reference/vp_classes/" (rest cls) ".md") +file_open_write))
		(write-line stream (cat "# " cls +LF))
		(write-line stream (cat "```image" +LF "docs/reference/vp_classes/" (rest cls) ".cwb" +LF "```" +LF))
		(defq file (. class_files :find cls) named (lambda (kinds)
				(sort (map (# (str (first %0))) (filter (# (and (find (third %0) kinds)
					(not (starts-with ":lisp_" (first %0))) (not (eql (first %0) :vtable)))) mthds)) (const cmp))))
		(diagram-job (list :class (str cls) (ifn file "")
				(chain-of (str cls) vp_supers)
				(sort (map (const first) (filter (# (eql (second %0) (str cls))) vp_nodes)) (const cmp))
				(filter (# (nempty? (second %0))) (cat
					;the fields of each class it comes of, from the first, in the
					;order they lie in an object, and then its own
					(map (lambda (parent)
							(defq pfile (. class_files :find (sym parent)))
							(list (cat "fields of " parent) (if pfile (vp-fields pfile (rest parent)) (list))))
						(chain-of (str cls) vp_supers))
					(list
					;its own, and if they lie over a field of what it comes of, which
					(list (if (and file (defq over (vp-over file (rest cls)))) (cat "fields, over " over) "fields")
						(if file (vp-fields file (rest cls)) (list)))
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
						(filter (# (starts-with ":lisp_" (first %0))) mthds)) (const cmp)))))))
			(cat "docs/reference/vp_classes/" (rest cls) ".cwb"))
		(unless (eql ":nil" super)
			(write-line stream (cat "## " super +LF)))
		(sort mthds (# (cmp (first %0) (first %1))))
		(defq lisp_mthds (filter (# (starts-with ":lisp_" (first %0))) mthds)
			mthds (filter (# (not (starts-with ":lisp_" (first %0)))) mthds))
		(when (nempty? lisp_mthds)
			(write-line stream (cat "## Lisp Bindings" +LF))
			(each (lambda ((mthd function &ignore))
				(and (defq i (some (# (if (eql function (first %0)) (!))) ffi_list))
						(defq info (first (second (elem-get ffi_list i))))
					(write-line stream (cat "### " info +LF)))) lisp_mthds))
		(when (nempty? mthds)
			(write-line stream (cat "## VP methods" +LF))
			(each (lambda ((mthd function &ignore))
				(write-line stream (cat "### " mthd " -> " function +LF))
				(when (and (defq f_entry (. func_db :find (str function)))
						(list? (defq info (. f_entry :find :docs))) (nempty? info))
					(write-line stream "```code")
					(each (# (write-line stream %0)) info)
					(write-line stream (const (str "```" +LF))))) mthds))
		(if (> *build_verb* 0) (print (cat "-> docs/reference/vp_classes/" (rest cls) ".md")))) classes)
	;and the diagrams, all of them at once
	(farm-diagrams dia_jobs)
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
	(pipe-run "files | includes" prin)
	(pipe-run "files | imports" prin))

(defun make-fmt ()
	;format every source file, only those that need it are written
	(pipe-run "files . | fmt -w" prin))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v 0 args (options stdio usage)))
		(each (# (def (penv) (sym %0) (find %0 args)))
			'("all" "platforms" "boot" "docs" "it" "apps"
				"validate" "test" "ai" "vp" "trace" "fmt"))
		(defq mode (if validate 2)
			*build_verb* opt_v)
		;a mode that was given is the mode of all that is built, for those
		;below that do not take one themselves
		(if mode (defq *build_mode* mode))
		(cond
			(fmt (make-fmt))
			(test (make-test))
			(vp (remake-all-vp mode))
			(trace (make-bundles mode))
			(it (remake-all-platforms mode) (make-docs) (make-check))
			(apps (make-app-platforms mode))
			((and boot all platforms) (remake-all-platforms mode))
			((and boot all) (remake-all))
			((and boot platforms) (remake-platforms mode))
			((and all platforms) (make-all-platforms mode))
			(all (make-all))
			(platforms (make-platforms mode))
			(boot (remake))
			(docs (make-docs))
			(ai (make-ai))
			((make)))))
