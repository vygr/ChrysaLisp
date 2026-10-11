(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/tree.inc")

(defq usage `(
(("-h" "--help")
"Usage: docs [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.

    Scan for documentation in files, creates
    a merged tree of all the information.

    If no paths given on command line
    then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
))

;the first of the words of a class's first line, after its name, that has
;a capital in it: the class it comes of
(defun parent? (nodes line)
	(some! (# (if (bfind %0 +char_class_upper) (sym %0)))
		(list (split (join (map (const tree-text) (filter (# (= (second %0) line)) nodes)) " ")
			(const (char-class " ()'\t\r\q")))) :nil 2))

;what is said of a definition, (tree-said), each line of it without its ;
;and the space after
(defun said (node nodes at)
	(map (# (trim-start (third %0) " ;"))
		(tree-said node nodes at (if (tree-form? node "defclass") 4 3))))

;the keys of a key map, a line of the file each: a key and what it does
(defun map-keys (node)
	(defq out (list) line -1)
	(each (lambda (kid)
		(unless (find (first kid) '(:comment :blank))
			(cond
				;what is on the line the map begins on is not a key
				((= (second kid) (second node)))
				((= (second kid) line) (push out (cat (pop out) " " (tree-text kid))))
				((push out (tree-text kid))))
			(setq line (second kid))))
		(third node))
	out)

;do the work on a file
(defun work (file)
	;The file is read as a tree, lib/text/tree.inc, and every form of it
	;looked at, in the order they are written, those inside others too.
	;A list of where it is in each list of nodes it is part way through,
	;each (nodes at methods), methods those of the class it is in
	(defq todo (list (list (first (tree-read (file-stream file))) -1 :nil)))
	(while (defq frame (pop todo))
		(bind '(nodes at methods) frame)
		(when (< (++ at) (length nodes))
			(push todo (list nodes at methods))
			(defq node (elem-get nodes at) kids (tree-form? node))
			(cond
				;a key map is a name in a (defq) and the form after it
				((and (eql (first node) :symbol)
						(find (third node) '("*key_map*" "*key_map_shift*" "*key_map_control*"))
						(< (inc at) (length nodes)) (tree-form? (defq next (elem-get nodes (inc at))))
						(= (second next) (second node)))
					(push key_list (list (sym file) (sym (third node)) (map-keys next))))
				;a form with a ' before it is a list, not a definition
				((and kids (> at 0) (eql (first (defq before (elem-get nodes (dec at)))) :symbol)
						(= (bskip "'" (third before) 0) (length (third before)))))
				((and kids (> (length kids) 1) (eql (first (first kids)) :symbol)
						(find (first (defq named (second kids))) '(:symbol :keysym :string1 :form)))
					(defq type (sym (third (first kids)))
						name (if (eql (first named) :form) :nil (sym (trim (third named) "\q"))))
					(cond
						;a name with a , in it is of a macro's making
						((and name (/= 0 (rbskipn (const (char-class "'`,~")) name -1))))
						((eql type 'ffi)
							;by the VP function it is, as the VP docs look for it,
							;a path, or one from where this file is. One with
							;nothing said of it has nothing to give
							(defq path (cond
									(name)
									((and (defq path (tree-form? named "path-to-absolute")) (> (length path) 1)
											(eql (first (second path)) :string1))
										(sym (path-to-absolute (trim (third (second path)) "\q") file))))
								info (said node nodes at))
							(if (and path (nempty? info)) (push ffi_list (list path info))))
						((not name))
						((eql type 'defclass)
							(push class_list (list name (parent? kids (second node))
								(setq methods (list)) (said node nodes at))))
						((find type '(defmethod deffimethod defabstractmethod defproxymethod))
							(if methods (push methods (list name (said node nodes at)))))
						((eql type 'defgetmethod)
							(if methods (push methods (list (sym (cat ":get_" (rest name))) (said node nodes at)))))
						((eql type 'defsetmethod)
							(if methods (push methods (list (sym (cat ":set_" (rest name))) (said node nodes at)))))
						((eql type 'defun)
							(push function_list (list name (said node nodes at))))
						((eql type 'defmacro)
							(push macro_list (list name (said node nodes at)))))))
			(if kids (push todo (list kids -1 methods))))))

;merge child work
(defun merge-work ((job result))
	(each (# (set (penv) %0 (cat (eval %0) %1)))
		'(function_list macro_list class_list ffi_list key_list)
		(gather (tree-load (string-stream result)) :functions :macros :classes :ffis :keys)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 args (options stdio usage)))
		(defq function_list (list) macro_list (list)
			class_list (list) key_list (list) ffi_list (list))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (const merge-work)
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))
		;output results
		(tree-save (io-stream 'stdout) (scatter (Lmap)
			:macros macro_list :classes class_list :ffis ffi_list
			:functions function_list :keys key_list))))
