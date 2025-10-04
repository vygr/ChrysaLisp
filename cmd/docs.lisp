(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: docs [options] [path] ...

	options:
		-h --help: this help info.

	Scan for documentation in files, creates
	a merged tree of all the information.

	If no paths given on command line
	then will take paths from stdin.")
))

;extract parent class name
(defun parent? (line_split)
	(some! (# (if (bfind %0 +char_class_upper) %0))
		(list line_split) :nil 2))

;do the work on a file
(defun work (file)
	(defq state :nil info :nil methods :nil split_cls (char-class " ()'\t\r"))
	(lines! (lambda (line)
		(when (find state '(:function :macro :class :method))
			(if (starts-with ";" (defq line_trim (trim line +char_class_space)))
				(push info (trim-start (rest line_trim)))
				(setq state :nil)))
		(when (eql state :keys)
			(if (nempty? (split line +char_class_space))
				(push info (trim-start (trim-end line ")") +char_class_space))
				(setq state :nil)))
		(when (eql state :nil)
			(defq line_split (split line split_cls)
				type (first line_split) name (second line_split))
			(case type
				(("*key_map*" "*key_map_shift*" "*key_map_control*")
					(push key_list (list file type (setq state :keys info (list)))))
				("defclass"
					(push class_list (list name (parent? line_split)
						(setq methods (list)) (setq state :class info (list)))))
				(("defmethod" "deffimethod" "defabstractmethod")
					(if methods (push methods (list name (setq state :method info (list))))))
				("defgetmethod"
					(push methods (list (cat ":get_" name) (setq state :method info (list)))))
				("defsetmethod"
					(push methods (list (cat ":set_" name) (setq state :method info (list)))))
				("defun"
					(push function_list (list name (setq state :function info (list)))))
				("defmacro"
					(push macro_list (list name (setq state :macro info (list))))))))
		(file-stream file)))

;merge child work
(defun merge-work (result)
	(defq work_map (tree-load (string-stream result)))
	(setq function_list (cat function_list (. work_map :find :functions))
		macro_list (cat macro_list (. work_map :find :macros))
		class_list (cat class_list (. work_map :find :classes))
		key_list (cat key_list (. work_map :find :keys))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq function_list (list) macro_list (list)
			class_list (list) key_list (list))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(work (pop jobs))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (merge-work result))
				(pipe-farm (map (# (cat (first args) " " %0)) jobs))))
		;output results
		(tree-save (io-stream 'stdout) (scatter (Lmap)
			:macros macro_list :classes class_list
			:functions function_list :keys key_list))))
