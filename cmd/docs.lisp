(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

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

;extract parent class name
(defun parent? (line_split)
	(some! (# (if (bfind %0 +char_class_upper) (sym %0)))
		(list line_split) :nil 2))

;do the work on a file
(defun work (file)
	(defq state :nil info :nil methods :nil)
	(lines! (lambda (line)
		(when (find state '(:function :macro :class :method :ffi))
			(if (starts-with ";" (defq line_trim (trim line +char_class_space)))
				(push info (trim-start line_trim " ;"))
				(setq state :nil)))
		(when (eql state :keys)
			(if (nempty? (split line +char_class_space))
				(push info (trim line (const (char-class (cat ")" +char_class_space)))))
				(setq state :nil)))
		(when (eql state :nil)
			(defq line_split (split line (const (char-class " ()'\t\r\q")))
				type (sym (first line_split)) name (sym (second line_split)))
			(when (= 0 (rbskipn (const (char-class "'`,~")) name -1))
				(case type
					((*key_map* *key_map_shift* *key_map_control*)
						(push key_list (list (sym file) type (setq state :keys info (list)))))
					(ffi
						(push ffi_list (list name (setq state :ffi info (list)))))
					(defclass
						(push class_list (list name (parent? line_split)
							(setq methods (list)) (setq state :class info (list)))))
					((defmethod deffimethod defabstractmethod)
						(if methods (push methods (list name (setq state :method info (list))))))
					(defgetmethod
						(push methods (list (sym (cat ":get_" name)) (setq state :method info (list)))))
					(defsetmethod
						(push methods (list (sym (cat ":set_" name)) (setq state :method info (list)))))
					(defun
						(push function_list (list name (setq state :function info (list)))))
					(defmacro
						(push macro_list (list name (setq state :macro info (list)))))))))
		(file-stream file)))

;merge child work
(defun merge-work ((job result))
	(defq work_map (tree-load (string-stream result)))
	(setq function_list (cat function_list (. work_map :find :functions))
		macro_list (cat macro_list (. work_map :find :macros))
		class_list (cat class_list (. work_map :find :classes))
		ffi_list (cat ffi_list (. work_map :find :ffis))
		key_list (cat key_list (. work_map :find :keys))))

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
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
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
