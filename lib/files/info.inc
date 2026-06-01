(import "lib/files/files.inc")

;module
(env-push)

(defq +class_db_file "lib/files/class_db.tre"
	+func_db_file "lib/files/func_db.tre"
	+split_class (char-class " ()'\t\r\q{@}<>")
	+implicit_file "lib/asm/func.inc"
	+int_regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
	+float_regs ''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))

(defq +regs_index_map
	(reduce (# (def %0 %1 (!)) %0) +int_regs
	(reduce (# (def %0 %1 (!)) %0) +float_regs (env 1))))

(defun reg-idx (r) (get (sym r) +regs_index_map))

(defun format-group (prefix indices)
	(map (lambda ((s e)) (if (= s (-- e))
			(str prefix s)
			(str prefix s "-" prefix e)))
		(slices indices)))

(defun format-regs (regs)
	(defq r_indices (list) f_indices (list) s_indices (list) expanded_regs (list))
	;expand any ranges like :r0-:r14 or :f0-:f15
	(each (lambda (r)
		(defq parts (split r "-"))
		(if (and (= (length parts) 2)
				(starts-with ":" (first parts))
				(starts-with ":" (second parts)))
			(progn
				(defq r_start (first parts) r_end (second parts)
					prefix (slice r_start 0 2)
					start_val (str-to-num (slice r_start 2 -1))
					end_val (str-to-num (slice r_end 2 -1)))
				(each (lambda (val)
					(push expanded_regs (sym (str prefix val))))
					(range start_val (inc end_val))))
			(push expanded_regs r)))
		regs)
	(setq regs expanded_regs)
	(each (lambda (r)
		(if (starts-with ":r" r)
			(if (and (defq idx (reg-idx r)) (= idx 15))
				(push s_indices ":rsp")
				(if idx (push r_indices idx)))
			(if (defq idx (reg-idx r)) (push f_indices idx))))
		regs)
	(defq formatted_parts (cat
		(format-group ":r" (sort r_indices))
		(format-group ":f" (sort f_indices))
		s_indices))
	(if (empty? formatted_parts) "none" (join formatted_parts ", ")))

(defun db-f-path (class_db class method)
	(if (defq class_entry (. class_db :find class))
		(if (defq method_entry (.-> class_entry (:find :methods) (:find method)))
			(. method_entry :find :function))))

(defun database-stale? (cache_file source_files)
	(defq cache_age (age cache_file))
	(if (= cache_age 0)
		:t
		(some (# (and (> (age %0) cache_age) (not (starts-with "apps/" %0)))) source_files)))

(defun first-pass (files class_db func_db)
	(defq active_class :nil)
	(files-scan files (lambda (tokens file line idx)
		(when (nempty? tokens)
			(defq op (first tokens))
			(cond
				((eql op "def-class")
					(defq class_name (sym (second tokens))
						parent_name (sym (third tokens))
						class_entry (. class_db :find class_name))
					(unless class_entry
						(setq class_entry (scatter (Emap)
							:parent parent_name
							:methods (Emap)))
						(. class_db :insert class_name class_entry))
					(setq active_class class_name))
				((eql op "dec-method")
					(when (and active_class (>= (length tokens) 3))
						(defq method_name (sym (second tokens))
							  func_name (third tokens)
							  mtype (if (and (> (length tokens) 3) (starts-with ":" (elem-get tokens 3)))
										(sym (elem-get tokens 3))
										:static)
							  methods_map (.-> class_db (:find active_class) (:find :methods))
							  ; Use standard global substr helper to find the end boundary of the function name
							  f_matches (substr line func_name)
							  sub_line (if (nempty? f_matches) (slice line (second (first (first f_matches))) -1) "")
							  matches_list (matches sub_line "\\(([^)]*)\\)")
							  inputs "none" outputs "none")
						(cond
							((= (length matches_list) 2)
								(setq inputs (slice sub_line (first (second (first matches_list))) (second (second (first matches_list))))
									  outputs (slice sub_line (first (second (second matches_list))) (second (second (second matches_list))))))
							((= (length matches_list) 1)
								(if (find ":nil" sub_line)
									(setq outputs (slice sub_line (first (second (first matches_list))) (second (second (first matches_list)))))
									(setq inputs (slice sub_line (first (second (first matches_list))) (second (second (first matches_list))))))))
						(when (nql inputs "none")
							(setq inputs (format-regs (map (const sym) (split inputs)))))
						(when (nql outputs "none")
							(setq outputs (format-regs (map (const sym) (split outputs)))))
						(. methods_map :insert method_name (scatter (Emap)
							:type mtype
							:function func_name
							:inputs inputs
							:outputs outputs
							:trashes "none"))
						; Populate func_db implements
						(defq f_entry (. func_db :find func_name))
						(unless f_entry
							(setq f_entry (scatter (Emap)
								:implements (list)
								:inputs "none"
								:outputs "none"
								:trashes "none"))
							(. func_db :insert func_name f_entry))
						(push (. f_entry :find :implements) (list active_class method_name))))))
		:nil)
		+split_class))

(defun second-pass (files class_db func_db)
	(defq state :none func_name :nil inputs (list) outputs (list) trashes "none" comment_state :none
		files (filter (lambda (f) (some (# (ends-with %0 f)) '(".vp" ".inc"))) files))
	(files-scan files (lambda (tokens file raw_line idx)
		(cond
			((and (nempty? tokens) (starts-with ";" (first tokens)))
				(when (nql state :none)
					(defq content (trim-start (slice (trim raw_line +char_class_space) 1 -1) " ;"))
					(cond
						((starts-with "inputs" (to-lower content))
							(setq comment_state :inputs))
						((starts-with "outputs" (to-lower content))
							(setq comment_state :outputs))
						((starts-with "trashes" (to-lower content))
							(setq comment_state :trashes))
						((eql comment_state :inputs)
							(when (starts-with ":" content)
								(push inputs (sym (first (split content (const (char-class " =("))))))))
						((eql comment_state :outputs)
							(when (starts-with ":" content)
								(push outputs (sym (first (split content (const (char-class " =("))))))))
						((eql comment_state :trashes)
							(when (or (starts-with ":" content) (eql (to-lower content) "none"))
								(setq trashes content comment_state :none))))))
			(:t	(when (nql state :none)
					(when func_name
						(defq f_entry (. func_db :find func_name))
						(unless f_entry
							(setq f_entry (scatter (Emap)
								:implements (list)
								:inputs "none"
								:outputs "none"
								:trashes "none"))
							(. func_db :insert func_name f_entry))
						(. f_entry :insert :inputs (format-regs inputs))
						(. f_entry :insert :outputs (format-regs outputs))
						(. f_entry :insert :trashes trashes))
					(setq state :none comment_state :none
						inputs (list) outputs (list) trashes "none"))
				(when (nempty? tokens)
					(defq op (first tokens))
					(cond
						((eql op "def-method")
							(defq class_name (sym (second tokens))
								method_name (sym (third tokens))
								f_path_val (db-f-path class_db class_name method_name))
							(when f_path_val
								(setq func_name f_path_val state :method)))
						((or (eql op "def-func") (eql op "defun"))
							(defq name (if (eql (second tokens) "path-to-absolute")
									(path-to-absolute (third tokens) file)
									(second tokens)))
							(when (or (eql op "def-func") (. func_db :find name))
								(setq func_name name state :func)))))))
		:nil) +split_class " "))

(defun link-databases (class_db func_db)
	(. class_db :each (lambda (class_name class_entry)
		(. (. class_entry :find :methods) :each (lambda (method_name method_entry)
			(defq func_name (. method_entry :find :function))
			(when (defq f_entry (. func_db :find func_name))
				(. method_entry :insert :inputs (. f_entry :find :inputs))
				(. method_entry :insert :outputs (. f_entry :find :outputs))
				(. method_entry :insert :trashes (. f_entry :find :trashes))))))))

(defun inherit-signatures (class_db)
	(. class_db :each (lambda (class_name class_entry)
		(defq parent_name (. class_entry :find :parent))
		(unless (eql parent_name :nil)
			(. (. class_entry :find :methods) :each (lambda (method_name method_entry)
				(when (and (eql (. method_entry :find :inputs) "none")
						   (eql (. method_entry :find :outputs) "none"))
					; Walk up the parent chain to find an ancestor with specified registers
					(defq p_name parent_name p_entry :nil)
					(while (and (nql p_name :nil) (not p_entry))
						(when (defq p_class (. class_db :find p_name))
							(when (defq p_method (.-> p_class (:find :methods) (:find method_name)))
								(unless (and (eql (. p_method :find :inputs) "none")
											 (eql (. p_method :find :outputs) "none"))
									(setq p_entry p_method)))
							(setq p_name (. p_class :find :parent))))
					(when p_entry
						(. method_entry :insert :inputs (. p_entry :find :inputs))
						(. method_entry :insert :outputs (. p_entry :find :outputs))
						(. method_entry :insert :trashes (. p_entry :find :trashes))))))))))

(defun build-and-save-databases (source_files)
	(defq class_db (Emap) func_db (Fmap 101))
	(first-pass source_files class_db func_db)
	(second-pass source_files class_db func_db)
	(link-databases class_db func_db)
	(inherit-signatures class_db)
	(when (defq stream (file-stream +class_db_file +file_open_write))
		(tree-save stream class_db))
	(when (defq stream (file-stream +func_db_file +file_open_write))
		(tree-save stream func_db))
	class_db)

(defun files-classes-info ()
	(defq source_files (files-all-depends
		(filter (# (not (starts-with "apps/" %0)))
			(files-all "." '(".vp") 2))))
	(if (database-stale? +class_db_file source_files)
		(build-and-save-databases source_files)
		(tree-load (file-stream +class_db_file))))

(defun files-function-info (&optional class_db)
	(defq source_files (files-all-depends
		(filter (# (not (starts-with "apps/" %0)))
			(files-all "." '(".vp") 2))))
	(if (database-stale? +func_db_file source_files)
		(progn
			(unless class_db (setq class_db (files-classes-info)))
			(tree-load (file-stream +func_db_file)))
		(tree-load (file-stream +func_db_file))))

;module
(export-symbols '(files-classes-info files-function-info))
(env-pop)