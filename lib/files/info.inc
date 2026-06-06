;;;;;;;;;;;;;;;;;
; vp source utils
;;;;;;;;;;;;;;;;;

(import "./files.inc")

;module
(env-push)

(defq *lib_root* (path-to-file)
	+lib_file (first (repl-info))
	+class_db_file (cat *lib_root* "class_db.tre")
	+func_db_file (cat *lib_root* "func_db.tre")
	+split_class (char-class " ()'\t\r\q{@}<>")
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
		(format-group ":r" r_indices)
		(format-group ":f" f_indices)
		s_indices))
	(if (empty? formatted_parts) "none" (join formatted_parts ", ")))

(defun db-f-path (class_db class method)
	(if (defq class_entry (. class_db :find class))
		(if (defq method_entry (.-> class_entry (:find :methods) (:find method)))
			(. method_entry :find :function))))

(defun find-dec-sig (class_db class_name method_name key)
	(defq current class_name sig "none")
	(while (and current (eql sig "none"))
		(when (defq c_entry (. class_db :find current))
			(when (defq m_entry (.-> c_entry (:find :methods) (:find method_name)))
				(setq sig (. m_entry :find key)))
			(setq current (. c_entry :find :parent))))
	sig)

(defun flush-metadata (class_db func_db func_name file inputs outputs trashes trashes_line seen_inputs seen_outputs)
	(unless (defq f_entry (. func_db :find func_name))
		(setq f_entry (scatter (Emap) :implements (list)))
		(. func_db :insert func_name f_entry))
	(defq formatted_inputs (format-regs inputs)
		formatted_outputs (format-regs outputs))
	(. class_db :each (lambda (class_name class_entry)
		(.-> class_entry (:find :methods) (:each (lambda (method_name method_entry)
			(when (eql (. method_entry :find :function) func_name)
				(when seen_inputs
					(unless (eql formatted_inputs (find-dec-sig class_db class_name method_name :inputs))
						(throw "Mismatched input comment !" (list class_name method_name))))
				(when seen_outputs
					(unless (eql formatted_outputs (find-dec-sig class_db class_name method_name :outputs))
						(throw "Mismatched output comment !" (list class_name method_name))))))))))
	(scatter f_entry 
		:inputs formatted_inputs
		:outputs formatted_outputs
		:trashes trashes
		:file file
		:trashes_line trashes_line))

(defun unified-pass (files class_db func_db)
	(defq active_class :nil state :nil func_name :nil
		inputs (list) outputs (list) trashes "none" trashes_line 0
		seen_inputs :nil seen_outputs :nil)
	(files-scan files (lambda (tokens file raw_line idx)
		(defq is_comment (starts-with ";" (first tokens)))
		(cond
			;discard comments immediately if we are not parsing a header block
			((and is_comment (not state)))
			;header comment parsing
			(is_comment
				(when (nempty? (defq content (trim-start raw_line (const (char-class (cat +char_class_space ";"))))))
					;parse metadata
					(cond
						((starts-with "inputs" (to-lower content))
							(setq state :inputs seen_inputs :t))
						((starts-with "outputs" (to-lower content))
							(setq state :outputs seen_outputs :t))
						((starts-with "trashes" (to-lower content))
							(setq state :trashes)))
					(case state
						(:inputs
							(when (starts-with ":" content)
								(push inputs (sym (first (split content (const (char-class " =("))))))))
						(:outputs
							(when (starts-with ":" content)
								(push outputs (sym (first (split content (const (char-class " =("))))))))
						(:trashes
							(when (or (starts-with ":" content) (eql (to-lower content) "none"))
								(setq trashes content state :header trashes_line idx))))))
			;non-commented line (code line or completely blank line)
			(:t ;a non-commented line terminates the header comment scanning state
				(when state
					(when func_name
						(flush-metadata class_db func_db func_name file inputs outputs trashes trashes_line seen_inputs seen_outputs))
					(setq state :nil func_name :nil
						inputs (list) outputs (list) trashes "none" trashes_line 0
						seen_inputs :nil seen_outputs :nil))
				;process code line under normal compilation rules
				(when (nempty? tokens)
					(when (find (defq op (first tokens)) '("errorcase" "validatecase" "noterrorcase"))
						(setq tokens (rest tokens) op (first tokens)))
					(cond
						((eql op "def-class")
							(defq class_name (sym (second tokens))
								parent_name (sym (third tokens))
								class_entry (. class_db :find class_name))
							(if class_entry
								(. class_entry :insert :parent parent_name)
								(progn
									(setq class_entry (scatter (Emap)
										:parent parent_name
										:subclasses (list)
										:methods (Emap)))
									(. class_db :insert class_name class_entry)))
							(unless (eql parent_name :nil)
								(defq parent_entry (. class_db :find parent_name))
								(unless parent_entry
									(setq parent_entry (scatter (Emap)
										:parent :nil
										:subclasses (list)
										:methods (Emap)))
									(. class_db :insert parent_name parent_entry))
								(defq subclasses (. parent_entry :find :subclasses))
								(unless (find class_name subclasses)
									(push subclasses class_name)))
							(setq active_class class_name))
						((eql op "dec-method")
							(when (and active_class (>= (length tokens) 3))
								(defq method_name (sym (second tokens))
									dec_func_name (third tokens)
									mtype (if (and (> (length tokens) 3) (starts-with ":" (elem-get tokens 3)))
											(sym (elem-get tokens 3))
											:static)
									methods_map (.-> class_db (:find active_class) (:find :methods))
									f_matches (substr raw_line dec_func_name)
									dec_inputs "none"
									dec_outputs "none")
								(when (nempty? f_matches)
									(bind '(((start end) &ignore) &ignore) f_matches)
									(defq sub_line (slice raw_line end -1)
										matches_list (matches sub_line "\\(([^)]*)\\)"))
									(when (nempty? matches_list)
										(bind '((& (in_g_s in_g_e) &ignore) &optional out_match) matches_list)
										(cond
											(out_match
												(bind '(& (out_g_s out_g_e) &ignore) out_match)
												(setq dec_inputs (slice sub_line in_g_s in_g_e)
													dec_outputs (slice sub_line out_g_s out_g_e)))
											(in_g_s
												(if (or (found? sub_line ":nil") (found? sub_line "()"))
													(setq dec_outputs (slice sub_line in_g_s in_g_e))
													(setq dec_inputs (slice sub_line in_g_s in_g_e)))))))
								(when (nql dec_inputs "none")
									(setq dec_inputs (format-regs (map (const sym) (split dec_inputs)))))
								(when (nql dec_outputs "none")
									(setq dec_outputs (format-regs (map (const sym) (split dec_outputs)))))
								(. methods_map :insert method_name (scatter (Emap)
									:type mtype
									:function dec_func_name
									:inputs dec_inputs
									:outputs dec_outputs
									:trashes "none"))
								(if (defq f_entry (. func_db :find dec_func_name))
									(progn
										(when (and (eql (. f_entry :find :inputs) "none") (nql dec_inputs "none"))
											(. f_entry :insert :inputs dec_inputs))
										(when (and (eql (. f_entry :find :outputs) "none") (nql dec_outputs "none"))
											(. f_entry :insert :outputs dec_outputs)))
									(. func_db :insert dec_func_name (setq f_entry (scatter (Emap)
										:implements (list)
										:inputs dec_inputs
										:outputs dec_outputs
										:trashes "none"
										:file "none"
										:trashes_line 0))))
								(push (. f_entry :find :implements) (list active_class method_name))))
						((eql op "def-method")
							(defq class_name (sym (second tokens))
								method_name (sym (third tokens))
								f_path_val (or (db-f-path class_db class_name method_name)
											(sym (cat class_name ":" method_name))))
							(setq func_name f_path_val state :header))
						((or (eql op "def-func") (eql op "defun"))
							(defq name (if (eql (second tokens) "path-to-absolute")
									(path-to-absolute (third tokens) file)
									(second tokens)))
							(when (or (eql op "def-func") (. func_db :find name))
								(setq func_name name state :header)))))))
		:nil) +split_class " "))

(defun link-databases (class_db func_db)
	(. class_db :each (lambda (class_name class_entry)
		(.-> class_entry (:find :methods) (:each (lambda (method_name method_entry)
			(defq func_name (. method_entry :find :function)
				temp_key (sym (cat class_name ":" method_name))
				f_entry (. func_db :find func_name)
				t_entry (. func_db :find temp_key))
			(when (or f_entry t_entry)
				(if (and f_entry t_entry)
					(progn
						(transfer t_entry f_entry '(:file :trashes_line))
						(. func_db :erase temp_key))
					(if t_entry
						(.-> func_db (:insert func_name (setq f_entry t_entry)) (:erase temp_key))))
				(transfer f_entry method_entry '(:inputs :outputs :trashes :file :trashes_line)))))))))

(defun inherit-signatures (class_db)
	(. class_db :each (lambda (class_name class_entry)
		(defq parent_name (. class_entry :find :parent))
		(unless (eql parent_name :nil)
			(.-> class_entry (:find :methods) (:each (lambda (method_name method_entry)
				(defq cur_sigs (gather method_entry :inputs :outputs :trashes))
				(when (some (# (eql %0 "none")) cur_sigs)
					(defq p_name parent_name)
					(while (and (nql p_name :nil) (some (# (eql %0 "none")) cur_sigs))
						(when (defq p_class (. class_db :find p_name))
							(when (defq p_method (.-> p_class (:find :methods) (:find method_name)))
								(scatter method_entry '(:inputs :outputs :trashes)
									(setq cur_sigs (map (lambda (c p) (if (and (eql c "none") (nql p "none")) p c))
										cur_sigs (gather p_method :inputs :outputs :trashes)))))
							(setq p_name (. p_class :find :parent))))))))))))

(defun build-method-overrides (class_db)
	(. class_db :each (lambda (class_name class_entry)
		(.-> class_entry (:find :methods) (:each (lambda (method_name method_entry)
			(defq mtype (. method_entry :find :type))
			(when (or (eql mtype :override) (eql mtype :final))
				(defq p_name (. class_entry :find :parent) found :nil)
				(while (and (nql p_name :nil) (not found))
					(when (defq p_class (. class_db :find p_name))
						(when (defq p_method (.-> p_class (:find :methods) (:find method_name)))
							(when (eql (. p_method :find :type) :virtual)
								(unless (find class_name (defq overrides (or (. p_method :find :overrides)
										(.-> p_method (:insert :overrides (list)) (:find :overrides)))))
									(push overrides class_name))
								(setq found :t)))
						(setq p_name (. p_class :find :parent)))))))))))

(defun build-and-save-databases (source_files)
	(defq class_db (Emap) func_db (Fmap 101))
	(unified-pass source_files class_db func_db)
	(link-databases class_db func_db)
	(inherit-signatures class_db)
	(build-method-overrides class_db)
	(when (defq stream (file-stream +class_db_file +file_open_write))
		(tree-save stream class_db))
	(when (defq stream (file-stream +func_db_file +file_open_write))
		(tree-save stream func_db))
	class_db)

(defun files-all-vp-source ()
	; (files-all-vp-source) -> ordered_files
	(tsort (filter (# (not (starts-with "apps/" %0))) (files-all "." '(".vp") 2))
		(const files-depends)))

(defun database-stale? (cache_file lib_file source_files)
	(defq cache_age (min (age cache_file) (age lib_file)))
	(some (# (> (age %0) cache_age)) source_files))

(defun files-classes-info (&optional forced)
	(defq source_files (files-all-vp-source))
	(if (or forced (database-stale? +class_db_file +lib_file source_files))
		(build-and-save-databases source_files)
		(tree-load (file-stream +class_db_file))))

(defun files-function-info (&optional class_db)
	(setd class_db (files-classes-info))
	(tree-load (file-stream +func_db_file)))

;module
(export-symbols '(files-classes-info files-function-info files-all-vp-source))
(env-pop)