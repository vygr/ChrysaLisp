;;;;;;;;;;;;;;;;;
; vp source utils
;;;;;;;;;;;;;;;;;

(import "./files.inc")
(import "././asm/regs.inc")
(import "././text/tree.inc")
(import "service/lock/app.inc")

;module
(env-push)

(defq *lib_root* (path-to-file)
	+lib_file (first (repl-info))
	+class_db_file (cat *lib_root* "class_db.tre")
	+func_db_file (cat *lib_root* "func_db.tre"))

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
			(if (and (defq idx (vp-reg? r)) (= idx 15))
				(push s_indices ":rsp")
				(if idx (push r_indices idx)))
			(if (defq idx (vp-reg? r)) (push f_indices idx))))
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

(defun node-name (node file)
	; (node-name node file) -> :nil | str
	;the name a node gives: a symbol, with no ' before it, a string, with
	;no quotes, or a (path-to-absolute "./name"), from where the file is
	(cond
		((find (first node) '(:symbol :keysym)) (trim-start (third node) "'"))
		((eql (first node) :string1) (trim (third node) "\q"))
		((and (defq path (tree-form? node "path-to-absolute")) (> (length path) 1)
				(eql (first (second path)) :string1))
			(path-to-absolute (trim (third (second path)) "\q") file))))

(defun node-regs (node)
	; (node-regs node) -> "none" | str
	;the registers a (dec-method) says, a list of them, or :nil for none
	(if (defq regs (tree-form? node))
		(format-regs (map (# (sym (third %0))) (filter (# (find (first %0) '(:symbol :keysym))) regs)))
		"none"))

(defun header-pass (class_db func_db func_name file said)
	;what a function says of itself in the comments after its first line:
	;inputs, outputs, each a line for a register, and trashes
	(defq state :header inputs (list) outputs (list) trashes "none" trashes_line 0
		seen_inputs :nil seen_outputs :nil)
	(each (lambda ((& line text))
		(when (nempty? (defq content (trim-start text (const (char-class (cat +char_class_space ";"))))))
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
						(setq trashes content state :header trashes_line (dec line)))))))
		said)
	(flush-metadata class_db func_db func_name file inputs outputs trashes trashes_line seen_inputs seen_outputs))

(defun unified-pass (files class_db func_db)
	;Each file is read as a tree, lib/text/tree.inc, and every form of it
	;looked at in the order they are written, those inside others too, a
	;(def-func) in an (errorcase) say. A list of where it is in each list
	;of nodes it is part way through, each (nodes at)
	(defq active_class :nil)
	(each (lambda (file)
		(with-read-lock file
			(defq todo (list (list (first (tree-read (file-stream file))) -1)))
			(while (defq frame (pop todo))
				(task-slice)
				(bind '(nodes at) frame)
				(when (< (++ at) (length nodes))
					(push todo (list nodes at))
					(defq node (elem-get nodes at) kids (tree-form? node))
					(when (and kids (eql (first (first kids)) :symbol))
						(defq op (third (first kids)) n (length kids)
							names (map (# (node-name %0 file)) (slice kids 1 (min n 3))))
						(cond
							((and (eql op "def-class") (>= n 3) (first names))
								(defq class_name (sym (first names))
									parent_name (sym (ifn (second names) ":nil"))
									class_entry (. class_db :find class_name))
								(if class_entry
									(. class_entry :insert :parent parent_name)
									(setq class_entry (scatter (Emap)
										:parent parent_name
										:subclasses (list)
										:methods (Emap)))
									(. class_db :insert class_name class_entry))
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
							((and (eql op "dec-method") active_class (>= n 3) (first names) (second names))
								;its name, the function that is it, what kind it is, :static
								;if not said, and what it takes and gives, either a list
								;of registers or :nil
								(defq method_name (sym (first names))
									dec_func_name (second names)
									more (slice kids 3 -1)
									kinded (and (nempty? more) (eql (first (first more)) :keysym)
										(nql (third (first more)) ":nil"))
									mtype (if kinded (sym (third (first more))) :static)
									more (if kinded (rest more) more)
									methods_map (.-> class_db (:find active_class) (:find :methods))
									dec_inputs (if (> (length more) 0) (node-regs (first more)) "none")
									dec_outputs (if (> (length more) 1) (node-regs (second more)) "none"))
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
								(push (. f_entry :find :implements) (list active_class method_name)))
							((and (find op '("def-method" "gen-create" "gen-type")) (first names))
								;generated functions are documented just as methods are,
								;by the header comment after the (gen-create) (gen-type) call
								(defq class_name (sym (first names))
									method_name (cond
										((eql op "def-method") (sym (second names)))
										((eql op "gen-type") :type)
										((second names) (sym (cat ":create_" (second names))))
										(:t :create)))
								(header-pass class_db func_db
									(or (db-f-path class_db class_name method_name)
										(sym (cat class_name ":" method_name)))
									file (tree-said node nodes at)))
							((and (find op '("def-func" "defun")) (first names)
									(or (eql op "def-func") (. func_db :find (first names))))
								(header-pass class_db func_db (first names) file (tree-said node nodes at)))))
					(if kids (push todo (list kids -1))))))) files))

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
	;initialize :overrides as an empty list for ALL existing method entries
	(. class_db :each (lambda (class_name class_entry)
		(.-> class_entry (:find :methods) (:each (lambda (method_name method_entry)
			(. method_entry :insert :overrides (list)))))))
	;populate the overrides lists by walking up the ancestor chain
	(. class_db :each (lambda (class_name class_entry)
		(.-> class_entry (:find :methods) (:each (lambda (method_name method_entry)
			(defq mtype (. method_entry :find :type))
			(when (or (eql mtype :override) (eql mtype :final))
				(defq p_name (. class_entry :find :parent))
				(while (nql p_name :nil)
					(when (defq p_class (. class_db :find p_name))
						(when (defq p_method (.-> p_class (:find :methods) (:find method_name)))
							(defq overrides (. p_method :find :overrides))
							(unless (find class_name overrides)
								(push overrides class_name))))
					(setq p_name (. p_class :find :parent)))))))))
	;propagation of virtual methods to subclasses that do not override them
	(defq propagate (lambda (class_name)
		(when (defq class_entry (. class_db :find class_name))
			(defq parent_name (. class_entry :find :parent))
			(when (nql parent_name :nil)
				(when (defq parent_entry (. class_db :find parent_name))
					(.-> parent_entry (:find :methods) (:each (lambda (method_name parent_method_entry)
						(defq mtype (. parent_method_entry :find :type))
						(when (find mtype '(:virtual :override :final))
							(ifn (defq methods_map (. class_entry :find :methods)
									method_entry (. methods_map :find method_name))
								(. methods_map :insert method_name
									(transfer parent_method_entry (scatter (Emap) :type :virtual)
										'(:function :inputs :outputs :trashes
										:file :trashes_line :overrides)))
								(unless (. method_entry :find :overrides)
									(. method_entry :insert :overrides (list)))))))))))
			(each propagate (. class_entry :find :subclasses))))
	(. class_db :each (lambda (class_name class_entry)
		(when (eql (. class_entry :find :parent) :nil)
			(propagate class_name)))))

(defun build-and-save-databases (vp_files)
	(defq class_db (Emap) func_db (Fmap 101))
	(unified-pass vp_files class_db func_db)
	(link-databases class_db func_db)
	(inherit-signatures class_db)
	(build-method-overrides class_db)
	(tree-save (file-stream +class_db_file +file_open_write) class_db)
	(with-write-lock +func_db_file
		(tree-save (file-stream +func_db_file +file_open_write) func_db))
	class_db)

(defun files-all-vp-source ()
	; (files-all-vp-source) -> ordered_files
	(tsort (files-all "." '(".vp") 2) (const files-depends)))

(defun database-stale? (cache_file lib_file vp_files)
	(defq cache_age (age cache_file))
	(some (# (> (age %0) cache_age)) (cat vp_files (list lib_file))))

(defun files-classes-info (&optional forced)
	; (files-classes-info [forced]) -> :nil | class_db
	(with-write-lock +class_db_file
		(catch
			(progn
				(defq vp_files (files-all-vp-source))
				(if (or forced (database-stale? +class_db_file +lib_file vp_files))
					(build-and-save-databases vp_files))
				(tree-load (file-stream +class_db_file)))
			:nil)))

(defun files-function-info (&optional class_db)
	; (files-function-info [forced]) -> :nil | func_db
	(if (setd class_db (files-classes-info))
		(with-read-lock +func_db_file
			(tree-load (file-stream +func_db_file)))))

;module
(export-symbols '(files-classes-info files-function-info files-all-vp-source))
(env-pop)