(import "lib/options/options.inc")
(import "lib/text/document.inc")
(import "lib/files/info.inc")

(defq usage `(
	(("-h" "--help")
	"Usage: trace [options] [function_name] ...

	options:
		-h --help: this help info.
		-v --verbosity num: how much info, default 0.
		-l --lint: lint documented vs calculated trace.
		-w --write: write back calculated trashes to source
			files on mismatch.

	Calculate and trace active transitive register clobber state for
	virtual methods and static functions. Analyses compiled instructions
	directly via symbolic execution and traces live registers.")
(("-v" "--verbosity") ,(opt-num 'opt_v))
(("-l" "--lint") ,(opt-flag 'opt_l))
(("-w" "--write") ,(opt-flag 'opt_w))
))

(defq +no_regs ''() +obj_dir "obj/vp/"
	+int_regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
	+float_regs ''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))
(defq +all_regs `'(~(last +int_regs) ~(last +float_regs))
	+all_extern_trashed_regs `'(~(most (last +int_regs)) ~(last +float_regs))
	+regs_index_map
		(reduce (# (def %0 %1 (!)) %0) +int_regs
		(reduce (# (def %0 %1 (!)) %0) +float_regs (env 1))))

(defun reg? (r) (if (eql :sym (pop (type-of r))) (def? r +regs_index_map)))

(defun gather-all-abi-trashed ()
	(defq union_set (list))
	(each (lambda ((*abi* *cpu*))
			(env-push)
			;evaluate the ABI case statement directly in this temporary scope
			(import "sys/pii/abi.inc")
			;collect the registers
			(merge union_set (eval (abi-trashed)))
			(env-pop))
		'((AMD64 x86_64) (WIN64 x86_64) (ARM64 arm64) (RISCV64 riscv64) (VP64 vp64)))
	(sort union_set (# (reg? %0) (reg? %1))))

;(defq +all_abi_trashed_regs '`,(gather-all-abi-trashed))
(defq +all_abi_trashed_regs
	''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))

; (env 1) +all_regs set, with :nil for 'erased' to keep slots lined up
(defmacro eset-copy (%0) (static-qq (env-copy ,%0 1)))
(defmacro eset-finsert (%0 %1) (static-qq (def ,%0 ,%1 :t)))
(defmacro eset-ferase (%0 %1) (static-qq (def ,%0 ,%1 :nil)))
(defmacro eset-insert (%0 %1) (static-qq (progn (eset-finsert ,%0 ,%1) ,%0)))
(defmacro eset-erase (%0 %1) (static-qq (progn (eset-ferase ,%0 ,%1) ,%0)))
(defmacro eset-union (%0 %1) (static-qq (reduce (lambda (%0 (%1 %2)) (if %2 (eset-finsert %0 %1)) %0) (tolist ,%1) ,%0)))
(defmacro eset-diff (%0 %1) (static-qq (reduce (lambda (%0 (%1 %2)) (if %2 (eset-ferase %0 %1)) %0) (tolist ,%1) ,%0)))
(defmacro eset-tolist (%0) (static-qq (map (const first) (filter (const second) (tolist ,%0)))))
(defmacro eset-size (%0) (static-qq (reduce (lambda (%0 (%1 %2)) (if %2 (inc %0) %0)) (tolist ,%0) 0)))
(defmacro eset-empty? (%0) (static-qq (notany (const second) (tolist ,%0))))
(defmacro eset-nempty? (%0) (static-qq (some (const second) (tolist ,%0))))
(defmacro eset () (static-qq (eset-copy (const (reduce (lambda (%0 %1) (eset-erase %0 %1)) +all_regs (env 1))))))

(defun def-reg (%0 %1)
	; define register value and trashed state
	(if (eql %0 (def reg_map %0 %1))
		(eset-ferase trace_set %0)
		(eset-finsert trace_set %0)))

(defun format-group (prefix indices)
	(map (lambda ((s e)) (if (= s (-- e))
			(str prefix s)
			(str prefix s "-" prefix e)))
		(slices indices)))

(defun format-trashes (func_set)
	(defq r_indices (list) f_indices (list))
	(each (# (if (starts-with ":r" %0)
			(if (/= (defq %0 (reg? %0)) (const (reg? :rsp)))
				(push r_indices %0))
			(push f_indices (reg? %0))))
		(if (list? func_set) func_set (eset-tolist func_set)))
	(defq formatted_parts (cat
		(format-group ":r" r_indices)
		(format-group ":f" f_indices)))
	(if (empty? formatted_parts) "none" (join formatted_parts ", ")))

(defun format-values (reg_map)
	(defq out (list))
	(. (reduce (lambda (m (r v))
				(. m :update v (# (setd %0 (eset)) (eset-insert %0 r))) m)
			(filter (lambda ((r v)) (nql r v)) (tolist reg_map)) (Emap))
		:each (lambda (v rs) (push out (str v " -> " (format-trashes rs)))))
	(if (empty? out) "none" (join out " | ")))

(defun resolve-static-method (insts lbl)
	;find function that implements this static method
	(second (elem-get insts
		(inc (get (second (second (elem-get insts
			(inc (get lbl))))))))))

(defun resolve-virtual-method (c m)
	;find function that implements this virtual method
	(.-> (. *class_db* :find c) (:find :methods) (:find m) (:find :function)))

(defun resolve-virtual-methods (c m)
	;find function that implements this virtual method
	;and all subclass overrides !
	(map! (# (resolve-virtual-method %0 m))
		(list (progn (defq m_entry (.-> (. *class_db* :find c) (:find :methods) (:find m)))
			(ifn (. m_entry :find :overrides) '())))
		0 -1 (list (. m_entry :find :function))))

(defun get-function-insts (function)
	; (get-function-insts function) -> :nil | insts
	(defq e (penv) insts (memoize function (progn
		(defq obj_path (cat +obj_dir function))
		(if (> (age obj_path) 0)
			(first (read (file-stream obj_path))))) 101))
	;inject local labels into callers env !
	(each (# (if (find (first %0) '(emit-label emit-tlabel))
		(def e (last (second %0)) (!)))) insts)
	;inject start and end pc's into callers env !
	(bind '(& & _s _e &ignore) (elem-get insts 3))
	(def e '_s (get _s) '_e (get _e))
	insts)

(defun get-dependencies (function)
	;scan the compiled VP object
	(defq deps (list))
	(when (and (nql function :indirect) (nql function :abicall)
			(defq insts (get-function-insts function)))
		(each! (lambda (inst)
				(cond
					((find (defq c :nil m :nil op (first inst)) '(emit-call-p emit-jmp-p))
						(merge deps (list (resolve-static-method insts (second inst)))))
					((find op '(emit-call-i emit-jmp-i))
						(bind '(& & & &optional c m) inst))
					((find op '(emit-call-r emit-jmp-r))
						(bind '(& & &optional c m) inst)))
				(and c m (merge deps (resolve-virtual-methods c m))))
			;scan from entry to link table start
			(list insts) _s _e)) deps)

(defun virtual-trashes-union (c m)
	;calculate the union of all this class method trashes
	;and its subclasses overrides, based on the current state of the active db
	(defq union_set (list))
	(each (# (if (defq f_entry (. db :find %0))
			(merge union_set (eset-tolist (second f_entry)))))
		(resolve-virtual-methods c m))
	(if (nempty? union_set) union_set +all_extern_trashed_regs))

(defmacro verbose (v &rest info)
	(static-qq (when (<= ,v opt_v)
		(print ~info)
		(stream-flush (io-stream 'stdout)))))

(defun analyze-function (function db)
	(cond
		((not (defq insts (get-function-insts function)))
			(list :external))
		(;register tracing simulation ! (as near as we can anyways)
		;start main trace from pc _s
		(defq label_map (Lmap) call_list (list) func_set (eset) trace -1 next_trace 0
			reg_map (env-copy (const (reduce (lambda (%0 %1) (def %0 %1 %1) %0) +all_regs (env 1))) 1)
			trace_map (scatter (Lmap) 0 (list _s 0 (Lmap) reg_map (eset) (list))))
		(verbose 3 "\ttracing " function)
		(while (<= (++ trace) next_trace)
			(task-slice)
			(bind '(*pc* *rsp* stack_map reg_map trace_set call_stack) (. trace_map :find trace))
			(while (< *pc* _e)
				(defq inst (elem-get insts *pc*) *pc* (inc *pc*))
				(verbose 3 "\t\t" function " trace " trace " pc " *pc* "\n\t\t\t" inst)
				(case (first inst)
					((emit-label emit-tlabel)
						;update merged state at labels
						(if (defq pc (get (last (second inst))) ls (. label_map :find pc))
							(eset-union trace_set ls))
						(. label_map :insert pc (eset-copy trace_set)))
					((emit-beq-cr emit-bne-cr emit-bge-cr emit-ble-cr emit-blt-cr emit-bgt-cr
					emit-beq-rr emit-bne-rr emit-bge-rr emit-ble-rr emit-blt-rr emit-bgt-rr
					emit-beq-ff emit-bne-ff emit-blt-ff emit-bgt-ff emit-ble-ff emit-bge-ff)
						;if branch would carry new state then create new trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(when (or (not ls) (eset-nempty? (eset-diff (eset-copy trace_set) ls)))
							(. trace_map :insert (++ next_trace) (list pc *rsp*
								(. stack_map :copy) (env-copy reg_map 1) (eset-copy trace_set) (cat call_stack)))))
					((emit-cpy-rr emit-cpy-ff)
						;copy value and mark as trashed or restored
						(def-reg (last inst) (def? (second inst) reg_map)))
					(emit-swp-rr
						;swap values and mark as trashed or restored
						(each (const def-reg)
							(rest inst) (map! (# (def? %0 reg_map)) (list inst) -1 1)))
					(emit-alloc
						(setq *rsp* (-- *rsp* (second inst))))
					(emit-free
						(setq *rsp* (++ *rsp* (second inst)))
						(each (lambda ((%0 &ignore))
								(if (< %0 *rsp*) (. stack_map :erase %0)))
							(. stack_map :tolist)))
					(emit-push
						;push the values of all registers pushed
						(each! (# (. stack_map :insert (-- *rsp* +long_size)
								(def? %0 reg_map)))
							(list inst) 1))
					(emit-pop
						;pop the values of all registers popped
						;and flag if register is now restored
						(each! (# (def-reg %0 (. stack_map :find *rsp*))
								(. stack_map :erase *rsp*)
								(++ *rsp* +long_size))
							(list inst) -1 1))
					((emit-cpy-ri emit-cpy-fi)
						;stack spill 64 bit
						(when (eql (third inst) :rsp)
							(bind '(& src & offset) inst)
							(. stack_map :insert (+ *rsp* offset) (def? src reg_map))))
					((emit-cpy-ir emit-cpy-if)
						;stack load 64 bit ?
						(bind '(& src offset dst) inst)
						(def-reg dst (if (eql src :rsp) (. stack_map :find (+ *rsp* offset)))))
					((emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i)
						;quantize offset down to the nearest 8-byte
						;boundary and erase the slot
						(when (eql (third inst) :rsp)
							(. stack_map :erase (+ *rsp* (logand (neg +long_size) (last inst))))))
					(emit-ret
						;we are inside an inlined local subroutine, return to the caller
						(unless (setq *pc* (pop call_stack))
							;return from main, merge clobbers and terminate path
							(eset-union func_set trace_set)
							(setq *pc* +max_long)))
					(emit-call
						;local subroutine call, inline it using the path's call stack
						(when (defq target_pc (get (second inst)))
							(push call_stack *pc*)
							(setq *pc* target_pc)))
					(emit-jmp
						;if jump would carry new state then jump, else kill trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(if (or (not ls) (eset-nempty? (eset-diff (eset-copy trace_set) ls)))
							(setq *pc* pc)
							(setq *pc* +max_long)))
					(emit-call-p
						;use the callee trashes set
						(merge call_list (list (defq callee (resolve-static-method insts (second inst)))))
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(each (# (def-reg %0 :nil)) (eset-tolist (second callee_entry)))))
					(emit-jmp-p
						;exit function, merge and kill trace or
						;return to local caller if in subroutine
						(merge call_list (list (defq callee (resolve-static-method insts (second inst)))))
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(each (# (def-reg %0 :nil)) (eset-tolist (second callee_entry))))
						(unless (setq *pc* (pop call_stack))
							(eset-union func_set trace_set)
							(setq *pc* +max_long)))
					(emit-call-r
						(if (bind '(& & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set +all_extern_trashed_regs calls '(:indirect)))
						(merge call_list calls)
						(each (# (def-reg %0 :nil)) call_set))
					(emit-call-i
						(if (bind '(& & & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set +all_extern_trashed_regs calls '(:indirect)))
						(merge call_list calls)
						(each (# (def-reg %0 :nil)) call_set))
					(emit-jmp-r
						;exit function, merge and kill trace or
						;return to local caller if in subroutine
						(if (bind '(& & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set +all_extern_trashed_regs calls '(:indirect)))
						(merge call_list calls)
						(each (# (def-reg %0 :nil)) call_set)
						(unless (setq *pc* (pop call_stack))
							(eset-union func_set trace_set)
							(setq *pc* +max_long)))
					(emit-jmp-i
						;exit function, merge and kill trace or
						;return to local caller if in subroutine
						(if (bind '(& & & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set +all_extern_trashed_regs calls '(:indirect)))
						(merge call_list calls)
						(each (# (def-reg %0 :nil)) call_set)
						(unless (setq *pc* (pop call_stack))
							(eset-union func_set trace_set)
							(setq *pc* +max_long)))
					(emit-call-abi
						;simulate the union of all platform clobbers
						(merge call_list '(:abicall))
						(each (# (def-reg %0 :nil))
							(cat (list :r0 (second inst) (third inst)) +all_abi_trashed_regs)))
					(emit-trash
						;simulate the trashing of listed regs
						(each! (# (def-reg %0 :nil)) (list inst) 1))
					((emit-cpy-rd emit-cpy-rd-b emit-cpy-rd-s emit-cpy-rd-i emit-cpy-fd)
						;ignore as no reg writes
						:ignore)
					((emit-land-rr emit-lnot-rr emit-div-rrr emit-div-rrr-u)
						;two reg writes
						(each (# (def-reg %0 :nil)) (slice inst -3 -1)))
					((emit-min-cr emit-min-rr emit-max-cr emit-max-rr vp-abs-rr)
						;has label offset as last arg !
						(def-reg (third inst) :nil))
					(:t	;all remaining, check last for reg
						(if (reg? (last inst)) (def-reg (last inst) :nil))))
				(verbose 3 "\t\t\t" (format-trashes trace_set))
				(verbose 4 "\t\t\t" (format-values reg_map) "\n\t\t\t" (. stack_map :tolist)))
			(. trace_map :erase trace)
			(verbose 4 "\t\tmerged " (format-trashes func_set)))
		(list :function func_set call_list))))

(defun propagate-trashes (functions)
	(defq db (Fmap 101) order (tsort functions (const get-dependencies)))
	;each caller now accurately steps through callee clobber states
	(each (lambda (function)
		(unless (. db :find function)
			(verbose 1 "analyzing " function)
			(bind '(type &optional func_set call_list) (analyze-function function db))
			(cond
				((eql type :external)
					(. db :insert function (list :external (eset))))
				((eql type :function)
					(verbose 2 "\tfunction " function "\n\t\tcalls " call_list
						"\n\t\ttrashes " (format-trashes func_set))
					(. db :insert function (list :function func_set call_list))))))
		order)
	;converge remaining by re-running analysis until register clobbers stabilize
	(defq changed :t changed_set (scatter (Fset 101) order) next_changed (Fset 101))
	(while changed
		(defq changed :nil)
		(. next_changed :empty)
		(each (lambda (function)
			(when (defq entry (. db :find function))
				(when (eql (first entry) :function)
					(bind '(& func_set call_list) entry)
					(when (or (. changed_set :find function)
							(some (# (. changed_set :find %0)) call_list))
						(defq old_size (eset-size func_set))
						;bypasses symbolic tracing if the function already trashes all registers
						(when (< old_size (const (length +all_regs)))
							(bind '(type &optional new_set new_calls) (analyze-function function db))
							(when (/= old_size (eset-size new_set))
								(. db :insert function (list :function new_set new_calls))
								(. next_changed :insert function)
								(setq changed :t)))))))
			order)
		(defq t_set changed_set changed_set next_changed next_changed t_set))
	db)

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v 0 opt_l :nil opt_w :nil args (options stdio usage)))
		(defq functions (rest args))
		(if (empty? functions)
			(lines! (# (push functions %0)) (io-stream 'stdin)))
		(when (nempty? functions)
			(if opt_w (setq opt_l :t))
			(defq functions (map (# (if (starts-with +obj_dir %0)
					(slice %0 (const (length +obj_dir)) -1) %0)) functions)
				*class_db* (files-classes-info)
				*doc_db* (files-function-info *class_db*)
				db (propagate-trashes functions))
			(if opt_l
				(progn
					(defq file_edits (Lmap))
					(each (lambda (function)
						(when (defq entry (. db :find function))
							(when (nql (first entry) :external)
								(if (defq doc_set :nil file :nil line_num :nil
										doc_entry (. *doc_db* :find function))
									(bind '(doc_set file line_num)
										(gather doc_entry :trashes :file :trashes_line)))
								(cond
									((not doc_set)
										(print "WARNING: No documentation found for " function))
									(:t
										(defq calc_set (format-trashes (second entry)))
										(unless (eql doc_set calc_set)
											(print "WARNING: Mismatch in " function)
											(print "\tDocumented: " doc_set)
											(print "\tCalculated: " calc_set)
											(print "\tFile: \q" file "\q Line: " line_num)
											(when (and opt_w file (str? file) (nql file "none") (nql line_num 0))
												(. file_edits :update file
													(# (if %0 (push %0 (list line_num calc_set))
														(list (list line_num calc_set))))))))))))
						functions)
					(when (and opt_w (not (. file_edits :empty?)))
						(. file_edits :each (lambda (file edits)
							(defq stream (file-stream file))
							(cond
								((not stream)
									(print "ERROR: Cannot open source file: \q" file "\q"))
								(:t
									(verbose 1 "Writing back changes to " file)
									(defq doc (Document))
									(. doc :stream_load stream)
									(sort edits (# (- (first %1) (first %0))))
									(each (lambda ((line_num calc_set))
										(defq orig_line (. doc :get_text_line line_num)
											indent (slice orig_line 0 (bskip +char_class_space orig_line 0)))
										(. doc :idelete 0 line_num (dec (length orig_line)) line_num)
										(. doc :iinsert 0 line_num (cat indent ";" calc_set)))
										edits)
									(. doc :stream_save (file-stream file +file_open_write))))))))
				(progn
					(each (lambda (function)
						(when (defq entry (. db :find function))
							(print function " -> " (format-trashes (second entry)))))
						functions))))))
