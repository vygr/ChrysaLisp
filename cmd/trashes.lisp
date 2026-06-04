(import "lib/options/options.inc")
(import "lib/text/document.inc")
;(import "lib/debug/frames.inc")
(import "lib/files/info.inc")

(defq usage `(
	(("-h" "--help")
	"Usage: trashes [options] [function_name] ...

	options:
		-h --help: this help info.
		-v --verbosity num: how much info, default 0.
		-l --lint: lint documented vs calculated trashes.
		-w --write: write back calculated trashes to source
			files on mismatch.

	Calculate and trace active transitive register clobber state for
	virtual methods and static functions. Analyses compiled instructions
	directly via symbolic execution and traces live registers.")
(("-v" "--verbosity") ,(opt-num 'opt_v))
(("-l" "--lint") ,(opt-flag 'opt_l))
(("-w" "--write") ,(opt-flag 'opt_w))
))

(defun verbose (v &rest info)
	(if (<= v opt_v) (apply (const print) info))
	(stream-flush (io-stream 'stdout)))

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

(defun format-group (prefix indices)
	(map (lambda ((s e)) (if (= s (-- e))
			(str prefix s)
			(str prefix s "-" prefix e)))
		(slices indices)))

(defun bit-count (n)
	; (bit-count n) -> count of set bits
	(defq c 0)
	(while (/= n 0)
		(++ c)
		(setq n (logand n (dec n)))) c)

(defun format-trashes (func_set)
	(defq r_indices (list) f_indices (list) s_indices (list) i 0)
	(while (/= func_set 0)
		(when (odd? func_set)
			(if (< i 16)
				(if (= i 15)
					(push s_indices ":rsp")
					(push r_indices i))
				(push f_indices (- i 16))))
		(++ i)
		(setq func_set (>> func_set 1)))
	(defq formatted_parts (cat
		(format-group ":r" r_indices)
		(format-group ":f" f_indices)
		s_indices))
	(if (empty? formatted_parts) "none" (join formatted_parts ", ")))

(defun format-values (reg_map)
	(defq out (list))
	(. (reduce (lambda (m (r v)) (. m :update v (# (ifn %0 (list r) (push %0 r)))) m)
			(filter (lambda ((r v)) (nql r v)) (tolist reg_map)) (Lmap))
		:each (lambda (v rs) (push out (str v " -> " (format-trashes 
			(reduce (lambda (m r) (logior m (<< 1 (reg? r)))) rs 0))))))
	(if (empty? out) "none" (join out " | ")))

(defun resolve-static-method (insts lbl)
	;find function that implements this static method
	(when (defq pc (get lbl (penv)))
		(defq inst (elem-get insts (inc pc)) op (first inst))
		(when (eql op 'emit-long)
			(defq expr (second inst))
			(when (and (list? expr) (= (length expr) 3) (eql (first expr) '-))
				(when (defq pc (get (second expr) (penv)))
					(defq inst (elem-get insts (inc pc)) op (first inst))
					(if (eql op 'emit-string) (second inst)))))))

(defun resolve-virtual-method (c m)
	;find function that implements this virtual method
	(when (defq c_entry (. *class_db* :find c))
		(when (defq m_entry (.-> c_entry (:find :methods) (:find m)))
			(. m_entry :find :function))))

(defun resolve-virtual-methods (c m)
	;find function that implements this virtual method
	;and all subclass overrides !
	(defq deps (list))
	(when (defq c_entry (. *class_db* :find c))
		(when (defq m_entry (.-> c_entry (:find :methods) (:find m)))
			(when (defq function (. m_entry :find :function))
				(push deps function)
				(when (defq o_entry (. m_entry :find :overrides))
					(each (lambda (over_c)
						(when (defq function (resolve-virtual-method over_c m))
							(push deps function)))
						o_entry)))))
	deps)

(defun get-function-insts (function)
	; (get-function-insts function) -> :nil | insts
	(memoize function (progn
		(defq obj_path (cat +obj_dir function))
		(if (> (age obj_path) 0)
			(first (read (file-stream obj_path))))) 101))

(defun get-dependencies (function)
	;scan the compiled VP object
	(defq deps (list))
	(when (and (nql function :indirect) (nql function :abicall)
			(> (age (defq obj_path (cat +obj_dir function))) 0))
		(unless (defq insts (get-function-insts function))
			(setq insts (first (read (file-stream obj_path))))
			(. *inst_cache* :insert obj_path insts))
		;local labels so resolve-static-method can find them
		(each (# (if (find (first %0) '(emit-label emit-tlabel))
				(def (penv) (last (second %0)) (!)))) insts)
		(each! (lambda (inst)
			(defq op (first inst) c :nil m :nil)
			(cond
				((find op '(emit-call-p emit-jmp-p))
					(when (defq target (resolve-static-method insts (second inst)))
						(merge deps (list target))))
				((find op '(emit-call-i emit-jmp-i))
					(bind '(& & & &optional c m) inst))
				((find op '(emit-call-r emit-jmp-r))
					(bind '(& & &optional c m) inst)))
			(when (and c m)
				(each (# (merge deps (list %0)))
					(resolve-virtual-methods c m))))
			(list insts) (get '_2))) deps)

(defun def-reg (%0 %1)
	; define register value and trashed state
	(def reg_map %0 %1)
	(if (eql %0 %1)
		(setq trace_set (logand trace_set (lognot (<< 1 (reg? %0)))))
		(setq trace_set (logior trace_set (<< 1 (reg? %0))))))

(defun virtual-trashes-union (c m)
	;calculate the union of all this class method trashes
	;and its subclasses overrides, based on the current state of the active db
	(defq union_set 0)
	(each (# (if (defq f_entry (. db :find %0))
				(setq union_set (logior union_set (second f_entry)))))
		(resolve-virtual-methods c m))
	(if (= union_set 0)
		(const (reduce (lambda (m r) (logior m (<< 1 (reg? r)))) +all_extern_trashed_regs 0))
		union_set))

(defun get-modified-regs (inst)
	;only care about the instructions we don't handle in analyze-function !
	(cond
		((find (defq op (first inst)) '(emit-cpy-rd emit-cpy-rd-b emit-cpy-rd-s
			emit-cpy-rd-i emit-cpy-fd)) '())
		((find op '(emit-swp-rr emit-land-rr emit-lnot-rr emit-div-rrr
			emit-div-rrr-u)) (slice inst -3 -1))
		((find op '(emit-min-cr emit-min-rr emit-max-cr emit-max-rr vp-abs-rr))
			(slice inst 2 3))
		((reg? (last inst)) (slice inst -2 -1))
		('())))

(defun analyze-function (function db)
	(cond
		((not (defq insts (get-function-insts function)))
			(list :external))
		(;register tracing simulation ! (as near as we can anyways)
		;local labels so we can resolve starting PC and jumps
		(each (# (if (find (first %0) '(emit-label emit-tlabel))
				(def (penv) (last (second %0)) (!)))) insts)
		;determine starting PC for this subroutine/entry point (defaulting to _2)
		(defq start_pc (ifn (defq start_pc (get '_2)) 0 start_pc)
			call_list (list) func_set 0 trace -1 next_trace 0
			reg_map (reduce (lambda (m r) (def m r r) m) +all_regs (env 1))
			label_map (Lmap)
			trace_map (scatter (Lmap) 0 (list start_pc 0 (Lmap) reg_map 0 (list))))
		(verbose 3 "\ttracing " function)
		(while (<= (++ trace) next_trace)
			(task-slice)
			(bind '(*pc* *rsp* stack_map reg_map trace_set call_stack) (. trace_map :find trace))
			(while (< *pc* (length insts))
				(defq inst (elem-get insts *pc*) *pc* (inc *pc*) op (first inst))
				(verbose 3 "\t\t" function " trace " trace " pc " *pc* "\n\t\t\t" inst)
				(cond
					;labels and branches
					((find op '(emit-label emit-tlabel))
						;update merged state at labels
						(if (defq pc (get (last (second inst))) ls (. label_map :find pc))
							(setq trace_set (logior trace_set ls)))
						(. label_map :insert pc trace_set))
					((find op '(emit-beq-cr emit-bne-cr emit-bge-cr
								emit-ble-cr emit-blt-cr emit-bgt-cr
								emit-beq-rr emit-bne-rr emit-bge-rr
								emit-ble-rr emit-blt-rr emit-bgt-rr))
						;if branch would carry new state then create new trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(when (or (not ls) (/= 0 (logand trace_set (lognot ls))))
							(. trace_map :insert (++ next_trace) (list pc *rsp*
								(. stack_map :copy) (env-copy reg_map 1) trace_set (cat call_stack)))))

					;register data flow
					((find op '(emit-cpy-rr emit-cpy-ff))
						;copy value and mark as trashed or restored
						(def-reg (last inst) (def? (second inst) reg_map)))
					((eql op 'emit-swp-rr)
						;swap values and mark as trashed or restored
						(each (const def-reg)
							(rest inst) (map (# (def? %0 reg_map)) (slice inst -1 1))))

					;stack tracking
					((eql op 'emit-alloc)
						(setq *rsp* (-- *rsp* (second inst))))
					((eql op 'emit-free)
						(setq *rsp* (++ *rsp* (second inst))))
					((eql op 'emit-push)
						;push the values of all registers pushed
						(each! (# (. stack_map :insert (-- *rsp* +long_size)
								(def? %0 reg_map)))
							(list inst) 1))
					((eql op 'emit-pop)
						;pop the values of all registers popped
						;and flag if register is now restored
						(each! (# (defq val (. stack_map :find *rsp*))
								(++ *rsp* +long_size)
								(def-reg %0 val))
							(list inst) -1 1))
					((and (find op '(emit-cpy-ri emit-cpy-fi)) (eql (third inst) :rsp))
						;stack spill 64 bit
						(bind '(& src & offset) inst)
						(. stack_map :insert (+ *rsp* offset) (def? src reg_map)))
					((and (find op '(emit-cpy-ri emit-cpy-fi)) (eql (third inst) :rsp))
						;stack spill 64 bit
						(bind '(& src & offset) inst)
						(. stack_map :insert (+ *rsp* offset) (def? src reg_map)))
					((and (find op '(emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i)) (eql (third inst) :rsp))
						;quantize offset down to the nearest 8-byte
						;boundary and invalidate the slot
						(. stack_map :insert (+ *rsp* (logand (neg +long_size) (last inst))) :nil))

					;internal call, jump and return
					((eql op 'emit-ret)
						;we are inside an inlined local subroutine, return to the caller
						(ifn (setq *pc* (pop call_stack))
							(progn
								;return from main, merge clobbers and terminate path
								(setq func_set (logior func_set trace_set))
								(setq *pc* (length insts)))))
					((eql op 'emit-call)
						;Local subroutine call: inline it using the path's call stack
						(when (defq target_pc (get (second inst)))
							(push call_stack *pc*)
							(setq *pc* target_pc)))
					((eql op 'emit-jmp)
						;if jump would carry new state then jump, else kill trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(if (or (not ls) (/= 0 (logand trace_set (lognot ls))))
							(setq *pc* pc)
							(setq *pc* (length insts))))

					;external static call and jump
					((eql op 'emit-call-p)
						;use the callee trashes set
						(defq callee (resolve-static-method insts (second inst)))
						(push call_list callee)
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(defq callee_trashes (second callee_entry))
							(setq trace_set (logior trace_set callee_trashes))
							(defq tmp_set callee_trashes i 0)
							(while (/= tmp_set 0)
								(when (odd? tmp_set)
									(def reg_map (elem-get +all_regs i) :nil))
								(++ i)
								(setq tmp_set (>> tmp_set 1)))))
					((eql op 'emit-jmp-p)
						;exit function, merge and kill trace
						(defq callee (resolve-static-method insts (second inst)))
						(push call_list callee)
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(defq callee_trashes (second callee_entry))
							(setq trace_set (logior trace_set callee_trashes)))
						(setq func_set (logior func_set trace_set))
						(setq *pc* (length insts)))

					;external virtual call and jump
					((eql op 'emit-call-r)
						(if (bind '(& & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set (const (reduce (lambda (m r) (logior m (<< 1 (reg? r)))) +all_extern_trashed_regs 0))
								calls '(:indirect)))
						(merge call_list calls)
						(setq trace_set (logior trace_set call_set))
						(defq tmp_set call_set i 0)
						(while (/= tmp_set 0)
							(when (odd? tmp_set)
								(def reg_map (elem-get +all_regs i) :nil))
							(++ i)
							(setq tmp_set (>> tmp_set 1))))
					((eql op 'emit-jmp-r)
						;exit function, merge and kill trace
						(if (bind '(& & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set (const (reduce (lambda (m r) (logior m (<< 1 (reg? r)))) +all_extern_trashed_regs 0))
								calls '(:indirect)))
						(merge call_list calls)
						(setq trace_set (logior trace_set call_set))
						(setq func_set (logior func_set trace_set))
						(setq *pc* (length insts)))
					((eql op 'emit-call-i)
						(if (bind '(& & & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set (const (reduce (lambda (m r) (logior m (<< 1 (reg? r)))) +all_extern_trashed_regs 0))
								calls '(:indirect)))
						(merge call_list calls)
						(setq trace_set (logior trace_set call_set))
						(defq tmp_set call_set i 0)
						(while (/= tmp_set 0)
							(when (odd? tmp_set)
								(def reg_map (elem-get +all_regs i) :nil))
							(++ i)
							(setq tmp_set (>> tmp_set 1))))
					((eql op 'emit-jmp-i)
						;exit function, merge and kill trace
						(if (bind '(& & & &optional c m) inst)
							(defq call_set (virtual-trashes-union c m)
								calls (resolve-virtual-methods c m))
							(defq call_set (const (reduce (lambda (m r) (logior m (<< 1 (reg? r)))) +all_extern_trashed_regs 0))
								calls '(:indirect)))
						(merge call_list calls)
						(setq trace_set (logior trace_set call_set))
						(setq func_set (logior func_set trace_set))
						(setq *pc* (length insts)))

					;external host os call
					((eql op 'emit-call-abi)
						;simulate the union of all platform clobbers
						(push call_list :abicall)
						(defq call_set (logior (const (reduce (lambda (m r) (logior m (<< 1 (reg? r)))) +all_abi_trashed_regs 0))
							(<< 1 (reg? :r0))
							(<< 1 (reg? (second inst)))
							(<< 1 (reg? (third inst)))))
						(setq trace_set (logior trace_set call_set))
						(defq tmp_set call_set i 0)
						(while (/= tmp_set 0)
							(when (odd? tmp_set)
								(def reg_map (elem-get +all_regs i) :nil))
							(++ i)
							(setq tmp_set (>> tmp_set 1))))

					;everything else
					(:t ;each modified register is flagged as trashed and value :nil
						(each (# (def-reg %0 :nil)) (get-modified-regs inst))))
				(verbose 3 "\t\t\t" (format-trashes trace_set))
				(verbose 4 "\t\t\t" (format-values reg_map)))
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
					(. db :insert function (list :external (scatter (Lset) +no_regs))))
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
						(defq old_size (bit-count func_set))
						;bypasses symbolic tracing if the function already trashes all registers
						(when (< old_size (const (length +all_regs)))
							(bind '(type &optional new_set new_calls) (analyze-function function db))
							(when (/= old_size (bit-count new_set))
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
