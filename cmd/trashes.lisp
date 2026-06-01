(import "lib/options/options.inc")
(import "lib/files/files.inc")

(defq usage `(
	(("-h" "--help")
	"Usage: trashes [options] [function_name] ...

	options:
		-h --help: this help info.
		-v --verbosity num: how much info, default 0.
		-l --lint: lint documented vs calculated trashes.

	Output transitive register TRASHES for given VP functions.
	If no paths are given on the command line, it will read
	paths from stdin.")
(("-v" "--verbosity") ,(opt-num 'opt_v))
(("-l" "--lint") ,(opt-flag 'opt_l))
))

(defun verbose (v &rest info)
	(if (<= v opt_v) (apply (const print) info))
	(stream-flush (io-stream 'stdout)))

(defq +no_regs ''()
	+int_regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14)
	+float_regs ''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))
(defq +all_regs `'(~(last +int_regs) ~(last +float_regs))
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
	(map (lambda ((s e)) (if (= s (setq e (dec e)))
			(str prefix s)
			(str prefix s "-" prefix e)))
		(slices indices)))

(defun format-trashes (func_set)
	(defq r_indices (list) f_indices (list))
	(each (# (if (starts-with ":r" %0)
			(push r_indices (reg? %0))
			(push f_indices (reg? %0))))
		(if (list? func_set) func_set (. func_set :tolist)))
	(defq formatted_parts (cat (format-group ":r" r_indices) (format-group ":f" f_indices)))
	(if (empty? formatted_parts) "none" (join formatted_parts ", ")))

(defun format-values (reg_map)
	(defq out (list))
	(. (reduce (lambda (m (r v)) (. m :update v (# (ifn %0 (list r) (push %0 r)))) m)
			(filter (lambda ((r v)) (nql r v)) (. reg_map :tolist)) (Lmap))
		:each (lambda (v rs) (push out (str v " -> " (format-trashes rs)))))
	(if (empty? out) "none" (join out " | ")))

(defun get-modified-regs (inst)
	(cond
		;exclude instructions that do not modify registers
		((find (defq op (first inst)) '(emit-push emit-alloc emit-free
			emit-ret emit-sync emit-brk emit-nop emit-label emit-tlabel
			emit-align emit-string emit-byte emit-short emit-int emit-long
			emit-cstr emit-call-p emit-call-i emit-call emit-call-r emit-call-abi
			emit-jmp-r emit-cpy-rd emit-cpy-rd-b emit-cpy-rd-s emit-cpy-rd-i
			emit-cpy-fd)) '())
		((eql op 'emit-pop) (rest inst))
		((find op '(emit-swp-rr emit-land-rr emit-lnot-rr emit-div-rrr
			emit-div-rrr-u)) (slice inst -3 -1))
		((reg? (last inst)) (slice inst -2 -1))
		('())))

(defun resolve-call (insts lbl)
	(when (defq pc (get lbl (penv)))
		(defq inst (elem-get insts (inc pc)) op (first inst))
		(when (eql op 'emit-long)
			(defq expr (second inst))
			(when (and (list? expr) (= (length expr) 3) (eql (first expr) '-))
				(when (defq pc (get (second expr) (penv)))
					(defq inst (elem-get insts (inc pc)) op (first inst))
					(if (eql op 'emit-string) (second inst)))))))

(defun get-dependencies (function)
	;scan the compiled VP object
	(defq deps (Lset))
	(when (and (nql function :indirect) (nql function :abicall)
			(> (age (defq obj_path (cat "obj/vp/" function))) 0))
		(defq insts (first (read (file-stream obj_path))))
		;local labels so resolve-call can find them
		(each (# (if (eql (first %0) 'emit-label)
				(def (penv) (last (second %0)) (!)))) insts)
		(each (# (if (find (first %0) '(emit-call-p emit-jmp-p))
				(. deps :insert (resolve-call insts (second %0))))) insts))
	(. deps :tolist))

(defun def-reg (%0 %1)
	; define register value and trashed state
	(. reg_map :insert %0 %1)
	(if (eql %0 %1)
		(. trace_set :erase %0)
		(. trace_set :insert %0)))

(defun analyze-function (function db)
	(cond
		((= (age (defq obj_path (cat "obj/vp/" function))) 0)
			(list :external))
		(;register tracing simulation ! (as near as we can anyways)
		(each (# (if (eql (first %0) 'emit-label)
				(def (penv) (last (second %0)) (!))))
			(defq insts (first (read (file-stream obj_path)))))
		(defq call_list (list) func_set (Lset) trace -1 next_trace 0
			reg_map (scatter (Lmap) (zip +all_regs +all_regs))
			label_map (Lmap)
			trace_map (scatter (Lmap) 0 (list _2 0 (Lmap) reg_map (Lset))))
		(verbose 3 "\ttracing " function)
		(while (<= (++ trace) next_trace)
			(task-slice)
			(bind '(*pc* *rsp* stack_map reg_map trace_set)
				(. trace_map :find trace))
			(while (< *pc* (length insts))
				(defq inst (elem-get insts *pc*) *pc* (inc *pc*) op (first inst))
				(verbose 3 "\t\t" function " trace " trace " pc " *pc* "\n\t\t\t" inst)
				(cond
					;labels and branches
					((eql op 'emit-label)
						;update merged state at labels
						(if (defq pc (get (last (second inst))) ls (. label_map :find pc))
							(. trace_set :union ls))
						(. label_map :insert pc (. trace_set :copy)))
					((find op '(emit-beq-cr emit-bne-cr emit-bge-cr
								emit-ble-cr emit-blt-cr emit-bgt-cr
								emit-beq-rr emit-bne-rr emit-bge-rr
								emit-ble-rr emit-blt-rr emit-bgt-rr))
						;if branch would carry new state then create new trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(when (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
							(. trace_map :insert (++ next_trace) (list pc *rsp*
								(. stack_map :copy)
								;(. label_map :copy)
								(. reg_map :copy)
								(. trace_set :copy)))))

					;register data flow
					((find op '(emit-cpy-rr emit-cpy-ff))
						;copy value and mark as trashed or restored
						(def-reg (last inst) (. reg_map :find (second inst))))
					((eql op 'emit-swp-rr)
						;swap values and mark as trashed or restored
						(each (const def-reg)
							(rest inst) (map (# (. reg_map :find %0)) (slice inst -1 1))))

					;stack tracking
					((eql op 'emit-alloc)
						(setq *rsp* (-- *rsp* (second inst))))
					((eql op 'emit-free)
						(setq *rsp* (++ *rsp* (second inst))))
					((eql op 'emit-push)
						;push the values of all registers pushed
						(each! (# (. stack_map :insert (-- *rsp* +long_size)
								(. reg_map :find %0)))
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
						(defq val (. reg_map :find src))
						(. stack_map :insert (+ *rsp* offset) val))
					((and (find op '(emit-cpy-ir emit-cpy-if)) (eql (second inst) :rsp))
						;stack load 64 bit
						(def-reg (last inst) (. stack_map :find (+ *rsp* (third inst)))))
					((and (find op '(emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i)) (eql (third inst) :rsp))
						;quantize offset down to the nearest 8-byte
						;boundary and invalidate the slot
						(. stack_map :insert (+ *rsp* (logand (neg +long_size) (last inst))) :nil))

					;internal call, jump and return
					((eql op 'emit-ret)
						;if ret would exit function merge and kill trace
						(setq *pc* (. stack_map :find *rsp*))
						(++ *rsp* +long_size)
						(unless (num? *pc*)
							(. func_set :union trace_set)
							(setq *pc* (length insts))))
					((eql op 'emit-call)
						(. func_set :union trace_set)
						(setq *pc* (length insts)))
						; ;if call would carry new state then call
						; (defq pc (get (last inst)) ls (. label_map :find pc))
						; (when (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
						; 	(. stack_map :insert (-- *rsp* +long_size) *pc*)
						; 	(setq *pc* pc)))
					((eql op 'emit-jmp)
						;if jump would carry new state then jump, else kill trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(if (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
							(setq *pc* pc)
							(setq *pc* (length insts))))

					;external call and jump
					((find op '(emit-call-i emit-call-r))
						;FIXME, should track virtual method calls !
						(merge call_list '(:indirect))
						(each (# (. reg_map :insert %0 :nil) (. trace_set :insert %0)) +all_regs))
					((find op '(emit-jmp-i emit-jmp-r))
						;FIXME, should track virtual method calls !
						(merge call_list '(:indirect))
						(each (# (. reg_map :insert %0 :nil) (. trace_set :insert %0)) +all_regs)
						(. func_set :union trace_set)
						(setq *pc* (length insts)))
					((eql op 'emit-call-abi)
						;simulate the union of all platform clobbers
						(merge call_list '(:abicall))
						(each (# (def-reg %0 :nil))
							(cat (list :r0 (second inst) (third inst)) +all_abi_trashed_regs)))
					((eql op 'emit-call-p)
						;use the callee trashes set
						(merge call_list (list (defq callee (resolve-call insts (second inst)))))
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(defq callee_trashes (second callee_entry))
							(each (# (def-reg %0 :nil)) (. callee_trashes :tolist))))
					((eql op 'emit-jmp-p)
						;exit function, merge and kill trace
						(merge call_list (list (defq callee (resolve-call insts (second inst)))))
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(defq callee_trashes (second callee_entry))
							(each (# (def-reg %0 :nil)) (. callee_trashes :tolist)))
						(. func_set :union trace_set)
						(setq *pc* (length insts)))

					;everything else
					(:t ;each modified register is flagged as trashed and value :nil
						(each (# (def-reg %0 :nil)) (get-modified-regs inst))))
				(verbose 3 "\t\t\t" (format-trashes trace_set))
				(verbose 4 "\t\t\t" (format-values reg_map)))
			(. trace_map :erase trace)
			(verbose 4 "\t\tmerged " (format-trashes func_set)))
		(list :resolved func_set call_list))))

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
				((eql type :resolved)
					(verbose 2 "\tfunction " function "\n\t\tcalls " call_list
						"\n\t\ttrashes " (format-trashes func_set))
					(. db :insert function (list :resolved func_set call_list))))))
		order)
	;converge remaining transitive sets (such as recursive structures)
	(defq changed :t)
	(while changed
		(setq changed :nil)
		(each (lambda (function)
			(when (defq entry (. db :find function))
				(bind '(type &optional func_set call_list) entry)
				(when (eql type :resolved)
					(defq size_before (. func_set :size))
					(each (lambda (target)
						(each (lambda (r) (. func_set :insert r))
							(case target
								(:abicall +no_regs)
								(:indirect +no_regs)
								(:t +no_regs))))
						call_list)
					(setq changed (or changed (/= (. func_set :size) size_before))))))
			order))
	db)

(defun load-doc-trashes ()
	(defq doc_db (Fmap 101) files (files-all "docs/reference/vp_classes" '(".md")))
	(each (lambda (file)
		(when (defq stream (file-stream file))
			(defq function :nil in_trashes :nil)
			(lines! (lambda (line)
				(cond
					((starts-with "### " line)
						(setq in_trashes :nil)
						(if (defq pos (find "-" line))
							(setq function (trim (slice line (+ pos 3) -1)))
							(setq function :nil)))
					((and function (eql line "trashes"))
						(setq in_trashes :t))
					(in_trashes
						(when (nempty? line)
							(. doc_db :insert function line)
							(setq in_trashes :nil function :nil)))))
				stream)))
		files)
	doc_db)

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v 0 opt_l :nil args (options stdio usage)))
		(defq functions (rest args))
		(if (empty? functions)
			(lines! (# (push functions %0)) (io-stream 'stdin)))
		(when (nempty? functions)
			(defq functions (map (# (if (starts-with "obj/vp/" %0) (slice %0 7 -1) %0)) functions)
				db (propagate-trashes functions))
			(if opt_l
				(progn
					(defq doc_db (load-doc-trashes))
					(each (lambda (function)
						(when (defq entry (. db :find function))
							(when (nql (first entry) :external)
								(defq doc_set (. doc_db :find function))
								(cond
									((not doc_set)
										(print "WARNING: No documentation found for " function))
									(:t
										(defq calc_set (format-trashes (second entry)))
										(unless (eql doc_set calc_set)
											(print "WARNING: Mismatch in " function)
											(print "  Documented: " doc_set)
											(print "  Calculated: " calc_set)))))))
						functions))
				(progn
					(each (lambda (function)
						(when (defq entry (. db :find function))
							(print function " -> " (format-trashes (second entry)))))
						functions))))))