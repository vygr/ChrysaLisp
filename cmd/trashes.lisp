(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "lib/debug/frames.inc")

(defq usage `(
	(("-h" "--help")
	"Usage: trashes [options] [function_name] ...

	options:
		-h --help: this help info.
		-v --verbosity num: how much info, default 0.

	Output transitive register TRASHES for given VP functions.
	If no paths are given on the command line, it will read
	paths from stdin.")
(("-v" "--verbosity") ,(opt-num 'opt_v))
))

(defun verbose (v &rest info)
	(if (<= v opt_v) (apply (const print) info)))

(defq +no_regs ''()
	+all_regs
		''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp
		:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
	+float_regs
		''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
	+regs_index_map
		(reduce (# (def %0 %1 (!)) %0)
			'(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
		(reduce (# (def %0 %1 (!)) %0)
			'(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
			(env 1))))

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
					(when (eql op 'emit-string)
						(second inst)))))))

(defun get-dependencies (func_name)
	;scan the compiled VP object
	(defq deps (Lset))
	(when (and (nql func_name :indirect) (nql func_name :abicall)
			(> (age (defq obj_path (cat "obj/vp/" func_name))) 0))
		(defq insts (first (read (file-stream obj_path))))
		;local labels so resolve-call can find them
		(each (# (if (eql (first %0) 'emit-label)
				(def (penv) (last (second %0)) (!))))
			insts)
		(each (# (defq op (first %0))
			(and (find op '(emit-call-p emit-jmp-p))
				(defq callee (resolve-call insts (second %0)))
				(. deps :insert callee)))
			insts))
	(. deps :tolist))

(defun topological-sort (targets)
	;iterative topological sort using a heap-allocated DFS stack
	(defq visited (Fset 101) visiting (Fset 101) order (list) stack (list))
	;initialize the stack with the top-level targets
	(each (# (push stack (list %0 :enter))) targets)
	(while (nempty? stack)
		(task-slice)
		(bind '(node state) (pop stack))
		(cond
			((eql state :enter)
				(unless (or (. visited :find node) (. visiting :find node))
					(. visiting :insert node)
					;push the exit state first so it is evaluated after its dependencies
					(push stack (list node :exit))
					;push all unvisited dependencies onto the stack
					(each (# (unless (or (. visited :find %0) (. visiting :find %0))
							(push stack (list %0 :enter))))
						(get-dependencies node))))
			((eql state :exit)
				(. visiting :erase node)
				(. visited :insert node)
				(push order node))))
	order)

(defun analyze-function (func_name db)
	(cond
		((= (age (defq obj_path (cat "obj/vp/" func_name))) 0)
			(list :external))
		(;register tracing simulation ! (as near as we can anyways)
		(each (# (if (eql (first %0) 'emit-label)
					(def (penv) (last (second %0)) (!))))
			(defq insts (first (read (file-stream obj_path)))))
		(defq call_list (list) func_set (Lset) trace -1 next_trace 0
			reg_map (scatter (Lmap) (zip +all_regs +all_regs))
			trace_map (scatter (Lmap) 0 (list _2 0 (Lmap) (Lmap) reg_map (Lset))))
		(verbose 3 "\ttracing " func_name)
		(while (<= (++ trace) next_trace)
			(bind '(*pc* *rsp* stack_map label_map reg_map trace_set)
				(. trace_map :find trace))
			(while (< *pc* (length insts))
				(defq inst (elem-get insts *pc*) *pc* (inc *pc*) op (first inst))
				(verbose 3 "\t\t" func_name " trace " trace " pc " *pc* "\n\t\t\t" inst)
				(cond
					((eql op 'emit-label)
						;update merged state at labels
						(if (defq pc (get (last (second inst))) ls (. label_map :find pc))
							(. trace_set :union ls))
						(. label_map :insert pc (. trace_set :copy)))
					((eql op 'emit-ret)
						;if ret would exit function merge and kill trace
						(setq *pc* (. stack_map :find *rsp*))
						(++ *rsp* +long_size)
						(unless (and *pc* (num? *pc*))
							(. func_set :union trace_set)
							(setq *pc* (length insts))))
					((find op '(emit-cpy-rr emit-cpy-ff))
						;copy value and mark as trashed or restored
						(bind '(%0 %1) (rest inst))
						(defq val (. reg_map :find %0))
						(. reg_map :insert %1 val)
						(if (eql %1 val)
							(. trace_set :erase %1)
							(. trace_set :insert %1)))
					((eql op 'emit-swp-rr)
						;swap values and mark as trashed or restored
						(each (# (. reg_map :insert %0 %1)
								(if (eql %0 %1)
									(. trace_set :erase %0)
									(. trace_set :insert %0)))
							;grab values before mutating them
							(rest inst) (map (# (. reg_map :find %0)) (slice inst -1 1))))
					((eql op 'emit-call)
						(. stack_map :insert (-- *rsp* +long_size) *pc*)
						(setq *pc* (get (second inst))))
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
								(. reg_map :insert %0 val)
								(if (eql val %0) (. trace_set :erase %0)))
							(list inst) -1 1))
					((and (find op '(emit-cpy-ri emit-cpy-fi)) (eql (third inst) :rsp))
						;stack spill 64 bit
						(bind '(& src & offset) inst)
						(defq val (. reg_map :find src))
						(. stack_map :insert (+ *rsp* offset) val))
					((and (find op '(emit-cpy-ir emit-cpy-if)) (eql (second inst) :rsp))
						;stack load 64 bit
						(bind '(& & offset dst) inst)
						(defq val (. stack_map :find (+ *rsp* offset)))
						(. reg_map :insert dst val)
						(if (eql val dst)
							(. trace_set :erase dst)
							(. trace_set :insert dst)))
					((and (find op '(emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i)) (eql (third inst) :rsp))
						;quantize offset down to the nearest 8-byte
						;boundary and invalidate the slot
						(. stack_map :insert (+ *rsp* (logand (neg +long_size) (last inst))) :nil))
					((find op '(emit-beq-cr emit-bne-cr emit-bge-cr
								emit-ble-cr emit-blt-cr emit-bgt-cr
								emit-beq-rr emit-bne-rr emit-bge-rr
								emit-ble-rr emit-blt-rr emit-bgt-rr))
						;if branch would carry new state then create new trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(when (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
							(. trace_map :insert (++ next_trace) (list pc *rsp*
								(. stack_map :copy) (. label_map :copy)
								(. reg_map :copy) (. trace_set :copy)))))
					((eql op 'emit-jmp)
						;if jump would carry new state then jump, else dead trace
						(defq pc (get (last inst)) ls (. label_map :find pc))
						(if (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
							(setq *pc* pc)
							(setq *pc* (length insts))))
					((find op '(emit-call-i emit-call-r))
						;FIXME, should track virtual method calls !
						(merge call_list '(:indirect)))
					((find op '(emit-jmp-i emit-jmp-r))
						;FIXME, should track virtual method calls !
						(merge call_list '(:indirect))
						(. func_set :union trace_set)
						(setq *pc* (length insts)))
					((eql op 'emit-call-abi)
						;simulate the union of all platform clobbers
						(merge call_list '(:abicall))
						(each (# (. reg_map :insert %0 :nil) (. trace_set :insert %0))
							(cat (list :r0 (second inst) (third inst)) +all_abi_trashed_regs)))
					((eql op 'emit-call-p)
						;use the callee trashes set
						(defq callee (resolve-call insts (second inst)))
						(merge call_list (list callee))
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(defq callee_trashes (second callee_entry))
							(each (# (. trace_set :insert %0) (. reg_map :insert %0 :nil))
								(if (list? callee_trashes) callee_trashes (. callee_trashes :tolist)))))
					((eql op 'emit-jmp-p)
						;exit function, merge and kill trace
						(defq callee (resolve-call insts (second inst)))
						(merge call_list (list callee))
						;known trashed registers from db during symbolic execution
						(when (defq callee_entry (. db :find callee))
							(defq callee_trashes (second callee_entry))
							(each (# (. trace_set :insert %0) (. reg_map :insert %0 :nil))
								(if (list? callee_trashes) callee_trashes (. callee_trashes :tolist))))
						(. func_set :union trace_set)
						(setq *pc* (length insts)))
					(:t	;each modified register is flagged as trashed and value :nil
						(each (# (. trace_set :insert %0) (. reg_map :insert %0 :nil))
							(get-modified-regs inst))))
				(verbose 3 "\t\t\t" (format-trashes trace_set))
				(verbose 4 "\t\t\t" (format-values reg_map)))
			(. trace_map :erase trace))
		(list :resolved func_set call_list))))

(defun propagate-trashes (functions)
	(defq db (Fmap 101) order (topological-sort functions))
	(verbose 1 "analysis order " order)
	;each caller now accurately steps through callee clobber states
	(each (lambda (f)
		(unless (. db :find f)
			(bind '(type &optional func_set call_list) (analyze-function f db))
			(cond
				((eql type :external)
					(. db :insert f (list :external (scatter (Lset) +no_regs))))
				((eql type :resolved)
					(verbose 2 "\tfunction " f "\n\t\tcalls " call_list
						"\n\t\ttrashes " (format-trashes func_set))
					(. db :insert f (list :resolved func_set call_list))))))
		order)
	;converge remaining transitive sets (such as recursive structures)
	(defq changed :t)
	(while changed
		(setq changed :nil)
		(each (lambda (f)
			(when (defq entry (. db :find f))
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

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v 0 args (options stdio usage)))
		(defq targets (rest args))
		(if (empty? targets)
			(lines! (lambda (line) (push targets (trim line))) (io-stream 'stdin)))
		(when (nempty? targets)
			(defq db (propagate-trashes targets))
			(each (lambda (f)
				(when (defq entry (. db :find f))
					(print f " -> " (format-trashes (second entry)))))
				targets))))