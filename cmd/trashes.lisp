(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "lib/debug/frames.inc")

(defq usage `(
	(("-h" "--help")
	"Usage: trashes [options] [function_name] ...

	options:
		-h --help: this help info.

	Output transitive register TRASHES for given VP functions.
	If no paths are given on the command line, it will read
	paths from stdin.")
))

(defq +no_regs ''() +all_regs
		''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14
		:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
	+opt_two_out_ops
		''(emit-swp-rr emit-land-rr emit-lnot-rr emit-div-rrr emit-div-rrr-u))

(defun reg? (r)
	(and (sym? r)
		(or (find r '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
			(find r '(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)))))

(defun strip-colon (s)
	(if (starts-with ":" s) (slice s 1 -1) s))

(defun reg-index (r)
	(str-to-num (slice r 2 -1)))

(defun format-group (prefix indices)
	(map (lambda ((s e)) (if (= s (setq e (dec e)))
			(str prefix s)
			(str prefix s "-" prefix e)))
		(slices indices)))

(defun format-trashes (trashes_set)
	(defq r_indices (list) f_indices (list))
	(each (# (if (starts-with ":r" %0)
			(push r_indices (reg-index %0))
			(push f_indices (reg-index %0))))
		(. trashes_set :tolist))
	(defq formatted_parts (cat (format-group ":r" r_indices) (format-group ":f" f_indices)))
	(if (empty? formatted_parts) "none" (join formatted_parts ", ")))

(defun get-modified-regs (inst)
	(cond
		((find (defq op (first inst))
			'(emit-push emit-alloc emit-free emit-ret emit-sync
			emit-brk emit-nop emit-label emit-tlabel emit-align
			emit-string emit-byte emit-short emit-int emit-long emit-cstr)) '())
		((find op '(emit-call-p emit-call-i emit-call emit-call-r emit-call-abi)) '())
		((eql op 'emit-pop) (rest inst))
		((find op +opt_two_out_ops) (slice inst -3 -1))
		((reg? (last inst)) (slice inst -2 -1))
		('())))

(defun resolve-call (insts lbl)
	(when (defq pc (def? lbl (penv)))
		(defq inst (elem-get insts (inc pc)) op (first inst))
		(when (eql op 'emit-long)
			(defq expr (second inst))
			(when (and (list? expr) (= (length expr) 3) (eql (first expr) '-))
				(when (defq pc (def? (second expr) (penv)))
					(defq inst (elem-get insts (inc pc)) op (first inst))
					(when (eql op 'emit-string)
						(second inst)))))))

(defun analyze-function (func_name)
	(cond
		((= (age (defq obj_path (cat "obj/vp/" func_name))) 0)
			(list :external))
		(;register tracing simulation ! (as near as we can anyways)
		(each (# (if (eql (first %0) 'emit-label)
					(def (penv) (last (second %0)) (!))))
			(defq insts (first (read (file-stream obj_path)))))
		(defq call_list (list) trashes_set (Lset) label_map (Lmap)
			trace 0 traces (list (list _2 0 (Lmap) (Lset) trace)))
		(each-mergeable (lambda ((*pc* *rsp* stack_map trace_set n))
			(while (< *pc* (length insts))
				(defq inst (elem-get insts *pc*) *pc* (inc *pc*) op (first inst))
				(print func_name " trace " n " pc " *pc*)
				(print inst)
				(cond
					((eql op 'emit-label)
						(if (defq pc (def? (last (second inst)) (penv)) ls (. label_map :find pc))
							(. ls :union trace_set)
							(. label_map :insert pc (. trace_set :copy))))
					((eql op 'emit-ret)
						(setq *pc* (. stack_map :find *rsp*) *rsp* (inc *rsp*))
						(unless (and *pc* (num? *pc*))
							(. trashes_set :union trace_set)
							(setq *pc* (length insts))))
					((eql op 'emit-call)
						(. stack_map :insert (setq *rsp* (dec *rsp*)) *pc*)
						(setq *pc* (def? (second inst) (penv))))
					((eql op 'emit-jmp)
						(defq pc (def? (last inst) (penv)) ls (. label_map :find pc))
						(if (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
							(setq *pc* pc)
							(setq *pc* (length insts))))
					((eql op 'emit-push)
						(each! (# (. stack_map :insert (setq *rsp* (dec *rsp*)) %0))
							(list inst) 1))
					((eql op 'emit-pop)
						(each! (# (defq r (. stack_map :find *rsp*))
								(setq *rsp* (inc *rsp*))
								(if (eql r %0) (. trace_set :erase %0)))
							(list inst) -1 1))
					((find op '(emit-beq-cr emit-bne-cr emit-bge-cr
								emit-ble-cr emit-blt-cr emit-bgt-cr
								emit-beq-rr emit-bne-rr emit-bge-rr
								emit-ble-rr emit-blt-rr emit-bgt-rr))
						(defq pc (def? (last inst) (penv)) ls (. label_map :find pc))
						(when (or (not ls) (not (.-> trace_set :copy (:difference ls) :empty?)))
							(push traces (list pc *rsp*
								(. stack_map :copy) (. trace_set :copy) (++ trace)))))
					((find op '(emit-call-i emit-call-r))
						(push call_list :indirect))
					((find op '(emit-jmp-i emit-jmp-r))
						(push call_list :indirect)
						(setq *pc* (length insts)))
					((eql op 'emit-call-abi)
						;FIXME, should account for the platform abi trashes set !
						(. trace_set :insert :r0)
						(. trace_set :insert (second inst))
						(. trace_set :insert (third inst)))
					((eql op 'emit-call-p)
						(push call_list (resolve-call insts (second inst))))
					((eql op 'emit-jmp-p)
						(push call_list (resolve-call insts (second inst)))
						(setq *pc* (length insts)))
					((each (# (. trace_set :insert %0))
						(get-modified-regs inst))))
			(print (format-trashes trace_set))
			(print))) traces)
		(list :resolved trashes_set call_list))))

(defun propagate-trashes (functions)
	(defq db (Fmap 101) documented_map (Fmap 101) worklist (cat functions))
	(each-mergeable (lambda (f)
		(unless (. db :find f)
			(cond
				;a dependency (not one of the initial targets) and has documented trashes, use them!
				((and (not (find f functions)) (defq doc_trashes (. documented_map :find f)))
					(. db :insert f (list :documented doc_trashes)))
				(:t (bind '(type &rest payload) (analyze-function f))
					(cond
						((eql type :external)
							(. db :insert f (list :external (scatter (Lset) +no_regs))))
						((eql type :resolved)
							(bind '(trashes_set call_list) payload)
							(. db :insert f (list :resolved trashes_set call_list))
							(each (lambda (target)
									(if (str? target) (merge worklist (list target))))
								call_list)))))))
		worklist)
	(defq changed :t)
	(while changed
		(setq changed :nil)
		(each (lambda (f)
			(when (defq entry (. db :find f))
				(bind '(type trashes_set &rest payload) entry)
				(when (eql type :resolved)
					(defq size_before (. trashes_set :size))
					(each! (lambda (target)
						(each (lambda (r) (. trashes_set :insert r))
							(if (find target '(:indirect :abicall))
								+no_regs
								(if (defq callee_entry (. db :find target))
									(. (second callee_entry) :tolist)
									+no_regs))))
						payload)
					(setq changed (/= (. trashes_set :size) size_before)))))
			worklist))
	db)

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq targets (rest args))
		(if (empty? targets)
			(lines! (lambda (line) (push targets (trim line))) (io-stream 'stdin)))
		(when (nempty? targets)
			(defq db (propagate-trashes targets))
			(each (lambda (f)
				(when (defq entry (. db :find f))
					(print f " -> " (format-trashes (second entry)))))
				targets))))