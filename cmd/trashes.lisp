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

(defun build-label-map (insts)
	(defq label_map (Fmap 31) current_label :nil)
	(each (lambda (inst)
		(cond
			((eql (first inst) 'emit-label)
				(setq current_label (last (second inst))))
			(current_label
				(. label_map :insert current_label inst)
				(setq current_label :nil))))
		insts)
	label_map)

(defun resolve-label (lbl label_map)
	(when (defq inst (. label_map :find lbl))
		(cond
			((eql (first inst) 'emit-string)
				(second inst))
			((eql (first inst) 'emit-long)
				(defq expr (second inst))
				(if (and (list? expr) (= (length expr) 3) (eql (first expr) '-))
					(resolve-label (second expr) label_map)
					:nil))
			(func_name))))

(defun analyze-function (func_name)
	(defq obj_path (cat "obj/vp/" func_name))
	(if (= (age obj_path) 0)
		(list :external)
		(progn
			(defq insts (first (read (file-stream obj_path)))
				label_map (build-label-map insts))
			;sequential simulation
			(defq trashes_set (Lset) call_sites (list))
			(each (lambda (inst)
				(defq op (first inst))
				(cond
					((find op '(emit-call-p emit-call-i emit-call emit-call-r emit-call-abi
								emit-jmp-p emit-jmp-i emit-jmp emit-jmp-r))
						(push call_sites (cond
							((find op '(emit-call-p emit-jmp-p emit-call emit-jmp))
								(resolve-label (second inst) label_map))
							((eql op 'emit-call-abi) :abicall)
							(:indirect))))
					((each (lambda (r) (. trashes_set :insert r))
						(get-modified-regs inst)))))
				insts)
			(list :resolved trashes_set call_sites))))

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
							(defq initial_trashes (Lset))
							(each (# (. initial_trashes :insert %0)) +no_regs)
							(. db :insert f (list :external initial_trashes)))
						((eql type :resolved)
							(bind '(trashes_set call_sites) payload)
							(. db :insert f (list :resolved trashes_set call_sites))
							(each (lambda (target)
									(if (str? target) (merge worklist (list target))))
								call_sites)))))))
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