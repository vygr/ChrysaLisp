(include "././obj/class.inc")
(include "././error/class.inc")
(include "././list/class.inc")
(include "./struct.inc")
(include "sys/task/class.inc")
(include "lib/consts/chars.inc")

(def-class :lisp :obj
	(dec-method :vtable class/lisp/vtable)
	(dec-method :type class/lisp/type :override)
	(dec-method :deinit class/lisp/deinit :final)
	(dec-method :create class/lisp/create :static (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :init class/lisp/init :static (:r0 :r1 :r2 :r3 :r4 :r5))
	(dec-method :env_push class/lisp/env_push :static (:r0) (:r0 :r1))
	(dec-method :env_pop class/lisp/env_pop :static (:r0) (:r0 :r1))
	(dec-method :env_bind class/lisp/env_bind :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :env_args_sig class/lisp/env_args_sig :static (:r1 :r2 :r3 :r4) (:r1 :r2))
	(dec-method :env_args_type class/lisp/env_args_type :static (:r1 :r2 :r3) (:r1 :r2))
	(dec-method :env_args_match class/lisp/env_args_match :static (:r1 :r2 :r3) (:r1 :r2))
	(dec-method :read class/lisp/read :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :read_char class/lisp/read_char :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :read_quasi class/lisp/read_quasi :static (:r0 :r1 :r2 :r3) (:r0 :r1 :r2))
	(dec-method :read_sym class/lisp/read_sym :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :read_str class/lisp/read_str :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :read_num class/lisp/read_num :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :repl_eval class/lisp/repl_eval :static (:r0 :r1) (:r0 :r1))
	(dec-method :repl_eval_list class/lisp/repl_eval_list :static (:r0 :r1) (:r0 :r1))
	(dec-method :repl_apply class/lisp/repl_apply :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :repl_print class/lisp/repl_print :static (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :repl_expand class/lisp/repl_expand :static (:r0 :r1) (:r0))
	(dec-method :repl_error class/lisp/repl_error :static (:r1 :r2 :r3) (:r0 :r1))
	(dec-method :repl_bind class/lisp/repl_bind :static (:r0 :r1) (:r0))
	(dec-method :repl_progn class/lisp/repl_progn :static (:r0 :r1 :r2 :r3) (:r0 :r1))
	(dec-method :lisp_ffi class/lisp/lisp_ffi :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_prebind class/lisp/lisp_prebind :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_macroexpand class/lisp/lisp_macroexpand :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_lambda class/lisp/lisp_lambda :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_macro class/lisp/lisp_macro :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_quote class/lisp/lisp_quote :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_qquote class/lisp/lisp_qquote :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_if class/lisp/lisp_if :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_ifn class/lisp/lisp_ifn :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_cond class/lisp/lisp_cond :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_condn class/lisp/lisp_condn :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_progn class/lisp/lisp_progn :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_while class/lisp/lisp_while :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_until class/lisp/lisp_until :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_print class/lisp/lisp_print :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_prin class/lisp/lisp_prin :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_copy class/lisp/lisp_copy :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_apply class/lisp/lisp_apply :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_repl class/lisp/lisp_repl :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_eval class/lisp/lisp_eval :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_eval_list class/lisp/lisp_eval_list :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_throw class/lisp/lisp_throw :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_catch class/lisp/lisp_catch :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_bind class/lisp/lisp_bind :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_read class/lisp/lisp_read :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mcall class/lisp/lisp_mcall :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_env_push class/lisp/lisp_env_push :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_env_pop class/lisp/lisp_env_pop :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_identity class/lisp/lisp_identity :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_repl_info class/lisp/lisp_repl_info :static (:r0 :r1) (:r0 :r1))
	(dec-method :run class/lisp/run))

(defun lisp-this (r)
	;emit the load of the task's lisp object, from its tcb, to a register.
	;The lisp object is the first input of most methods here, and by
	;contract is given back as the first output. A method that calls a lot
	;need not keep it on the stack to do that, it can fetch it again.
	(fn-bind 'sys/statics/statics r)
	(assign `((,r statics_sys_task_current_tcb)) `(,r))
	(assign `((,r +tk_node_lisp)) `(,r)))

(defun lisp-bind-fresh (steal bail not_fresh)
	;inputs
	;:r0 = lisp object (ptr)
	;:r1 = vars list object (ptr)
	;:r2 = vals list object (ptr)
	;steal = :t to take the values from the vals list, and leave it empty
	;bail = label of the code (lisp-bind-fresh-bail) emits
	;not_fresh = label to jump to if this is not the case for it
	;outputs
	;:r0 = lisp object (ptr)
	;:r1 = return value object (ptr)
	;:r12 = vars list object (ptr)
	;:r13 = vals list object (ptr)
	;trashes
	;:r1-:r13
	;info
	;emit the bind of the args of a lambda call. A list of plain symbols,
	;bound to a list of as many values, in an environment that is empty,
	;as :env_push gave it, a single bucket with the room. So no search, no
	;call, and no frame, each pair is put in place, and the symbol told its
	;slot. That slot also shows up a symbol given twice, the only way it
	;can point at itself here is if this loop put it there.
	;
	;That, a & symbol, a list to destructure, a count that differs, and
	;all else, jumps to not_fresh, with the inputs as they were, to do it
	;with the general code of :env_bind.
	;
	;If the caller owns the vals list, and is done with it, steal moves
	;the values, with no ref, and none to drop after.
	(vp-rdef (this vars vals env iter_end index sym val dst tmp tmp2 sym_vtable)
		'(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11))
	(assign `((,vars +obj_vtable) (,vals +obj_vtable) (@ ,(f-path :list :vtable)))
		`(,tmp ,tmp2 ,sym_vtable))
	(gotoif `(,tmp /= ,sym_vtable) not_fresh)
	(gotoif `(,tmp2 /= ,sym_vtable) not_fresh)
	(assign `((,vars +array_length) (,vals +array_length) (,this +lisp_environment))
		`(,iter_end ,tmp ,env))
	(gotoif `(,iter_end /= ,tmp) not_fresh)
	(vp-cpy-rr vars :r12)
	(vp-cpy-rr vals :r13)
	(vpif `(,iter_end = 0))
		;no vars, so no value bound
		(assign `((,this +lisp_sym_nil)) `(,val))
	(else)
		(assign `((,env +array_length) (,env +array_num_buckets) (,env +array_capacity))
			`(,tmp ,tmp2 ,index))
		(gotoif `(,tmp /= 0) not_fresh)
		(gotoif `(,tmp2 /= 1) not_fresh)
		(vp-shr-cr 1 index)
		(gotoif `(,iter_end > ,index) not_fresh)
		;from here on :r1 and :r2 are iterators
		(assign `((,vars +array_begin) (,vals +array_begin) (,env +array_begin) (@ ,(f-path :sym :vtable)))
			`(,vars ,vals ,dst ,sym_vtable))
		(vp-shl-cr (log2 +ptr_size) iter_end)
		(vp-xor-rr index index)
		(vp-add-rr vars iter_end)
		(loop-start)
			(assign `((,vars 0)) `(,sym))
			(assign `((,sym +obj_vtable) (,sym +str_data ub)) `(,tmp ,tmp2))
			(gotoif `(,tmp /= ,sym_vtable) bail)
			(gotoif `(,tmp2 = +char_ampersand) bail)
			(assign `((,sym +str_hashslot)) `(,tmp))
			(vpif `(,tmp < ,index))
				(assign `((,env +array_begin)) `(,tmp2))
				(vp-shl-cr (log2 (* +ptr_size 2)) tmp)
				(vp-cpy-dr tmp2 tmp tmp)
				(gotoif `(,tmp = ,sym) bail)
			(endif)
			(assign `((,vals 0) ,index) `(,val (,sym +str_hashslot)))
			(class/obj/ref sym tmp)
			(unless steal (class/obj/ref val tmp))
			(assign `(,sym ,val) `((,dst 0) (,dst ,+ptr_size)))
			(vp-add-cr 1 index)
			(vp-add-cr +ptr_size vars)
			(vp-add-cr +ptr_size vals)
			(vp-add-cr (* +ptr_size 2) dst)
		(loop-until `(,vars = ,iter_end))
		(vp-shl-cr 1 index)
		(assign `(,index) `((,env +array_length)))
		(when steal
			(vp-xor-rr tmp tmp)
			(assign `(,tmp) '((:r13 +array_length))))
	(endif)
	;the last value bound is what is given back
	(class/obj/ref val tmp)
	(vp-cpy-rr val vars))

(defun lisp-bind-fresh-bail (steal bail not_fresh)
	;emit the code (lisp-bind-fresh) jumps to if it has to give up part way,
	;out of its flow. It puts the inputs back, and goes on to not_fresh.
	(vp-rdef (this vars vals env iter_end index sym val dst tmp tmp2 sym_vtable)
		'(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11))
(vp-label bail)
	(cond
		(steal
			;nothing was bound, the values are still the list's, so take
			;back the ref each symbol was given
			(loop-while `(,index /= 0))
				(vp-sub-cr (* +ptr_size 2) dst)
				(vp-sub-cr 1 index)
				(assign `((,dst 0)) `(,sym))
				(class/obj/ref sym tmp -1)
			(loop-end))
		(:t	;keep what is bound so far, to bind one twice does no harm
			(vp-shl-cr 1 index)
			(assign `(,index) `((,env +array_length)))))
	(vp-cpy-rr :r12 vars)
	(vp-cpy-rr :r13 vals)
	(goto not_fresh))
