(include 'class/array/class.inc)

(def-class 'vector 'array)
(dec-method 'vtable 'class/vector/vtable)
(dec-method 'create 'class/vector/create)

(dec-method 'deinit 'class/vector/deinit 'final)
(dec-method 'ref_element 'class/vector/ref_element 'final)
(dec-method 'slice 'class/vector/slice 'final)
(dec-method 'cat 'class/vector/cat 'final)
(dec-method 'clear 'class/vector/clear 'final)
(dec-method 'ref_back 'class/vector/ref_back 'final)
(dec-method 'set_element 'class/vector/set_element 'final)
(dec-method 'append 'class/vector/append 'final)

(dec-method 'lisp_list 'class/vector/lisp_list 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_elemset 'class/vector/lisp_elemset 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_merge 'class/vector/lisp_merge 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_part 'class/vector/lisp_part 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_match 'class/vector/lisp_match 'static '(r0 r1) '(r0 r1))

(def-struct 'vector 'array)
(def-struct-end)

;;;;;;;;;;;;;;;;;;
; paramater macros
;;;;;;;;;;;;;;;;;;

(defcfun-bind class/vector/cast_args (regs types)
	;inputs
	;(reg ...)
	;'({num str char _} ...)
	;outputs
	;cast register list
	(assign-asm-to-asm (map (lambda (r _)
		(case _
			(num (list r num_value))
			(str (list '& r str_data))
			(char (list r str_data 'ub))
			(t r))) regs types) regs))

(defcfun-bind class/vector/bind_args (a regs types)
	;inputs
	;vector object
	;(reg ...)
	;'({num str char _} ...)
	;outputs
	;load and cast register list from vector
	(defq rl (list) tp (list))
	(each! 0 -1 (lambda (x y)
		(when (not (eql '_ y))
			(push rl x)
			(push tp y))) (list regs types))
	(class/array/bind_args a rl)
	(class/vector/cast_args rl tp))
