(include "class/array/class.inc")

(def-class 'list 'array)
(dec-method :vtable 'class/list/vtable)
(dec-method :create 'class/list/create)

(dec-method :vcreate 'class/list/create :final)
(dec-method :deinit 'class/list/deinit :final)
(dec-method :ref_element 'class/list/ref_element :final)
(dec-method :slice 'class/list/slice :final)
(dec-method :clear 'class/list/clear :final)
(dec-method :ref_back 'class/list/ref_back :final)
(dec-method :set_element 'class/list/set_element :final)
(dec-method :append 'class/list/append :final)
(dec-method :print 'class/list/print :final)

(dec-method :lisp_list 'class/list/lisp_list :static '(r0 r1) '(r0 r1))
(dec-method :lisp_elemset 'class/list/lisp_elemset :static '(r0 r1) '(r0 r1))
(dec-method :lisp_merge 'class/list/lisp_merge :static '(r0 r1) '(r0 r1))
(dec-method :lisp_part 'class/list/lisp_part :static '(r0 r1) '(r0 r1))
(dec-method :lisp_match 'class/list/lisp_match :static '(r0 r1) '(r0 r1))

(def-struct 'list 'array)
(def-struct-end)

;;;;;;;;;;;;;;;;;;;
; paramater helpers
;;;;;;;;;;;;;;;;;;;

(defcfun-bind list-cast-args (regs types)
	;inputs
	;(reg ...)
	;'({num str char _} ...)
	;outputs
	;cast register list
	(assign-asm-to-asm (map (lambda (r _)
		(case _
			((num fixed real) (list r num_value))
			(str (list '& r str_data))
			(char (list r str_data 'ub))
			(t r))) regs types) regs))

(defcfun-bind list-bind-args (a regs types)
	;inputs
	;list object
	;(reg ...)
	;'({num str char _} ...)
	;outputs
	;load and cast register list from list
	(defq rl (list) tp (list))
	(each! 0 -1 (lambda (x y)
		(when (not (eql '_ y))
			(push rl x)
			(push tp y))) (list regs types))
	(array-bind-args a rl)
	(list-cast-args rl tp))
