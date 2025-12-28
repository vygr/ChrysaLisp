(include "././array/class.inc")

(def-class :list :array
	(dec-method :vtable class/list/vtable)
	(dec-method :create class/list/create)
	(dec-method :copy class/list/copy :static (:r0) (:r0 :r1))
	(dec-method :min_length class/list/min_length :static (:r0) (:r0 :r1))
	(dec-method :collect class/list/collect :static (:r0 :r1 :r2) (:r0))
	(dec-method :vcreate class/list/create :final)
	(dec-method :deinit class/list/deinit :final)
	(dec-method :ref_elem class/list/ref_elem :final)
	(dec-method :slice class/list/slice :final)
	(dec-method :rslice class/list/rslice :final)
	(dec-method :clear class/list/clear :final)
	(dec-method :ref_back class/list/ref_back :final)
	(dec-method :set_elem class/list/set_elem :final)
	(dec-method :append class/list/append :final)
	(dec-method :print class/list/print :final)
	(dec-method :find class/list/find :final)
	(dec-method :rfind class/list/rfind :final)
	(dec-method :erase class/list/erase :final)
	(dec-method :erase2 class/list/erase2 :final)
	(dec-method :lisp_list class/list/lisp_list :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_merge class/list/lisp_merge :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pivot class/list/lisp_pivot :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_match class/list/lisp_match :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_copy class/list/lisp_copy :static (:r0 :r1) (:r0 :r1)))

(def-struct list array_size)

;;;;;;;;;;;;;;;;;;;
; parameter helpers
;;;;;;;;;;;;;;;;;;;

(defun list-cast-args (src types &optional dst)
	;inputs
	;(reg ...)
	;'({:num :str :char _} ...)
	;outputs
	;cast register list
	(assign (map (# (case %1
		((:num :fixed :real) (list %0 num_value))
		(:str (list '& %0 str_data))
		(:char (list %0 str_data 'ub))
		(:t %0))) src types) (ifn dst src)))

(defun list-bind-args (l src types &optional dst)
	;inputs
	;list object
	; (reg ...)
	;'({:num :str :char _} ...)
	;outputs
	;load and cast register list from list
	(bind '(src types dst)
		(reduce! (# (when (nql %2 '_)
			(push (first %0) %1)
			(push (second %0) %2)
			(push (third %0) %3))
			%0) (list src types (ifn dst src)) (lists 3)))
	(array-bind-args l src)
	(list-cast-args src types dst))
