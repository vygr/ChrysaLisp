(include "././array/class.inc")
(include "./struct.inc")

(def-class :list :array
	(dec-method :vtable class/list/vtable)
	(dec-method :type class/list/type :override :nil :nil (:r1-:r5 :f0-:f15) (:r1-:r5 :f0-:f15))
	(dec-method :create class/list/create :static :nil :nil (:r0-:r2 :f0-:f15))
	(dec-method :copy class/list/copy :static (:r0) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :min_length class/list/min_length :static (:r0) (:r0 :r1) (:r1-:r5 :r14))
	(dec-method :ref_all class/list/ref_all :static (:r0) (:r0) (:r1-:r4))
	(dec-method :collect class/list/collect :static (:r0 :r1 :r2) (:r0) (:r1-:r6 :r8-:r11 :r14 :f0-:f15))
	(dec-method :vcreate class/list/create :override :nil :nil (:r0-:r2 :f0-:f15) (:r0-:r2 :f0-:f15))
	(dec-method :deinit class/list/deinit :override :nil :nil (:r1-:r4 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :ref_elem class/list/ref_elem :final :nil :nil (:r1-:r2) (:r1-:r2))
	(dec-method :cat class/list/cat :final :nil :nil (:r0-:r9 :r14 :f0-:f15) (:r0-:r9 :r14 :f0-:f15))
	(dec-method :slice class/list/slice :final :nil :nil (:r0-:r8 :r14 :f0-:f15) (:r0-:r8 :r14 :f0-:f15))
	(dec-method :clear class/list/clear :final :nil :nil (:r1-:r2 :f0-:f15) (:r1-:r2 :f0-:f15))
	(dec-method :ref_back class/list/ref_back :final :nil :nil (:r1-:r2) (:r1-:r2))
	(dec-method :set_elem class/list/set_elem :final :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :append class/list/append :final :nil :nil (:r1-:r9 :f0-:f15) (:r1-:r9 :f0-:f15))
	(dec-method :print class/list/print :final :nil :nil (:r1-:r9 :r14 :f0-:f15) (:r1-:r9 :r14 :f0-:f15))
	(dec-method :find class/list/find :final :nil :nil (:r2-:r9) (:r2-:r9))
	(dec-method :rfind class/list/rfind :final :nil :nil (:r2-:r9) (:r2-:r9))
	(dec-method :erase class/list/erase :final :nil :nil (:r2-:r3 :f0-:f15) (:r2-:r3 :f0-:f15))
	(dec-method :erase2 class/list/erase2 :final :nil :nil (:r2-:r3 :f0-:f15) (:r2-:r3 :f0-:f15))
	(dec-method :splice class/list/splice :final :nil :nil (:r0-:r11 :r14 :f0-:f15) (:r0-:r11 :r14 :f0-:f15))
	(dec-method :lisp_list class/list/lisp_list :static (:r0 :r1) (:r0 :r1) (:r2))
	(dec-method :lisp_merge class/list/lisp_merge :static (:r0 :r1) (:r0 :r1) (:r1-:r12 :f0-:f15))
	(dec-method :lisp_pivot class/list/lisp_pivot :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_match class/list/lisp_match :static (:r0 :r1) (:r0 :r1) (:r1-:r6))
	(dec-method :lisp_copy class/list/lisp_copy :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15)))

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
		((:num :fixed :real) (list %0 +num_value))
		(:str (list '& %0 +str_data))
		(:char (list %0 +str_data 'ub))
		(:t %0))) src types) (ifn dst src)))

(defun list-bind-args (lst src types &optional dst)
	;inputs
	;list object
	; (reg ...)
	;'({:num :str :char & &&} ...)
	;outputs
	;load and cast register list from list
	;&& ignore out element
	;& skip src argument
	;filter skipped args
	(setd dst src)
	(bind '(types index)
		(reduce! (# (unless (eql %1 '&)
			(push (first %0) %1)
			(push (second %0) (* (!) +ptr_size)))
			%0) (list types) (lists 2)))
	;test for mismatch
	(unless (apply (const =) (map (const length) (list src dst types)))
		(throw "Mismatched bind specs !" (list src dst types)))
	;filter ignored elements
	(defq index_offset 0)
	(bind '(types src dst index)
		(reduce! (# (if (eql %1 '&&)
			(++ index_offset +ptr_size)
			(push (first %0) %1)
			(push (second %0) %2)
			(push (third %0) %3)
			(push (last %0) (- %4 index_offset)))
			%0) (list types src dst index) (lists 4)))
	;load from args list
	(defq i (last src))
	(assign `((,lst +array_begin)) `(,i))
	(assign (map (# (list i %0)) index) src)
	;cast to destination
	(list-cast-args src types dst))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun class/list/cat ()
	;inputs
	;:r0 = list object (ptr)
	;:r1 = list of list objects (ptr)
	;outputs
	;:r0 = new list object (ptr)
	;trashes
	;:r0-:r14, :f0-:f15
	(s-call :list :cat '(:r0 :r1) '(:r0))
	(call :list :ref_all '(:r0) '(:r0)))

(defun class/list/splice ()
	;inputs
	;:r0 = src1 list object (ptr)
	;:r1 = src2 list object (ptr)
	;:r2 = nums object (ptr)
	;outputs
	;:r0 = splice list object (ptr)
	;trashes
	;:r0-:r14, :f0-:f15
	(s-call :list :splice '(:r0 :r1 :r2) '(:r0))
	(call :list :ref_all '(:r0) '(:r0)))

(defun class/list/slice ()
	;inputs
	;:r0 = list object (ptr)
	;:r1 = element start index (uint)
	;:r2 = element end index (uint)
	;outputs
	;:r0 = slice list object (ptr)
	;trashes
	;:r0-:r14, :f0-:f15
	(s-call :list :slice '(:r0 :r1 :r2) '(:r0))
	(call :list :ref_all '(:r0) '(:r0)))
