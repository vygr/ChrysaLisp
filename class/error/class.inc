(include "././obj/class.inc")

(def-class error obj
	(dec-method :vtable class/error/vtable)
	(dec-method :create class/error/create :static (:r0 :r1 :r2 :r3 :r4 :r5) (:r0))
	(dec-method :init class/error/init :static (:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7))
	(dec-method :get_description class/error/get_description :static (:r0) (:r0 :r1))
	(dec-method :get_msg class/error/get_msg :static (:r0) (:r0 :r1))
	(dec-method :get_object class/error/get_object :static (:r0) (:r0 :r1))
	(dec-method :get_file class/error/get_file :static (:r0) (:r0 :r1))
	(dec-method :get_line class/error/get_line :static (:r0) (:r0 :r1))
	(dec-method :get_frame class/error/get_frame :static (:r0) (:r0 :r1))
	(dec-method :print class/error/print :final)
	(dec-method :deinit class/error/deinit :final))

(def-struct error obj_size
	(ptr description object file frame)
	(uint msg line))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun class/error/get_description ()
	;inputs
	;:r0 = error object (ptr)
	;outputs
	;:r0 = error object (ptr)
	;:r1 = str object (ptr)
	;trashes
	;:r1
	(assign '((:r0 error_description)) '(:r1)))

(defun class/error/get_file ()
	;inputs
	;:r0 = error object (ptr)
	;outputs
	;:r0 = error object (ptr)
	;:r1 = str object (ptr)
	;trashes
	;:r1
	(assign '((:r0 error_file)) '(:r1)))

(defun class/error/get_object ()
	;inputs
	;:r0 = error object (ptr)
	;outputs
	;:r0 = error object (ptr)
	;:r1 = error payload object (ptr)
	;trashes
	;:r1
	(assign '((:r0 error_object)) '(:r1)))

(defun class/error/get_line ()
	;inputs
	;:r0 = error object (ptr)
	;outputs
	;:r0 = error object (ptr)
	;:r1 = line number (uint)
	;trashes
	;:r1
	(assign '((:r0 error_line)) '(:r1)))

(defun class/error/get_frame ()
	;inputs
	;:r0 = error object (ptr)
	;outputs
	;:r0 = error object (ptr)
	;:r1 = error payload object (ptr)
	;trashes
	;:r1
	(assign '((:r0 error_frame)) '(:r1)))

;;;;;;;;;;;;
; error msgs
;;;;;;;;;;;;

(defcvar '*error_msgs* (list))
(defun deferror (_)
	(defcvar (sym (cat "+error_msg_" _)) (inc (length *error_msgs*)))
	(push *error_msgs* (cat _ " !")))

(deferror "not_a_canvas")
(deferror "not_a_class")
(deferror "not_a_filename")
(deferror "not_a_function")
(deferror "not_a_list")
(deferror "not_a_number")
(deferror "not_a_pipe")
(deferror "not_a_sequence")
(deferror "not_a_stream")
(deferror "not_a_string")
(deferror "not_a_symbol")
(deferror "not_all_lists")
(deferror "not_all_nums")
(deferror "not_all_strings")
(deferror "not_an_environment")
(deferror "not_valid_index")
(deferror "open_error")
(deferror "symbol_not_bound")
(deferror "wrong_num_of_args")
(deferror "wrong_types")
(deferror "rebind_constant")
