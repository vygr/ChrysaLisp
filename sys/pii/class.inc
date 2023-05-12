(include "lib/asm/class.inc")
(include "sys/pii/abi.inc")

(def-class host :nil
	(dec-method :exit :nil :virtual (abi-args 1))
	(dec-method :stat :nil :virtual (abi-args 2) (:r0))
	(dec-method :open :nil :virtual (abi-args 2) (:r0))
	(dec-method :close :nil :virtual (abi-args 1) (:r0))
	(dec-method :unlink :nil :virtual (abi-args 1) (:r0))
	(dec-method :read :nil :virtual (abi-args 3) (:r0))
	(dec-method :write :nil :virtual (abi-args 3) (:r0))
	(dec-method :mmap :nil :virtual (abi-args 3) (:r0))
	(dec-method :munmap :nil :virtual (abi-args 3) (:r0))
	(dec-method :mprotect :nil :virtual (abi-args 3) (:r0))
	(dec-method :gettime :nil :virtual () (:r0))
	(dec-method :open_shared :nil :virtual (abi-args 2) (:r0))
	(dec-method :close_shared :nil :virtual (abi-args 2) (:r0))
	(dec-method :clear_icache :nil :virtual (abi-args 2) (:r0))
	(dec-method :dirlist :nil :virtual (abi-args 3) (:r0))
	(dec-method :remove :nil :virtual (abi-args 1) (:r0))
	(dec-method :seek :nil :virtual (abi-args 3) (:r0))
	(dec-method :rand :nil :virtual (abi-args 2) (:r0))
	(dec-method :sleep :nil :virtual (abi-args 1) (:r0))
	(dec-method :usb_start :nil :virtual (abi-args 1) (:r0))
	(dec-method :usb_stop :nil :virtual (abi-args 1) (:r0))
	(dec-method :usb_running :nil :virtual (abi-args 1) (:r0)))

(def-class host_gui :nil
	(dec-method :init :nil :virtual (abi-args 1) (:r0))
	(dec-method :deinit :nil :virtual (abi-args 0) (:r0))
	(dec-method :box :nil :virtual (abi-args 1) (:r0))
	(dec-method :filled_box :nil :virtual (abi-args 1) (:r0))
	(dec-method :blit :nil :virtual (abi-args 3) (:r0))
	(dec-method :set_clip :nil :virtual (abi-args 1) (:r0))
	(dec-method :set_color :nil :virtual (abi-args 4) (:r0))
	(dec-method :set_texture_color :nil :virtual (abi-args 4) (:r0))
	(dec-method :destroy_texture :nil :virtual (abi-args 1) (:r0))
	(dec-method :create_texture :nil :virtual (abi-args 5) (:r0))
	(dec-method :begin_composite :nil :virtual (abi-args 0) (:r0))
	(dec-method :end_composite :nil :virtual (abi-args 0) (:r0))
	(dec-method :flush :nil :virtual (abi-args 1) (:r0))
	(dec-method :resize :nil :virtual (abi-args 2) (:r0))
	(dec-method :poll_event :nil :virtual (abi-args 1) (:r0)))

(defun abi-call-table (_1 _2 _5 &optional _3 _4 _6)
	;_1 = class name
	;_2 = member name
	;_5 = host function table
	;_3 = in parameters
	;_4 = out parameters
	;_6 = temp reg
	(setd _6 :r13)
	(defq m (method-lookup _1 _2) x (list) s (elem-get 2 m))
	(if (eql (elem-get 1 m) :static)
		(throw "Abi methods must be virtual !" (list _1 _2)))
	(if (or (find-rev _5 s) (find-rev _6 s))
		(throw "Dispatch/Temp register conflicts with arg !" (list _5 _6 s)))
	(assign _3 s)
	(when (> (defq ls (length s)) (defq la (length (abi-args))))
		(setq x (reverse (slice la ls s))))
	(vp-call-abi _6 _5 (- (elem-get 4 m) +ptr_size) (length s) x)
	(if _4 (assign (elem-get 3 m) _4)))

(defun host-os-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	;_6 = temp reg
	(setd _5 :r14 _6 :r13)
	(fn-bind 'sys/statics/statics _5)
	(vp-cpy-ir _5 statics_sys_load_host_os_funcs _5)
	(abi-call-table _1 _2 _5 _3 _4 _6))

(defun host-gui-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	;_6 = temp reg
	(setd _5 :r14 _6 :r13)
	(fn-bind 'sys/statics/statics _5)
	(vp-cpy-ir _5 statics_sys_load_host_gui_funcs _5)
	(abi-call-table _1 _2 _5 _3 _4 _6))

(def-class sys_pii :nil
	(dec-method :exit sys/pii/exit :static (:r0))
	(dec-method :mmap sys/pii/mmap :static (:r0 :r1 :r2) (:r0))
	(dec-method :munmap sys/pii/munmap :static (:r0 :r1 :r2) (:r0))
	(dec-method :mprotect sys/pii/mprotect :static (:r0 :r1 :r2) (:r0))
	(dec-method :open sys/pii/open :static (:r0 :r1) (:r0))
	(dec-method :close sys/pii/close :static (:r0) (:r0))
	(dec-method :open_shared sys/pii/open_shared :static (:r0 :r1) (:r0))
	(dec-method :close_shared sys/pii/close_shared :static (:r0 :r1) (:r0))
	(dec-method :unlink sys/pii/unlink :static (:r0) (:r0))
	(dec-method :stat sys/pii/stat :static (:r0 :r1) (:r0))
	(dec-method :write sys/pii/write :static (:r0 :r1 :r2) (:r0))
	(dec-method :write_char sys/pii/write_char :static (:r0 :r1) (:r0))
	(dec-method :write_str sys/pii/write_str :static (:r0 :r1) (:r0))
	(dec-method :write_num sys/pii/write_num :static (:r0 :r1 :r2))
	(dec-method :read sys/pii/read :static (:r0 :r1 :r2) (:r0))
	(dec-method :read_char sys/pii/read_char :static (:r0) (:r0))
	(dec-method :time sys/pii/time :static :nil (:r0))
	(dec-method :clear_icache sys/pii/clear_icache :static (:r0 :r1) (:r0))
	(dec-method :dirlist sys/pii/dirlist :static (:r0 :r1 :r2) (:r0))
	(dec-method :remove sys/pii/remove :static (:r0) (:r0))
	(dec-method :seek sys/pii/seek :static (:r0 :r1 :r2) (:r0))
	(dec-method :rand sys/pii/rand :static (:r0 :r1) (:r0))
	(dec-method :usb_start sys/pii/usb_start :static (:r0) (:r0))
	(dec-method :usb_stop sys/pii/usb_stop :static (:r0) (:r0))
	(dec-method :usb_running sys/pii/usb_running :static (:r0) (:r0))
	(dec-method :lisp_readchar sys/pii/lisp_readchar :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_writechar sys/pii/lisp_writechar :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_time sys/pii/lisp_time :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_age sys/pii/lisp_age :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_dirlist sys/pii/lisp_dirlist :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_fstat sys/pii/lisp_fstat :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_remove sys/pii/lisp_remove :static (:r0 :r1) (:r0 :r1)))

;debug helpers
(defun debug-num (s n)
	(when (> *debug_mode* 0)
		(def-vars
			(ptr _a _b _c))
		(push-scope)
		(assign '(:r0 :r1 :r2) {_a, _b, _c})
		(fn-string (cat s ": ") :r1)
		(call 'sys_pii :write_str '(2 :r1))
		(assign {_a, _b, _c} '(:r0 :r1 :r2))
		(assign n '(:r1))
		(call 'sys_pii :write_num '(2 :r1 16))
		(call 'sys_pii :write_char '(2 10))
		(assign {_a, _b, _c} '(:r0 :r1 :r2))
		(pop-scope)))

(defun debug-str (s)
	(when (> *debug_mode* 0)
		(def-vars
			(ptr _a _b))
		(push-scope)
		(assign '(:r0 :r1) {_a, _b})
		(assign s '(:r1))
		(call 'sys_pii :write_str '(2 :r1))
		(call 'sys_pii :write_char '(2 10))
		(assign {_a, _b} '(:r0 :r1))
		(pop-scope)))

(def-struct host_gui_rect 0
	(int x y w h))

(def-enum +file_open 0
	(enum read write append))

(def-enum +mmap 0
	(enum data exec shared none))

(def-struct stat 0
	(long mtime fsize)
	(ushort mode))

(defcvar 's_ifmt 0xf000)
(defcvar 's_ifdir 0x4000)
(defcvar 's_ifreg 0x8000)
