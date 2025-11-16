(import "sys/wasm/class.vp")
(import "class/obj/class.inc")
(import "class/str/class.inc")
(import "class/nums/class.inc")

;;;;;;;;;;;;;;;;
; Wasm class
;;;;;;;;;;;;;;;;

(defclass-bind Wasm ()
	; Wasm module instance
	; Provides high-level interface to WebAssembly modules

	(defmethod-bind :create (_filepath)
		; Create and load a WASM module
		; inputs
		;   _filepath: path to .wasm file (str)
		; outputs
		;   instance: Wasm object or nil on error

		(defq this (alloc-obj +obj_size))
		(def-method this :vtable 'class/wasm)
		(array this
			_filepath  ; filepath string
			0          ; instance_id
			0)         ; cache for exports

		; Convert filepath to C string
		(defq cstr (str-alloc (length _filepath)))
		(str-copy _filepath 0 cstr 0 (length _filepath))

		; Allocate space for instance_id
		(defq id_ptr (nums-new 1))

		; Load WASM module
		(defq result (host-wasm-load cstr id_ptr))

		(if (= result 0)
			; Load failed
			(progn
				(elem-set this 0 nil)
				nil)
			; Success - store instance_id
			(progn
				(elem-set this 1 (nums-get id_ptr 0))
				(elem-set this 2 (array))  ; export cache
				this)))

	(defmethod-bind :call (this export_name &rest args)
		; Call a WASM export function
		; inputs
		;   export_name: name of exported function (str)
		;   args: variadic arguments (will be converted to i64)
		; outputs
		;   result: return value from WASM function (long)

		(defq instance_id (elem-get this 1))
		(if (= instance_id 0)
			(throw "WASM instance not loaded" this))

		; Convert export name to C string
		(defq cstr (str-alloc (length export_name)))
		(str-copy export_name 0 cstr 0 (length export_name))

		; Get export function pointer (or use cached)
		(defq cache (elem-get this 2))
		(defq func_ptr (get cache export_name))
		(if (not func_ptr)
			(progn
				(setq func_ptr (host-wasm-get_export instance_id cstr))
				(if (= func_ptr 0)
					(throw "Export not found" export_name))
				; Cache the function pointer
				(set cache export_name func_ptr)))

		; Convert args to nums array
		(defq arg_array (nums-new (length args)))
		(each! 0 args
			(# (nums-set arg_array %0 %1)))

		; Call the function
		(host-wasm-call_i64 func_ptr arg_array (length args)))

	(defmethod-bind :get_memory (this &optional offset size)
		; Read from WASM linear memory
		; inputs
		;   offset: byte offset (default 0)
		;   size: bytes to read (default all remaining)
		; outputs
		;   data: str containing memory contents

		(setd offset 0 size 0)
		(defq instance_id (elem-get this 1))
		(if (= instance_id 0)
			(throw "WASM instance not loaded" this))

		; Get memory pointer and size
		(defq size_ptr (nums-new 1))
		(defq mem_ptr (host-wasm-get_memory instance_id size_ptr))
		(defq mem_size (nums-get size_ptr 0))

		(if (= mem_ptr 0)
			(throw "Failed to get WASM memory" this))

		; Calculate actual size to read
		(if (or (= size 0) (> (+ offset size) mem_size))
			(setq size (- mem_size offset)))

		(if (< size 0)
			(setq size 0))

		; Allocate buffer and read
		(defq buffer (str-alloc size))
		(host-wasm-read_memory instance_id offset buffer size)
		buffer)

	(defmethod-bind :write_memory (this offset data)
		; Write to WASM linear memory
		; inputs
		;   offset: byte offset
		;   data: str to write

		(defq instance_id (elem-get this 1))
		(if (= instance_id 0)
			(throw "WASM instance not loaded" this))

		(host-wasm-write_memory instance_id offset data (length data)))

	(defmethod-bind :get_error (this)
		; Get last error message
		; outputs
		;   error: error string

		(defq instance_id (elem-get this 1))
		(host-wasm-get_error instance_id))

	(defmethod-bind :close (this)
		; Unload WASM module and free resources

		(defq instance_id (elem-get this 1))
		(when (!= instance_id 0)
			(host-wasm-unload instance_id)
			(elem-set this 1 0))

		; Free filepath string
		(defq filepath (elem-get this 0))
		(when filepath
			(elem-set this 0 nil))))

; Constructor function
(defun Wasm (filepath)
	(. Wasm :create filepath))
