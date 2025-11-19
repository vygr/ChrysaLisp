(import "lib/class/class.inc")
(import "sys/wasm/class.vp")

;;;;;;;;;;;;;;;;
; Wasm class
;;;;;;;;;;;;;;;;

(defclass Wasm (filepath) :nil
	; Wasm module instance
	; Provides high-level interface to WebAssembly modules
	;
	; Fields:
	;   :filepath - path to .wasm file (str)
	;   :instance_id - WASM runtime instance ID (int)
	;   :export_cache - cached export function pointers (map)

	; Initialize fields
	(def this :filepath filepath)
	(def this :instance_id 0)
	(def this :export_cache (map))

	; Convert filepath to C string
	(defq cstr (str-alloc (length filepath)))
	(str-copy filepath 0 cstr 0 (length filepath))

	; Allocate space for instance_id
	(defq id_ptr (nums-new 1))

	; Load WASM module
	(defq result (host-wasm-load cstr id_ptr))

	(cond
		((eql result 0)
			; Load failed - mark as invalid
			(def this :filepath :nil))
		(:t
			; Success - store instance_id
			(def this :instance_id (nums-get id_ptr 0))))

	(defmethod :call (export_name . args)
		; Call a WASM export function
		; inputs
		;   export_name: name of exported function (str)
		;   args: variadic arguments (will be converted to i64)
		; outputs
		;   result: return value from WASM function (long)

		(defq instance_id (get :instance_id this))
		(cond
			((eql instance_id 0)
				(throw "WASM instance not loaded" this)))

		; Convert export name to C string
		(defq cstr (str-alloc (length export_name)))
		(str-copy export_name 0 cstr 0 (length export_name))

		; Get export function pointer (or use cached)
		(defq cache (get :export_cache this))
		(defq func_ptr (get cache export_name))
		(unless func_ptr
			(setq func_ptr (host-wasm-get_export instance_id cstr))
			(cond
				((eql func_ptr 0)
					(throw "Export not found" export_name)))
			; Cache the function pointer
			(set cache export_name func_ptr))

		; Convert args to nums array
		(defq arg_array (nums-new (length args)))
		(each! 0 args
			(# (nums-set arg_array %0 %1)))

		; Call the function
		(host-wasm-call_i64 func_ptr arg_array (length args)))

	(defmethod :get_memory (&optional offset size)
		; Read from WASM linear memory
		; inputs
		;   offset: byte offset (default 0)
		;   size: bytes to read (default all remaining)
		; outputs
		;   data: str containing memory contents

		(setd offset 0 size 0)
		(defq instance_id (get :instance_id this))
		(cond
			((eql instance_id 0)
				(throw "WASM instance not loaded" this)))

		; Get memory pointer and size
		(defq size_ptr (nums-new 1))
		(defq mem_ptr (host-wasm-get_memory instance_id size_ptr))
		(defq mem_size (nums-get size_ptr 0))

		(cond
			((eql mem_ptr 0)
				(throw "Failed to get WASM memory" this)))

		; Calculate actual size to read
		(cond
			((or (eql size 0) (> (+ offset size) mem_size))
				(setq size (- mem_size offset))))

		(cond
			((< size 0)
				(setq size 0)))

		; Allocate buffer and read
		(defq buffer (str-alloc size))
		(host-wasm-read_memory instance_id offset buffer size)
		buffer)

	(defmethod :write_memory (offset data)
		; Write to WASM linear memory
		; inputs
		;   offset: byte offset
		;   data: str to write

		(defq instance_id (get :instance_id this))
		(cond
			((eql instance_id 0)
				(throw "WASM instance not loaded" this)))

		(host-wasm-write_memory instance_id offset data (length data)))

	(defmethod :get_error ()
		; Get last error message
		; outputs
		;   error: error string

		(defq instance_id (get :instance_id this))
		(host-wasm-get_error instance_id))

	(defmethod :close ()
		; Unload WASM module and free resources

		(defq instance_id (get :instance_id this))
		(when (!= instance_id 0)
			(host-wasm-unload instance_id)
			(def this :instance_id 0))

		; Clear filepath
		(def this :filepath :nil)))
