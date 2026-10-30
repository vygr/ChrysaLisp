;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; compute on the GPU, the host's GPU table, src/host/gpu_sdl3.cpp
;
; Buffers on the GPU, kernels that read and write them, and what they
; hold read back. Work goes in order on the one queue of the GPU: a write,
; a dispatch and a read of what it wrote need no wait between them. A
; dispatch and a read give a ticket, waited for by (gpu-wait), which looks
; each half a millisecond and gives way to the node's other tasks between.
; A node with no GPU, its host on no SDL3, has a (gpu-format) of 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(ffi "sys/gpu/lisp_call" gpu-call)

(enums +gpu 0
	(enum format buffer_create buffer_destroy buffer_write buffer_read done take
		kernel_create kernel_destroy dispatch))

(defun gpu-format ()
	; (gpu-format) -> 0 | 1 | 2
	;the language a kernel is given in, 1 MSL, 2 SPIR-V, 0 if no GPU
	(max 0 (gpu-call +gpu_format 0 0 0 0 0 0 0)))

(defun gpu-buffer (size)
	; (gpu-buffer size) -> :nil | buffer
	;a buffer of size bytes on the GPU
	(if (> (defq b (gpu-call +gpu_buffer_create size 0 0 0 0 0 0)) 0) b))

(defun gpu-buffer-free (b)
	; (gpu-buffer-free buffer) -> :nil
	(gpu-call +gpu_buffer_destroy b 0 0 0 0 0 0) :nil)

(defun gpu-write (b offset data)
	; (gpu-write buffer offset str) -> :t | :nil
	;the bytes of data into the buffer at offset, in order after what was
	;given before, not waited for
	(= (gpu-call +gpu_buffer_write b offset data (length data) 0 0 0) 1))

(defun gpu-wait (ticket)
	; (gpu-wait ticket) -> ticket
	;once the work of the ticket is over, giving way to other tasks till then
	(until (= (gpu-call +gpu_done ticket 0 0 0 0 0 0) 1) (task-sleep 500))
	ticket)

(defun gpu-read (b offset len)
	; (gpu-read buffer offset len) -> :nil | str
	;len bytes of the buffer at offset, once what was given before is done
	(when (> (defq ticket (gpu-call +gpu_buffer_read b offset len 0 0 0 0)) 0)
		(gpu-wait ticket)
		(defq data (str-alloc len))
		(gpu-call +gpu_take ticket data len 0 0 0 0)
		data))

(defun gpu-kernel (code layout)
	; (gpu-kernel code (reads writes uniforms tx ty tz)) -> :nil | kernel
	;a kernel of code, MSL text or a SPIR-V module, of the language
	;(gpu-format) says, its function kernel_main. It reads buffers, reads
	;and writes buffers, has a block of uniforms if uniforms is 1, and runs
	;in groups of tx ty tz threads. It is built on a thread of the host,
	;(gpu-dispatch) waits for that
	(defq l (str-alloc 24))
	(each (# (set-int l (* (!) 4) %0)) layout)
	(if (> (defq k (gpu-call +gpu_kernel_create code (length code) l 0 0 0 0)) 0) k))

(defun gpu-kernel-free (k)
	; (gpu-kernel-free kernel) -> :nil
	(gpu-call +gpu_kernel_destroy k 0 0 0 0 0 0) :nil)

(defun gpu-dispatch (k buffers uniforms groups)
	; (gpu-dispatch kernel (buffer ...) uniforms (x y z)) -> :nil | ticket
	;the kernel run over groups of its threads, the buffers it reads then
	;those it writes, uniforms a str of its block or :nil. Waits for the
	;kernel to be built, :nil if it did not build
	(defq bs (str-alloc (* 8 (max 1 (length buffers)))) gs (str-alloc 12))
	(each (# (set-long bs (* (!) 8) %0)) buffers)
	(each (# (set-int gs (* (!) 4) %0)) groups)
	(while (= 0 (defq ticket (gpu-call +gpu_dispatch k bs (ifn uniforms 0) (if uniforms (length uniforms) 0) gs 0 0)))
		(task-sleep 500))
	(if (> ticket 0) ticket))

(defun gpu-run (k buffers uniforms groups)
	; (gpu-run kernel (buffer ...) uniforms (x y z)) -> :t | :nil
	;(gpu-dispatch) and wait for it to be over
	(when (defq ticket (gpu-dispatch k buffers uniforms groups))
		(gpu-wait ticket)
		(gpu-call +gpu_take ticket 0 0 0 0 0 0)
		:t))

;module
(export-symbols '(gpu-call gpu-format gpu-buffer gpu-buffer-free gpu-write gpu-read gpu-wait
	gpu-kernel gpu-kernel-free gpu-dispatch gpu-run))
(env-pop)
