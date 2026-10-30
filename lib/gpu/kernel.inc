;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, kernels on the GPU.
; A kernel made for the GPU the node has, MSL or SPIR-V as
; (gpu-format) says, and run over a range of indices. The CPU runs the
; same kernel with (shader-vp-kernel), lib/gpu/vp.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./msl.inc")
(import "./spirv.inc")
(import "./compute.inc")

(defun shader-gpu-kernel (program)
	; (shader-gpu-kernel program) -> :nil | kernel
	;the kernel for this node's GPU, :nil if it has none. It is built on
	;a thread of the host, (shader-gpu-run) waits for that
	(defq bufs (shader-buffers program) reads (length (filter (# (eql (third %0) :read)) bufs))
		layout (list reads (- (length bufs) reads) 1 +sh_kernel_threads 1 1))
	(case (gpu-format)
		(1 (gpu-kernel (shader-msl-kernel program) layout))
		(2 (gpu-kernel (shader-spirv-kernel program) layout))))

(defun shader-gpu-run (program kernel block from to bufs)
	; (shader-gpu-run program kernel block from to (buffer ...)) -> :t | :nil
	;the kernel run for each index from to to, and waited for, the other
	;tasks of the node have their turns meanwhile. block is the inputs, as
	;(shader-pack) makes them, and bufs a GPU buffer for each buffer, in
	;the order the program has them. Nothing is bounds checked, so each
	;must be long enough, 4 bytes to an element
	(cond
		((>= from to))
		(:t (defq names (map (const first) (shader-buffers program)))
			(gpu-run kernel
				(map (lambda ((name &rest _)) (elem-get bufs (find name names))) (sh-kernel-order program))
				(shader-range program block from to)
				(list (/ (+ (- to from) (dec +sh_kernel_threads)) +sh_kernel_threads) 1 1)))))
