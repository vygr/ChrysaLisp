(report-header "GPU: shader language, kernels on the CPU, the VP and the GPU")

(import "lib/gpu/cpu.inc")
(import "lib/gpu/vp.inc")
(import "lib/gpu/kernel.inc")

(defun kt-src (&rest lines)
	(shader-compile (shader-read (string-stream (join lines "\n")))))

(defun kt-refused (text)
	(catch (progn (kt-src text) :nil) :t))

;a kernel is a file with buffers, its main takes an index and gives nothing
(assert-true "a kernel writes no buffer it reads"
	(kt-refused "(defbuffer b :float :read) (defun main :void ((i :int)) (elem-set b i 1.0))"))
(assert-true "its main gives nothing"
	(kt-refused "(defbuffer b :float :write) (defun main :float ((i :int)) 1.0)"))
(assert-true "a main that gives nothing has buffers"
	(kt-refused "(defun main :void ((i :int)) (defq x 1))"))
(assert-true "an element is of the buffer's type"
	(kt-refused "(defbuffer b :float :write) (defun main :void ((i :int)) (elem-set b i 1))"))
(assert-true "a buffer is not a value"
	(kt-refused "(defbuffer b :float :read) (defbuffer c :float :write) (defun main :void ((i :int)) (defq x b))"))
(assert-true "a shift takes two"
	(kt-refused "(defbuffer b :int :write) (defun main :void ((i :int)) (elem-set b i (<< i 1 2)))"))

;the buffers are given in an order that is not reads then writes, a GPU
;binds them that way round, and it is sorted out
(defq kt_prog (kt-src
	"(defbuffer dst :float :write) (defbuffer src :float :read)"
	"(defbuffer packed :int :write) (defbuffer cols :int :read)"
	"(definput scale :float 2.0) (definput n :int 8)"
	"(defun sq :float ((x :float)) (* x x))"
	"(defun main :void ((i :int))"
	"	(when (>= i n) (return))"
	"	(elem-set dst i (+ (* (elem-get src i) scale) (sq (float (logxor i 3)))))"
	"	(defq c (elem-get cols i))"
	"	(elem-set packed i (logior (<< (logand (>> c 8) 255) 16) (logand c 255) (- c))))"))
(assert-eq "it is a kernel" :kernel (shader-stage kt_prog))

(defq kt_n 300 kt_from 5 kt_to 290 kt_vals '((scale 1.5) (n 280))
	kt_src (str-alloc (* kt_n 4)) kt_cols (str-alloc (* kt_n 4)))
(each (# (set-int kt_src (* %0 4) (sh-real-to-float (n2r (- %0 3))))
	(set-int kt_cols (* %0 4) (- (* %0 0x010307) 0x5000))) (range 0 kt_n))
(defun kt-zeros ()
	(defq s (str-alloc (* kt_n 4)))
	(each (# (set-long s %0 0)) (range 0 (* kt_n 4) 8))
	s)

;the CPU back end is the reference
(defq kt_cdst (kt-zeros) kt_cpk (kt-zeros))
((eval (shader-cpu-kernel kt_prog)) kt_from kt_to kt_cdst kt_src kt_cpk kt_cols (n2r 1.5) 280)
(assert-eq "the CPU writes the range" (sh-real-to-float (n2r 39)) (get-int kt_cdst (* 5 4)))
(assert-eq "and not before it" 0 (get-int kt_cdst (* 4 4)))
(assert-eq "nor at n and after" 0 (get-int kt_cdst (* 280 4)))
(defq kt_c (- (* 5 0x010307) 0x5000))
(assert-eq "an int buffer" (logior (<< (logand (>> kt_c 8) 255) 16) (logand kt_c 255) (neg kt_c))
	(get-int kt_cpk (* 5 4)))

;the VP back end, the same bytes
(defq kt_native (shader-vp-kernel kt_prog) kt_vdst (kt-zeros) kt_vpk (kt-zeros))
(shader-vp-run kt_prog kt_native (shader-vp-frame kt_prog kt_native kt_vals)
	kt_from kt_to (list kt_vdst kt_src kt_vpk kt_cols))
(assert-eq "the VP writes the same floats" kt_cdst kt_vdst)
(assert-eq "and the same ints" kt_cpk kt_vpk)

;the GPU, MSL or SPIR-V, the same bytes
(cond
	((= (gpu-format) 0)
		(test-skip "a kernel on the GPU" "this node has no GPU"))
	(:t
		(defq kt_kernel (shader-gpu-kernel kt_prog)
			kt_bufs (map (lambda (_) (gpu-buffer (* kt_n 4))) (range 0 4)))
		(assert-true "a kernel for this GPU" (num? kt_kernel))
		(each (# (gpu-write %0 0 %1)) kt_bufs (list (kt-zeros) kt_src (kt-zeros) kt_cols))
		(assert-true "it runs" (shader-gpu-run kt_prog kt_kernel (shader-pack kt_prog kt_vals) kt_from kt_to kt_bufs))
		(assert-eq "the GPU writes the same floats" kt_cdst (gpu-read (first kt_bufs) 0 (* kt_n 4)))
		(assert-eq "and the same ints" kt_cpk (gpu-read (third kt_bufs) 0 (* kt_n 4)))
		(each (const gpu-buffer-free) kt_bufs)
		(gpu-kernel-free kt_kernel)))

(undef (env) 'kt-src 'kt-refused 'kt-zeros 'kt_prog 'kt_n 'kt_from 'kt_to 'kt_vals 'kt_src 'kt_cols
	'kt_c 'kt_cdst 'kt_cpk 'kt_native 'kt_vdst 'kt_vpk 'kt_kernel 'kt_bufs)
