(report-header "GPU: shader language, kernels on the CPU, the VP and the GPU")

(import "lib/gpu/vp.inc")
(import "lib/gpu/kernel.inc")

(defun kt-src (&rest lines)
	(shader-compile (shader-read (string-stream (join lines "\n")))))

;a kernel is a file with buffers, its main takes an index and gives nothing
(assert-error "a kernel writes no buffer it reads"
	(kt-src "(defbuffer b :float :read) (defun main :void ((i :int)) (elem-set b i 1.0))") {Shader, a buffer the kernel only reads !})
(assert-error "its main gives nothing"
	(kt-src "(defbuffer b :float :write) (defun main :float ((i :int)) 1.0)") {Shader, no (defun main :void ((i :int)) ...) of a kernel !})
(assert-error "a main that gives nothing has buffers"
	(kt-src "(defun main :void ((i :int)) (defq x 1))") {Shader, only a kernel's main is :void, it has buffers !})
(assert-error "an element is of the buffer's type"
	(kt-src "(defbuffer b :float :write) (defun main :void ((i :int)) (elem-set b i 1))") {Shader, wrong types !})
(assert-error "a buffer is not a value"
	(kt-src "(defbuffer b :float :read) (defbuffer c :float :write) (defun main :void ((i :int)) (defq x b))") {Shader, a buffer is only for (elem-get buf i) !})
(assert-error "a shift takes two"
	(kt-src "(defbuffer b :int :write) (defun main :void ((i :int)) (elem-set b i (<< i 1 2)))") {Shader, wrong types !})

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

;the float buffers a floats, the int buffers a str of 4 bytes each
(defq kt_n 300 kt_from 5 kt_to 290 kt_vals '((scale 1.5) (n 280))
	kt_src (apply (const floats) (map! (# (n2f (- (!) 3))) '() 0 kt_n)) kt_cols (str-alloc (* kt_n 4)))
(each! (# (set-int kt_cols (* (!) 4) (- (* (!) 0x010307) 0x5000))) '() 0 kt_n)
(defun kt-zeros ()
	(defq s (str-alloc (* kt_n 4)))
	(for 0 (>> (inc kt_n) 1) (set-long s (<< (!) 3) 0))
	s)
(defun kt-fzeros () (fill (floats) kt_n (const (n2f 0))))

;what it writes, worked out: in the range and less than n, the src by the
;scale and the square of the index xor 3, every one a whole number or a
;half, so the 32 bit float is exact, and the bits of the int
(defq kt_cdst (kt-fzeros) kt_cpk (kt-zeros))
(each! (lambda ()
		(elem-set kt_cdst (!) (n2f (+ (* (n2d (- (!) 3)) (const (n2d 1.5))) (n2d (* (logxor (!) 3) (logxor (!) 3))))))
		(defq c (- (* (!) 0x010307) 0x5000))
		(set-int kt_cpk (* (!) 4) (logior (<< (logand (>> c 8) 255) 16) (logand c 255) (neg c))))
	'() kt_from (min kt_to 280))

;the VP back end
(defq kt_native (shader-vp-kernel kt_prog) kt_vdst (kt-fzeros) kt_vpk (kt-zeros))
(shader-vp-run kt_prog kt_native (shader-vp-frame kt_prog kt_native kt_vals)
	kt_from kt_to (list kt_vdst kt_src kt_vpk kt_cols))
(assert-eq "the VP writes the range" (n2f 39) (elem-get kt_vdst 5))
(assert-eq "and not before it" (n2f 0) (elem-get kt_vdst 4))
(assert-eq "nor at n and after" (n2f 0) (elem-get kt_vdst 280))
(assert-eq "the VP writes the floats" kt_cdst kt_vdst)
(assert-eq "and the ints" kt_cpk kt_vpk)

;the GPU, MSL or SPIR-V, the same bytes as the VP
(cond
	((= (gpu-format) 0)
		(test-skip "a kernel on the GPU" "this node has no GPU"))
	(:t
		(defq kt_kernel (shader-gpu-kernel kt_prog)
			kt_bufs (map! (lambda () (gpu-buffer (* kt_n 4))) '() 0 4))
		(assert-true "a kernel for this GPU" (num? kt_kernel))
		(each (# (gpu-write %0 0 %1)) kt_bufs (list (kt-fzeros) kt_src (kt-zeros) kt_cols))
		(assert-true "it runs" (shader-gpu-run kt_prog kt_kernel (shader-pack kt_prog kt_vals) kt_from kt_to kt_bufs))
		;not the same: what shows why, the buffer read again, then the kernel run again
		(defq kt_got (gpu-read (first kt_bufs) 0 (kt-fzeros)) kt_why "")
		(unless (eql kt_got kt_cdst)
			(defq kt_again (gpu-read (first kt_bufs) 0 (kt-fzeros)))
			(shader-gpu-run kt_prog kt_kernel (shader-pack kt_prog kt_vals) kt_from kt_to kt_bufs)
			(setq kt_why (cat ", read again " (if (eql kt_again kt_cdst) "right" "wrong")
				", run again " (if (eql (gpu-read (first kt_bufs) 0 (kt-fzeros)) kt_cdst) "right" "wrong"))))
		(assert-eq (cat "the GPU writes the same floats" kt_why) kt_cdst kt_got)
		(assert-eq "and the same ints" kt_cpk (gpu-read (third kt_bufs) 0 (* kt_n 4)))
		(each (const gpu-buffer-free) kt_bufs)
		(gpu-kernel-free kt_kernel)))

;sin cos exp log tanh: the VP back end calls the f_ functions of sys/math,
;which those of a float in Lisp are too, so the two are the same to the bit;
;the GPU's own are near them, a bit or two of a single, or a little more of
;a number near 0
(defun kt-math (op from step)
	(defq prog (kt-src "(defbuffer dst :float :write) (defbuffer src :float :read)"
			(cat "(defun main :void ((i :int)) (elem-set dst i (" op " (elem-get src i))))"))
		n 64 src (apply (const floats) (map! (# (n2f (+ (n2d from) (* (n2d (!)) (n2d step))))) '() 0 n))
		want (apply (const floats) (map (eval (sym op)) src))
		native (shader-vp-kernel prog) got (fill (floats) n (const (n2f 0))))
	(shader-vp-run prog native (shader-vp-frame prog native '()) 0 n (list got src))
	(assert-eq (cat op " on the VP, as Lisp's of a float") want got)
	(cond
		((= (gpu-format) 0)
			(test-skip (cat op " on the GPU") "this node has no GPU"))
		(:t
			(defq kernel (shader-gpu-kernel prog) bufs (map! (lambda () (gpu-buffer (* n 4))) '() 0 2))
			(gpu-write (second bufs) 0 src)
			(shader-gpu-run prog kernel (shader-pack prog '()) 0 n bufs)
			(defq gpu (gpu-read (first bufs) 0 (fill (floats) n (const (n2f 0)))))
			(assert-true (cat op " on the GPU, near the VP's")
				(every (# (< (abs (- (n2d %0) (n2d %1))) (+ (* (abs (n2d %1)) (str-to-double "2e-6")) (str-to-double "2e-6"))))
					gpu want))
			(each (const gpu-buffer-free) bufs)
			(gpu-kernel-free kernel))))
(kt-math "sin" -12.0 0.375)
(kt-math "cos" -12.0 0.375)
(kt-math "exp" -12.0 0.375)
(kt-math "log" 0.01 0.75)
(kt-math "tanh" -12.0 0.375)

;a pixel shader as a kernel, (shader-pixel-kernel): the pixels of a tile
;it writes are those the pixel shader gives of that tile, each in its place
;and of its colour, to the bit, as the tile children have them. On the VP,
;and on the GPU if there is one. The alpha main gives is not the pixel's
(defq kt_pfile "tests/scratch/tmp_pixel_kernel.shader")
(save (join (list
		"(definput k :float 0.25)"
		"(defun main :vec4 ((frag :vec2))"
		"	(vec4 (/ (:x frag) 16.0) (/ (:y frag) 8.0) k 0.5))") "\n") kt_pfile)
(defq kt_pixel (shader-load kt_pfile) kt_pk (shader-pixel-kernel kt_pfile)
	kt_h 8 kt_x 3 kt_y 2 kt_x1 11 kt_y1 7 kt_pn (* (- kt_x1 kt_x) (- kt_y1 kt_y))
	kt_pvals '((k 0.75)) kt_pnative (shader-vp kt_pixel)
	kt_want (shader-vp-argb kt_pnative (shader-vp-frame kt_pixel kt_pnative kt_pvals) kt_x kt_y kt_x1 kt_y1 kt_h)
	kt_tile (cat kt_pvals (list (list 'tile_x kt_x) (list 'tile_y kt_y)
		(list 'tile_w (- kt_x1 kt_x)) (list 'tile_top kt_h)))
	kt_knative (shader-vp-kernel kt_pk) kt_out (str-alloc (* kt_pn 4)))
(pii-remove kt_pfile)
(assert-eq "a pixel shader made a kernel is one" :kernel (shader-stage kt_pk))
(assert-eq "a file with no main of a pixel shader makes none" :nil (shader-pixel-kernel "lib/gpu/shaders/sdf.shader"))
(shader-vp-run kt_pk kt_knative (shader-vp-frame kt_pk kt_knative kt_tile) 0 kt_pn (list kt_out))
(assert-eq "its top left pixel is the tile's, opaque" (code kt_want 4 0) (code kt_out 4 0))
(assert-eq "it gives the tile's pixels, on the VP" kt_want kt_out)
(cond
	((= (gpu-format) 0)
		(test-skip "a pixel shader as a kernel on the GPU" "this node has no GPU"))
	(:t (defq kt_pkernel (shader-gpu-kernel kt_pk) kt_pbuf (gpu-buffer (* kt_pn 4)))
		(shader-gpu-run kt_pk kt_pkernel (shader-pack kt_pk kt_tile) 0 kt_pn (list kt_pbuf))
		(assert-eq "and on the GPU" kt_want (gpu-read kt_pbuf 0 (* kt_pn 4)))
		(gpu-buffer-free kt_pbuf)
		(gpu-kernel-free kt_pkernel)))

(undef (env) 'kt-src 'kt-refused 'kt-zeros 'kt-fzeros 'kt_prog 'kt_n 'kt_from 'kt_to 'kt_vals 'kt_src 'kt_cols
	'kt_cdst 'kt_cpk 'kt_native 'kt_vdst 'kt_vpk 'kt_kernel 'kt_bufs
	'kt-math)
