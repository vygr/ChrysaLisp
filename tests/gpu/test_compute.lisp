(report-header "GPU compute: a buffer written and read back, and a kernel run over one")

(import "lib/gpu/compute.inc")
(import "lib/gpu/shader.inc")

;n floats, 32 bits each, as a GPU has them, made from reals
(defun gc-floats (vals)
	(defq s (str-alloc (* 4 (length vals))))
	(each (# (set-int s (* (!) 4) (sh-real-to-float %0))) vals)
	s)

(defq gc_format (gpu-format))
(cond
	((= gc_format 0)
		(test-skip "GPU compute" "this node has no GPU"))
	(:t
		;a buffer, bytes written and the same read back
		(defq gc_bytes (apply (const cat) (map (const char) (range 0 256))) gc_b (gpu-buffer 1024))
		(assert-true "a buffer of the GPU" (num? gc_b))
		(assert-true "written" (and (gpu-write gc_b 0 gc_bytes) (gpu-write gc_b 256 gc_bytes)))
		(assert-eq "and the same read back" (cat gc_bytes gc_bytes) (gpu-read gc_b 0 512))
		(assert-eq "a part of it" (slice gc_bytes 16 48) (gpu-read gc_b 272 32))
		(assert-eq "none past its end" :nil (gpu-read gc_b 1000 32))
		(gpu-buffer-free gc_b)

		;a kernel: each float doubled, then times a scale and plus an add, of
		;a block of uniforms
		(defq gc_n 256 gc_in (gc-floats (map (# (n2r (- %0 100))) (range 0 gc_n))))
		(cond
			((= gc_format 1)
				(defq gc_double (gpu-kernel (cat
						"#include <metal_stdlib>\n"
						"using namespace metal;\n"
						"kernel void kernel_main(device const float *src [[buffer(0)]],\n"
						"	device float *dst [[buffer(1)]], uint i [[thread_position_in_grid]])\n"
						"{ dst[i] = src[i] * 2.0; }\n")
						'(1 1 0 64 1 1))
					gc_scale (gpu-kernel (cat
						"#include <metal_stdlib>\n"
						"using namespace metal;\n"
						"struct block { float scale; float add; };\n"
						"kernel void kernel_main(constant block &u [[buffer(0)]],\n"
						"	device const float *src [[buffer(1)]], device float *dst [[buffer(2)]],\n"
						"	uint i [[thread_position_in_grid]])\n"
						"{ dst[i] = src[i] * u.scale + u.add; }\n")
						'(1 1 1 64 1 1))))
			((= gc_format 2)
				;the same two as SPIR-V, compute_double.comp and compute_scale.comp
				;made by glslangValidator -V -e kernel_main --source-entrypoint main
				(defq gc_double (gpu-kernel (load "tests/gpu/compute_double.spv") '(1 1 0 64 1 1))
					gc_scale (gpu-kernel (load "tests/gpu/compute_scale.spv") '(1 1 1 64 1 1))))
			(:t (defq gc_double :nil gc_scale :nil)))
		(cond
			((not gc_double)
				(test-skip "a kernel" "no kernel of this GPU's language"))
			(:t
				(defq gc_src (gpu-buffer (* 4 gc_n)) gc_dst (gpu-buffer (* 4 gc_n)))
				(gpu-write gc_src 0 gc_in)
				(assert-true "a kernel run over 256 floats" (gpu-run gc_double (list gc_src gc_dst) :nil (list (/ gc_n 64) 1 1)))
				(assert-eq "each one doubled"
					(gc-floats (map (# (n2r (* 2 (- %0 100)))) (range 0 gc_n)))
					(gpu-read gc_dst 0 (* 4 gc_n)))
				(assert-true "a kernel with uniforms"
					(gpu-run gc_scale (list gc_src gc_dst) (gc-floats (list (n2r 3) (n2r 1))) (list (/ gc_n 64) 1 1)))
				(assert-eq "each one times 3 and plus 1"
					(gc-floats (map (# (n2r (+ 1 (* 3 (- %0 100))))) (range 0 gc_n)))
					(gpu-read gc_dst 0 (* 4 gc_n)))
				(gpu-buffer-free gc_src)
				(gpu-buffer-free gc_dst)
				(gpu-kernel-free gc_double)
				(gpu-kernel-free gc_scale)))))

(undef (env) 'gc-floats 'gc_format 'gc_bytes 'gc_b 'gc_n 'gc_in 'gc_double 'gc_scale 'gc_src 'gc_dst)
