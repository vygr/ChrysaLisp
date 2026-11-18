;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a model trained on the GPU, lib/nn/shaders
;
; Its params stay on the GPU, in floats, two of each layer's weights and
; biases, those of the step read and those of the next written, as a
; kernel writes no buffer it reads. The data set is put there once, its
; bytes as they are, and a step is a chain of kernels given to the GPU's
; one queue with a wait only at its end: the batch picked out of the
; data, each layer forward, the loss and its error at the outputs, the
; error back a layer at a time, and each layer's numbers moved down their
; gradients. What comes back is a few bytes a sample, its loss, what it
; was taken for and what it was.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././gpu/kernel.inc")
(import "././gpu/vp.inc")
(import "./model.inc")

(defq +nn_gpu_shaders ''("gather" "forward" "loss" "back" "update"))

;a GPU model: (model capacity programs kernels bufs layers set data)
;programs and kernels by name, bufs (idx x y loss guess), layers a list of
;(w b a d) each layer, w and b two of them, set which is the step's
(enums +nn_gpu 0
	(enum model capacity programs kernels bufs layers set data))

(defun nn-gpu-act (act)
	; (nn-gpu-act act) -> num
	;an activation as the kernels know it
	(case act (:relu 1) (:sigmoid 2) (:tanh 3) (:t 0)))

(defun nn-gpu (model capacity)
	; (nn-gpu model capacity) -> :nil | gpu
	;the kernels and buffers to train the model on a batch of up to so
	;many, :nil if there is no GPU or a kernel will not build on it. Its
	;kernels are of dense layers, as yet
	(unless (every (# (eql (first %0) :dense)) (nn-layers model))
		(throw "The GPU's kernels are of dense layers only, as yet !" (map (const first) (nn-layers model))))
	(when (> (gpu-format) 0)
		(defq programs (map (# (shader-load (cat "lib/nn/shaders/" %0 ".shader"))) +nn_gpu_shaders)
			kernels (map (const shader-gpu-kernel) programs) ins (nn-ins model))
		(when (every (const identity) kernels)
			(defq float-buffer (lambda (n) (gpu-buffer (* (max 1 n) +int_size))))
			(list model capacity programs kernels
				(list (float-buffer capacity) (float-buffer (* capacity ins)) (float-buffer capacity)
					(float-buffer capacity) (float-buffer capacity))
				(map (lambda ((kind ins outs &ignore))
						(list (list (float-buffer (* ins outs)) (float-buffer (* ins outs)))
							(list (float-buffer outs) (float-buffer outs))
							(float-buffer (* capacity outs)) (float-buffer (* capacity outs))))
					(nn-layers model))
				0 :nil))))

(defun nn-gpu-free (gpu)
	; (nn-gpu-free gpu) -> :nil
	(bind '(model capacity programs kernels bufs layers set data) gpu)
	(each (const gpu-kernel-free) kernels)
	(each (const gpu-buffer-free) bufs)
	(each (lambda ((w b a d)) (each (const gpu-buffer-free) (cat w b (list a d)))) layers)
	(if data (gpu-buffer-free data))
	:nil)

(defun nn-gpu-data (gpu bytes)
	; (nn-gpu-data gpu str) -> gpu
	;a data set's bytes onto the GPU, once
	(if (defq old (elem-get gpu +nn_gpu_data)) (gpu-buffer-free old))
	(defq data (gpu-buffer (length bytes)))
	(gpu-write data 0 bytes)
	(elem-set gpu +nn_gpu_data data))

(defun nn-gpu-params (gpu params)
	; (nn-gpu-params gpu params) -> gpu
	;the params, a vector of any kind, onto the GPU as floats, the step's
	(bind '(model capacity programs kernels bufs layers set &ignore) gpu)
	(defq fs (nums-convert params (floats)))
	(each (lambda ((kind ins outs act name w b &ignore) (ws bs &ignore))
			(gpu-write (elem-get ws set) 0 (slice fs w b))
			(gpu-write (elem-get bs set) 0 (slice fs b (+ b outs))))
		(nn-layers model) layers)
	gpu)

(defun nn-gpu-read (gpu)
	; (nn-gpu-read gpu) -> params
	;the params as they are on the GPU, back as the model's kind
	(bind '(model capacity programs kernels bufs layers set &ignore) gpu)
	(nums-convert (apply (const cat) (reduce (lambda (out ((kind ins outs &ignore) (ws bs &ignore)))
			(push out (gpu-read (elem-get ws set) 0 (fill (floats) (* ins outs) (n2f 0)))
				(gpu-read (elem-get bs set) 0 (fill (floats) outs (n2f 0)))))
		(map (const list) (nn-layers model) layers) (list))) (nn-vector model)))

(defun nn-gpu-go (gpu name vals from to bufs)
	; (nn-gpu-go gpu name vals from to bufs) -> ticket
	;a kernel given to the GPU, not waited for
	(bind '(model capacity programs kernels &ignore) gpu)
	(defq at (find name +nn_gpu_shaders) program (elem-get programs at))
	(shader-gpu-dispatch program (elem-get kernels at) (shader-pack program vals) from to bufs))

(defun nn-gpu-forward-chain (gpu idx inputs labels)
	; (nn-gpu-forward-chain gpu idx inputs labels) -> (ticket ...)
	;the batch picked out and taken through the layers to its loss
	(bind '(model capacity programs kernels (bidx x y loss guess) layers set data) gpu)
	(defq n (length idx) tickets (list) ins (nn-ins model) in x)
	(gpu-write bidx 0 (apply (const cat) (map (# (char %0 +int_size)) idx)))
	(push tickets (nn-gpu-go gpu "gather" `((ins ,ins) (inputs ,inputs) (labels ,labels))
		0 (* n ins) (list data bidx x y)))
	(each (lambda ((kind ins outs act &ignore) (ws bs a d))
			(push tickets (nn-gpu-go gpu "forward" `((ins ,ins) (outs ,outs) (act ,(nn-gpu-act act)))
				0 (* n outs) (list in (elem-get ws set) (elem-get bs set) a)))
			(setq in a))
		(nn-layers model) layers)
	(bind '(& & a d) (last layers))
	(push tickets (nn-gpu-go gpu "loss" `((outs ,(nn-outs model))) 0 n (list a y d loss guess)))
	tickets)

(defun nn-gpu-answers (gpu n tickets)
	; (nn-gpu-answers gpu n tickets) -> (loss guesses labels)
	;what the chain worked out, the wait for it, and its tickets let go of
	(bind '(model capacity programs kernels (bidx x y loss guess) &ignore) gpu)
	(defq losses (gpu-read loss 0 (fill (floats) n (n2f 0)))
		guesses (gpu-read guess 0 (* n +int_size)) labels (gpu-read y 0 (* n +int_size)))
	(each (const gpu-take) tickets)
	(list (n2d (nums-sum losses))
		(map! (# (code guesses +int_size (* (!) +int_size))) '() 0 n)
		(map! (# (code labels +int_size (* (!) +int_size))) '() 0 n)))

(defun nn-gpu-forward (gpu idx inputs labels)
	; (nn-gpu-forward gpu idx inputs labels) -> (loss guesses labels)
	;samples by their index in the data set taken through the network, the
	;loss summed, what each is taken for and what each was, no training.
	;inputs and labels are where they start in the data, in bytes
	(nn-gpu-answers gpu (length idx) (nn-gpu-forward-chain gpu idx inputs labels)))

(defun nn-gpu-step (gpu idx inputs labels rate)
	; (nn-gpu-step gpu idx inputs labels rate) -> (loss guesses labels)
	;a step of training on a batch, as (nn-gpu-forward), then the error
	;back through the layers and their numbers moved down their gradients
	;by rate over the batch, into the other of each, which is the next
	;step's
	(bind '(model capacity programs kernels (bidx x &ignore) layers set &ignore) gpu)
	(defq n (length idx) tickets (nn-gpu-forward-chain gpu idx inputs labels)
		descr (nn-layers model) l (dec (length descr)) next (- 1 set)
		step (/ (n2d rate) (n2d n)))
	;the error back, the last layer's to the first's
	(while (> l 0)
		(bind '((& ins outs &ignore) (ws & & d)) (list (elem-get descr l) (elem-get layers l)))
		(bind '(& & pa pd) (elem-get layers (dec l)))
		;worked out first, a macro, dec, in an unquote is not run
		(defq act (nn-gpu-act (elem-get (elem-get descr (dec l)) +nn_layer_act)))
		(push tickets (nn-gpu-go gpu "back" `((ins ,ins) (outs ,outs) (act ,act))
			0 (* n ins) (list d (elem-get ws set) pa pd)))
		(setq l (dec l)))
	;each layer's numbers moved, into the other
	(defq in x)
	(each (lambda ((kind ins outs &ignore) (ws bs a d))
			(push tickets (nn-gpu-go gpu "update" `((ins ,ins) (outs ,outs) (batch ,n) (step ,step))
				0 (+ (* ins outs) outs)
				(list (elem-get ws set) (elem-get bs set) d in (elem-get ws next) (elem-get bs next))))
			(setq in a))
		descr layers)
	(elem-set gpu +nn_gpu_set next)
	(nn-gpu-answers gpu n tickets))
