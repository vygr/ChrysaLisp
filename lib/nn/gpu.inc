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
;
; A dense layer is a thread an output forward, an input back, and a weight
; to move. A layer of convolutions is the same, convolved where it is, no
; patches made: an output the bias and its filter over the patch under
; it, an input back the error of each output whose patch it is in by the
; weight it met, a weight the sum over the batch and the places of the
; error by the input it met. A pool layer has no numbers, an average
; forward and its error spread back.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././gpu/kernel.inc")
(import "././gpu/vp.inc")
(import "./model.inc")

;the kernels, and the file of each in that order, named in full: what
;stands on this file stands on them, the cache of the tests reads the names
(enums +nn_kernel 0
	(enum gather forward loss back update conv conv_back conv_update pool pool_back))
(defq +nn_gpu_files ''("lib/nn/shaders/gather.shader" "lib/nn/shaders/forward.shader"
	"lib/nn/shaders/loss.shader" "lib/nn/shaders/back.shader" "lib/nn/shaders/update.shader"
	"lib/nn/shaders/conv.shader" "lib/nn/shaders/conv_back.shader" "lib/nn/shaders/conv_update.shader"
	"lib/nn/shaders/pool.shader" "lib/nn/shaders/pool_back.shader"))
;how far the loops of the kernels go, the kernel and the constant of its
;file, in the order (nn-gpu-fits?) takes them
(defq +nn_gpu_most `'((,+nn_kernel_update max_batch) (,+nn_kernel_conv_update max_batch)
	(,+nn_kernel_loss max_outs) (,+nn_kernel_forward max_ins) (,+nn_kernel_back max_outs)
	(,+nn_kernel_conv max_planes) (,+nn_kernel_conv max_k) (,+nn_kernel_conv_back max_k)
	(,+nn_kernel_conv_back max_filters) (,+nn_kernel_conv_update max_places) (,+nn_kernel_pool max_q)))

;a GPU model: (model capacity programs kernels bufs layers set data)
;programs and kernels by name, bufs (idx x y loss guess), layers a list of
;(w b a d) each layer, w and b two of them, set which is the step's
(enums +nn_gpu 0
	(enum model capacity programs kernels bufs layers set data))

(defun nn-gpu-act (act)
	; (nn-gpu-act act) -> num
	;an activation as the kernels know it
	(case act (:relu 1) (:sigmoid 2) (:tanh 3) (:t 0)))

(defun nn-gpu-programs ()
	; (nn-gpu-programs) -> (program ...)
	;the kernels' programs, a +nn_kernel each
	(map (const shader-load) +nn_gpu_files))

(defun nn-gpu-fits? (model capacity programs)
	; (nn-gpu-fits? model capacity programs) -> :t | :nil
	;is every layer, and a batch of capacity, within what the kernels sum.
	;A shader's loop goes to a constant, so each kernel has a most it sums
	;of each thing, a constant of its file, and past it would stop short
	;and say nothing
	(bind '(batch conv_batch loss_outs dense_ins dense_outs planes k back_k filters places block)
		(map (lambda ((kernel most)) (shader-const (elem-get programs kernel) most)) +nn_gpu_most))
	(and (<= capacity (min batch conv_batch)) (<= (nn-outs model) loss_outs)
		(every (lambda ((kind ins outs act name wt bs shape))
				(case kind
					(:dense (and (<= ins dense_ins) (<= outs dense_outs)))
					(:conv (bind '(c & & fk & & f oh ow) shape)
						(and (<= c planes) (<= fk (min k back_k)) (<= f filters) (<= (* oh ow) places)))
					(:pool (<= (elem-get shape 3) block))))
			(nn-layers model))))

(defun nn-gpu (model capacity)
	; (nn-gpu model capacity) -> :nil | gpu
	;the kernels and buffers to train the model on a batch of up to so
	;many, :nil if there is no GPU, a kernel will not build on it, or the
	;model is past what the kernels sum, (nn-gpu-fits?)
	(when (and (> (gpu-format) 0) (nn-gpu-fits? model capacity (defq programs (nn-gpu-programs))))
		(defq kernels (map (const shader-gpu-kernel) programs) ins (nn-ins model))
		(when (every (const identity) kernels)
			(defq float-buffer (lambda (n) (gpu-buffer (* (max 1 n) +int_size))))
			(list model capacity programs kernels
				(list (float-buffer capacity) (float-buffer (* capacity ins)) (float-buffer capacity)
					(float-buffer capacity) (float-buffer capacity))
				(map (lambda (layer)
						(bind '(rows row biases) (nn-layer-sizes layer))
						(defq outs (elem-get layer +nn_layer_outs))
						(list (list (float-buffer (* rows row)) (float-buffer (* rows row)))
							(list (float-buffer biases) (float-buffer biases))
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
	;a pool layer has none
	(each (lambda (layer (ws bs &ignore))
			(bind '(rows row biases) (nn-layer-sizes layer))
			(defq w (elem-get layer +nn_layer_weights) b (elem-get layer +nn_layer_biases))
			(when (> biases 0)
				(gpu-write (elem-get ws set) 0 (slice fs w b))
				(gpu-write (elem-get bs set) 0 (slice fs b (+ b biases)))))
		(nn-layers model) layers)
	gpu)

(defun nn-gpu-read (gpu)
	; (nn-gpu-read gpu) -> params
	;the params as they are on the GPU, back as the model's kind
	(bind '(model capacity programs kernels bufs layers set &ignore) gpu)
	(nums-convert (apply (const cat) (reduce (lambda (out (layer (ws bs &ignore)))
			(bind '(rows row biases) (nn-layer-sizes layer))
			(if (> biases 0)
				(push out (gpu-read (elem-get ws set) 0 (fill (floats) (* rows row) +float_0))
					(gpu-read (elem-get bs set) 0 (fill (floats) biases +float_0)))
				out))
		(map (const list) (nn-layers model) layers) (list))) (nn-vector model)))

(defun nn-gpu-shape (shape &rest more)
	; (nn-gpu-shape shape [key val] ...) -> vals
	;a conv or pool layer's shape as the inputs of its kernels
	(cat (if (= (length shape) 9)
			(map (const list) '(planes rows cols k stride pad filters oh ow) shape)
			(map (const list) '(planes rows cols q oh ow) shape))
		(partition more 2)))

(defun nn-gpu-go (gpu kernel vals from to bufs)
	; (nn-gpu-go gpu kernel vals from to bufs) -> ticket
	;a kernel, a +nn_kernel, given to the GPU, not waited for
	(bind '(model capacity programs kernels &ignore) gpu)
	(defq program (elem-get programs kernel))
	(shader-gpu-dispatch program (elem-get kernels kernel) (shader-pack program vals) from to bufs))

(defun nn-gpu-forward-chain (gpu idx inputs labels)
	; (nn-gpu-forward-chain gpu idx inputs labels) -> (ticket ...)
	;the batch picked out and taken through the layers to its loss
	(bind '(model capacity programs kernels (bidx x y loss guess) layers set data) gpu)
	(defq n (length idx) tickets (list) ins (nn-ins model) in x)
	;the buffers are for capacity samples, and a GPU checks no bounds
	(if (> n capacity) (throw "A batch bigger than the GPU model was made for !" n))
	(gpu-write bidx 0 (apply (const cat) (map (# (char %0 +int_size)) idx)))
	(push tickets (nn-gpu-go gpu +nn_kernel_gather `((ins ,ins) (inputs ,inputs) (labels ,labels))
		0 (* n ins) (list data bidx x y)))
	(each (lambda ((kind ins outs act name wt bs_at shape) (ws bs a d))
			(push tickets (case kind
				(:dense (nn-gpu-go gpu +nn_kernel_forward `((ins ,ins) (outs ,outs) (act ,(nn-gpu-act act)))
					0 (* n outs) (list in (elem-get ws set) (elem-get bs set) a)))
				(:conv (nn-gpu-go gpu +nn_kernel_conv (nn-gpu-shape shape 'act (nn-gpu-act act))
					0 (* n outs) (list in (elem-get ws set) (elem-get bs set) a)))
				(:pool (nn-gpu-go gpu +nn_kernel_pool (nn-gpu-shape shape)
					;an index each plane's average of each sample, the planes run on
					0 (* n outs) (list in a)))))
			(setq in a))
		(nn-layers model) layers)
	(bind '(& & a d) (last layers))
	(push tickets (nn-gpu-go gpu +nn_kernel_loss `((outs ,(nn-outs model))) 0 n (list a y d loss guess)))
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
		(bind '((kind ins outs act name wt bs_at shape) (ws & & d)) (list (elem-get descr l) (elem-get layers l)))
		(bind '(& & pa pd) (elem-get layers (dec l)))
		;worked out first, a macro, dec, in an unquote is not run
		(defq act (nn-gpu-act (elem-get (elem-get descr (dec l)) +nn_layer_act)))
		(push tickets (case kind
			(:dense (nn-gpu-go gpu +nn_kernel_back `((ins ,ins) (outs ,outs) (act ,act))
				0 (* n ins) (list d (elem-get ws set) pa pd)))
			(:conv (nn-gpu-go gpu +nn_kernel_conv_back (nn-gpu-shape shape 'act act)
				0 (* n ins) (list d (elem-get ws set) pa pd)))
			(:pool (nn-gpu-go gpu +nn_kernel_pool_back (nn-gpu-shape shape 'act act)
				0 (* n ins) (list d pa pd)))))
		(setq l (dec l)))
	;each layer's numbers moved, into the other, a pool layer has none
	(defq in x)
	(each (lambda (layer (ws bs a d))
			(bind '(kind ins outs act name wt bs_at shape) layer)
			(case kind
				(:dense (push tickets (nn-gpu-go gpu +nn_kernel_update `((ins ,ins) (outs ,outs) (batch ,n) (step ,step))
					0 (+ (* ins outs) outs)
					(list (elem-get ws set) (elem-get bs set) d in (elem-get ws next) (elem-get bs next)))))
				(:conv
					(bind '(rows row biases) (nn-layer-sizes layer))
					(push tickets (nn-gpu-go gpu +nn_kernel_conv_update (nn-gpu-shape shape 'batch n 'step step)
						0 (+ (* rows row) biases)
						(list (elem-get ws set) (elem-get bs set) d in (elem-get ws next) (elem-get bs next))))))
			(setq in a))
		descr layers)
	(elem-set gpu +nn_gpu_set next)
	(nn-gpu-answers gpu n tickets))
