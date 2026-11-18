;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; neural networks, made from a description
;
; A description is a list of layers, the first the input:
;
;	((:input 784) (:dense 128 :relu) (:dense 10 :softmax))
;
; a dense layer its width, its activation, :relu :sigmoid :tanh :none or
; :softmax, the last only, and a name if given, "fc1", for the tensors of
; a weights file, else its place, "0", "1", as PyTorch names the layers of
; a Sequential. The model made of it says where each layer's numbers are
; in one vector, the params: a layer's weights a row for each unit, a
; weight for each unit of the layer before, then its biases. Its numbers
; are of a kind, doubles, or floats as a GPU has them, and every vector
; of it and constant its sums meet is of that kind.
;
; The sums are Lisp over the vectors, a row a vector at a time by the
; native dot, scale and add, into vectors made once and used again. Each
; is what a kernel will be later, a row a thread: a layer forward, the
; error back through it, and the gradient of its weights.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././math/vector.inc")

;native, lib/nn/lisp.vp
(ffi (path-to-absolute "./scale_bytes") nn-scale-bytes)
; (nn-scale-bytes bytes scale out) -> out
;each byte of a str times the double scale into out, doubles or floats,
;as many as both have
(ffi (path-to-absolute "./gemm") nn-gemm)
; (nn-gemm c a b m n k flags [bias]) -> c
;the m by n c the m by k a times the k by n b, each a vector of its rows:
;flags 1 a is given turned, k rows of m, 2 b is, n rows of k, 4 added to
;c, else c is each row's bias, or 0, and the product. Of one kind, doubles
;or floats
(ffi (path-to-absolute "./im2col") nn-im2col)
; (nn-im2col planes c h w k s pad patches) -> patches
;the k by k patches of c planes of h by w, s apart, from pad before the
;edges, a row for each number of a patch, c * k * k, a column for each
;place, (h + 2pad - k) / s + 1 down and so across, outside the planes 0
(ffi (path-to-absolute "./col2im") nn-col2im)
; (nn-col2im patches c h w k s pad planes) -> planes
;the patches added back into the places of the planes they were of, the
;planes made 0 first
(ffi (path-to-absolute "./pool") nn-pool)
; (nn-pool planes c h w q pooled) -> pooled
;the average of each q by q block of c planes of h by w, q apart
(ffi (path-to-absolute "./pool_back") nn-pool-back)
; (nn-pool-back errors c h w q planes) -> planes
;the error of each average over q^2 to each number of its block, 0 where
;no block is

;a layer of a model, offsets counts of doubles
(enums +nn_layer 0
	(enum kind ins outs act name weights biases))

(defun nn-model (description &optional kind)
	; (nn-model description [kind]) -> model
	;(layers params kind), each layer (kind ins outs act name weights
	;biases), the kind of its numbers :doubles, or :floats
	(defq ins (second (first description)) at 0 layers (list))
	(each! (lambda ((kind outs &optional act name))
			(push layers (list kind ins outs (ifn act :none) (ifn name (str (length layers)))
				at (+ at (* ins outs))))
			(setq at (+ at (* ins outs) outs) ins outs))
		(list description) 1)
	(list layers at (ifn kind :doubles)))

(defun nn-kind (model)
	; (nn-kind model) -> :doubles | :floats
	(elem-get model 2))

(defun nn-vector (model)
	; (nn-vector model) -> doubles | floats
	;an empty vector of the model's kind
	(if (eql (nn-kind model) :floats) (floats) (doubles)))

(defun nn-num (model num)
	; (nn-num model num) -> double | float
	;a number made the model's kind
	(if (eql (nn-kind model) :floats) (n2f num) (n2d num)))

(defun nn-bytes-of (model count)
	; (nn-bytes-of model count) -> bytes
	;of so many of its numbers, 4 bytes a float, 8 a double
	(* count (if (eql (nn-kind model) :floats) +int_size +long_size)))

;the 0, the 1, and the least chance a log is taken of, of a vector's kind
(defq *nn_doubles_consts* (list +double_0 +double_1 (str-to-double "1e-300"))
	*nn_floats_consts* (list +float_0 +float_1 (n2f (str-to-double "1e-37"))))
(defmacro nn-consts (v)
	`(if (floats? ,v) *nn_floats_consts* *nn_doubles_consts*))

(defun nn-loss (p label)
	; (nn-loss outputs label) -> double
	;the cross entropy of softmax outputs for the label, a double whatever
	;the kind, to be summed
	(n2d (neg (log (max (elem-get p label) (third (nn-consts p)))))))

(defun nn-layers (model)
	; (nn-layers model) -> ((kind ins outs act name weights biases) ...)
	(first model))

(defun nn-params (model)
	; (nn-params model) -> count
	;how many numbers the model has
	(second model))

(defun nn-ins (model)
	; (nn-ins model) -> count
	(elem-get (first (nn-layers model)) +nn_layer_ins))

(defun nn-outs (model)
	; (nn-outs model) -> count
	(elem-get (last (nn-layers model)) +nn_layer_outs))

(defun nn-init (model)
	; (nn-init model) -> params
	;weights at random, each layer's spread by how many feed a unit, He's
	;uniform, the biases 0
	(defq params (cap (nn-params model) (nn-vector model)) zero (nn-num model 0))
	(each (lambda ((kind ins outs &ignore))
			(defq l (sqrt (/ +double_6 (n2d ins))))
			(times (* ins outs) (push params (nn-num model (* (- (random +double_2) +double_1) l))))
			(times outs (push params zero)))
		(nn-layers model))
	params)

(defun nn-net (model params)
	; (nn-net model params) -> ((rows biases) ...)
	;the params as rows, each its own vector, a layer at a time
	(map (lambda ((kind ins outs act name w b))
			(list (map! (lambda () (slice params (+ w (* (!) ins)) (+ w (* (inc (!)) ins)))) '() 0 outs)
				(slice params b (+ b outs))))
		(nn-layers model)))

(defun nn-zeros (model)
	; (nn-zeros model) -> ((rows biases) ...)
	;a net of 0s, the shape of the model's, for gradients
	(defq zero (nn-num model 0))
	(map (lambda ((kind ins outs &ignore))
			(list (map! (lambda () (fill (nn-vector model) ins zero)) '() 0 outs)
				(fill (nn-vector model) outs zero)))
		(nn-layers model)))

(defun nn-flat (net)
	; (nn-flat net) -> params
	;a net as one vector, as the params are kept
	(apply (const cat) (reduce (lambda (out (rows biases)) (cat out rows (list biases))) net (list))))

(defun nn-scratch (model)
	; (nn-scratch model) -> (acts deltas)
	;what a sample's sums are worked in, made once: each layer's outputs and
	;its error
	(defq layers (nn-layers model) zero (nn-num model 0))
	(list (map (# (fill (nn-vector model) (elem-get %0 +nn_layer_outs) zero)) layers)
		(map (# (fill (nn-vector model) (elem-get %0 +nn_layer_outs) zero)) layers)))

(defun nn-activate (act a)
	; (nn-activate act a) -> a
	;an activation of a layer's scores, in place
	(bind '(zero one &ignore) (nn-consts a))
	(case act
		(:relu (each (# (if (< %0 zero) (elem-set a (!) zero))) a))
		(:sigmoid (each (# (elem-set a (!) (/ one (+ one (exp (neg %0)))))) a))
		(:tanh (each (# (elem-set a (!) (tanh %0))) a))
		(:softmax
			;its biggest taken from all first, so none overflows
			(defq top (reduce (const max) a (elem-get a 0)))
			(each (# (elem-set a (!) (exp (- %0 top)))) a)
			(nums-scale a (/ one (nums-sum a)) a)))
	a)

(defun nn-slope (act a d)
	; (nn-slope act a d) -> d
	;an error d at a layer's outputs a taken back through its activation,
	;in place, the slope of it written by the outputs
	(bind '(zero one &ignore) (nn-consts a))
	(case act
		(:relu (each (# (if (<= %0 zero) (elem-set d (!) zero))) a))
		(:sigmoid (each (# (elem-set d (!) (* %1 %0 (- one %0)))) a d))
		(:tanh (each (# (elem-set d (!) (* %1 (- one (* %0 %0))))) a d)))
	d)

(defun nn-forward (model net x scratch)
	; (nn-forward model net x scratch) -> outputs
	;a sample through the layers, each one's outputs kept in the scratch
	(defq in x)
	(each (lambda ((kind ins outs act &ignore) (rows biases) a)
			(each (# (elem-set a (!) (+ (nums-dot %0 in) %1))) rows biases)
			(setq in (nn-activate act a)))
		(nn-layers model) net (first scratch))
	in)

(defun nn-guess (p)
	; (nn-guess outputs) -> index
	;the output that is biggest
	(defq best 0)
	(each (# (if (> %0 (elem-get p best)) (setq best (!)))) p)
	best)

(defun nn-backward (model net grads x scratch label)
	; (nn-backward model net grads x scratch label) -> loss
	;the gradient of the loss of a sample, after its (nn-forward), added to
	;grads: the cross entropy of softmax outputs, so the error at them is
	;the outputs less the label's one, and back a layer at a time, through
	;each one's weights and the activation of the one before
	(bind '(acts deltas) scratch)
	(defq layers (nn-layers model) l (dec (length layers)) p (elem-get acts l)
		loss (nn-loss p label) d (elem-get deltas l))
	(bind '(zero one &ignore) (nn-consts p))
	(nums-scale p one d)
	(elem-set d label (- (elem-get d label) one))
	(while (>= l 0)
		(bind '((kind ins outs &ignore) (rows &ignore) (grows gbiases) d)
			(list (elem-get layers l) (elem-get net l) (elem-get grads l) (elem-get deltas l)))
		(defq in (if (= l 0) x (elem-get acts (dec l))))
		(nums-add gbiases d gbiases)
		(each (lambda (g e)
				(unless (= e zero) (nums-add-scaled g in e g)))
			grows d)
		;the error at the layer before, and back through its activation
		(when (> l 0)
			(defq back (elem-get deltas (dec l)))
			(nums-scale back zero back)
			(each (lambda (row e)
					(unless (= e zero) (nums-add-scaled back row e back)))
				rows d)
			(nn-slope (elem-get (elem-get layers (dec l)) +nn_layer_act) in back))
		(setq l (dec l)))
	loss)
