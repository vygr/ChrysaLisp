;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; neural networks, made from a description
;
; A description is a list of layers, the first the input:
;
;	((:input 784) (:dense 128 :relu) (:dense 10 :softmax))
;
;	((:input 1 28 28) (:conv 6 5 :tanh :pad 2) (:pool 2)
;		(:conv 16 5 :tanh) (:pool 2)
;		(:dense 120 :tanh) (:dense 84 :tanh) (:dense 10 :softmax))
;
; the input so many numbers, or c planes of h by w. A dense layer its
; width, its activation, :relu :sigmoid :tanh :none or :softmax, which is
; the last layer's and no other's, and a name if given, "fc1", for the tensors of a weights file,
; else its place, "0", "1", as PyTorch names the layers of a Sequential.
; A conv layer its filters and their size, k by k across all the planes
; before, its activation, and :pad :stride and :name if given, 0 1 and
; its place if not. A pool layer the q of the average of each q by q
; block, q apart, and :name.
;
; The model made of it says where each layer's numbers are in one vector,
; the params: a dense layer's weights a row for each unit, a weight for
; each unit of the layer before, a conv layer's a row for each filter, a
; weight for each number of a patch, plane by plane, row by row, then the
; biases, a pool layer none. Its numbers are of a kind, doubles, or
; floats as a GPU has them, and every vector of it and constant its sums
; meet is of that kind.
;
; The sums of a dense layer are Lisp over the vectors, a row a vector at
; a time by the native dot, scale and add, into vectors made once and
; used again, what a kernel will be later, a row a thread. Those of conv
; and pool layers are the natives of lib/nn/lisp.vp: the patches of the
; planes as a matrix, im2col, the filters times it, gemm, and back.
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
(ffi (path-to-absolute "./act") nn-act)
; (nn-act code a) -> a
;an activation of each number in place, 1 a ReLU, 2 a sigmoid, 3 tanh
(ffi (path-to-absolute "./act_slope") nn-act-slope)
; (nn-act-slope code a d) -> d
;the error d at the outputs a of an activation taken back through it

;a layer of a model, offsets counts of numbers, its shape :nil for a dense
;layer, (c h w k s pad f oh ow) for a conv, (c h w q oh ow) for a pool, the
;planes it takes and those it gives
(enums +nn_layer 0
	(enum kind ins outs act name weights biases shape))

;the activations there are, :softmax the last layer's and no other's
(defq +nn_acts ''(:none :relu :sigmoid :tanh :softmax))

(defun nn-act-of (act layer)
	; (nn-act-of act layer) -> act
	;a layer's activation, :none if it gives none, one of those there are
	(setd act :none)
	(unless (find act +nn_acts) (throw "No such activation !" layer))
	act)

(defun nn-option (options key default)
	; (nn-option options key default) -> val
	;the value after a key of a list of keys and values, else the default
	(if (defq i (find key options)) (elem-get options (inc i)) default))

(defun nn-model (description &optional kind)
	; (nn-model description [kind]) -> model
	;(layers params kind), each layer (kind ins outs act name weights
	;biases shape), the kind of its numbers :doubles, or :floats
	(bind '(c &optional h w) (rest (first description)))
	(setd h 1 w 1)
	(defq at 0 layers (list))
	(each! (lambda (layer)
			(defq ins (* c h w) place (str (length layers)))
			(case (first layer)
				(:dense
					(bind '(outs &optional act name) (rest layer))
					(if (< outs 1) (throw "A layer of no units !" layer))
					(push layers (list :dense ins outs (nn-act-of act layer) (ifn name place)
						at (+ at (* ins outs)) :nil))
					(setq at (+ at (* ins outs) outs) c outs h 1 w 1))
				(:conv
					(bind '(f k &optional act &rest options) (rest layer))
					(defq pad (nn-option options :pad 0) s (nn-option options :stride 1))
					(if (or (< f 1) (< k 1) (< s 1) (< pad 0)) (throw "A filter of no size, stride or padding !" layer))
					(defq oh (inc (/ (- (+ h pad pad) k) s)) ow (inc (/ (- (+ w pad pad) k) s)) n (* f c k k))
					(if (or (< (+ h pad pad) k) (< (+ w pad pad) k)) (throw "A filter bigger than its planes !" layer))
					(push layers (list :conv ins (* f oh ow) (nn-act-of act layer) (nn-option options :name place)
						at (+ at n) (list c h w k s pad f oh ow)))
					(setq at (+ at n f) c f h oh w ow))
				(:pool
					(bind '(q &rest options) (rest layer))
					(if (< q 1) (throw "A block of no size !" layer))
					(defq oh (/ h q) ow (/ w q))
					(if (or (< oh 1) (< ow 1)) (throw "A block bigger than its planes !" layer))
					(push layers (list :pool ins (* c oh ow) :none (nn-option options :name place)
						at at (list c h w q oh ow)))
					(setq h oh w ow))
				(:t (throw "No such layer !" layer))))
		(list description) 1)
	;its outputs are chances, softmax, of the last layer and of no other:
	;the loss is their cross entropy, on the CPU and the GPU, the one there is
	(if (empty? layers) (throw "No layers !" description))
	(each (lambda (layer)
			(unless (eql (eql (elem-get layer +nn_layer_act) :softmax) (= (!) (dec (length layers))))
				(throw "Softmax is of the last layer, and only of it !" (elem-get description (inc (!))))))
		layers)
	(list layers at (ifn kind :doubles)))

(defun nn-layer-sizes (layer)
	; (nn-layer-sizes layer) -> (rows row biases)
	;its weights, so many rows of so many, and its biases: a dense layer a
	;row for each unit, a conv layer one row of all its filters
	(bind '(kind ins outs &ignore) layer)
	(case kind
		(:dense (list outs ins outs))
		(:conv
			(bind '(c & & k & & f &ignore) (elem-get layer +nn_layer_shape))
			(list 1 (* f c k k) f))
		(:t '(0 0 0))))

(defun nn-fan-in (layer)
	; (nn-fan-in layer) -> count
	;how many numbers feed one of its units
	(bind '(kind ins &ignore) layer)
	(case kind
		(:conv (bind '(c & & k &ignore) (elem-get layer +nn_layer_shape)) (* c k k))
		(:t ins)))

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
	;uniform for a ReLU, LeCun's for the rest, the biases 0
	(defq params (cap (nn-params model) (nn-vector model)) zero (nn-num model 0))
	(each (lambda (layer)
			(bind '(rows row biases) (nn-layer-sizes layer))
			(defq l (sqrt (/ (if (eql (elem-get layer +nn_layer_act) :relu) +double_6 +double_3)
					(n2d (nn-fan-in layer)))))
			(times (* rows row) (push params (nn-num model (* (- (random +double_2) +double_1) l))))
			(times biases (push params zero)))
		(nn-layers model))
	params)

(defun nn-net (model params)
	; (nn-net model params) -> ((rows biases) ...)
	;the params as rows, each its own vector, a layer at a time
	(map (lambda (layer)
			(bind '(rows row biases) (nn-layer-sizes layer))
			(defq w (elem-get layer +nn_layer_weights) b (elem-get layer +nn_layer_biases))
			(list (map! (lambda () (slice params (+ w (* (!) row)) (+ w (* (inc (!)) row)))) '() 0 rows)
				(slice params b (+ b biases))))
		(nn-layers model)))

(defun nn-zeros (model)
	; (nn-zeros model) -> ((rows biases) ...)
	;a net of 0s, the shape of the model's, for gradients
	(defq zero (nn-num model 0))
	(map (lambda (layer)
			(bind '(rows row biases) (nn-layer-sizes layer))
			(list (map! (lambda () (fill (nn-vector model) row zero)) '() 0 rows)
				(fill (nn-vector model) biases zero)))
		(nn-layers model)))

(defun nn-clear (net)
	; (nn-clear net) -> net
	;each number of a net 0, gradients made ready again; a pool layer's
	;vectors are empty, which the nums natives refuse, so they are passed by
	(defq zero (first (nn-consts (second (first net)))))
	(each (lambda ((rows biases))
			(each (# (nums-scale %0 zero %0)) rows)
			(if (nempty? biases) (nums-scale biases zero biases)))
		net)
	net)

(defun nn-flat (net)
	; (nn-flat net) -> params
	;a net as one vector, as the params are kept
	(apply (const cat) (reduce (lambda (out (rows biases)) (cat out rows (list biases))) net (list))))

(defun nn-scratch (model)
	; (nn-scratch model) -> (acts deltas works)
	;what a sample's sums are worked in, made once: each layer's outputs and
	;its error, and of a conv layer its patches, their error, and ones to
	;sum its biases' gradient by
	(defq layers (nn-layers model) zero (nn-num model 0) one (nn-num model 1))
	(list (map (# (fill (nn-vector model) (elem-get %0 +nn_layer_outs) zero)) layers)
		(map (# (fill (nn-vector model) (elem-get %0 +nn_layer_outs) zero)) layers)
		(map (lambda (layer)
				(when (eql (first layer) :conv)
					(bind '(c & & k & & & oh ow) (elem-get layer +nn_layer_shape))
					(list (fill (nn-vector model) (* c k k oh ow) zero) (fill (nn-vector model) (* c k k oh ow) zero)
						(fill (nn-vector model) (* oh ow) one))))
			layers)))

(defun nn-activate (act a)
	; (nn-activate act a) -> a
	;an activation of a layer's scores, in place, native but for softmax
	(case act
		(:relu (nn-act 1 a))
		(:sigmoid (nn-act 2 a))
		(:tanh (nn-act 3 a))
		(:softmax
			;its biggest taken from all first, so none overflows
			(defq top (reduce (const max) a (elem-get a 0)))
			(each (# (elem-set a (!) (exp (- %0 top)))) a)
			(nums-scale a (/ (second (nn-consts a)) (nums-sum a)) a)))
	a)

(defun nn-slope (act a d)
	; (nn-slope act a d) -> d
	;an error d at a layer's outputs a taken back through its activation,
	;in place, the slope of it written by the outputs
	(case act
		(:relu (nn-act-slope 1 a d))
		(:sigmoid (nn-act-slope 2 a d))
		(:tanh (nn-act-slope 3 a d)))
	d)

(defun nn-forward (model net x scratch)
	; (nn-forward model net x scratch) -> outputs
	;a sample through the layers, each one's outputs kept in the scratch
	(defq in x)
	(each (lambda ((kind ins outs act name wt bs shape) (rows biases) a work)
			(case kind
				(:dense (each (# (elem-set a (!) (+ (nums-dot %0 in) %1))) rows biases))
				(:conv
					(bind '(c h w k s pad f oh ow) shape)
					(nn-im2col in c h w k s pad (first work))
					(nn-gemm a (first rows) (first work) f (* oh ow) (* c k k) 0 biases))
				(:pool
					(bind '(c h w q &ignore) shape)
					(nn-pool in c h w q a)))
			(setq in (nn-activate act a)))
		(nn-layers model) net (first scratch) (third scratch))
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
	(bind '(acts deltas works) scratch)
	(defq layers (nn-layers model) l (dec (length layers)) p (elem-get acts l)
		loss (nn-loss p label) d (elem-get deltas l))
	(bind '(zero one &ignore) (nn-consts p))
	(nums-scale p one d)
	(elem-set d label (- (elem-get d label) one))
	(while (>= l 0)
		(bind '((kind ins outs act name wt bs shape) (rows &ignore) (grows gbiases) d work)
			(list (elem-get layers l) (elem-get net l) (elem-get grads l) (elem-get deltas l) (elem-get works l)))
		(defq in (if (= l 0) x (elem-get acts (dec l))) back (if (> l 0) (elem-get deltas (dec l))))
		;the gradient of its weights and biases, and the error at the layer
		;before
		(case kind
			(:dense
				(nums-add gbiases d gbiases)
				(each (lambda (g e)
						(unless (= e zero) (nums-add-scaled g in e g)))
					grows d)
				(when back
					(nums-scale back zero back)
					(each (lambda (row e)
							(unless (= e zero) (nums-add-scaled back row e back)))
						rows d)))
			(:conv
				(bind '(c h w k s pad f oh ow) shape)
				(bind '(patches dpatches ones) work)
				(defq places (* oh ow) n (* c k k))
				(nn-gemm gbiases d ones f 1 places 4)
				(nn-gemm (first grows) d patches f n places 6)
				(when back
					(nn-gemm dpatches (first rows) d n places f 1)
					(nn-col2im dpatches c h w k s pad back)))
			(:pool
				(bind '(c h w q &ignore) shape)
				(if back (nn-pool-back d c h w q back))))
		;back through the activation of the layer before
		(if back (nn-slope (elem-get (elem-get layers (dec l)) +nn_layer_act) in back))
		(setq l (dec l)))
	loss)
