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
; in one vector of doubles, the params: a layer's weights a row for each
; unit, a weight for each unit of the layer before, then its biases.
;
; The sums are Lisp over doubles, a row a vector at a time by the native
; dot, scale and add, into vectors made once and used again. Each is what
; a kernel will be later, a row a thread: a layer forward, the error back
; through it, and the gradient of its weights.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")

;a layer of a model, offsets counts of doubles
(enums +nn_layer 0
	(enum kind ins outs act name weights biases))

(defun nn-model (description)
	; (nn-model description) -> model
	;(layers params), each layer (kind ins outs act name weights biases)
	(defq ins (second (first description)) at 0 layers (list))
	(each! (lambda ((kind outs &optional act name))
			(push layers (list kind ins outs (ifn act :none) (ifn name (str (length layers)))
				at (+ at (* ins outs))))
			(setq at (+ at (* ins outs) outs) ins outs))
		(list description) 1)
	(list layers at))

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
	(defq params (cap (nn-params model) (doubles)))
	(each (lambda ((kind ins outs &ignore))
			(defq l (sqrt (/ (n2d 6) (n2d ins))) two (n2d 2) one (n2d 1))
			(times (* ins outs) (push params (* (- (random two) one) l)))
			(times outs (push params (n2d 0))))
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
	(map (lambda ((kind ins outs &ignore))
			(list (map! (lambda () (fill (doubles) ins (n2d 0))) '() 0 outs)
				(fill (doubles) outs (n2d 0))))
		(nn-layers model)))

(defun nn-flat (net)
	; (nn-flat net) -> params
	;a net as one vector, as the params are kept
	(apply (const cat) (reduce (lambda (out (rows biases)) (cat out rows (list biases))) net (list))))

(defun nn-bytes (params)
	; (nn-bytes params) -> str
	;the bytes of a vector of doubles, to go into a buf or a file
	(defq out (str-alloc (* (length params) +long_size)))
	(each (# (set-double out (* (!) +long_size) %0)) params)
	out)

(defun nn-scratch (model)
	; (nn-scratch model) -> (acts deltas tmps)
	;what a sample's sums are worked in, made once: each layer's outputs,
	;its error, and a vector the width of what feeds it
	(defq layers (nn-layers model))
	(list (map (# (fill (doubles) (elem-get %0 +nn_layer_outs) (n2d 0))) layers)
		(map (# (fill (doubles) (elem-get %0 +nn_layer_outs) (n2d 0))) layers)
		(map (# (fill (doubles) (elem-get %0 +nn_layer_ins) (n2d 0))) layers)))

(defun nn-activate (act a)
	; (nn-activate act a) -> a
	;an activation of a layer's scores, in place
	(case act
		(:relu (each (# (if (< %0 (const (n2d 0))) (elem-set a (!) (const (n2d 0))))) a))
		(:sigmoid (each (# (elem-set a (!) (/ (const (n2d 1)) (+ (const (n2d 1)) (exp (neg %0)))))) a))
		(:tanh (each (# (elem-set a (!) (- (/ (const (n2d 2)) (+ (const (n2d 1)) (exp (* (const (n2d -2)) %0))))
				(const (n2d 1))))) a))
		(:softmax
			;its biggest taken from all first, so none overflows
			(defq top (reduce (const max) a (elem-get a 0)))
			(each (# (elem-set a (!) (exp (- %0 top)))) a)
			(nums-scale a (/ (const (n2d 1)) (nums-sum a)) a)))
	a)

(defun nn-slope (act a d)
	; (nn-slope act a d) -> d
	;an error d at a layer's outputs a taken back through its activation,
	;in place, the slope of it written by the outputs
	(case act
		(:relu (each (# (if (<= %0 (const (n2d 0))) (elem-set d (!) (const (n2d 0))))) a))
		(:sigmoid (each (# (elem-set d (!) (* %1 %0 (- (const (n2d 1)) %0)))) a d))
		(:tanh (each (# (elem-set d (!) (* %1 (- (const (n2d 1)) (* %0 %0))))) a d)))
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
	(bind '(acts deltas tmps) scratch)
	(defq layers (nn-layers model) l (dec (length layers)) p (elem-get acts l)
		loss (neg (log (max (elem-get p label) (const (str-to-double "1e-300")))))
		d (elem-get deltas l))
	(nums-scale p (const (n2d 1)) d)
	(elem-set d label (- (elem-get d label) (const (n2d 1))))
	(while (>= l 0)
		(bind '((kind ins outs &ignore) (rows &ignore) (grows gbiases) d tmp)
			(list (elem-get layers l) (elem-get net l) (elem-get grads l) (elem-get deltas l) (elem-get tmps l)))
		(defq in (if (= l 0) x (elem-get acts (dec l))))
		(nums-add gbiases d gbiases)
		(each (lambda (g e)
				(unless (= e (const (n2d 0))) (nums-add g (nums-scale in e tmp) g)))
			grows d)
		;the error at the layer before, and back through its activation
		(when (> l 0)
			(defq back (elem-get deltas (dec l)))
			(nums-scale back (const (n2d 0)) back)
			(each (lambda (row e)
					(unless (= e (const (n2d 0))) (nums-add back (nums-scale row e tmp) back)))
				rows d)
			(nn-slope (elem-get (elem-get layers (dec l)) +nn_layer_act) in back))
		(setq l (dec l)))
	loss)
