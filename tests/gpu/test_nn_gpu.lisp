(report-header "GPU: a model trained on the GPU, against the Lisp model")

(import "lib/nn/gpu.inc")

;a data set of bytes, a picture of 16 a sample, 4 by 4, then a label each
(defq ng_n 40 ng_labels (* ng_n 16)
	ng_data (apply (const cat) (cat
		(map! (# (char (% (+ (* (!) 37) 11) 256))) '() 0 ng_labels)
		(map! (# (char (% (* (!) 7) 4))) '() 0 ng_n)))
	ng_batch (map! (# (% (* (!) 3) ng_n)) '() 0 12) ng_k (/ (n2d 1) (n2d 255)))

(defun ng-near (a b tol) (<= (abs (- a b)) tol))

(defun ng-check (name model)
	;a model on the GPU against the Lisp one of doubles: its params there
	;and back, a forward's loss and guesses, and a step's params
	(cond
		((not (defq gpu (nn-gpu model 64)))
			(test-skip (cat name " on the GPU") "this node has no GPU"))
		(:t (defq params (nn-init model) scratch (nn-scratch model) x (fill (doubles) 16 +double_0))
			;biases not 0, so a ReLU is not at its corner for every unit
			(each (lambda (layer)
					(defq b (elem-get layer +nn_layer_biases))
					(each! (lambda () (elem-set params (+ b (!)) (n2d 0.05))) '() 0 (third (nn-layer-sizes layer))))
				(nn-layers model))
			(nn-gpu-data gpu ng_data)
			(nn-gpu-params gpu params)
			(assert-true (cat name ", the params there and back")
				(every (# (ng-near %0 %1 (str-to-double "1e-6"))) (nn-gpu-read gpu) params))
			;forward, the loss and the guesses as the Lisp model has them
			(defq net (nn-net model params) loss +double_0 guesses (list) grads (nn-zeros model))
			(each (lambda (s)
					(nn-scale-bytes (slice ng_data (* s 16) (* (inc s) 16)) ng_k x)
					(defq p (nn-forward model net x scratch) label (code ng_data 1 (+ ng_labels s)))
					(push guesses (nn-guess p))
					(setq loss (+ loss (nn-backward model net grads x scratch label))))
				ng_batch)
			(bind '(gl gg gy) (nn-gpu-forward gpu ng_batch 0 ng_labels))
			(assert-true (cat name ", the loss") (ng-near gl loss (n2d 0.001)))
			(assert-list-eq (cat name ", the guesses") guesses gg)
			(assert-list-eq (cat name ", the labels") (map (# (code ng_data 1 (+ ng_labels %0))) ng_batch) gy)
			;a step, the params as the Lisp model's moved down their gradients
			(defq rate (n2d 0.5))
			(bind '(sl &ignore) (nn-gpu-step gpu ng_batch 0 ng_labels rate))
			(assert-true (cat name ", the loss of a step is the loss before it") (ng-near sl loss (n2d 0.001)))
			(defq want (nums-add params (nums-scale (nn-flat grads) (/ (neg rate) (n2d (length ng_batch))))))
			(assert-true (cat name ", each number moved down its gradient")
				(every (# (ng-near %0 %1 (n2d 0.0001))) (nn-gpu-read gpu) want))
			(assert-true (cat name ", and moved") (some (# (not (ng-near %0 %1 (n2d 0.0001)))) want params))
			(nn-gpu-free gpu))))

;dense layers of every activation; convolutions padded, pooled, and one
;of what a pool gave, then dense; and a stride, a ReLU, and a pool with an
;edge left over
(ng-check "dense" (nn-model '((:input 16) (:dense 8 :tanh) (:dense 6 :sigmoid) (:dense 5 :relu) (:dense 4 :softmax))))
(ng-check "conv" (nn-model '((:input 1 4 4) (:conv 3 3 :tanh :pad 1) (:pool 2) (:conv 2 2 :sigmoid) (:dense 4 :softmax))))
(ng-check "stride" (nn-model '((:input 1 4 4) (:conv 3 3 :relu :stride 2 :pad 1) (:conv 2 2 :tanh :pad 1) (:pool 2) (:dense 4 :softmax))))

(undef (env) 'ng-check 'ng-near 'ng_n 'ng_labels 'ng_data 'ng_batch 'ng_k)
