(report-header "GPU: a model trained on the GPU, against the Lisp model")

(import "lib/nn/gpu.inc")

(defq ng_model (nn-model '((:input 16) (:dense 8 :tanh) (:dense 6 :sigmoid) (:dense 5 :relu) (:dense 4 :softmax)))
	ng_n 40 ng_labels (* ng_n 16))

(cond
	((not (defq ng_gpu (nn-gpu ng_model 64)))
		(test-skip "a model on the GPU" "this node has no GPU"))
	(:t ;a data set of bytes, a picture of 16 a sample then a label each
		(defq ng_data (apply (const cat) (cat
				(map! (# (char (% (+ (* (!) 37) 11) 256))) '() 0 ng_labels)
				(map! (# (char (% (* (!) 7) 4))) '() 0 ng_n)))
			ng_params (nn-init ng_model) ng_batch (map! (# (% (* (!) 3) ng_n)) '() 0 12)
			ng_scratch (nn-scratch ng_model) ng_x (fill (doubles) 16 (n2d 0))
			ng_k (/ (n2d 1) (n2d 255)))
		;biases not 0, so a ReLU is not at its corner for every unit
		(each (lambda ((kind ins outs act name w b))
				(each! (lambda () (elem-set ng_params (+ b (!)) (n2d 0.05))) '() 0 outs))
			(nn-layers ng_model))
		(nn-gpu-data ng_gpu ng_data)
		(nn-gpu-params ng_gpu ng_params)
		(defun ng-near (a b tol) (<= (abs (- a b)) tol))

		;the params there and back, as floats
		(assert-true "the params there and back"
			(every (# (ng-near %0 %1 (str-to-double "1e-6"))) (nn-gpu-read ng_gpu) ng_params))

		;forward, the loss and the guesses as the Lisp model has them
		(defq ng_net (nn-net ng_model ng_params) ng_loss (n2d 0) ng_guesses (list) ng_grads (nn-zeros ng_model))
		(each (lambda (s)
				(nn-scale-bytes (slice ng_data (* s 16) (* (inc s) 16)) ng_k ng_x)
				(defq p (nn-forward ng_model ng_net ng_x ng_scratch) label (code ng_data 1 (+ ng_labels s)))
				(push ng_guesses (nn-guess p))
				(setq ng_loss (+ ng_loss (nn-backward ng_model ng_net ng_grads ng_x ng_scratch label))))
			ng_batch)
		(bind '(gl gg gy) (nn-gpu-forward ng_gpu ng_batch 0 ng_labels))
		(assert-true "the loss" (ng-near gl ng_loss (n2d 0.001)))
		(assert-list-eq "the guesses" ng_guesses gg)
		(assert-list-eq "the labels" (map (# (code ng_data 1 (+ ng_labels %0))) ng_batch) gy)

		;a step, the params as the Lisp model's moved down their gradients
		(defq ng_rate (n2d 0.5))
		(bind '(sl &ignore) (nn-gpu-step ng_gpu ng_batch 0 ng_labels ng_rate))
		(assert-true "the loss of a step is the loss before it" (ng-near sl ng_loss (n2d 0.001)))
		(defq ng_want (nums-add ng_params (nums-scale (nn-flat ng_grads) (/ (neg ng_rate) (n2d (length ng_batch))))))
		(assert-true "each number moved down its gradient"
			(every (# (ng-near %0 %1 (n2d 0.0001))) (nn-gpu-read ng_gpu) ng_want))
		(assert-true "and moved" (some (# (not (ng-near %0 %1 (n2d 0.0001)))) ng_want ng_params))
		(nn-gpu-free ng_gpu)))
