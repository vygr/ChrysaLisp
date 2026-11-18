(report-header "Neural networks: a model from a description, forward, and its gradients")

(import "lib/nn/model.inc")

;the layout of a model
(defq nn_mnist (nn-model '((:input 784) (:dense 128 :relu) (:dense 10 :softmax))))
(assert-eq "how many numbers" 101770 (nn-params nn_mnist))
(assert-eq "ins" 784 (nn-ins nn_mnist))
(assert-eq "outs" 10 (nn-outs nn_mnist))
(assert-list-eq "the first layer" '(:dense 784 128 :relu "0" 0 100352) (first (nn-layers nn_mnist)))
(assert-list-eq "the second, after the first's biases" '(:dense 128 10 :softmax "1" 100480 101760)
	(second (nn-layers nn_mnist)))
(assert-eq "a name given" "fc1"
	(elem-get (first (nn-layers (nn-model '((:input 2) (:dense 3 :relu "fc1"))))) +nn_layer_name))
(assert-eq "params at random, as many as it has" 101770 (length (nn-init nn_mnist)))
(defq nn_p (nn-init nn_mnist))
(assert-eq "the net of them back as they were" nn_p (nn-flat (nn-net nn_mnist nn_p)))

;a small model of every activation, its gradients against the slope of
;its loss found by moving each number a little either way
(defq nn_small (nn-model '((:input 5) (:dense 4 :tanh) (:dense 4 :sigmoid) (:dense 3 :relu) (:dense 3 :softmax)))
	nn_x (doubles (n2d 0.5) (n2d -0.25) (n2d 0.75) (n2d 0.1) (n2d -0.6)) nn_label 1
	nn_params (nn-init nn_small) nn_scratch (nn-scratch nn_small))
;biases not 0, so a ReLU is not at its corner for every unit
(each (lambda ((kind ins outs act name w b))
		(each! (lambda () (elem-set nn_params (+ b (!)) (n2d 0.05))) '() 0 outs))
	(nn-layers nn_small))

(defun nn-loss-at (params)
	(defq p (nn-forward nn_small (nn-net nn_small params) nn_x nn_scratch))
	(neg (log (elem-get p nn_label))))

(defq nn_grads (nn-zeros nn_small) nn_net (nn-net nn_small nn_params))
(nn-forward nn_small nn_net nn_x nn_scratch)
(defq nn_loss (nn-backward nn_small nn_net nn_grads nn_x nn_scratch nn_label)
	nn_flat (nn-flat nn_grads) nn_h (str-to-double "1e-6") nn_worst (n2d 0))
(assert-true "the loss it gives is the loss" (< (abs (- nn_loss (nn-loss-at nn_params))) (str-to-double "1e-12")))
(each! (lambda ()
		(defq up (cat nn_params) down (cat nn_params) i (!))
		(elem-set up i (+ (elem-get nn_params i) nn_h))
		(elem-set down i (- (elem-get nn_params i) nn_h))
		(defq slope (/ (- (nn-loss-at up) (nn-loss-at down)) (* (n2d 2) nn_h)))
		(setq nn_worst (max nn_worst (abs (- slope (elem-get nn_flat i))))))
	'() 0 (nn-params nn_small))
(assert-true "every gradient is the slope of the loss" (< nn_worst (str-to-double "1e-6")))

;the outputs of softmax are chances
(defq nn_p (nn-forward nn_small nn_net nn_x nn_scratch))
(assert-true "softmax adds up to 1" (< (abs (- (nums-sum nn_p) (n2d 1))) (str-to-double "1e-12")))
(assert-eq "the guess is the biggest" (nn-guess (doubles (n2d 0.1) (n2d 0.7) (n2d 0.2))) 1)

;bytes as doubles, native, as the sum of each in Lisp
(defq nn_bytes (apply (const cat) (map! (# (char (* (!) 17))) '() 0 15)) nn_k (/ (n2d 1) (n2d 255))
	nn_out (fill (doubles) 20 (n2d -1)))
(nn-scale-bytes nn_bytes nn_k nn_out)
(assert-eq "bytes times a scale" (apply (const doubles) (map! (# (* (n2d (* (!) 17)) nn_k)) '() 0 15))
	(slice nn_out 0 15))
(assert-eq "no further than there are bytes" (n2d -1) (elem-get nn_out 15))
(assert-eq "no further than there are doubles" 3
	(length (nn-scale-bytes nn_bytes nn_k (fill (doubles) 3 (n2d 0)))))
;and as floats, each the nearest float of the double sum
(defq nn_fout (fill (floats) 20 (n2f -1)))
(nn-scale-bytes nn_bytes nn_k nn_fout)
(assert-eq "bytes times a scale into floats" (apply (const floats) (map! (# (n2f (* (n2d (* (!) 17)) nn_k))) '() 0 15))
	(slice nn_fout 0 15))
(assert-eq "no further than there are bytes, floats" (n2f -1) (elem-get nn_fout 15))
(assert-true "nothing else that comes of a doubles" (catch (progn (nn-scale-bytes nn_bytes nn_k (nums 1 2)) :nil) :t))

;a model of floats does the sums of one of doubles, to a float's precision
(defq nn_fsmall (nn-model '((:input 5) (:dense 4 :tanh) (:dense 4 :sigmoid) (:dense 3 :relu) (:dense 3 :softmax)) :floats)
	nn_fparams (nums-convert nn_params (floats)) nn_fnet (nn-net nn_fsmall nn_fparams)
	nn_fscratch (nn-scratch nn_fsmall) nn_fx (nums-convert nn_x (floats)) nn_fgrads (nn-zeros nn_fsmall))
(assert-eq "its params are floats" :floats (nn-kind nn_fsmall))
(assert-true "and so is every vector of it" (floats? (first (first (nn-scratch nn_fsmall)))))
(defq nn_fp (nn-forward nn_fsmall nn_fnet nn_fx nn_fscratch))
(assert-true "its outputs those of doubles"
	(every (# (< (abs (- (n2d %0) %1)) (str-to-double "1e-6"))) nn_fp nn_p))
(defq nn_floss (nn-backward nn_fsmall nn_fnet nn_fgrads nn_fx nn_fscratch nn_label))
(assert-true "its loss a double, that of doubles" (and (double?? nn_floss) (< (abs (- nn_floss nn_loss)) (str-to-double "1e-5"))))
(assert-true "its gradients those of doubles"
	(every (# (< (abs (- (n2d %0) %1)) (str-to-double "1e-5"))) (nn-flat nn_fgrads) nn_flat))
