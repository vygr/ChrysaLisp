;what the app and its children both need: the network's shape, where each
;thing is in the bufs they share, the data set's files, and the jobs

(import "lib/task/jobs.inc")
(import "class/buf/lisp.inc")

;;;;;;;;;;;;;
; the network
;;;;;;;;;;;;;

;a picture of 28 by 28 grey levels in, a layer of hidden units, and a
;chance for each digit out, lib/nn
(import "lib/nn/model.inc")

(defq +mnist_side 28 *mnist_model* (nn-model '((:input 784) (:dense 128 :relu) (:dense 10 :softmax))))
(defq +mnist_inputs (nn-ins *mnist_model*) +mnist_outputs (nn-outs *mnist_model*)
	+mnist_params (nn-params *mnist_model*))

;;;;;;;;;;;;;;
; the buf layout
;;;;;;;;;;;;;;

;Three blocks: the data set, loaded once by the app, the params, written by
;the app between steps and read by every child in one, and the gradients,
;a slice for each child that only it writes. All that is about where things
;go is here, so a layout can be tried by changing these. A slice starts on
;a line of its own, the M4's lines are 128 bytes, so no two children write
;the same line; a page, 16384 on macOS, is the other to try
(defq +mnist_align 128)

(defun mnist-align (bytes)
	; (mnist-align bytes) -> bytes
	;up to a whole number of +mnist_align
	(* (/ (+ bytes (dec +mnist_align)) +mnist_align) +mnist_align))

(defq +mnist_params_bytes (mnist-align (* +mnist_params +long_size)))

(defun mnist-slice (slot)
	; (mnist-slice slot) -> offset
	;where a child's gradients are in the gradient block
	(* slot +mnist_params_bytes))

;;;;;;;;;;;;;;
; the data set
;;;;;;;;;;;;;;

;MNIST, from where PyTorch and TensorFlow fetch it, Yann LeCun's own pages
;having gone. Each file is a .gz there, kept here as it is inflated, in the
;app's data/ folder, which git leaves alone. Each is (name bytes head): an
;idx file has a head of its magic number and sizes, then a byte a pixel or
;a label
(defq +mnist_mirrors ''("https://ossci-datasets.s3.amazonaws.com/mnist/"
		"https://storage.googleapis.com/cvdf-datasets/mnist/")
	+mnist_files ''(("train-images-idx3-ubyte" 47040016 16) ("train-labels-idx1-ubyte" 60008 8)
		("t10k-images-idx3-ubyte" 7840016 16) ("t10k-labels-idx1-ubyte" 10008 8))
	+mnist_train 60000 +mnist_test 10000)

;the last of the training set is kept back from training, to say at the end
;of each epoch how it does on what it has not learnt from, and so when to
;stop, the test set kept for the end
(defq +mnist_valid 5000 +mnist_used (- +mnist_train +mnist_valid))

;where each is in the data block, their heads left out, each on a line of
;its own
(defq +mnist_train_images 0)
(defq +mnist_train_labels (mnist-align (* +mnist_train +mnist_inputs)))
(defq +mnist_test_images (+ +mnist_train_labels (mnist-align +mnist_train)))
(defq +mnist_test_labels (+ +mnist_test_images (mnist-align (* +mnist_test +mnist_inputs))))
(defq +mnist_data_bytes (+ +mnist_test_labels (mnist-align +mnist_test)))

;;;;;;;;;;
; the jobs
;;;;;;;;;;

;a job, what to do with which samples: train, the gradients of the loss
;over them summed into the child's slice, or test or valid, how many are
;right and the confusion of them, of the test set or the training set. The keys of the three bufs, so a child opens them
;once, and the samples, an int each, follow
(enums +mnist_kind 0
	(enum train test valid))

(structure +mnist_job +job_size
	(long kind step slot count data_key params_key grads_key children))

;its answer: how many, the loss summed over them, how many it got right,
;and for a test the confusion, an int for each digit it was and each it
;was taken for, follows
(structure +mnist_reply +job_reply_size
	(long step count right)
	(double loss))

;;;;;;;;;;;;;
; the samples
;;;;;;;;;;;;;

;each grey level of a pixel, 0 to 255, as a double 0 to 1
(defq +mnist_grey (apply (const doubles) (map! (# (/ (n2d (!)) (n2d 255))) '() 0 256)))

(defun mnist-input (pixels x)
	; (mnist-input str x) -> x
	;the bytes of a picture as doubles into x
	(each! (lambda () (elem-set x (!) (elem-get +mnist_grey (code pixels 1 (!)))))
		'() 0 +mnist_inputs)
	x)
