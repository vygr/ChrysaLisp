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

(defq +mnist_side 28 *digits_model* (nn-model '((:input 784) (:dense 128 :relu) (:dense 10 :softmax))))
(defq +digits_inputs (nn-ins *digits_model*) +digits_outputs (nn-outs *digits_model*)
	+digits_params (nn-params *digits_model*))

;;;;;;;;;;;;;;
; the buf layout
;;;;;;;;;;;;;;

;Three blocks: the data set, loaded once by the app, the params, written by
;the app between steps and read by every child in one, and the gradients,
;a slice for each child that only it writes. All that is about where things
;go is here, so a layout can be tried by changing these. A slice starts on
;a line of its own, the M4's lines are 128 bytes, so no two children write
;the same line; a page, 16384 on macOS, is the other to try
(defq +digits_align 128)

(defun digits-align (bytes)
	; (digits-align bytes) -> bytes
	;up to a whole number of +digits_align
	(* (/ (+ bytes (dec +digits_align)) +digits_align) +digits_align))

(defq +digits_params_bytes (digits-align (* +digits_params +long_size)))

(defun digits-slice (slot)
	; (digits-slice slot) -> offset
	;where a child's gradients are in the gradient block
	(* slot +digits_params_bytes))

;;;;;;;;;;;;;;
; the data set
;;;;;;;;;;;;;;

;MNIST, from where PyTorch and TensorFlow fetch it, Yann LeCun's own pages
;having gone. Each file is a .gz there, kept here as it is inflated, in
;downloads/mnist/, which git leaves alone. Each is (name bytes head): an
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
(defq +digits_valid 5000 +digits_used (- +mnist_train +digits_valid))

;where each is in the data block, their heads left out, each on a line of
;its own
(defq +mnist_train_images 0)
(defq +mnist_train_labels (digits-align (* +mnist_train +digits_inputs)))
(defq +mnist_test_images (+ +mnist_train_labels (digits-align +mnist_train)))
(defq +mnist_test_labels (+ +mnist_test_images (digits-align (* +mnist_test +digits_inputs))))
(defq +mnist_data_bytes (+ +mnist_test_labels (digits-align +mnist_test)))

;;;;;;;;;;
; the jobs
;;;;;;;;;;

;a job, what to do with which samples: train, the gradients of the loss
;over them summed into the child's slice, or test or valid, how many are
;right and the confusion of them, of the test set or the training set. The keys of the three bufs, so a child opens them
;once, and the samples, an int each, follow
(enums +digits_kind 0
	(enum train test valid))

(structure +digits_job +job_size
	(long kind step slot count data_key params_key grads_key children))

;its answer: how many, the loss summed over them, how many it got right,
;and for a test the confusion, an int for each digit it was and each it
;was taken for, follows
(structure +digits_reply +job_reply_size
	(long step count right)
	(double loss))

;a job of the GPU's task, a +digits_job, slot the generation of the jobs it
;is one of, those of an older one not done, then flags, load the params
;from their buf before it, sync write them after into the gradient block's
;slice past the farm's, and the rate of a step. The samples follow. Its
;answer is a +digits_reply, a count of -1 for no GPU there
(structure +digits_gpu_job +digits_job_size
	(long flags)
	(double rate))

(bits +digits_gpu_flag 0
	(bit load sync))

;the most samples the GPU works on at once, a batch or a part of a check
(defq +digits_gpu_batch 1024)

;;;;;;;;;;;;;
; the samples
;;;;;;;;;;;;;

(defun digits-input (pixels x)
	; (digits-input str x) -> x
	;the bytes of a picture as doubles 0 to 1 into x
	(nn-scale-bytes pixels (const (/ (n2d 1) (n2d 255))) x))
