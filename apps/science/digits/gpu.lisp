(defq *app_root* (path-to-file))
(import "./app.inc")
(import "lib/nn/gpu.inc")

;The GPU's task, on a node away from the window, so its frames hold none
;of this up. The app sends it jobs a few ahead, a step on a batch or a
;check, and it does them as they come: the model on the GPU and the data
;set put there the first time, the params loaded from their buf if the
;job says, and written back after it if it says. Told to go, an empty
;message, the GPU's buffers are let go of

;the dense net, the one the GPU's kernels are of
(defq gpu :nil bufs (list) gen_had 0 *gpu_model* (first *digits_models*)
	+params_bytes (digits-params-size *gpu_model*))

(defun open-bufs (data_key params_key grads_key children)
	; (open-bufs data_key params_key grads_key children) -> :nil | (data params grads)
	;the three bufs, opened again only if a key is not the one they were,
	;the data set put on the GPU when it is a new one
	(defq want (list data_key params_key grads_key children))
	;the keys by =, two lists of the same big numbers are not eql
	(unless (and (nempty? bufs) (every (const =) (first bufs) want))
		(setq bufs (list want (buf +mnist_data_bytes data_key) (buf +digits_params_bytes params_key)
			(buf (digits-slice (inc children)) grads_key)))
		(if (second bufs) (nn-gpu-data gpu (buf-get (second bufs) 0 +mnist_data_bytes))))
	(if (some! (const identity) (list bufs) :t 1 -1) (rest bufs) (setq bufs (list)) :nil))

(defun check (kind count msg)
	; (check kind count msg) -> (loss right confusion)
	;samples through the network, a part at a time, how many right and the
	;confusion of them
	(defq test (= kind +digits_kind_test) loss (n2d 0) right 0 confusion (fill (nums) 100 0) at 0)
	(while (< at count)
		(defq n (min +digits_gpu_batch (- count at))
			idx (map! (# (code msg +int_size (+ +digits_gpu_job_size (* (!) +int_size)))) '() at (+ at n)))
		(bind '(l guesses labels) (nn-gpu-forward gpu idx
			(if test +mnist_test_images +mnist_train_images) (if test +mnist_test_labels +mnist_train_labels)))
		(setq loss (+ loss l) at (+ at n))
		(each (lambda (guess label)
				(if (= guess label) (setq right (inc right)))
				(elem-set confusion (defq c (+ (* label 10) guess)) (inc (elem-get confusion c))))
			guesses labels))
	(list loss right confusion))

(defun work ((data params grads) kind count children flags rate msg)
	; (work bufs kind count children flags rate msg) -> (loss right confusion)
	(if (bits? flags +digits_gpu_flag_load)
		(nn-gpu-params gpu (push (nn-vector *gpu_model*) (buf-get params 0 +params_bytes))))
	(defq out (cond
		((= count 0) (list (n2d 0) 0 :nil))
		((= kind +digits_kind_train)
			(bind '(loss guesses labels) (nn-gpu-step gpu
				(map! (# (code msg +int_size (+ +digits_gpu_job_size (* (!) +int_size)))) '() 0 count)
				+mnist_train_images +mnist_train_labels rate))
			(defq right 0)
			(each (# (if (= %0 %1) (setq right (inc right)))) guesses labels)
			(list loss right :nil))
		(:t (check kind count msg))))
	(if (bits? flags +digits_gpu_flag_sync)
		(buf-set grads (digits-slice children) +params_bytes (nn-gpu-read gpu)))
	out)

(defun job (msg)
	(bind '(reply kind step gen count data_key params_key grads_key children flags rate)
		(getf-> msg +job_reply +digits_job_kind +digits_job_step +digits_job_slot +digits_job_count
			+digits_job_data_key +digits_job_params_key +digits_job_grads_key +digits_job_children
			+digits_gpu_job_flags +digits_gpu_job_rate))
	;a job of an older generation is not wanted, its answer let go of
	(when (>= gen gen_had)
		(setq gen_had gen)
		(unless gpu (setq gpu (nn-gpu *gpu_model* +digits_gpu_batch)))
		(defq opened (if gpu (open-bufs data_key params_key grads_key children)))
		(cond
			((not gpu)
				(mail-send reply (setf-> (str-alloc +digits_reply_size) (+digits_reply_step step)
					(+digits_reply_count -1))))
			(opened
				(bind '(loss right confusion) (work opened kind count children flags rate msg))
				(mail-send reply (cat (setf-> (str-alloc +digits_reply_size) (+digits_reply_step step)
						(+digits_reply_count count) (+digits_reply_right right) (+digits_reply_loss loss))
					(if confusion (apply (const cat) (map (# (char %0 +int_size)) confusion)) "")))))))

(defun main ()
	(while (nempty? (defq msg (mail-read (task-mbox))))
		(job msg))
	(if gpu (nn-gpu-free gpu)))
