(defq *app_root* (path-to-file))
(import "./app.inc")
(import "lib/nn/gpu.inc")

;The GPU's task, a farm of one, lib/task/jobs.inc, on a node away from the
;window, so its frames hold none of this up. It does the jobs it is given
;as they come, a step on a batch or a check: the model on the GPU of the
;net a job says, and the data set put there, the first time; the params
;loaded from their buf before its first job, and when a job says; and
;written back after a job that says. Told to go, or given nothing for a
;while, the GPU's buffers are let go of

;the GPU model of the net of the jobs, made again when it is another, and
;if it has been given its params
(defq gpu :nil gpu_net -1 bufs (list) model :nil params_bytes 0 loaded :nil)

(defun net-is (which)
	; (net-is which) -> :nil | gpu
	;the GPU model of a net, the old one let go of, and the data set put on
	;it again when the bufs are next opened
	(unless (= which gpu_net)
		(if gpu (nn-gpu-free gpu))
		(setq gpu_net which model (elem-get *digits_models* which) params_bytes (digits-params-size model)
			gpu (nn-gpu model +digits_gpu_batch) bufs (list) loaded :nil))
	gpu)

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
	;a task that has just started has none, whatever the job says
	(when (or (not loaded) (bits? flags +digits_gpu_flag_load))
		(nn-gpu-params gpu (push (nn-vector model) (buf-get params 0 params_bytes)))
		(setq loaded :t))
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
		(buf-set grads (digits-slice children) params_bytes (nn-gpu-read gpu)))
	out)

(defun job (msg &ignore)
	(bind '(key reply kind step count data_key params_key grads_key children which flags rate)
		(getf-> msg +job_key +job_reply +digits_job_kind +digits_job_step +digits_job_count
			+digits_job_data_key +digits_job_params_key +digits_job_grads_key +digits_job_children
			+digits_job_net +digits_gpu_job_flags +digits_gpu_job_rate))
	(net-is which)
	(defq opened (if gpu (open-bufs data_key params_key grads_key children)))
	(cond
		((not gpu)
			(mail-send reply (setf-> (str-alloc +digits_reply_size) (+job_reply_key key)
				(+digits_reply_step step) (+digits_reply_count -1))))
		(opened
			(bind '(loss right confusion) (work opened kind count children flags rate msg))
			(mail-send reply (cat (setf-> (str-alloc +digits_reply_size) (+job_reply_key key)
					(+digits_reply_step step) (+digits_reply_count count) (+digits_reply_right right)
					(+digits_reply_loss loss))
				(if confusion (apply (const cat) (map (# (char %0 +int_size)) confusion)) ""))))))

(defun main ()
	(job-child (* +digits_gpu_idle 1000000) (const job))
	(if gpu (nn-gpu-free gpu)))
