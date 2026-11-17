(defq *app_root* (path-to-file))
(import "./app.inc")

;A child of the farm. The bufs it is given the keys of are opened the
;first time, the data set, the params and the gradients; the params are
;read once a step, and each job's samples are worked through the model,
;their gradients summed into the job's slice of the gradient block

(defq bufs (list) step_had -1 net :nil grads :nil
	scratch (nn-scratch *mnist_model*) x (fill (doubles) +mnist_inputs (n2d 0)))

(defun open-bufs (data_key params_key grads_key children)
	; (open-bufs data_key params_key grads_key children) -> :nil | (data params grads)
	;the three bufs, opened again only if a key is not the one they were.
	;:nil if one can not be, its app gone and its key with it, a job given
	;out as it closed
	(unless (eql (first bufs) (defq want (list data_key params_key grads_key children)))
		(setq bufs (list want (buf +mnist_data_bytes data_key) (buf +mnist_params_bytes params_key)
				(buf (mnist-slice children) grads_key)) step_had -1))
	(if (some! (const identity) (list bufs) :t 1 -1) (rest bufs) (setq bufs (list)) :nil))

(defun work ((data params grads_buf) reply key kind step slot count msg)
	;the samples of a job, through the model, and what came of them sent back
	;the params of this step, read the once
	(unless (= step step_had)
		(setq step_had step net (nn-net *mnist_model*
				(push (doubles) (buf-get params 0 (* +mnist_params +long_size))))))
	(defq train (= kind +mnist_kind_train) right 0 loss (n2d 0) confusion (fill (nums) 100 0)
		test (= kind +mnist_kind_test)
		images (if test +mnist_test_images +mnist_train_images)
		labels (if test +mnist_test_labels +mnist_train_labels))
	(if train
		(if grads (each (lambda ((rows biases)) (each (# (nums-scale %0 (const (n2d 0)) %0)) rows)
				(nums-scale biases (const (n2d 0)) biases)) grads)
			(setq grads (nn-zeros *mnist_model*))))
	(each! (lambda ()
			(defq at (code msg 4 (+ +mnist_job_size (* (!) 4)))
				label (buf-get data (+ labels at) +type_ubyte)
				p (nn-forward *mnist_model* net
					(mnist-input (buf-get data (+ images (* at +mnist_inputs)) +mnist_inputs) x) scratch)
				guess (nn-guess p))
			(if (= guess label) (setq right (inc right)))
			(elem-set confusion (+ (* label 10) guess) (inc (elem-get confusion (+ (* label 10) guess))))
			(setq loss (+ loss (if train
				(nn-backward *mnist_model* net grads x scratch label)
				(neg (log (max (elem-get p label) (const (str-to-double "1e-300")))))))))
		'() 0 count)
	(if train (buf-set grads_buf (mnist-slice slot) (* +mnist_params +long_size) (nn-flat grads)))
	(mail-send reply (cat (setf-> (str-alloc +mnist_reply_size)
				(+job_reply_key key) (+mnist_reply_step step) (+mnist_reply_count count)
				(+mnist_reply_right right) (+mnist_reply_loss loss))
			(if train "" (apply (const cat) (map (# (char %0 +int_size)) confusion))))))

(defun job (msg _)
	(bind '(key reply kind step slot count data_key params_key grads_key children)
		(getf-> msg +job_key +job_reply +mnist_job_kind +mnist_job_step +mnist_job_slot +mnist_job_count
			+mnist_job_data_key +mnist_job_params_key +mnist_job_grads_key +mnist_job_children))
	(if (defq opened (open-bufs data_key params_key grads_key children))
		(work opened reply key kind step slot count msg)))

(defun main ()
	(job-child 10000000 (const job)))
