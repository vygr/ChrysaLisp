(defq *app_root* (path-to-file))
(import "./app.inc")

;A child of the farm. The bufs it is given the keys of are opened the
;first time, the data set, the params and the gradients; the params are
;read once a step, and each job's samples are worked through the model of
;the net it says, their gradients summed into the job's slice of the
;gradient block

(defq bufs (list) step_had -1 net :nil grads :nil net_had -1
	model :nil size 0 scratch :nil x :nil zero :nil)

(defun net-is (which)
	;the model of the net a job is of, and what its sums are worked in, made
	;again only when it is another
	(unless (= which net_had)
		(setq net_had which model (elem-get *digits_models* which) size (digits-params-size model)
			scratch (nn-scratch model) x (fill (nn-vector model) +digits_inputs (nn-num model 0))
			zero (nn-num model 0) grads :nil step_had -1)))

(defun open-bufs (data_key params_key grads_key children)
	; (open-bufs data_key params_key grads_key children) -> :nil | (data params grads)
	;the three bufs, opened again only if a key is not the one they were.
	;:nil if one can not be, its app gone and its key with it, a job given
	;out as it closed
	(defq want (list data_key params_key grads_key children))
	;the keys by =, two lists of the same big numbers are not eql
	(unless (and (nempty? bufs) (every (const =) (first bufs) want))
		(setq bufs (list want (buf +mnist_data_bytes data_key) (buf +digits_params_bytes params_key)
				(buf (digits-slice children) grads_key)) step_had -1))
	(if (some! (const identity) (list bufs) :t 1 -1) (rest bufs) (setq bufs (list)) :nil))

(defun work ((data params grads_buf) reply key kind step slot count msg)
	;the samples of a job, through the model, and what came of them sent back
	;the params of this step, read the once
	(unless (= step step_had)
		(setq step_had step net (nn-net model (push (nn-vector model) (buf-get params 0 size)))))
	(defq train (= kind +digits_kind_train) right 0 loss (const (n2d 0)) confusion (fill (nums) 100 0)
		test (= kind +digits_kind_test)
		images (if test +mnist_test_images +mnist_train_images)
		labels (if test +mnist_test_labels +mnist_train_labels))
	(if train
		(if grads (nn-clear grads)
			(setq grads (nn-zeros model))))
	(each! (lambda ()
			;a child is on any node of the machine, the GUI's too, and on a
			;machine of one node there is no other: the node's other tasks
			;are let in at each sample
			(task-slice)
			(defq at (code msg 4 (+ +digits_job_size (* (!) 4)))
				label (buf-get data (+ labels at) +type_ubyte)
				p (nn-forward model net
					(digits-input (buf-get data (+ images (* at +digits_inputs)) +digits_inputs) x) scratch)
				guess (nn-guess p))
			(if (= guess label) (setq right (inc right)))
			(elem-set confusion (+ (* label 10) guess) (inc (elem-get confusion (+ (* label 10) guess))))
			(setq loss (+ loss (if train
				(nn-backward model net grads x scratch label)
				(nn-loss p label)))))
		'() 0 count)
	(if train (buf-set grads_buf (digits-slice slot) size (nn-flat grads)))
	(mail-send reply (cat (setf-> (str-alloc +digits_reply_size)
				(+job_reply_key key) (+digits_reply_step step) (+digits_reply_count count)
				(+digits_reply_right right) (+digits_reply_loss loss))
			(if train "" (apply (const cat) (map (# (char %0 +int_size)) confusion))))))

(defun job (msg &ignore)
	(bind '(key reply kind step slot count data_key params_key grads_key children which)
		(getf-> msg +job_key +job_reply +digits_job_kind +digits_job_step +digits_job_slot +digits_job_count
			+digits_job_data_key +digits_job_params_key +digits_job_grads_key +digits_job_children
			+digits_job_net))
	(net-is which)
	(if (defq opened (open-bufs data_key params_key grads_key children))
		(work opened reply key kind step slot count msg)))

(defun main ()
	(job-child 10000000 (const job)))
