(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")
(import "./data.inc")
(import "lib/nn/view.inc")
(import "./draw.inc")

;Handwritten digits, MNIST, learnt. The data set is fetched the first time
;it is wanted and kept, and loaded into a buf the farm's children read
;their pictures from. Each step the app writes the params into a second
;buf and gives each child a part of the batch; each sums the gradients of
;its samples into its own slice of a third, and the app adds the slices up
;and moves the params down them. The end of each epoch is a test of all
;10000, the share right, and the confusion of them

(enums +event +event_user
	(enum reset train step test)
	(enum rate)
	(enum batch))

;by priority, mail-select takes the first that has mail: what the user
;does first, the timer last so a slow frame starves nothing
(enums +select 0
	(enum main tip task reply fetch timer))

(defq +mnist_rates ''("0.05" "0.1" "0.2" "0.5") +mnist_batches ''(16 32 64)
	;jobs a step, each its slice of the gradient buf, and a frame each
	;twentieth of a second, a new sample shown each second
	+jobs 16 +frame_rate 50000 +show_rate 1000000 +features_rate 2000000
	*running* :t rate 1 batch 1 select :nil
	;the bufs, the farm, and where it is
	data :nil params_buf :nil grads_buf :nil jobs :nil fetch :nil
	params :nil net :nil out 0 answers (list) busy :nil training :nil after :nil test_next :nil
	step 0 epoch 0 order :nil at 0 losses (list) tests (list) confusion :nil right 0
	;each epoch's check, the best of them and its params, the epochs since
	;one was better, and what the rate is cut to, halved at each of them
	+patience 3 valids (list) best_right -1 best_params :nil best_epoch 0 misses 0
	factor (n2d 1) stopped :nil
	;what is shown
	view :nil scratch :nil x :nil shown 0 show_t 0 features_t 0 last_t 0
	dirty_loss :t dirty_features :t dirty_confusion :t samples 0 samples_t 0 speed 0)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "MNIST" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_reset +sym_play +sym_step +sym_grid) +event_reset))
		(ui-label _ (:text " rate" :font :font_body :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *rate_bar* ("0.05" "0.1" "0.2" "0.5") (:font :font_body)) :connect +event_rate)
		(ui-label _ (:text " batch" :font :font_body :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *batch_bar* ("16" "32" "64") (:font :font_body)) :connect +event_batch)
		(ui-label *status* (:text "" :font :font_body :min_width 420
			:flow_flags +flow_flag_align_vcenter))
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-element *net_canvas* (Canvas 560 504 1))
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-element *loss_canvas* (Canvas 480 236 1))
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-element *feat_canvas* (Canvas 240 268 1))
				(ui-element *conf_canvas* (Canvas 240 268 1))))))

(import "./config.inc")

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar* '("reset" "train" "step" "test"))
	(ui-tool-tips *rate_bar* '("rate" "rate" "rate" "rate"))
	(ui-tool-tips *batch_bar* '("batch" "batch" "batch" "batch")))

(defun rate-now ()
	; (rate-now) -> rate
	;the rate chosen, cut by each epoch that was no better
	(* (str-to-double (elem-get +mnist_rates rate)) factor))

(defun percent (right)
	; (percent right) -> str
	;so many right of 10000 as a percentage
	(cat (double-to-str (/ (n2d right) (n2d 100)) 2) "%"))

(defun show-status (&optional text)
	(. *status* :set_text (ifn text
		(cat " epoch " (str epoch) "  step " (str step) "  rate " (double-to-str (rate-now) 4)
			(if (> speed 0) (cat "  " (str speed) " a second") "")
			(if (nempty? valids) (cat "  valid " (percent (second (last valids)))) "")
			(if (nempty? tests) (cat "  test " (percent (second (last tests)))) "")
			(ifn stopped "")))))

(defun buttons ()
	;the train button lit while it trains, and its symbol pause, to stop it
	(. (second (. *main_toolbar* :children)) :set_text (num-to-utf8 (if training +sym_pause +sym_play)))
	(toolbar-states *main_toolbar* (list :nil training :nil :nil)))

;;;;;;;;;;
; the data
;;;;;;;;;;

(defun show-sample ()
	;the digit shown run through the weights as they are now, and the view
	;eased to what it makes of it
	(when data
		(defq pixels (buf-get data (+ +mnist_test_images (* shown +mnist_inputs)) +mnist_inputs))
		(nn-forward *mnist_model* net (mnist-input pixels x) scratch)
		(nn-view-show view (first scratch) (buf-get data (+ +mnist_test_labels shown) +type_ubyte))))

(defun show-next ()
	;the next digit of the test set
	(setq shown (% (inc shown) +mnist_test) show_t (pii-time))
	(show-sample))

(defun data-ready (then)
	; (data-ready then) -> :t | :nil
	;the data set loaded, or its fetch started and then done after it
	(cond
		(data :t)
		((nempty? (mnist-missing))
			(setq after then)
			(unless fetch
				(elem-set select +select_fetch (defq reply (mail-mbox)))
				(mail-send (setq fetch (open-child (cat *app_root* "fetch.lisp") +kn_call_pin)) reply)
				(show-status " fetching the data set"))
			:nil)
		(:t (setq data (mnist-load (buf +mnist_data_bytes))
				params_buf (buf +mnist_params_bytes) grads_buf (buf (mnist-slice +jobs)))
			(show-sample)
			:t)))

(defun farm-start ()
	;the children, on the nodes of this machine, the first time
	(unless jobs
		(setq jobs (Jobs (cat *app_root* "child.lisp")
			(elem-get select +select_task) (elem-get select +select_reply) (list +max_int 2 2)))))

(defun farm-forget ()
	;what the farm has still to answer is not wanted, its answers go to a
	;mailbox let go of
	(when jobs
		(elem-set select +select_reply (mail-mbox))
		(. jobs :reply_to (elem-get select +select_reply)))
	(setq busy :nil out 0 answers (list)))

(defun job-msg (kind slot samples)
	(cat (setf-> (str-alloc +mnist_job_size) (+mnist_job_kind kind) (+mnist_job_step step)
			(+mnist_job_slot slot) (+mnist_job_count (length samples))
			(+mnist_job_data_key (buf-key data)) (+mnist_job_params_key (buf-key params_buf))
			(+mnist_job_grads_key (buf-key grads_buf)) (+mnist_job_children +jobs))
		(apply (const cat) (map (# (char %0 +int_size)) samples))))

(defun jobs-give (kind msgs)
	;the params where the children read them, and the jobs out
	(farm-start)
	(buf-set params_buf 0 (* +mnist_params +long_size) (nn-bytes params))
	(setq step (inc step) busy kind out (length msgs) answers (list))
	(. jobs :add msgs))

(defun shuffled (n)
	(defq order (map! (const !) '() 0 n))
	;each swapped with one at random at or before it, Fisher and Yates
	(each! (lambda (v) (defq j (random (inc (!))))
			(elem-set order (!) (elem-get order j)) (elem-set order j v)) (list order))
	order)

(defun step-start ()
	;a batch, a part of it to each job, the next of the shuffled order
	(defq per (elem-get +mnist_batches batch) total (* per +jobs))
	(unless order (setq order (shuffled +mnist_used)))
	(jobs-give :train (map! (lambda () (job-msg +mnist_kind_train (!)
			(slice order (+ at (* (!) per)) (+ at (* (inc (!)) per))))) '() 0 +jobs))
	(setq at (+ at total)))

(defun test-start ()
	;all the test set, a part to each job
	(defq per (/ +mnist_test +jobs))
	(jobs-give :test (map! (lambda () (job-msg +mnist_kind_test (!)
			(map! (const !) '() (* (!) per) (* (inc (!)) per)))) '() 0 +jobs)))

(defun valid-start ()
	;the part of the training set kept back, a part to each job
	(defq per (/ (+ +mnist_valid (dec +jobs)) +jobs))
	(jobs-give :valid (map! (lambda () (job-msg +mnist_kind_valid (!)
			(map! (const !) '() (+ +mnist_used (* (!) per)) (min +mnist_train (+ +mnist_used (* (inc (!)) per))))))
		'() 0 +jobs)))

(defun train-start ()
	(setq training :t stopped :nil)
	(buttons)
	(unless busy (step-start)))

(defun next ()
	;after a step or a test, the next: a test asked for while it was busy,
	;and if it is training the check of the kept back part at the end of an
	;epoch, else a step
	(cond
		;a test asked for while it was busy
		(test_next
			(setq test_next :nil)
			(test-start))
		((not training))
		((> (+ at (* (elem-get +mnist_batches batch) +jobs)) +mnist_used)
			(setq order :nil at 0 epoch (inc epoch))
			(valid-start))
		(:t (step-start))))

(defun stepped ()
	;the slices added up, and the params moved down them
	(defq bytes (* +mnist_params +long_size) sum (push (doubles) (buf-get grads_buf 0 bytes))
		total (* (elem-get +mnist_batches batch) +jobs))
	(each! (lambda () (nums-add sum (push (doubles) (buf-get grads_buf (mnist-slice (!)) bytes)) sum))
		'() 1 +jobs)
	(nums-add params (nums-scale sum (/ (neg (rate-now)) (n2d total)) sum) params)
	(setq net (nn-net *mnist_model* params) dirty_loss :t)
	;idle, the same digit again, to see what this step did to it
	(unless training (show-sample))
	(push losses (/ (reduce (# (+ %0 (getf %1 +mnist_reply_loss))) answers (n2d 0)) (n2d total))))

(defun tested ()
	;how many right, and the confusion of all of them
	(setq confusion (fill (nums) 100 0) right 0)
	(each (lambda (msg)
			(setq right (+ right (getf msg +mnist_reply_right)))
			(each! (lambda (n) (elem-set confusion (!)
					(+ n (code msg +int_size (+ +mnist_reply_size (* (!) +int_size))))))
				(list confusion)))
		answers)
	(push tests (list (length losses) right))
	(setq dirty_confusion :t dirty_loss :t))

(defun validated ()
	;how many of the kept back part are right, of 10000. Better than the
	;best, these params kept; no better, the rate halved, and at the third
	;such epoch the training stopped, the best params put back, and tested
	(defq got (* (reduce (# (+ %0 (getf %1 +mnist_reply_right))) answers 0) (/ 10000 +mnist_valid)))
	(push valids (list (length losses) got))
	(cond
		((> got best_right)
			(setq best_right got best_params (cat params) best_epoch epoch misses 0))
		((>= (setq misses (inc misses)) +patience)
			(setq training :nil params (cat best_params) net (nn-net *mnist_model* params) test_next :t
				stopped (cat "  stopped, best epoch " (str best_epoch)))
			(buttons)
			(show-sample))
		(:t (setq factor (* factor (n2d 0.5)))))
	(setq dirty_loss :t))

(defun answered (msg)
	;an answer of a job, and when all are in, what they come to
	(when (defq left (. jobs :answered msg))
		(push answers msg)
		(setq samples (+ samples (getf msg +mnist_reply_count)))
		(when (= left 0)
			(defq kind busy)
			(setq busy :nil)
			(case kind
				(:train (stepped))
				(:valid (validated))
				(:test (tested)))
			(next))))


(defun fetched (msg)
	;what the fetch says, a file started on, all kept, or why not
	(case (first msg)
		("+" (show-status (cat " fetching " (rest msg))))
		("." (setq fetch :nil)
			(when (data-ready :nil)
				(defq then after)
				(setq after :nil)
				(case then
					(:train (train-start))
					(:step (step-start))
					(:test (test-start)))))
		("!" (setq fetch :nil after :nil training :nil)
			(buttons)
			(show-status (cat " could not fetch: " (rest msg))))))

(defun reset ()
	(farm-forget)
	(setq training :nil test_next :nil params (nn-init *mnist_model*) step 0 epoch 0 order :nil at 0
		losses (list) tests (list) confusion :nil right 0
		valids (list) best_right -1 best_params :nil best_epoch 0 misses 0 factor (n2d 1) stopped :nil
		dirty_loss :t dirty_features :t dirty_confusion :t)
	(setq net (nn-net *mnist_model* params))
	(nn-view-weights view net)
	(show-sample)
	(buttons))

;;;;;;;;;;;;
; the frames
;;;;;;;;;;;;

(defun frame ()
	(defq now (pii-time) dt (min (n2q 0.1) (/ (n2q (- now last_t)) 1000000.0)))
	(setq last_t now)
	;training, a new test digit through the network each second
	(and data training (> (- now show_t) +show_rate) (show-next))
	(nn-view-draw view *net_canvas* dt (if data (buf-get data (+ +mnist_test_images (* shown +mnist_inputs)) +mnist_inputs)))
	(. *net_canvas* :swap +swap_write)
	;the charts when they have changed, the weights every few seconds
	(when dirty_loss
		(setq dirty_loss :nil)
		(draw-loss *loss_canvas* losses valids tests))
	(when (or dirty_features (and training (> (- now features_t) +features_rate)))
		(setq dirty_features :nil features_t now)
		(draw-features *feat_canvas* params)
		(nn-view-weights view net))
	(when dirty_confusion
		(setq dirty_confusion :nil)
		(draw-confusion *conf_canvas* confusion right))
	;how many a second, over the last second
	(when (> (- now samples_t) 1000000)
		(setq speed samples samples 0 samples_t now)
		(and data (not fetch) (show-status))))

(defun main ()
	(setq select (task-mboxes +select_size))
	(tooltips)
	(config-load)
	(config-apply)
	;antialiased, the words, the lines and the curves of the charts
	(each (# (. %0 :set_canvas_flags +canvas_flag_antialias))
		(list *net_canvas* *loss_canvas* *feat_canvas* *conf_canvas*))
	(setq view (nn-view *mnist_model* 560 504) scratch (nn-scratch *mnist_model*)
		x (fill (doubles) +mnist_inputs (n2d 0)))
	(reset)
	(show-status " ready, the data set is fetched when it is first wanted")
	(. *window* :open)
	(setq last_t (pii-time))
	(mail-timeout (elem-get select +select_timer) +frame_rate 0)
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (defq id (getf msg +ev_msg_target_id)) +event_close)
						(setq *running* :nil))
					((= id +event_reset) (reset))
					((and (= id (. *net_canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0) (> (getf msg +ev_msg_mouse_count) 0))
						;a click, the next digit
						(show-next))
					((= id +event_train)
						(cond
							(training (setq training :nil) (buttons))
							((data-ready :train) (train-start))
							(:t (setq training :t) (buttons))))
					((= id +event_step)
						(setq training :nil)
						(buttons)
						(and (not busy) (data-ready :step) (step-start)))
					((= id +event_test)
						;busy, it is done when the step ends
						(cond
							(busy (setq test_next :t))
							((data-ready :test) (test-start))))
					((= id +event_rate) (setq rate (. *rate_bar* :get_selected)))
					((= id +event_batch) (setq batch (. *batch_bar* :get_selected)))
					((. *window* :event msg))))
			(+select_task (. jobs :launched msg))
			(+select_reply (answered msg))
			(+select_fetch (fetched msg))
			(+select_tip (. *window* :tip msg))
			(:t (defq start (pii-time))
				(if jobs (. jobs :refresh (task-timeout 30)))
				(frame)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +frame_rate (- (pii-time) start))) 0))))
	(if jobs (. jobs :close))
	(config-save)
	(. *window* :close))
