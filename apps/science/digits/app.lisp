(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")
(import "./data.inc")
(import "lib/nn/view.inc")
(import "lib/gpu/compute.inc")
(import "./draw.inc")

;Handwritten digits, MNIST, learnt. The data set is fetched the first time
;it is wanted and kept, and loaded into a buf the farm's children read
;their pictures from. Each step the app writes the params into a second
;buf and gives each child a part of the batch; each sums the gradients of
;its samples into its own slice of a third, and the app adds the slices up
;and moves the params down them. Or a task of its own on another node has
;the GPU do the lot, its params kept there, given its steps a few ahead.
;The end of each epoch is a check of the part of the training set kept
;back, and a test is of all 10000, the share right, and the confusion of
;them

(enums +event +event_user
	(enum reset train step test)
	(enum rate)
	(enum batch)
	(enum mode))

;by priority, mail-select takes the first that has mail: what the user
;does first, the timer last so a slow frame starves nothing
(enums +select 0
	(enum main tip task reply gpu fetch timer))

(defq +digits_rates ''("0.05" "0.1" "0.2" "0.5") +digits_batches ''(16 32 64)
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
	;the GPU, if it is chosen: its task, the params here newer than its, the
	;step of a job out that loads them, the step whose params it writes back
	;and when, the farm wanted once it has them back, the generation of its
	;jobs, and how many steps it is given ahead
	has_gpu (> (gpu-format) 0) gpu_mode :nil gpu_child :nil gpu_stale :t loading :nil
	sync_step -1 sync_t 0 to_farm :nil gen 0 +gpu_ahead 8
	;what is shown
	view :nil scratch :nil x :nil shown 0 show_t 0 features_t 0 last_t 0
	dirty_loss :t dirty_features :t dirty_confusion :t samples 0 samples_t 0 speed 0)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Digits" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_reset +sym_play +sym_step +sym_grid) +event_reset))
		(ui-title _ (:text "rate" :paper :paper_title :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *rate_bar* ("0.05" "0.1" "0.2" "0.5") (:font :font_body :paper :paper_toolbar2)) :connect +event_rate)
		(ui-title _ (:text "batch" :paper :paper_title :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *batch_bar* ("16" "32" "64") (:font :font_body :paper :paper_toolbar2)) :connect +event_batch)
		(ui-label _ (:text " " :font :font_body :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *mode_bar* ("CPU" "GPU") (:font :font_body :paper :paper_toolbar2)) :connect +event_mode)
		(ui-label *status* (:text "" :font :font_body :min_width 600
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
	(ui-tool-tips *batch_bar* '("batch" "batch" "batch" "batch"))
	(ui-tool-tips *mode_bar* '("farm" "GPU")))

(defun rate-now ()
	; (rate-now) -> rate
	;the rate chosen, cut by each epoch that was no better
	(* (str-to-double (elem-get +digits_rates rate)) factor))

(defun percent (right)
	; (percent right) -> str
	;so many right of 10000 as a percentage
	(cat (double-to-str (/ (n2d right) (n2d 100)) 2) "%"))

(defun show-status (&optional text)
	(. *status* :set_text (ifn text
		(cat " epoch " (str epoch) "  step " (str step) "  rate " (double-to-str (rate-now) 4)
			(if (> speed 0) (cat "  " (str speed) "/s") "")
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
		(defq pixels (buf-get data (+ +mnist_test_images (* shown +digits_inputs)) +digits_inputs))
		(nn-forward *digits_model* net (digits-input pixels x) scratch)
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
				params_buf (buf +digits_params_bytes)
				;a slice each child of the farm, and one the GPU's task writes its params to
				grads_buf (buf (digits-slice (inc +jobs))))
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
	;and the GPU's task does no more of its jobs out
	(elem-set select +select_gpu (mail-mbox))
	(setq busy :nil out 0 answers (list) gen (inc gen) loading :nil))

(defun job-msg (kind slot samples)
	(cat (setf-> (str-alloc +digits_job_size) (+digits_job_kind kind) (+digits_job_step step)
			(+digits_job_slot slot) (+digits_job_count (length samples))
			(+digits_job_data_key (buf-key data)) (+digits_job_params_key (buf-key params_buf))
			(+digits_job_grads_key (buf-key grads_buf)) (+digits_job_children +jobs))
		(apply (const cat) (map (# (char %0 +int_size)) samples))))

(defun jobs-give (kind msgs)
	;the params where the children read them, and the jobs out
	(farm-start)
	(buf-set params_buf 0 (* +digits_params +long_size) params)
	(setq step (inc step) busy kind out (length msgs) answers (list))
	(. jobs :add msgs))

(defun gpu-give (kind samples sync)
	;a job to the GPU's task, started on another node the first time. The
	;params put where it loads them from first if they have changed here,
	;and its params written back after it if sync
	(unless gpu_child
		(defq me (task-nodeid) node me)
		(each (# (unless (eql %0 me) (setq node %0))) (lisp-nodes :t))
		(setq gpu_child (open-remote (cat *app_root* "gpu.lisp") node +kn_call_pin)))
	(defq flags 0)
	(when gpu_stale
		(buf-set params_buf 0 (* +digits_params +long_size) params)
		(setq gpu_stale :nil loading step flags +digits_gpu_flag_load))
	(when sync
		(setq sync_step step sync_t (pii-time) flags (logior flags +digits_gpu_flag_sync)))
	(mail-send gpu_child (cat (setf-> (str-alloc +digits_gpu_job_size)
			(+job_reply (elem-get select +select_gpu)) (+digits_job_kind kind) (+digits_job_step step)
			(+digits_job_slot gen) (+digits_job_count (length samples))
			(+digits_job_data_key (buf-key data)) (+digits_job_params_key (buf-key params_buf))
			(+digits_job_grads_key (buf-key grads_buf)) (+digits_job_children +jobs)
			(+digits_gpu_job_flags flags) (+digits_gpu_job_rate (rate-now)))
		(apply (const cat) (cat '("") (map (# (char %0 +int_size)) samples)))))
	(setq step (inc step) out (inc out)))

(defun gpu-steps ()
	;steps to the GPU's task while it trains, a few ahead so it does not
	;wait on the window, none past the end of the epoch, none after one that
	;loads the params till it is done, none once the farm or a test is
	;wanted, and the params back every so often
	(defq total (* (elem-get +digits_batches batch) +jobs))
	(while (and training (not loading) (not to_farm) (not test_next) (< out +gpu_ahead)
			(<= (+ at total) +digits_used))
		(gpu-give +digits_kind_train (slice order at (+ at total)) (> (- (pii-time) sync_t) +features_rate))
		(setq at (+ at total))))

(defun shuffled (n)
	(defq order (map! (const !) '() 0 n))
	;each swapped with one at random at or before it, Fisher and Yates
	(each! (lambda (v) (defq j (random (inc (!))))
			(elem-set order (!) (elem-get order j)) (elem-set order j v)) (list order))
	order)

(defun step-start ()
	;a batch, a part of it to each job, the next of the shuffled order
	(defq per (elem-get +digits_batches batch) total (* per +jobs))
	(unless order (setq order (shuffled +digits_used)))
	(cond
		(gpu_mode
			;idle, its params back after it, to be shown
			(setq busy :train answers (list))
			(gpu-give +digits_kind_train (slice order at (+ at total)) (not training))
			(setq at (+ at total))
			(gpu-steps))
		(:t (jobs-give :train (map! (lambda () (job-msg +digits_kind_train (!)
				(slice order (+ at (* (!) per)) (+ at (* (inc (!)) per))))) '() 0 +jobs))
			(setq at (+ at total)))))

(defun test-start ()
	;all the test set, a part to each job
	(defq per (/ +mnist_test +jobs))
	(cond
		(gpu_mode (setq busy :test answers (list))
			(gpu-give +digits_kind_test (map! (const !) '() 0 +mnist_test) :nil))
		(:t (jobs-give :test (map! (lambda () (job-msg +digits_kind_test (!)
				(map! (const !) '() (* (!) per) (* (inc (!)) per)))) '() 0 +jobs)))))

(defun valid-start ()
	;the part of the training set kept back, a part to each job
	(defq per (/ (+ +digits_valid (dec +jobs)) +jobs))
	(cond
		;the params back after it, to keep if they are the best
		(gpu_mode (setq busy :valid answers (list))
			(gpu-give +digits_kind_valid (map! (const !) '() +digits_used +mnist_train) :t))
		(:t (jobs-give :valid (map! (lambda () (job-msg +digits_kind_valid (!)
				(map! (const !) '() (+ +digits_used (* (!) per)) (min +mnist_train (+ +digits_used (* (inc (!)) per))))))
			'() 0 +jobs)))))

(defun train-start ()
	(setq training :t stopped :nil)
	(buttons)
	(unless busy (step-start)))

(defun next ()
	;after a step or a test, the next: a test asked for while it was busy,
	;and if it is training the check of the kept back part at the end of an
	;epoch, else a step
	(cond
		;the farm wanted, the GPU's params back first
		(to_farm (setq busy :sync answers (list))
			(gpu-give +digits_kind_train (list) :t))
		;a test asked for while it was busy
		(test_next
			(setq test_next :nil)
			(test-start))
		((not training))
		((> (+ at (* (elem-get +digits_batches batch) +jobs)) +digits_used)
			(setq order :nil at 0 epoch (inc epoch))
			(valid-start))
		(:t (step-start))))

(defun stepped ()
	;the slices added up, and the params moved down them
	(defq bytes (* +digits_params +long_size) sum (push (doubles) (buf-get grads_buf 0 bytes))
		total (* (elem-get +digits_batches batch) +jobs))
	(each! (lambda () (nums-add sum (push (doubles) (buf-get grads_buf (digits-slice (!)) bytes)) sum))
		'() 1 +jobs)
	(nums-add params (nums-scale sum (/ (neg (rate-now)) (n2d total)) sum) params)
	(setq net (nn-net *digits_model* params) dirty_loss :t)
	;idle, the same digit again, to see what this step did to it
	(unless training (show-sample))
	(push losses (/ (reduce (# (+ %0 (getf %1 +digits_reply_loss))) answers (n2d 0)) (n2d total))))

(defun test-done ()
	(push tests (list (length losses) right))
	(setq dirty_confusion :t dirty_loss :t))

(defun tested ()
	;how many right, and the confusion of all of them
	(setq confusion (fill (nums) 100 0) right 0)
	(each (lambda (msg)
			(setq right (+ right (getf msg +digits_reply_right)))
			(each! (lambda (n) (elem-set confusion (!)
					(+ n (code msg +int_size (+ +digits_reply_size (* (!) +int_size))))))
				(list confusion)))
		answers)
	(test-done))

(defun valid-done (got)
	;how many of the kept back part are right, of 10000. Better than the
	;best, these params kept; no better, the rate halved, and at the third
	;such epoch the training stopped, the best params put back, and tested
	(push valids (list (length losses) got))
	(cond
		((> got best_right)
			(setq best_right got best_params (cat params) best_epoch epoch misses 0))
		((>= (setq misses (inc misses)) +patience)
			(setq training :nil params (cat best_params) net (nn-net *digits_model* params) test_next :t gpu_stale :t
				stopped (cat "  best epoch " (str best_epoch)))
			(buttons)
			(show-sample))
		(:t (setq factor (* factor (n2d 0.5)))))
	(setq dirty_loss :t))

(defun validated ()
	(valid-done (* (reduce (# (+ %0 (getf %1 +digits_reply_right))) answers 0) (/ 10000 +digits_valid))))

(defun gpu-params ()
	;the params the GPU's task wrote back
	(setq params (push (doubles) (buf-get grads_buf (digits-slice +jobs) (* +digits_params +long_size)))
		net (nn-net *digits_model* params) dirty_features :t))

(defun gpu-answered (msg)
	;an answer of the GPU's task, a job done, or there is no GPU there. A
	;step answered while it trains, more given it, and when all it has are
	;answered what they come to and the next
	(bind '(s count) (getf-> msg +digits_reply_step +digits_reply_count))
	(setq out (dec out))
	(cond
		((< count 0)
			;the farm after all, what the GPU's task has out let go of
			(farm-forget)
			(setq has_gpu :nil gpu_mode :nil to_farm :nil gpu_stale :t)
			(. *mode_bar* :set_selected 0)
			(next))
		(:t (and loading (= s loading) (setq loading :nil))
			(if (= s sync_step) (gpu-params))
			(setq samples (+ samples count))
			(cond
				((eql busy :train)
					(push losses (/ (getf msg +digits_reply_loss) (n2d count)))
					(setq dirty_loss :t)
					;idle, the same digit again, to see what this step did to it
					(unless training (show-sample)))
				((push answers msg)))
			(cond
				((> out 0) (if (eql busy :train) (gpu-steps)))
				(:t (defq kind busy)
					(setq busy :nil)
					(case kind
						(:valid (validated))
						(:test (tested))
						(:sync (when to_farm
							(setq gpu_mode :nil to_farm :nil gpu_stale :t))))
					(next))))))

(defun mode-switch (to_gpu)
	;to the GPU, the farm's work let go of and given the GPU's task, the
	;params loaded first; to the farm at once if the GPU has nothing newer
	;than here, no task yet or params not loaded, else once the GPU's task
	;has done what it has out and its params are back
	(cond
		((and (not to_gpu) (not busy) (or (not gpu_child) gpu_stale))
			(setq gpu_mode :nil to_farm :nil))
		((not to_gpu) (setq to_farm :t)
			(unless busy (next)))
		(gpu_mode (setq to_farm :nil)
			(if (eql busy :train) (gpu-steps)))
		(:t (defq kind busy)
			(farm-forget)
			(setq gpu_mode :t gpu_stale :t)
			(case kind
				(:valid (valid-start))
				(:test (test-start))
				(:t (next))))))

(defun answered (msg)
	;an answer of a job, and when all are in, what they come to
	(when (defq left (. jobs :answered msg))
		(push answers msg)
		(setq samples (+ samples (getf msg +digits_reply_count)))
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
	(setq training :nil test_next :nil params (nn-init *digits_model*) step 0 epoch 0 order :nil at 0
		gpu_stale :t
		losses (list) tests (list) confusion :nil right 0
		valids (list) best_right -1 best_params :nil best_epoch 0 misses 0 factor (n2d 1) stopped :nil
		dirty_loss :t dirty_features :t dirty_confusion :t)
	;the farm wanted, it is had now, the new params are here
	(if to_farm (setq gpu_mode :nil to_farm :nil))
	(setq net (nn-net *digits_model* params))
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
	(nn-view-draw view *net_canvas* dt (if data (buf-get data (+ +mnist_test_images (* shown +digits_inputs)) +digits_inputs)))
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
	(setq view (nn-view *digits_model* 560 504) scratch (nn-scratch *digits_model*)
		x (fill (doubles) +digits_inputs (n2d 0)))
	(reset)
	;each canvas the colour of what is drawn on it, before the window is seen,
	;so till the first frame it is not bare
	(each (# (.-> %0 (:fill +chart_back) (:swap +swap_write)))
		(list *net_canvas* *loss_canvas* *feat_canvas* *conf_canvas*))
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
					((= id +event_mode)
						(defq to_gpu (and has_gpu (= (. *mode_bar* :get_selected) 1)))
						(. *mode_bar* :set_selected (if to_gpu 1 0))
						(unless (eql to_gpu (and gpu_mode (not to_farm))) (mode-switch to_gpu)))
					((. *window* :event msg))))
			(+select_task (. jobs :launched msg))
			(+select_reply (answered msg))
			(+select_gpu (gpu-answered msg))
			(+select_fetch (fetched msg))
			(+select_tip (. *window* :tip msg))
			(:t (defq start (pii-time))
				(if jobs (. jobs :refresh (task-timeout 30)))
				(frame)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +frame_rate (- (pii-time) start))) 0))))
	(if jobs (. jobs :close))
	(if gpu_child (mail-send gpu_child ""))
	(config-save)
	(. *window* :close))
