(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")
(import "./data.inc")
(import "lib/nn/view.inc")
(import "lib/gpu/render_farm.inc")
(import "./draw.inc")

;Handwritten digits, MNIST, learnt, by the net chosen, a dense one or
;LeNet-5, on the farm or the GPU. The data set is fetched the first time
;it is wanted and kept, and loaded into a buf the farm's children read
;their pictures from. Each step the app writes the params into a second
;buf and gives each child a part of the batch; each sums the gradients of
;its samples into its own slice of a third, and the app adds the slices up
;and moves the params down them. Or the GPU does the lot, its params kept
;there: a farm too, of one task on another node, given a step a job. The
;farms are a (Render-work)'s, lib/gpu/render_farm.inc, with no canvas,
;the work is not a picture. A farm lasts while its work goes on, and is
;let go of when it stops or is no longer wanted; what starts again makes
;a new one.
;The end of each epoch is a check of the part of the training set kept
;back, and a test is of all 10000, the share right, and the confusion of
;them

(enums +event +event_user
	(enum reset train step test)
	(enum rate)
	(enum batch)
	(enum mode)
	(enum net))

;by priority, mail-select takes the first that has mail: what the user
;does first, the timer last so a slow frame starves nothing
(enums +select 0
	(enum main tip fetch timer))

(defq +digits_rates ''("0.05" "0.1" "0.2" "0.5")
	;a frame each twentieth of a second, a new sample shown each second
	+frame_rate 50000 +show_rate 1000000 +features_rate 2000000
	*running* :t rate 1 batch 1 select :nil
	;the net chosen, its model and the bytes of its params
	which 0 model (first *digits_models*) params_size (digits-params-size (first *digits_models*))

	;the bufs, the farms, an answer of theirs not yet looked at, and where
	;it is
	data :nil params_buf :nil grads_buf :nil render :nil heard :nil fetch :nil
	params :nil net :nil answers (list) busy :nil training :nil after :nil test_next :nil
	step 0 epoch 0 order :nil at 0 losses (list) tests (list) confusion :nil right 0
	;the number a job is given, which tells a child of the farm if it has
	;the params of the job already: never one used before, as the step is
	;when the net is made afresh
	serial 0
	;each epoch's check, the best of them and its params, the epochs since
	;one was better, and what the rate is cut to, halved at each of them
	+patience 3 valids (list) best_right -1 best_params :nil best_epoch 0 misses 0
	factor (n2d 1) stopped :nil
	;the GPU, if it is chosen: a farm too, of one task, as the CPU's is of
	;many. What is out is its, and is to write its params back; the params
	;here newer than its, its params newer than here, when they last came
	;back, and the CPU's farm wanted once they have
	has_gpu (> (gpu-format) 0) gpu_mode :nil gave_gpu :nil gpu_synced :nil gpu_stale :t gpu_ahead :nil
	sync_t 0 to_farm :nil
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
		(ui-title _ (:text "modes" :paper :paper_title :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *mode_bar* ("CPU" "GPU") (:font :font_body :paper :paper_toolbar2)) :connect +event_mode)
		(ui-label _ (:text " " :font :font_body :flow_flags +flow_flag_align_vcenter))
		(. (ui-radio-bar *net_bar* ("dense" "LeNet") (:font :font_body :paper :paper_toolbar2)) :connect +event_net)
		;room for the longest status, the toolbar no wider than the canvases
		(ui-label *status* (:text "" :font :font_body :min_width 440
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
	(ui-tool-tips *mode_bar* '("farm" "GPU"))
	(ui-tool-tips *net_bar* '("dense" "LeNet-5")))

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
		(nn-forward model net (digits-input pixels x) scratch)
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
				grads_buf (buf (digits-slice (inc +digits_jobs))))
			(show-sample)
			:t)))

(defun farm-forget ()
	;a farm lasts only while its work goes on. When what it has out is no
	;longer wanted, a reset or a switch of form, or there is no more to
	;give it, it is let go of, its children told to go, and the next work
	;makes a new one, its mailboxes new from here. So nothing late comes
	;in, and no child that may have gone, given nothing for a while, is
	;ever given a job. A new task of the GPU's has no params, so loads
	;them first
	(. render :stop)
	(setq busy :nil answers (list) heard :nil gpu_stale :t))

(defun job-msg (kind slot samples)
	(cat (setf-> (str-alloc +digits_job_size) (+digits_job_kind kind) (+digits_job_step serial)
			(+digits_job_slot slot) (+digits_job_count (length samples))
			(+digits_job_data_key (buf-key data)) (+digits_job_params_key (buf-key params_buf))
			(+digits_job_grads_key (buf-key grads_buf)) (+digits_job_children +digits_jobs) (+digits_job_net which))
		(apply (const cat) (map (# (char %0 +int_size)) samples))))

(defun jobs-give (kind msgs)
	;the params where the children read them, and the jobs out, to the
	;CPU's farm, its children on the nodes of this machine, made when
	;there is work for it
	(buf-set params_buf 0 params_size params)
	(setq step (inc step) serial (inc serial) busy kind answers (list) gave_gpu :nil)
	(. render :mode :cpu)
	(. render :frame msgs))

(defun gpu-give (kind samples sync)
	;a job to the GPU's farm, one task kept off this node, made when there
	;is work for it: a step, a check, or with no samples only its params
	;back. The
	;params put where the task loads them from first if they have changed
	;here, and its params written back after the job if sync
	(defq flags 0)
	(when gpu_stale
		(buf-set params_buf 0 params_size params)
		(setq gpu_stale :nil flags +digits_gpu_flag_load))
	(when sync
		(setq sync_t (pii-time) flags (logior flags +digits_gpu_flag_sync)))
	(defq msg (cat (setf-> (str-alloc +digits_gpu_job_size)
			(+digits_job_kind (case kind (:test +digits_kind_test) (:valid +digits_kind_valid) (:t +digits_kind_train)))
			(+digits_job_step serial) (+digits_job_count (length samples))
			(+digits_job_data_key (buf-key data)) (+digits_job_params_key (buf-key params_buf))
			(+digits_job_grads_key (buf-key grads_buf)) (+digits_job_children +digits_jobs) (+digits_job_net which)
			(+digits_gpu_job_flags flags) (+digits_gpu_job_rate (rate-now)))
		(apply (const cat) (cat '("") (map (# (char %0 +int_size)) samples)))))
	(setq step (inc step) serial (inc serial) busy kind answers (list) gave_gpu :t gpu_synced sync)
	(. render :mode :gpu)
	(. render :frame (list msg)))

(defun farm-launched (kind msg)
	;a task of the GPU's farm has started, the first, or one in place of one
	;that went. It has no params, and loads them from their buf before its
	;first job, so those here are put there, the latest there are
	(when (eql kind :gpu)
		(buf-set params_buf 0 params_size params)
		(setq gpu_ahead :nil)))

(defun shuffled (n)
	(defq order (map! (const !) '() 0 n))
	;each swapped with one at random at or before it, Fisher and Yates
	(each! (lambda (v) (defq j (random (inc (!))))
			;55000 of them at the start of each epoch, the GUI is let in
			(if (= (logand (!) 1023) 0) (task-slice))
			(elem-set order (!) (elem-get order j)) (elem-set order j v)) (list order))
	order)

(defun step-start ()
	;a batch, a part of it to each job, the next of the shuffled order
	(defq per (elem-get +digits_batches batch) total (* per +digits_jobs))
	(unless order (setq order (shuffled +digits_used)))
	(cond
		(gpu_mode
			;its params back after a step by hand, and every so often as it
			;trains, to be shown
			(gpu-give :train (slice order at (+ at total))
				(or (not training) (> (- (pii-time) sync_t) +features_rate)))
			(setq at (+ at total)))
		(:t (jobs-give :train (map! (lambda () (job-msg +digits_kind_train (!)
				(slice order (+ at (* (!) per)) (+ at (* (inc (!)) per))))) '() 0 +digits_jobs))
			(setq at (+ at total)))))

(defun test-start ()
	;all the test set, a part to each job
	(defq per (/ +mnist_test +digits_jobs))
	(cond
		(gpu_mode (gpu-give :test (map! (const !) '() 0 +mnist_test) :nil))
		(:t (jobs-give :test (map! (lambda () (job-msg +digits_kind_test (!)
				(map! (const !) '() (* (!) per) (* (inc (!)) per)))) '() 0 +digits_jobs)))))

(defun valid-start ()
	;the part of the training set kept back, a part to each job
	(defq per (/ (+ +digits_valid (dec +digits_jobs)) +digits_jobs))
	(cond
		;the params back after it, to keep if they are the best
		(gpu_mode (gpu-give :valid (map! (const !) '() +digits_used +mnist_train) :t))
		(:t (jobs-give :valid (map! (lambda () (job-msg +digits_kind_valid (!)
				(map! (const !) '() (+ +digits_used (* (!) per)) (min +mnist_train (+ +digits_used (* (inc (!)) per))))))
			'() 0 +digits_jobs)))))

(defun step-next ()
	;a step, if the epoch has a batch left for it, else the end of the
	;epoch, the check of the kept back part. Every step is started by this:
	;one asked for by hand, or as it trains again, may be at an epoch's end,
	;and the batch may have been made bigger since the last
	(cond
		((> (+ at (* (elem-get +digits_batches batch) +digits_jobs)) +digits_used)
			(setq order :nil at 0 epoch (inc epoch))
			(valid-start))
		(:t (step-start))))

(defun train-start ()
	(setq training :t stopped :nil)
	(buttons)
	(unless busy (step-next)))

(defun next ()
	;after a step or a test, the next: a test asked for while it was busy,
	;and if it is training the check of the kept back part at the end of an
	;epoch, else a step
	(cond
		;the farm wanted, the GPU's params back first
		(to_farm (gpu-give :sync (list) :t))
		;a test asked for while it was busy
		(test_next
			(setq test_next :nil)
			(test-start))
		;stopped: what the GPU has learnt since its params last came back is
		;asked for, they go with its farm
		((not training) (and gpu_mode gpu_ahead (gpu-give :sync (list) :t)))
		(:t (step-next)))
	;nothing was given: there is no more for the farms
	(unless busy (farm-forget)))

(defun stepped ()
	;the slices added up, and the params moved down them
	(defq bytes params_size sum (push (nn-vector model) (buf-get grads_buf 0 bytes))
		total (* (elem-get +digits_batches batch) +digits_jobs))
	(each! (lambda () (nums-add sum (push (nn-vector model) (buf-get grads_buf (digits-slice (!)) bytes)) sum))
		'() 1 +digits_jobs)
	(nums-add params (nums-scale sum (nn-num model (/ (neg (rate-now)) (n2d total))) sum) params)
	(setq net (nn-net model params) dirty_loss :t)
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
			(setq training :nil params (cat best_params) net (nn-net model params) test_next :t gpu_stale :t
				stopped (cat "  best epoch " (str best_epoch)))
			(buttons)
			(show-sample))
		(:t (setq factor (* factor (n2d 0.5)))))
	(setq dirty_loss :t))

(defun validated ()
	(valid-done (* (reduce (# (+ %0 (getf %1 +digits_reply_right))) answers 0) (/ 10000 +digits_valid))))

(defun gpu-params ()
	;the params the GPU's task wrote back
	(setq params (push (nn-vector model) (buf-get grads_buf (digits-slice +digits_jobs) params_size))
		net (nn-net model params) dirty_features :t gpu_ahead :nil))

(defun gpu-answered (msg)
	;an answer of the GPU's farm: a job done, or there is no GPU there. Its
	;params are taken if the job wrote them back, then what the job came
	;to, and the next
	(defq count (getf msg +digits_reply_count) kind busy synced gpu_synced)
	(setq busy :nil)
	(cond
		((< count 0)
			;the CPU's farm after all
			(farm-forget)
			(setq has_gpu :nil gpu_mode :nil to_farm :nil)
			(. *mode_bar* :set_selected 0))
		(:t (if synced (gpu-params))
			(setq samples (+ samples count))
			(case kind
				(:train (unless synced (setq gpu_ahead :t))
					(push losses (/ (getf msg +digits_reply_loss) (n2d count)))
					(setq dirty_loss :t)
					;idle, the same digit again, to see what this step did to it
					(unless training (show-sample)))
				(:valid (push answers msg) (validated))
				(:test (push answers msg) (tested))
				;the CPU's farm from here, the GPU's let go of
				(:sync (when to_farm
					(farm-forget)
					(setq gpu_mode :nil to_farm :nil))))))
	(next))

(defun mode-switch (to_gpu)
	;to the GPU, the farm's work let go of and given the GPU's task, the
	;params loaded first; to the farm at once if the GPU has nothing newer
	;than here, no task yet or params not loaded, else once the GPU's task
	;has done what it has out and its params are back
	(cond
		((and (not to_gpu) (not busy) gpu_stale)
			(setq gpu_mode :nil to_farm :nil))
		((not to_gpu) (setq to_farm :t)
			(unless busy (next)))
		(gpu_mode (setq to_farm :nil))
		(:t (defq kind busy)
			(farm-forget)
			(setq gpu_mode :t gpu_stale :t)
			(case kind
				(:valid (valid-start))
				(:test (test-start))
				(:t (next))))))

(defun answered (msg)
	;an answer of a job of the CPU's farm, and when all are in, what they
	;come to
	(push answers msg)
	(setq samples (+ samples (getf msg +digits_reply_count)))
	(unless (. render :busy)
		(defq kind busy)
		(setq busy :nil)
		(case kind
			(:train (stepped))
			(:valid (validated))
			(:test (tested)))
		(next)))

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
					(:step (step-next))
					(:test (test-start)))))
		("!" (setq fetch :nil after :nil training :nil)
			(buttons)
			(show-status (cat " could not fetch: " (rest msg))))))

(defun reset ()
	(farm-forget)
	(setq training :nil test_next :nil params (nn-init model) step 0 epoch 0 order :nil at 0
		gpu_stale :t
		losses (list) tests (list) confusion :nil right 0
		valids (list) best_right -1 best_params :nil best_epoch 0 misses 0 factor (n2d 1) stopped :nil
		dirty_loss :t dirty_features :t dirty_confusion :t)
	;the farm wanted, it is had now, the new params are here
	(if to_farm (setq gpu_mode :nil to_farm :nil))
	(setq net (nn-net model params))
	(nn-view-weights view net)
	(show-sample)
	(buttons))

(defun net-switch (to)
	;another net: what is out let go of, its view made, and its params
	;afresh
	(farm-forget)
	(setq which to model (elem-get *digits_models* to) params_size (digits-params-size model)
		view (nn-view model 560 504) scratch (nn-scratch model)
		x (fill (nn-vector model) +digits_inputs (nn-num model 0)))
	;the GPU's params are of the net before, so load the new ones first
	(setq gpu_stale :t)
	(reset))

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
		(draw-features *feat_canvas* model params)
		(nn-view-weights view net))
	(when dirty_confusion
		(setq dirty_confusion :nil)
		(draw-confusion *conf_canvas* confusion right))
	;how many a second, over the last second
	(when (> (- now samples_t) 1000000)
		(setq speed samples samples 0 samples_t now)
		(and data (not fetch) (show-status))))

(defun main ()
	;the farms: the CPU's two children on each node of this machine, the
	;GPU's one task. An answer is only kept by the render's mail, and
	;looked at here after it; and a farm is let go of when its work
	;stops, here, not by the render after a while
	(setq select (task-mboxes +select_size)
		render (Render-work :nil (cat *app_root* "child.lisp") (cat *app_root* "gpu.lisp")))
	(def render :reply_fn (lambda (msg) (setq heard msg)) :launched_fn (const farm-launched)
		:herd (list +max_int 2 2) :idle :nil :patience (task-timeout 30)
		:gpu_patience (task-timeout +digits_gpu_patience))
	(tooltips)
	(config-load)
	(config-apply)
	;antialiased, the words, the lines and the curves of the charts
	(each (# (. %0 :set_canvas_flags +canvas_flag_antialias))
		(list *net_canvas* *loss_canvas* *feat_canvas* *conf_canvas*))
	(net-switch which)
	;each canvas the colour of what is drawn on it, before the window is seen,
	;so till the first frame it is not bare
	(each (# (.-> %0 (:fill +chart_back) (:swap +swap_write)))
		(list *net_canvas* *loss_canvas* *feat_canvas* *conf_canvas*))
	(show-status " ready, the data set is fetched when it is first wanted")
	(. *window* :open)
	(setq last_t (pii-time))
	(mail-timeout (elem-get select +select_timer) +frame_rate 0)
	(while *running*
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;an answer of a farm's
				(when (defq answer heard)
					(setq heard :nil)
					(if gave_gpu (gpu-answered answer) (answered answer))))
			((= idx +select_main)
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
						(and (not busy) (data-ready :step) (step-next)))
					((= id +event_test)
						;busy, it is done when the step ends
						(cond
							(busy (setq test_next :t))
							((data-ready :test) (test-start))))
					((= id +event_rate) (setq rate (. *rate_bar* :get_selected)))
					((= id +event_batch) (setq batch (. *batch_bar* :get_selected)))
					((= id +event_net)
						(unless (= (defq to (. *net_bar* :get_selected)) which) (net-switch to)))
					((= id +event_mode)
						(defq to_gpu (and has_gpu (= (. *mode_bar* :get_selected) 1)))
						(. *mode_bar* :set_selected (if to_gpu 1 0))
						(unless (eql to_gpu (and gpu_mode (not to_farm))) (mode-switch to_gpu)))
					((. *window* :event msg))))
			((= idx +select_fetch) (fetched msg))
			((= idx +select_tip) (. *window* :tip msg))
			(:t (defq start (pii-time))
				;the patience of the farm: a task that has gone, or has said
				;nothing for it, is started again and its job given again
				(. render :tick)
				(frame)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +frame_rate (- (pii-time) start))) 0))))
	(. render :close)
	(config-save)
	(. *window* :close))
