(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/task/farm.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/gui.inc")
(import "./app.inc")

(enums +event 0
	(enum close)
	(enum mode))

(enums +select 0
	(enum main task reply timer))

(defq +width 640 +height 480 +scale 1 +line_batch 8 +steps 1000
	;the timer ticks at the rate of a GPU frame, the slow jobs are done every so many
	+timer_rate (/ 1000000 60) +slow_ticks 30 ticks 0 +retry_timeout (task-timeout 5)
	program (shader-load +shader_file) controls (list)
	jobs (list) tiles (list) farm :nil select :nil id :t
	start_time (pii-time) frame_time 0
	;the shader on the GPU, if the host GUI driver can draw one, and
	;the frames it has drawn since the status line was last set
	gpu_shader :nil gpu_mode :nil gpu_frames 0 gpu_time 0
	;a notice on the status line stays for a while
	+notice_time 4000000 notice_until 0)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Surface" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font *env_body_font*)) :connect +event_mode)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill :font *env_body_font*)
		(ui-grid *names* (:grid_width 1))
		(ui-grid *values* (:grid_width 1))
		(ui-grid *sliders* (:grid_width 1)))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-canvas *canvas* +width +height +scale))

(defun control-value ((name type lo hi slider label val init))
	;the value of the input, from where its slider is, and the
	;default to the last digit until the slider is moved
	(defq v (get :value slider))
	(cond
		((eql type :int) (+ lo v))
		((= v init) (shader-real val))
		((+ (shader-real lo) (/ (* (- (shader-real hi) (shader-real lo)) (n2r v))
			(const (n2r +steps)))))))

(defun control-text (type val)
	(if (eql type :int) (str val) (real-to-str val 4)))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun make-label (text)
	(defq label (Label))
	(def label :text text :border *env_label_border* :min_width 64
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))
	label)

(defun make-controls ()
	;a slider for each input of the shader that has a range, the app
	;gives the others, the time and the size of the frame
	(each (lambda ((name type val lo hi))
		(when (and lo hi)
			(defq slider (Slider) label (make-label "")
				steps (if (eql type :int) (- hi lo) +steps))
			(def slider :maximum steps :portion (max 1 (/ steps 10))
				:min_width 256 :color *env_slider_col*
				:value (defq init (if (eql type :int) (- val lo)
					(n2i (/ (* (- (shader-real val) (shader-real lo)) (n2r steps))
						(- (shader-real hi) (shader-real lo)))))))
			(. *names* :add_child (make-label (str name)))
			(. *values* :add_child label)
			(. *sliders* :add_child slider)
			(push controls (list name type lo hi slider label val init))))
		(first program)))

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> job
					(+job_key key)
					(+job_reply (elem-get select +select_reply)))))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task (const (cat *app_root* "child.lisp")) (elem-get nodes (random (length nodes)))
		+kn_call_run key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun start-frame ()
	;the inputs block for this frame, from the time and the controls,
	;goes out with every tile of it
	(defq vals (list
			(list 'time (/ (n2r (- (setq frame_time (pii-time)) start_time)) (const (n2r 1000000))))
			(list 'resolution (list +width +height))))
	(each (lambda (control)
		(bind '(name type lo hi slider label &rest _) control)
		(defq val (control-value control))
		(push vals (list name val))
		(set-label label (control-text type val))) controls)
	(defq inputs (shader-pack program vals))
	(cond
		(gpu_mode
			;the GPU draws the frame into the texture of the canvas
			(. *canvas* :shade gpu_shader inputs)
			(++ gpu_frames))
		(:t ;the nodes shade it, a tile each, as native code
			(setq tiles (range 0 +height +line_batch)
				jobs (map (lambda (y)
					(cat (setf-> (str-alloc +job_size)
						(+job_x 0)
						(+job_y y)
						(+job_x1 +width)
						(+job_y1 (min +height (+ y +line_batch)))
						(+job_height +height)) inputs)) tiles))
			;wake the children that have no job
			(. farm :each (lambda (key val)
				(if (and (get :child val) (not (get :job val)))
					(dispatch-job key val)))))))

(defun set-mode ()
	;the CPU or GPU button was pressed
	(defq gpu (= (. *mode* :get_selected) 1))
	(cond
		((and gpu (not gpu_shader))
			(. *mode* :set_selected 0)
			(setq notice_until (+ (pii-time) +notice_time))
			(set-label *status* "No GPU, this host GUI driver can not draw a shader, see make gui GUI=sdl3"))
		((not (eql gpu gpu_mode))
			;drop what is left of a CPU frame, a tile that comes in late is not shown
			(setq gpu_mode gpu jobs (list) tiles (list) gpu_frames 0 gpu_time (pii-time) ticks 0)
			(unless gpu
				;a child with no work for a while has gone, so back on the CPU
				;every child is started again, and takes a tile as it comes up
				(defq keys (list) vals (list))
				(. farm :each (# (push keys %0) (push vals %1)))
				(each (# (. farm :restart %0 %1)) keys vals)
				(setq jobs (list)))
			(start-frame))))

(defun main ()
	(setq select (task-mboxes +select_size))
	(make-controls)
	(.-> *canvas* (:fill +argb_black) (:swap 0))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq farm (Farm create destroy (length (lisp-nodes)))
		gpu_shader (shader-gui program))
	(. *mode* :set_selected 0)
	(start-frame)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((= id +event_mode)
						(set-mode))
					((. *window* :event msg))))
			(+select_task
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(def val :child child)
					(dispatch-job key val)))
			(+select_reply
				;child response
				(bind '(key x y x1 y1) (getf-> (slice msg (- -1 +job_reply) -1)
					+job_key +job_x +job_y +job_x1 +job_y1))
				(when (defq val (. farm :find key))
					(dispatch-job key val))
				(unless gpu_mode (. *canvas* :tile msg x y x1 y1))
				(when (defq i (find y tiles))
					(setq tiles (erase tiles i (inc i)))
					(when (empty? tiles)
						;the frame is done, show it and start the next
						(. *canvas* :swap 0)
						(if (> (pii-time) notice_until)
							(set-label *status* (cat "Frame "
								(str (/ (- (pii-time) frame_time) 1000)) "ms, "
								(str (length (lisp-nodes))) " nodes, native code, no GPU")))
						(start-frame))))
			(:t ;timer event, a GPU frame every tick
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(if gpu_mode (start-frame))
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					(cond
						(gpu_mode
							(defq now (pii-time))
							(set-label *status* (cat "GPU, "
								(str (/ (* gpu_frames 1000000) (- now gpu_time))) " frames a second"))
							(setq gpu_frames 0 gpu_time now))
						((. farm :refresh +retry_timeout)))))))
	;close window and children
	(if gpu_shader (canvas-shader-destroy gpu_shader))
	(. farm :close)
	(gui-sub-rpc *window*))
