(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/compute.inc")
(import "lib/gpu/tile.inc")
(import "lib/gpu/shade.inc")
(import "./app.inc")

(enums +event +event_user
	(enum mode))

(enums +select 0
	(enum main task reply gpu_task gpu timer))

(defq +width 640 +height 480 +scale 1 +line_batch 8 +steps 1000
	;the pixels of the canvas are in shared memory if the host has it, and
	;the key they are under goes out with every tile
	shared_pixmap (pixmap-shared +width +height 0)
	shared_key (if shared_pixmap (pixmap-key shared_pixmap) 0)
	;the timer ticks at the rate of a GPU frame, the slow jobs are done every so many
	+timer_rate (/ 1000000 60) +slow_ticks 30 ticks 0
	;the patience of each farm: a tile is quick, the first frame of the GPU
	;waits for its kernel to be built
	+retry_timeout (task-timeout 5) +gpu_timeout (task-timeout 30)
	program (shader-load +shader_file) controls (list)
	select :nil id :t
	start_time (pii-time) frame_time 0
	;The frames are drawn by a farm, lib/task/jobs.inc, of many children on
	;the CPU, lib/gpu/tile.inc, or of one on the GPU, lib/gpu/shade.inc:
	;the one protocol and the one job, a tile of the frame, the GPU's all
	;of it. Each child draws on the pixels of the canvas, in shared memory.
	;A farm lasts while it draws the frames, and is closed when the other
	;takes over
	jobs :nil gpu_jobs :nil
	;the GPU is wanted, the button, if there is one; a frame is out to it;
	;and gpu_mode it is drawing the frames, which it does from its first,
	;once its kernel is built. Till then the CPU draws them, if it has
	;been started
	has_gpu (> (gpu-format) 0) gpu_want :nil gpu_out :nil gpu_mode :nil cpu_running :nil
	;the frames the GPU has drawn since the status line was last set
	gpu_frames 0 gpu_time 0 gpu_last 0 gpu_given 0
	;a notice on the status line stays for a while
	+notice_time 4000000 notice_until 0
	;the sliders and CPU or GPU as they were when last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "surface.tre"))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Surface" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font :font_body)) :connect +event_mode)
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-flow _ (:flow_flags +flow_right_fill :font :font_body)
		(ui-grid *names* (:grid_width 1))
		(ui-grid *values* (:grid_width 1))
		(ui-grid *sliders* (:grid_width 1)))
	(ui-label *status* (:text "..." :font :font_body))
	;the pixels of the canvas are in shared memory, the nodes of this
	;machine draw their tiles straight onto them
	(ui-element *canvas* (if shared_pixmap (Canvas-pixmap shared_pixmap)
			(Canvas +width +height +scale))))

(defun control-value ((name type lo hi slider label val init))
	;the value of the input, from where its slider is, and the
	;default to the last digit until the slider is moved
	(defq v (get :value slider))
	(cond
		((eql type :int) (+ lo v))
		((= v init) (shader-double val))
		((+ (shader-double lo) (/ (* (- (shader-double hi) (shader-double lo)) (n2d v))
					(const (n2d +steps)))))))

(defun control-text (type val)
	(if (eql type :int) (str val) (double-to-str val 4)))

(defun make-label (text)
	(defq label (Label))
	(def label :text text :min_width 64
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))
	(view-variant label :border :border_label))

(defun make-controls ()
	;a slider for each input of the shader that has a range, the app
	;gives the others, the time and the size of the frame
	(each (lambda ((name type val lo hi))
			(when (and lo hi)
				(defq slider (Slider) label (make-label "")
					steps (if (eql type :int) (- hi lo) +steps))
				(def slider :maximum steps :portion (max 1 (/ steps 10))
					:min_width 256
					:value (defq init (if (eql type :int) (- val lo)
							(n2i (/ (* (- (shader-double val) (shader-double lo)) (n2d steps))
									(- (shader-double hi) (shader-double lo)))))))
				(view-variant slider :paper :paper_slider)
				(. *names* :add_child (make-label (str name)))
				(. *values* :add_child label)
				(. *sliders* :add_child slider)
				(push controls (list name type lo hi slider label val init))))
		(first program)))

(defun frame-inputs (now)
	;the inputs block for a frame, from the time and the controls
	(defq vals (list
			(list 'time (/ (n2d (- now start_time)) (const (n2d 1000000))))
			(list 'resolution (list +width +height))))
	(each (lambda (control)
			(bind '(name type lo hi slider label &ignore) control)
			(defq val (control-value control))
			(push vals (list name val))
			(. label :set_text (control-text type val))) controls)
	(shader-pack program vals))

(defun notice (text)
	;a word on the status line that stays for a while
	(setq notice_until (+ (pii-time) +notice_time))
	(. *status* :set_text text))

(defun cpu-stop ()
	;the CPU's farm is closed, its children told to go, and its mailboxes
	;made new, so a tile that is late comes nowhere
	(when jobs
		(. jobs :close)
		(elem-set select +select_task (mail-mbox))
		(elem-set select +select_reply (mail-mbox))
		(setq jobs :nil))
	(setq cpu_running :nil))

(defun gpu-stop ()
	;the GPU's farm is closed, as the CPU's is
	(when gpu_jobs
		(. gpu_jobs :close)
		(elem-set select +select_gpu_task (mail-mbox))
		(elem-set select +select_gpu (mail-mbox))
		(setq gpu_jobs :nil))
	(setq gpu_out :nil gpu_mode :nil))

(defun start-frame ()
	;a frame on the CPU, the nodes shade it, a tile each, as native code.
	;The inputs block goes out with every tile of it. The farm is made when
	;there is none: a herd on the nodes of this machine, a child a node,
	;which shade straight into the pixels that only they can reach; one of
	;another machine would send its pixels back, and every frame waits for
	;its last tile. Kept off this node, a tile is one long call of native
	;code, and this node is the window's
	(unless jobs
		(setq jobs (Jobs +shader_tile_child
			(elem-get select +select_task) (elem-get select +select_reply) (list +max_int 1 1) :t)))
	(defq inputs (frame-inputs (setq frame_time (pii-time))))
	(setq cpu_running :t)
	(. jobs :add (map! (lambda () (defq y (* (!) +line_batch))
					(shader-tile +shader_file inputs 0 y +width (min +height (+ y +line_batch)) +width +height shared_key))
			'() 0 (const (/ (+ +height +line_batch -1) +line_batch)))))

(defun gpu-frame ()
	;a frame on the GPU, the one tile, a job of its farm, one task. The
	;farm is made when there is none, its child kept off this node, as the
	;CPU's are
	(unless gpu_jobs
		(setq gpu_jobs (Jobs +shader_shade_child
			(elem-get select +select_gpu_task) (elem-get select +select_gpu) 1 :t)))
	(setq gpu_out :t gpu_given (pii-time))
	(. gpu_jobs :add (list (shader-tile +shader_file (frame-inputs (pii-time))
		0 0 +width +height +width +height shared_key))))

(defun to-cpu ()
	;the frames are drawn by the CPU from here on, the GPU's farm let go of
	(gpu-stop)
	(setq ticks 0)
	(unless cpu_running (start-frame)))

(defun gpu-answered (msg)
	;a frame of the GPU's is drawn, or its task has no GPU, or could make
	;no kernel of the shader. One that is late, of a task started again, is
	;the farm's own
	(when (. gpu_jobs :answered msg)
		(setq gpu_out :nil)
		(cond
			((= (getf msg +shade_reply_drawn) 0)
				;so it is the CPU
				(setq gpu_want :nil has_gpu :nil)
				(. *mode* :set_selected 0)
				(to-cpu)
				(notice "No GPU, it could not run the shader"))
			(:t (defq now (pii-time))
				(cond
					(gpu_mode (++ gpu_frames))
					(:t ;the kernel is built, the GPU draws the frames from here
						;on, and the CPU's farm is let go of. The wait for the build
						;says nothing of the GPU, its rate is from this frame on
						(cpu-stop)
						(setq gpu_mode :t gpu_frames 0 gpu_time now ticks 0)))
				;the frame is on the pixels, as one of tiles is
				(setq gpu_last now)
				(. *canvas* :swap +swap_write)
				;the next frame at once, if this one took a tick or more: its
				;answer drives the next, a GPU that takes longer than a tick
				;does not wait for the one after. A faster one is given its
				;next by the timer, a frame a tick
				(and gpu_want (>= (- now gpu_given) +timer_rate) (gpu-frame))))))

(defun set-mode ()
	;the CPU or GPU button was pressed
	(cond
		((/= (. *mode* :get_selected) 1)
			(setq gpu_want :nil)
			(to-cpu))
		((not has_gpu)
			(. *mode* :set_selected 0)
			(notice "No GPU, this host has none, see docs/intro/sdl3.md"))
		;the GPU takes over when it has drawn its first frame
		((setq gpu_want :t))))

(defun config-load ()
	;as it was when last closed, or as it is the first time: the GPU if
	;there is one, the shader's own values
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version :gpu :t :sliders (list))))
	;each slider where it was, by its input's name, an input the shader no
	;longer has is passed over
	(each (lambda ((name value))
			(when (defq control (some (# (if (eql (str (first %0)) name) %0)) controls))
				(defq slider (elem-get control 4))
				(set slider :value (max 0 (min (get :maximum slider) value)))))
		(. *config* :find :sliders)))

(defun config-save ()
	;where each slider is, by its input's name, and CPU or GPU
	(scatter *config* :gpu (= (. *mode* :get_selected) 1)
		:sliders (map (# (list (str (first %0)) (get :value (elem-get %0 4)))) controls))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun main ()
	(setq select (task-mboxes +select_size))
	(make-controls)
	(config-load)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	;it comes up on the GPU if there is one and it was last time. The CPU
	;is not started at once, most drivers have the shader built before it
	;would have a frame
	(cond
		((and has_gpu (. *config* :find :gpu))
			(. *mode* :set_selected 1)
			(setq gpu_want :t)
			(. *status* :set_text "GPU, its kernel is being built"))
		(:t (. *mode* :set_selected 0)
			(start-frame)))
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
				;a child has started
				(. jobs :launched msg))
			(+select_reply
				;a tile is shaded
				(when (defq out (. jobs :answered msg))
					;a child that could not reach the canvas sends the pixels
					(shader-tile-show *canvas* msg)
					(when (= out 0)
						;the frame is done, show it and start the next
						(. *canvas* :swap +swap_write)
						(if (> (pii-time) notice_until)
							(. *status* :set_text (cat "Frame "
									(str (/ (- (pii-time) frame_time) 1000)) "ms, "
									(str (length (lisp-nodes :t))) " nodes, native code on the CPU"
									(if gpu_want ", while the kernel is built for the GPU" ""))))
						(start-frame))))
			(+select_gpu_task
				;the GPU's child has started
				(. gpu_jobs :launched msg))
			(+select_gpu
				;a frame is drawn
				(gpu-answered msg))
			(:t ;timer event, a GPU frame every tick, if the last is drawn
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(and gpu_want (not gpu_out) (gpu-frame))
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					(cond
						(gpu_mode
							;the rate, from a frame to a frame, so of a slow GPU as well, whose
							;frames are further apart than this is looked at
							(when (> gpu_frames 0)
								(defq rate (/ (* gpu_frames 10000000) (max 1 (- gpu_last gpu_time))))
								(. *status* :set_text (cat "GPU, "
										(str (/ rate 10)) "." (str (% rate 10)) " frames a second"))
								(setq gpu_frames 0 gpu_time gpu_last)))
						;the kernel is taking a while to build, so the CPU starts
						((not cpu_running) (start-frame)))
					;the patience of each farm
					(if jobs (. jobs :refresh +retry_timeout))
					(if gpu_jobs (. gpu_jobs :refresh +gpu_timeout))))))
	(config-save)
	;close the farms, their children told to go, and the window
	(cpu-stop)
	(gpu-stop)
	(. *window* :close))
