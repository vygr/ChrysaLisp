(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/gpu/render_farm.inc")
(import "./app.inc")

(enums +event +event_user
	(enum mode))

(enums +select 0
	(enum main timer))

(defq +width 640 +height 480 +scale 1 +line_batch 8 +steps 1000
	;the pixels of the canvas are in shared memory if the host has it
	shared_pixmap (pixmap-shared +width +height 0)
	;the timer ticks at the rate of a GPU frame, the slow jobs are done every so many
	+timer_rate (/ 1000000 60) +slow_ticks 30 ticks 0
	program (shader-load +shader_file) controls (list)
	select :nil id :t
	start_time (pii-time)
	;The frames are drawn by a (Render-shade), lib/gpu/render_farm.inc: by
	;the GPU, a farm of one, straight into the canvas's texture where this
	;node has the GUI, or by the nodes of the CPU, a tile of the frame
	;each, onto the pixels of the canvas, in shared memory
	render :nil
	;when the last frame was given, and the frames drawn since the status
	;line was last set, and when that was
	given 0 frames 0 since (pii-time)
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

(defun frame-values (now)
	;the inputs of the shader for a frame, from the time and the controls
	(defq vals (list
			(list 'time (/ (n2d (- now start_time)) (const (n2d 1000000))))
			(list 'resolution (list +width +height))))
	(each (lambda (control)
			(bind '(name type lo hi slider label &ignore) control)
			(defq val (control-value control))
			(push vals (list name val))
			(. label :set_text (control-text type val))) controls)
	vals)

(defun notice (text)
	;a word on the status line that stays for a while
	(setq notice_until (+ (pii-time) +notice_time))
	(. *status* :set_text text))

(defun next-frame ()
	;a frame of the moment, to the render
	(. render :frame (frame-values (setq given (pii-time))))
	(++ frames))

(defun mode-show ()
	;the button as the render has it: the GPU is not to be had on a host
	;with none, or once it could not run the shader
	(. *mode* :set_selected (if (eql (. render :mode) :gpu) 1 0)))

(defun set-mode ()
	;the CPU or GPU button was pressed
	(defq want (if (= (. *mode* :get_selected) 1) :gpu :cpu))
	(unless (eql (. render :mode want) want)
		(notice "No GPU for it on this host, see docs/intro/sdl3.md"))
	(mode-show))

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
	(setq select (task-mboxes +select_size) render (Render-shade *canvas* +shader_file +line_batch))
	(make-controls)
	(config-load)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	;it comes up on the GPU if there is one and it was last time
	(unless (. *config* :find :gpu) (. render :mode :cpu))
	(mode-show)
	(if (eql (. render :mode) :gpu) (. *status* :set_text "GPU, its shader is being built"))
	(next-frame)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;a frame that was out is whole: the next at once if it took a
				;tick or more, its answer drives the next. A faster one is
				;given its next by the timer, a frame a tick
				(and (not (. render :busy)) (>= (- (pii-time) given) +timer_rate) (next-frame)))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((= id +event_mode)
						(set-mode))
					((. *window* :event msg))))
			(:t ;timer event, a frame every tick, if the last is drawn
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(unless (. render :busy) (next-frame))
				(. render :tick)
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					;the rate, and how the frames are drawn. Over a second or
					;more, and a frame or more: a slow frame is longer than this
					;is looked at
					(mode-show)
					(defq now (pii-time))
					(when (and (> frames 0) (>= (- now since) 1000000))
						(defq rate (/ (* frames 10000000) (- now since)))
						(if (> now notice_until)
							(. *status* :set_text (cat (str (/ rate 10)) "." (str (% rate 10))
									" frames a second, " (. render :how))))
						(setq frames 0 since now))))))
	(config-save)
	;the render's farm let go of, its children told to go, and the window
	(. render :close)
	(. *window* :close))
