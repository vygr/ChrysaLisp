(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "./app.inc")
(import "service/source/app.inc")
(import "lib/gpu/render_farm.inc")

;Fly into and out of the set. The left button held flies in, the right
;flies out, and where the pointer is from the middle of the picture is
;where it goes, the further out the faster; at the middle it holds still.
;Nothing steps: each frame moves on by how long the last one took. A
;picture is worked out by a (Render-work), lib/gpu/render_farm.inc, with
;children of this app's: one task on the GPU, gpu.lisp, given the whole
;picture as its square, a pixel a thread, or children on the nodes of
;the CPU, child.lisp, a square each. The one protocol and the one job,
;each child drawing on the pixels of the canvas, in shared memory, and
;each started on the view of the moment when a picture is done. Both run
;the one kernel, kernel.shader, the GPU in floats, the CPU in doubles as
;native code. A farm lasts while its work goes on: it is let go of when
;the other takes over, and when the picture has stood still a while,
;and the next work makes a new one

(enums +event +event_user
	(enum home pal mode))

(enums +select 0
	(enum main tip timer))

(defq +width 800 +height 800 +job_rect_size 32
	;a frame each sixtieth of a second, and the CPU's picture shown ten
	;times a second as it fills in
	+frame_rate 16666 +show_rate 100000
	;where it starts, and how far out and in it goes. In a float of the
	;GPU, 24 bits, the step from one pixel to the next is lost in the
	;centre's own a little past 1/20000 of the start
	+home_x +double_-1/2 +home_y +double_0 +home_z +double_1
	+z_max +double_2 +z_min (/ +double_1 (n2d 20000))
	;and how far the middle of the picture may go, round the whole set
	+x_min (n2d -2.2) +x_max (n2d 0.8) +y_min (n2d -1.3) +y_max (n2d 1.3)
	;how fast it flies, octaves a second, and pans, halves of the picture
	;a second, and the still part in the middle of it, and how soon a
	;speed comes up to what it is to be, the less the softer
	+fly_rate +double_1 +pan_rate (n2d 0.6) +still (n2d 0.1) +ease (n2d 4)
	+min_top 256 +level_top 64
	;the patience of the CPU's farm, a square is quick
	+retry_timeout 5000000
	;the GPU if there is one, and it can be switched to the CPU and back
	id :t has_gpu (> (gpu-format) 0)
	;a child draws a square of the picture straight onto the canvas, its
	;pixels in shared memory
	shared_canvas (canvas-shared +width +height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	;where the picture is, how far it is from its middle to its top, the
	;palette, the speeds of the moment, and the buttons held
	cx +home_x cy +home_y z +home_z pal 0 zv +double_0 vx +double_0 vy +double_0 held 0
	;the render, its picture not yet shown, and one out with it
	select :nil render :nil dirty :nil out :nil
	last_t 0 last_show 0 changed :t
	;how long the last whole picture took, and when the farm's was started
	took 0 farm_t 0
	;CPU or GPU, and the palette, as they were when last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "mandelbrot.tre"))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Mandelbrot" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_reset) +event_home))
		(. (ui-radio-bar *mode_toolbar* ("CPU" "GPU") (:font :font_body)) :connect +event_mode)
		(. (ui-radio-bar *pal_toolbar* (+sym_filled_circle +sym_filled_circle +sym_filled_circle
					+sym_filled_circle +sym_filled_circle +sym_filled_circle +sym_filled_circle)
				(:font :font_medium_symbol)) :connect +event_pal)
		;its text as wide as it gets, the window is laid out to that
		(ui-label *status* (:text " x20000 deepest  10000.0 ms " :font :font_body
				:flow_flags +flow_flag_align_vcenter))
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1))))

;each palette's chip in its colour
(each (lambda (button palette)
		(def button :ink (second palette))
		(. button :dirty_all))
	(. *pal_toolbar* :children) +mandel_palettes)
(. *pal_toolbar* :set_selected 0)
(. *mode_toolbar* :set_selected (if has_gpu 1 0))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar* '("home"))
	(ui-tool-tips *mode_toolbar* '("farm" "GPU"))
	(ui-tool-tips *pal_toolbar* (map (const first) +mandel_palettes)))

(defun top-of ()
	;how many turns a point is given to get out of the set, more the deeper
	;the picture, or the detail there is taken to be inside it
	(defq k (n2i (/ +home_z z)) levels 0)
	(while (> k 1) (setq k (>> k 1) levels (inc levels)))
	(+ +min_top (* +level_top levels)))

(defun status ()
	;how far in, and if it is as far as it goes
	(defq k (n2i (/ +home_z z)))
	(. *status* :set_text (cat " x" (str k) (if (<= z +z_min) " deepest" "")
			"  " (str (/ took 1000)) "." (str (/ (% took 1000) 100)) " ms ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the farm, and a picture of it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-grid ()
	; (on-grid) -> (x y step)
	;the middle of the picture on the grid of its pixels, so a pan moves the
	;picture by whole pixels, and what was worked out for a pixel is what is
	;worked out for the one it moved to
	(defq step (/ (* z +double_2) (n2d +height)))
	(list (* (n2d (n2i (/ cx step))) step) (* (n2d (n2i (/ cy step))) step) step))

(defun mode-show ()
	;the button as the render has it: the GPU is not to be had on a host
	;with none, or once its child could not have the kernel
	(defq to (if (eql (. render :mode) :gpu) 1 0))
	(unless (= (. *mode_toolbar* :get_selected) to) (. *mode_toolbar* :set_selected to)))

(defun forget ()
	;what the farm has still to answer of the last picture is not wanted,
	;the view it was of has gone. The children are left as they are, one
	;part way through a square finishes it and takes its next
	(. render :drop)
	(setq out :nil))

(defun rect-job (x y x1 y1 gx gy top)
	;a square of the picture of the moment, for a child
	(setf-> (str-alloc +rect_size)
		(+rect_x x) (+rect_y y) (+rect_x1 x1) (+rect_y1 y1)
		(+rect_w +width) (+rect_h +height) (+rect_shared shared_key)
		(+rect_top top) (+rect_pal pal) (+rect_cx gx) (+rect_cy gy) (+rect_z z)))

(defun frame-jobs (kind count ready)
	;the jobs of the picture of the moment: all of it the one square to
	;the GPU's child, or a square to each child of the CPU's. No child is
	;asked if it is ready
	(unless ready
		(bind '(gx gy &ignore) (on-grid))
		(defq work (list) top (top-of))
		(if (eql kind :gpu) (push work (rect-job 0 0 +width +height gx gy top))
			(each! (lambda ()
					(defq y (* (!) +job_rect_size))
					(each! (lambda ()
							(defq x (* (!) +job_rect_size))
							(push work (rect-job x y (min +width (+ x +job_rect_size))
									(min +height (+ y +job_rect_size)) gx gy top)))
						'() 0 (const (/ (+ +width +job_rect_size -1) +job_rect_size))))
				'() 0 (const (/ (+ +height +job_rect_size -1) +job_rect_size))))
		work))

(defun square-done (msg)
	;a square is done. One whose child could not reach the canvas has its
	;pixels with it
	(setq dirty :t)
	(if (= (getf msg +rect_reply_drawn) 0)
		(apply draw-rect (cat (list *canvas* (slice msg +rect_reply_size -1))
				(getf-> msg +rect_reply_x +rect_reply_y +rect_reply_x1 +rect_reply_y1
					+rect_reply_ix +rect_reply_iy +rect_reply_ix1 +rect_reply_iy1
					+rect_reply_inside))))
	:t)

(defun square-cannot (msg)
	; (square-cannot msg) -> :nil | :t
	;the child could not have the kernel where it is. A child saying it has
	;gone is shorter than an answer, and is not that
	(and (>= (length msg) +rect_reply_size) (/= (getf msg +rect_reply_failed) 0)))

(defun farm-frame ()
	;the picture of the moment, to the render
	(setq changed :nil farm_t (pii-time) out :t)
	(. render :frame))

;;;;;;;;
; flying
;;;;;;;;

(defun ease (v to dt)
	;v brought toward to, as far as the ease of this much time has it
	(+ v (* (- to v) (min +double_1 (* +ease dt)))))

(defun pointer ()
	;where the pointer is from the middle of the picture, each -1 to 1 at
	;its edges, :nil if it is not over it
	(bind '(mx my &ignore) (gui-info))
	(bind '(px py &ignore) (. (penv *window*) :get_relative *canvas*))
	(defq ox (/ (n2d (- mx px (/ +width 2))) (n2d (/ +width 2)))
		oy (/ (n2d (- my py (/ +height 2))) (n2d (/ +height 2))))
	(and (<= +double_-1 ox +double_1) (<= +double_-1 oy +double_1) (list ox oy)))

(defun near-end (r)
	;1 more than an octave from an end, r its ratio to it, and down to 0 at it
	(max +double_0 (min +double_1 (- r +double_1))))

(defun pan-speed (o at lo hi)
	;none in the still part, then up to the rate at the edge. Going toward
	;an end of where the middle may go, it slows over the last half of a
	;picture of that end, z the half of a picture, and stops at it
	(cond
		((< (abs o) +still) +double_0)
		((defq v (* (/ (- (abs o) +still) (- +double_1 +still)) +pan_rate))
			(if (< o +double_0)
				(* (neg v) (near-end (+ +double_1 (/ (- at lo) (* z +double_1/2)))))
				(* v (near-end (+ +double_1 (/ (- hi at) (* z +double_1/2)))))))))

(defun fly (dt)
	;the speeds brought on, and the picture moved by them. The zoom comes
	;to a stop at the end it is going to, slowing in the last octave of it
	(defq to (cond ((= held 1) +fly_rate) ((or (= held 2) (= held 3)) (neg +fly_rate)) (+double_0)))
	(cond
		((> to +double_0) (setq to (* to (near-end (/ z +z_min)))))
		((< to +double_0) (setq to (* to (near-end (/ +z_max z))))))
	(setq zv (ease zv to dt))
	(defq p (pointer))
	(setq vx (ease vx (if p (pan-speed (first p) cx +x_min +x_max) +double_0) dt)
		vy (ease vy (if p (pan-speed (second p) cy +y_min +y_max) +double_0) dt))
	;the zoom is about the point under the pointer, which stays under it,
	;or the middle if the pointer is not over the picture: the middle goes
	;toward that point as the picture gets bigger, away as it gets smaller
	(defq z1 (max +z_min (min +z_max (* z (pow +double_2 (neg (* zv dt))))))
		k (- +double_1 (/ z1 z))
		x1 (+ cx (* vx z dt)) y1 (+ cy (* vy z dt)))
	(when p
		(setq x1 (+ x1 (* k (first p) z)) y1 (+ y1 (* k (second p) z))))
	(setq x1 (max +x_min (min +x_max x1)) y1 (max +y_min (min +y_max y1)))
	(when (or (/= z1 z) (/= x1 cx) (/= y1 cy))
		(setq z z1 cx x1 cy y1 changed :t)))

(defun home ()
	(setq cx +home_x cy +home_y z +home_z zv +double_0 vx +double_0 vy +double_0 changed :t))

(defun tick ()
	;a frame: the time since the last, the picture moved, and drawn
	(defq now (pii-time) dt (min (n2d 0.1) (/ (n2d (- now last_t)) (n2d 1000000))))
	(setq last_t now)
	(fly dt)
	(when (and changed (not (. render :busy)))
		(farm-frame)
		(status))
	;a child that has gone is started again, and when the picture has stood
	;still a while the farm is let go of, its work has stopped
	(. render :tick)
	;the CPU's picture is shown as it fills in, when the view stands still:
	;of one that is moving that would be two pictures mixed
	(when (and dirty (not changed) (= held 0) (> (- now last_show) +show_rate))
		(setq dirty :nil last_show now)
		(. *canvas* :swap +swap_write)))

(defun config-load ()
	;as it was when last closed, or as it is the first time: the GPU if
	;there is one, the first palette
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version :pal 0 :gpu :t)))
	(setq pal (max 0 (min (dec (length +mandel_palettes)) (. *config* :find :pal))))
	(. *pal_toolbar* :set_selected pal))

(defun config-save ()
	(scatter *config* :pal pal :gpu (= (. *mode_toolbar* :get_selected) 1))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun main ()
	;the children are this app's: the GPU's one task, kept off this node,
	;which is the window's, and the CPU's two on each node of this machine,
	;each read from this system, by its @Source, its Lisp and the kernel,
	;made native code where it runs. One that can not reach the canvas
	;sends its pixels
	(setq select (task-mboxes +select_size)
		render (Render-work *canvas* (source-task (cat *app_root* "child.lisp")) (cat *app_root* "gpu.lisp")))
	(def render :jobs_fn (const frame-jobs) :reply_fn (const square-done) :cannot_fn (const square-cannot)
		:herd (list +max_int 2 2) :send_back :t :patience +retry_timeout :part "square")
	(config-load)
	;it comes up on the GPU if there is one and it was last time
	(unless (. *config* :find :gpu) (. render :mode :cpu))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	(tooltips)
	(mode-show)
	(setq last_t (pii-time))
	(mail-timeout (elem-get select +select_timer) +frame_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;a square is done, or the picture is whole: shown, and how long
				;it took. If the view has moved meanwhile the next is started at
				;once: an answer drives the next, it does not wait for the tick
				(mode-show)
				(when (and out (not (. render :busy)))
					(setq out :nil dirty :nil took (- (pii-time) farm_t))
					(status)
					(if changed (farm-frame))))
			((= idx +select_tip)
				(. *window* :tip msg))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((and (= id (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_mouse))
						;the button held, the next frames fly by it. A press
						;says which, 1 the left, 3 the right, by its count of
						;clicks; a move while held says the buttons another
						;way, and is not looked at; up is 0
						(defq buttons (getf msg +ev_msg_mouse_buttons))
						(cond
							((= buttons 0) (setq held 0))
							((> (getf msg +ev_msg_mouse_count) 0) (setq held buttons))))
					((= id +event_home)
						(forget)
						(home))
					((= id +event_mode)
						;the GPU's farm or the CPU's, from the next frame: the
						;one there is is let go of, the other is to take over
						(defq was (. render :mode))
						(unless (eql was (. render :mode (if (= (. *mode_toolbar* :get_selected) 1) :gpu :cpu)))
							(setq changed :t out :nil dirty :nil))
						(mode-show))
					((= id +event_pal)
						(setq pal (. *pal_toolbar* :get_selected) changed :t)
						(forget))
					((. *window* :event msg))))
			(:t ;a frame, the next one a frame from when this one started
				(defq start (pii-time))
				(tick)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +frame_rate (- (pii-time) start))) 0))))
	(config-save)
	(. render :close)
	(. *window* :close))
