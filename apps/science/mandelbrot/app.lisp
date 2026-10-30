(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")
(import "service/source/app.inc")
(import "lib/gpu/kernel.inc")

;Fly into and out of the set. The left button held flies in, the right
;flies out, and where the pointer is from the middle of the picture is
;where it goes, the further out the faster; at the middle it holds still.
;Nothing steps: each frame moves on by how long the last one took. On the
;GPU a frame is the whole picture, a pixel a thread; with none the children
;of the farm work it out, a square each, and are started on the view of the
;moment each time a picture is done. Both run the one kernel, kernel.shader,
;the GPU in floats, the farm in reals as native code

(enums +event +event_user
	(enum home pal mode))

(enums +select 0
	(enum main task reply timer tip))

(defq +width 800 +height 800 +job_rect_size 32
	;a frame each sixtieth of a second, and the farm's picture shown ten
	;times a second as it fills in
	+frame_rate 16666 +show_rate 100000
	;where it starts, and how far out and in it goes. In a float of the
	;GPU, 24 bits, the step from one pixel to the next is lost in the
	;centre's own a little past 1/20000 of the start
	+home_x +real_-1/2 +home_y +real_0 +home_z +real_1
	+z_max +real_2 +z_min (/ +real_1 (n2r 20000))
	;and how far the middle of the picture may go, round the whole set
	+x_min (n2r -2.2) +x_max (n2r 0.8) +y_min (n2r -1.3) +y_max (n2r 1.3)
	;how fast it flies, octaves a second, and pans, halves of the picture
	;a second, and the still part in the middle of it, and how soon a
	;speed comes up to what it is to be, the less the softer
	+fly_rate +real_1 +pan_rate (n2r 0.6) +still (n2r 0.1) +ease (n2r 4)
	+min_top 256 +level_top 64
	+retry_timeout (task-timeout 5)
	;the GPU if there is one, and it can be switched to the farm and back
	id :t has_gpu (> (gpu-format) 0) gpu has_gpu
	;the farm draws a square of the picture at a time straight onto the
	;canvas, its pixels in shared memory if the host has them; the GPU's
	;frame is put on the same pixels
	shared_canvas (canvas-shared +width +height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	;where the picture is, how far it is from its middle to its top, the
	;palette, the speeds of the moment, and the buttons held
	cx +home_x cy +home_y z +home_z pal 0 zv +real_0 vx +real_0 vy +real_0 held 0
	;the kernel, the GPU's of it, its buffers, and the farm
	program (shader-load (cat *app_root* "kernel.shader"))
	kernel :nil pal_buf :nil out_buf :nil jobs :nil dirty :nil busy :nil
	last_t 0 last_show 0 changed :t
	;how long the last whole picture took, and when the farm's was started
	took 0 farm_t 0)

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
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1)) (:paper 0)))

;each palette's chip in its colour
(each (lambda (button palette)
		(def button :ink (second palette))
		(. button :dirty_all))
	(. *pal_toolbar* :children) +mandel_palettes)
(. *pal_toolbar* :set_selected 0)
(. *mode_toolbar* :set_selected (if gpu 1 0))

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
	(set *status* :text (cat " x" (str k) (if (<= z +z_min) " deepest" "")
		"  " (str (/ took 1000)) "." (str (/ (% took 1000) 100)) " ms "))
	(.-> *status* :layout :dirty))

;;;;;;;;;;;;;;;;
; the GPU's frame
;;;;;;;;;;;;;;;;

(defun gpu-init ()
	;the kernel, in the language of this GPU, the palette, and the pixels
	(when (setq kernel (shader-gpu-kernel program))
		(setq pal_buf (gpu-buffer (* +mandel_pal_size +int_size))
			out_buf (gpu-buffer (* +width +height +int_size)))
		(gpu-palette)))

(defun gpu-palette ()
	(gpu-write pal_buf 0 (mandel-pal pal)))

(defun on-grid ()
	; (on-grid) -> (x y step)
	;the middle of the picture on the grid of its pixels, so a pan moves the
	;picture by whole pixels, and what was worked out for a pixel is what is
	;worked out for the one it moved to
	(defq step (/ (* z +real_2) (n2r +height)))
	(list (* (n2r (n2i (/ cx step))) step) (* (n2r (n2i (/ cy step))) step) step))

(defun gpu-frame ()
	;the picture of the moment, worked out, read back, and shown, and how
	;long that took
	(defq t0 (pii-time))
	(bind '(gx gy step) (on-grid))
	(when (shader-gpu-run program kernel
			(shader-pack program (list (list 'cx gx) (list 'cy gy) (list 'step step)
				(list 'w +width) (list 'h +height) (list 'ow +width) (list 'top (top-of))
				(list 'base +mandel_pal_base) (list 'rate +mandel_pal_rate)
				(list 'lbase +mandel_light_base) (list 'lgain +mandel_light_gain)
				(list 'ss +mandel_samples)))
			0 (* +width +height) (list pal_buf out_buf))
		(. *canvas* :tile (gpu-read out_buf 0 (* +width +height +int_size)) 0 0 +width +height)
		(setq dirty :nil took (- (pii-time) t0))
		(. *canvas* :swap +swap_write)))

;;;;;;;;;;;;;;;;;
; the farm's frame
;;;;;;;;;;;;;;;;;

(defun farm-start ()
	;the children, started the first time the farm is wanted. Each is read
	;from this system, by its @Source, its Lisp and the kernel, made native
	;code where it runs
	(setq jobs (Jobs (source-task (cat *app_root* "child.lisp"))
		(elem-get select +select_task) (elem-get select +select_reply)
		;two on each node of this machine, none on another
		(list +max_int 2 2))
		busy :nil))

(defun farm-forget ()
	;what the farm has still to answer of the last picture is not wanted.
	;Its answers go to the mailbox they were asked for with, let go of, and
	;a new one takes those from now on. The children are left as they are,
	;one part way through a square finishes it and takes its next
	(when jobs
		(elem-set select +select_reply (mail-mbox))
		(. jobs :reply_to (elem-get select +select_reply))
		(setq busy :nil)))

(defun farm-frame ()
	;the picture of the moment, a square to each child
	(farm-forget)
	(bind '(gx gy &ignore) (on-grid))
	(defq work (list) top (top-of))
	(each (lambda (y)
		(each (lambda (x)
			(push work (setf-> (str-alloc +rect_size)
				(+rect_x x)
				(+rect_y y)
				(+rect_x1 (min +width (+ x +job_rect_size)))
				(+rect_y1 (min +height (+ y +job_rect_size)))
				(+rect_w +width)
				(+rect_h +height)
				(+rect_shared shared_key)
				(+rect_top top)
				(+rect_pal pal)
				(+rect_cx gx)
				(+rect_cy gy)
				(+rect_z z))))
			(range 0 +width +job_rect_size)))
		(range 0 +height +job_rect_size))
	(. jobs :add work)
	(setq busy :t farm_t (pii-time)))

;;;;;;;;
; flying
;;;;;;;;

(defun ease (v to dt)
	;v brought toward to, as far as the ease of this much time has it
	(+ v (* (- to v) (min +real_1 (* +ease dt)))))

(defun pointer ()
	;where the pointer is from the middle of the picture, each -1 to 1 at
	;its edges, :nil if it is not over it
	(bind '(mx my &ignore) (gui-info))
	(bind '(px py &ignore) (. (penv *window*) :get_relative *canvas*))
	(defq ox (/ (n2r (- mx px (/ +width 2))) (n2r (/ +width 2)))
		oy (/ (n2r (- my py (/ +height 2))) (n2r (/ +height 2))))
	(and (<= +real_-1 ox +real_1) (<= +real_-1 oy +real_1) (list ox oy)))

(defun pan-speed (o at lo hi)
	;none in the still part, then up to the rate at the edge. Going toward
	;an end of where the middle may go, it slows over the last half of a
	;picture of that end, z the half of a picture, and stops at it
	(cond
		((< (abs o) +still) +real_0)
		((defq v (* (/ (- (abs o) +still) (- +real_1 +still)) +pan_rate))
			(if (< o +real_0)
				(* (neg v) (near-end (+ +real_1 (/ (- at lo) (* z +real_1/2)))))
				(* v (near-end (+ +real_1 (/ (- hi at) (* z +real_1/2)))))))))

(defun near-end (r)
	;1 more than an octave from an end, r its ratio to it, and down to 0 at it
	(max +real_0 (min +real_1 (- r +real_1))))

(defun fly (dt)
	;the speeds brought on, and the picture moved by them. The zoom comes
	;to a stop at the end it is going to, slowing in the last octave of it
	(defq to (cond ((= held 1) +fly_rate) ((or (= held 2) (= held 3)) (neg +fly_rate)) (+real_0)))
	(cond
		((> to +real_0) (setq to (* to (near-end (/ z +z_min)))))
		((< to +real_0) (setq to (* to (near-end (/ +z_max z))))))
	(setq zv (ease zv to dt))
	(defq p (pointer))
	(setq vx (ease vx (if p (pan-speed (first p) cx +x_min +x_max) +real_0) dt)
		vy (ease vy (if p (pan-speed (second p) cy +y_min +y_max) +real_0) dt))
	;the zoom is about the point under the pointer, which stays under it,
	;or the middle if the pointer is not over the picture: the middle goes
	;toward that point as the picture gets bigger, away as it gets smaller
	(defq z1 (max +z_min (min +z_max (* z (pow +real_2 (neg (* zv dt))))))
		k (- +real_1 (/ z1 z))
		x1 (+ cx (* vx z dt)) y1 (+ cy (* vy z dt)))
	(when p
		(setq x1 (+ x1 (* k (first p) z)) y1 (+ y1 (* k (second p) z))))
	(setq x1 (max +x_min (min +x_max x1)) y1 (max +y_min (min +y_max y1)))
	(when (or (/= z1 z) (/= x1 cx) (/= y1 cy))
		(setq z z1 cx x1 cy y1 changed :t)))

(defun home ()
	(setq cx +home_x cy +home_y z +home_z zv +real_0 vx +real_0 vy +real_0 changed :t))

(defun tick ()
	;a frame: the time since the last, the picture moved, and drawn
	(defq now (pii-time) dt (min (n2r 0.1) (/ (n2r (- now last_t)) (n2r 1000000))))
	(setq last_t now)
	(fly dt)
	(cond
		(gpu (when changed
			(setq changed :nil)
			(gpu-frame)
			(status)))
		(:t (unless jobs (farm-start))
			(unless busy
				(when changed
					(setq changed :nil)
					(farm-frame)
					(status)))
			(when (and dirty (> (- now last_show) +show_rate))
				(setq dirty :nil last_show now)
				(. *canvas* :swap +swap_write)))))

(defun main ()
	(defq select (task-mboxes +select_size))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;a GPU that could not take the kernel, the farm does it
	(and gpu (not (gpu-init)) (setq gpu :nil has_gpu :nil))
	(. *mode_toolbar* :set_selected (if gpu 1 0))
	(tooltips)
	(setq last_t (pii-time))
	(mail-timeout (elem-get select +select_timer) +frame_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
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
						(farm-forget)
						(home))
					((= id +event_mode)
						;the GPU or the farm, from the next frame
						(setq gpu (and has_gpu (= (. *mode_toolbar* :get_selected) 1)) changed :t)
						(. *mode_toolbar* :set_selected (if gpu 1 0))
						;to the GPU, what the farm has not answered is not wanted
						(if gpu (farm-forget)))
					((= id +event_pal)
						(setq pal (. *pal_toolbar* :get_selected) changed :t)
						(farm-forget)
						(if gpu (gpu-palette)))
					((. *window* :event msg))))
			(+select_task
				(. jobs :launched msg))
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(+select_reply
				;a square is done, one of an old picture is left alone
				(when (defq out (. jobs :answered msg))
					(setq dirty :t)
					(when (= (getf msg +rect_reply_drawn) 0)
						(apply draw-rect (cat (list *canvas* (slice msg +rect_reply_size -1))
							(getf-> msg +rect_reply_x +rect_reply_y +rect_reply_x1 +rect_reply_y1
								+rect_reply_ix +rect_reply_iy +rect_reply_ix1 +rect_reply_iy1
								+rect_reply_inside))))
					(when (= out 0)
						;the picture is whole, shown, how long it took, and the
						;next may start
						(setq busy :nil dirty :nil took (- (pii-time) farm_t))
						(. *canvas* :swap +swap_write)
						(status))))
			(:t ;a frame, the next one a frame from when this one started
				(defq start (pii-time))
				(if jobs (. jobs :refresh +retry_timeout))
				(tick)
				(mail-timeout (elem-get select +select_timer)
					(max 1000 (- +frame_rate (- (pii-time) start))) 0))))
	(if jobs (. jobs :close))
	(if kernel (gpu-kernel-free kernel))
	(if pal_buf (gpu-buffer-free pal_buf))
	(if out_buf (gpu-buffer-free out_buf))
	(gui-sub-rpc *window*))
