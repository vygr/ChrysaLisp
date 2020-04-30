;imports
(import 'apps/math.inc)
(import 'apps/bubbles/app.inc)

(structure 'event 0
	(byte 'close 'max 'min)
	(byte 'reset)
	(byte 'grid 'plain 'axis))

(defq canvas_width 600 canvas_height 600 min_width 300 min_height 300
	style_buttons (list) rate (/ 1000000 60) base 0.3
	palette (map (lambda (_) (vec-i2n
			(/ (logand (>> _ 16) 0xff) 0xff)
			(/ (logand (>> _ 8) 0xff) 0xff)
			(/ (logand _ 0xff) 0xff)))
		(list argb_cyan argb_yellow argb_magenta argb_red argb_green argb_blue)))

(ui-window window ()
	(ui-title-bar _ "Bubbles" (0xea19 0xea1b 0xea1a) (const event_close))
	(ui-tool-bar _ ()
		(ui-buttons (0xe938) (const event_reset))
		(ui-buttons (0xe9a3 0xe976 0xe9f0) (const event_grid) () style_buttons))
	(ui-scroll image_scroll (logior scroll_flag_vertical scroll_flag_horizontal)
			(min_width canvas_width min_height canvas_height)
		(ui-backdrop backdrop (color argb_black ink_color argb_grey8 style 1)
			(ui-canvas layer1_canvas canvas_width canvas_height 1))))

(defun-bind radio-select (l i)
	;radio select buttons
	(each (lambda (b)
		(def (view-dirty b) 'color (if (= _ i) (const argb_grey14) (const *env_toolbar_col*)))) l) i)

(defun-bind redraw (verts mask)
	;redraw layer/s
	(tuple-set dlist_layer1_verts dlist verts)
	(tuple-set dlist_mask dlist (logior (tuple-get dlist_mask dlist) mask)))

(defun-bind vertex-cloud (num)
	;array of random verts
	(defq out (cap num (list)))
	(while (> (setq num (dec num)) -1)
		(push out (list
			(vec-i2n (- (random (const (* box_size 2))) box_size)
				(- (random (const (* box_size 2))) box_size)
				(- (random (const (* box_size 2))) box_size))
			(vec-i2n (- (random (const (inc (* max_vel 2)))) (const max_vel))
				(- (random (const (inc (* max_vel 2)))) (const max_vel))
				(- (random (const (inc (* max_vel 2)))) (const max_vel)))
			(i2n (const bubble_radius))
			(vec-add (const (vec-f2n base base base))
				(vec-scale (elem (random (length palette)) palette)
					(f2n (random (const (- 1.0 base))))))))) out)

(defun-bind vertex-update (verts)
	(each (lambda (vert)
		(bind '(p v _ _) vert)
		(vec-add p v p)
		(bind '(x y z) p)
		(bind '(vx vy vz) v)
		(if (or (> x (const (i2n box_size))) (< x (const (i2n (neg box_size)))))
			(setq vx (* vx (const (i2n -1)))))
		(if (or (> y (const (i2n box_size))) (< y (const (i2n (neg box_size)))))
			(setq vy (* vy (const (i2n -1)))))
		(if (or (> z (const (i2n box_size))) (< z (const (i2n (neg box_size)))))
			(setq vz (* vz (const (i2n -1)))))
		(tuple-set vertex_v vert (vec vx vy vz))) verts))

(defun-bind main ()
	;ui tree initial setup
	(defq dlist (list 0 rate light_pos layer1_canvas (list)))
	(canvas-set-flags layer1_canvas 1)
	(view-set-size backdrop canvas_width canvas_height)
	(radio-select style_buttons 1)
	(gui-add (apply view-change (cat (list window 256 192) (view-pref-size window))))
	(def image_scroll 'min_width min_width 'min_height min_height)

	;create child and send args
	(mail-send dlist (defq child_mbox (open-child "apps/bubbles/child.lisp" kn_call_open)))

	;random cloud of verts
	(defq verts (vertex-cloud num_bubbles))
	(redraw verts 1)

	;main event loop
	(defq last_state 'u id t)
	(while id (while (mail-poll (array (task-mailbox))) (cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) (const ev_msg_target_id))) (const event_close))
			;close button
			(setq id nil))
		((= id (const event_min))
			;min button
			(apply view-change-dirty (cat (list window) (view-get-pos window) (view-pref-size window))))
		((= id (const event_max))
			;max button
			(def image_scroll 'min_width canvas_width 'min_height canvas_height)
			(apply view-change-dirty (cat (list window) (view-get-pos window) (view-pref-size window)))
			(def image_scroll 'min_width min_width 'min_height min_height))
		((= id (const event_reset))
			;reset button
			(setq verts (vertex-cloud num_bubbles)))
		((<= (const event_grid) id (const event_axis))
			;styles
			(def (view-dirty backdrop) 'style (radio-select style_buttons (- id (const event_grid)))))
		((= id (component-get-id layer1_canvas))
			;event for canvas
			(when (= (get-long msg (const ev_msg_type)) (const ev_type_mouse))
				;mouse event in canvas
				(bind '(w h) (view-get-size layer1_canvas))
				(defq rx (- (get-int msg (const ev_msg_mouse_rx)) (/ w 2))
					ry (- (get-int msg (const ev_msg_mouse_ry)) (/ h 2)))
				(cond
					((/= (get-int msg (const ev_msg_mouse_buttons)) 0)
						;mouse button is down
						(case last_state
							(d	;was down last time
								)
							(u	;was up last time
								(setq last_state 'd)))
						;set light pos
						(tuple-set dlist_light_pos dlist
							(vec-i2n (* rx 4) (* ry 4) (neg (* box_size 4)))))
					(t	;mouse button is up
						(case last_state
							(d	;was down last time
								(setq last_state 'u))
							(u	;was up last time, so we are hovering
								t))))))
		(t (view-event window msg))))
		(vertex-update verts)
		(redraw verts 1)
		(task-sleep rate))
	;close child and window
	(mail-send "" child_mbox)
	(view-hide window))
