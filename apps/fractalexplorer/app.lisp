(import "././login/env.inc")
;jit compile apps native functions
(jit "apps/fractalexplorer/" "lisp.vp" '("tile"))

(import "gui/lisp.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close)
	(enum fractal_type)
	(enum color_scheme)
	(enum reset zoom_in zoom_out)
	(enum param1_slider param2_slider param3_slider))

(enums +select 0
	(enum main task reply timer))

(defq +width 800 +height 800 +scale 1 +timer_rate (/ 1000000 10) id :t dirty :nil
	center_x (mbfp-from-fixed -0.5) center_y (mbfp-from-fixed 0.0) zoom (mbfp-from-fixed 1.0)
	+retry_timeout (task-timeout 5) jobs :nil farm :nil
	current_fractal +fractal_julia
	current_color_scheme +color_scheme_rainbow
	param1 (mbfp-from-fixed -0.4) ; Julia real part
	param2 (mbfp-from-fixed 0.6)  ; Julia imaginary part
	param3 (mbfp-from-fixed 2.0)  ; Generic parameter
	)

(ui-window *window* ()
	(ui-title-bar _ "Fractal Explorer" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-canvas *canvas* +width +height +scale))
		(ui-flow control_panel (:flow_flags +flow_down_fill :color *env_toolbar2_col*)
			(ui-label _ (:text "Fractal Type:" :font *env_medium_font* :color *env_toolbar2_col*))
			(. (ui-select *fractal_selector* (:text "Julia Set")
				(each (lambda (name)
					(ui-label _ (:text name)))
					'("Julia Set" "Burning Ship" "Newton" "Tricorn"
					  "Phoenix" "Mandelbrot^3" "Mandelbrot^4"
					  "Mandelbox" "Lyapunov")))
				:connect +event_fractal_type)

			(ui-label _ (:text "" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "Color Scheme:" :font *env_medium_font* :color *env_toolbar2_col*))
			(. (ui-select *color_selector* (:text "Rainbow")
				(each (lambda (name)
					(ui-label _ (:text name)))
					'("Classic" "Rainbow" "Fire" "Ice" "Psychedelic"
					  "Ocean" "Sunset" "Electric" "Forest" "Copper")))
				:connect +event_color_scheme)

			(ui-label _ (:text "" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label *param1_label* (:text "Parameter 1: -0.40" :font *env_medium_font* :color *env_toolbar2_col*))
			(. (ui-slider *param1_slider* (:value 40 :maximum 100 :minimum 0))
				:connect +event_param1_slider)

			(ui-label *param2_label* (:text "Parameter 2: 0.60" :font *env_medium_font* :color *env_toolbar2_col*))
			(. (ui-slider *param2_slider* (:value 80 :maximum 100 :minimum 0))
				:connect +event_param2_slider)

			(ui-label *param3_label* (:text "Parameter 3: 2.00" :font *env_medium_font* :color *env_toolbar2_col*))
			(. (ui-slider *param3_slider* (:value 50 :maximum 100 :minimum 0))
				:connect +event_param3_slider)

			(ui-label _ (:text "" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "Controls:" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "• Left Click: Zoom In" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "• Right Click: Zoom Out" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "• Middle Click: Reset" :font *env_medium_font* :color *env_toolbar2_col*))

			(ui-label _ (:text "" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-button *reset_btn* (:text "Reset View"))
				(ui-button *zoom_in_btn* (:text "Zoom In"))
				(ui-button *zoom_out_btn* (:text "Zoom Out")))
			(.-> *reset_btn* (:connect +event_reset))
			(.-> *zoom_in_btn* (:connect +event_zoom_in))
			(.-> *zoom_out_btn* (:connect +event_zoom_out))

			(ui-label _ (:text "" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label *info_label* (:text "Fractal Explorer v1.0" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "Interactive fractal" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "visualization with" :font *env_medium_font* :color *env_toolbar2_col*))
			(ui-label _ (:text "distributed rendering" :font *env_medium_font* :color *env_toolbar2_col*)))))

(defun tile (canvas data color_scheme)
	; (tile canvas data color_scheme) -> area
	(defq data (string-stream data) x (read-int data) y (read-int data)
		x1 (read-int data) y1 (read-int data) yp (dec y))
	(while (/= (++ yp) y1)
		(defq xp (dec x))
		(while (/= (++ xp) x1)
			(defq iter (read-char data))
			(defq color (apply-color-scheme iter 255 color_scheme))
			(.-> canvas (:set_color color) (:plot xp yp)))
		(task-slice))
	(* (- x1 x) (- y1 y)))

;native versions
(ffi "apps/fractalexplorer/tile" tile)
; (tile canvas data color_scheme) -> area

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> job
					(+job_key key)
					(+job_reply (elem-get select +select_reply)))))
		(:t ;no jobs in queue
			(undef val :job :timestamp))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/fractalexplorer/child.lisp" (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun update-param-labels ()
	(case current_fractal
		(+fractal_julia
			(set *param1_label* :text (cat "Julia Real: " (str (/ param1 (mbfp-from-fixed 1.0)))))
			(set *param2_label* :text (cat "Julia Imag: " (str (/ param2 (mbfp-from-fixed 1.0)))))
			(set *param3_label* :text "Unused"))
		(+fractal_phoenix
			(set *param1_label* :text (cat "Phoenix Real: " (str (/ param1 (mbfp-from-fixed 1.0)))))
			(set *param2_label* :text (cat "Phoenix Imag: " (str (/ param2 (mbfp-from-fixed 1.0)))))
			(set *param3_label* :text "Unused"))
		(+fractal_mandelbox
			(set *param1_label* :text (cat "Scale: " (str (/ param1 (mbfp-from-fixed 1.0)))))
			(set *param2_label* :text "Unused")
			(set *param3_label* :text "Unused"))
		(:t
			(set *param1_label* :text "Unused")
			(set *param2_label* :text "Unused")
			(set *param3_label* :text "Unused")))
	(.-> *param1_label* :layout :dirty)
	(.-> *param2_label* :layout :dirty)
	(.-> *param3_label* :layout :dirty))

(defun reset-view ()
	;reset to default view for current fractal
	(case current_fractal
		(+fractal_julia
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.5)))
		(+fractal_burning_ship
			(setq center_x (mbfp-from-fixed -0.5) center_y (mbfp-from-fixed -0.5)
				zoom (mbfp-from-fixed 0.8)))
		(+fractal_newton
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 2.0)))
		(+fractal_tricorn
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.0)))
		(+fractal_phoenix
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.5)))
		(+fractal_mandel3
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.2)))
		(+fractal_mandel4
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.2)))
		(+fractal_mandelbox
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 4.0)))
		(+fractal_lyapunov
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.0)))
		(:t
			(setq center_x (mbfp-from-fixed 0.0) center_y (mbfp-from-fixed 0.0)
				zoom (mbfp-from-fixed 1.0)))))

(defun reset ()
	(if farm (. farm :close))
	(elem-set select +select_reply (mail-mbox))
	(setq jobs (map (lambda (y)
			(setf-> (str-alloc +job_size)
				(+job_x 0)
				(+job_y y)
				(+job_x1 (* +width +scale))
				(+job_y1 (inc y))
				(+job_w (* +width +scale))
				(+job_h (* +height +scale))
				(+job_cx center_x)
				(+job_cy center_y)
				(+job_z zoom)
				(+job_fractal_type current_fractal)
				(+job_param1 param1)
				(+job_param2 param2)
				(+job_param3 param3)
				(+job_color_scheme current_color_scheme)))
			(range (dec (* +height +scale)) -1))
		farm (Farm create destroy (* 2 (length (lisp-nodes))))))

(defun main ()
	(defq select (task-mboxes +select_size))
	(.-> *canvas* (:fill +argb_black) (:swap 0))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(reset-view)
	(update-param-labels)
	(reset)
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

					((= id +event_fractal_type)
						;fractal type changed
						(defq sel_idx (. *fractal_selector* :get_selected))
						(setq current_fractal sel_idx)
						(reset-view)
						(update-param-labels)
						(reset))

					((= id +event_color_scheme)
						;color scheme changed
						(defq sel_idx (. *color_selector* :get_selected))
						(setq current_color_scheme sel_idx)
						(reset))

					((= id +event_param1_slider)
						;parameter 1 slider
						(defq val (. *param1_slider* :get_value))
						(setq param1 (mbfp-from-fixed (- (/ val 50.0) 1.0)))
						(update-param-labels)
						(reset))

					((= id +event_param2_slider)
						;parameter 2 slider
						(defq val (. *param2_slider* :get_value))
						(setq param2 (mbfp-from-fixed (- (/ val 50.0) 1.0)))
						(update-param-labels)
						(reset))

					((= id +event_param3_slider)
						;parameter 3 slider
						(defq val (. *param3_slider* :get_value))
						(setq param3 (mbfp-from-fixed (* (/ val 50.0) 2.0)))
						(update-param-labels)
						(reset))

					((= id +event_reset)
						;reset button
						(reset-view)
						(reset))

					((= id +event_zoom_in)
						;zoom in button
						(setq zoom (mbfp-mul zoom (mbfp-from-fixed 0.5)))
						(reset))

					((= id +event_zoom_out)
						;zoom out button
						(setq zoom (mbfp-mul zoom (mbfp-from-fixed 2.0)))
						(reset))

					((and (= id (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_mouse)
							(/= (getf msg +ev_msg_mouse_buttons) 0))
						;mouse click on the canvas view
						(bind '(w h) (. *canvas* :get_size))
						(defq rx (- (getf msg +ev_msg_mouse_rx) (/ (- w +width) 2))
							ry (- (getf msg +ev_msg_mouse_ry) (/ (- h +height) 2))
							buttons (getf msg +ev_msg_mouse_buttons))
						(cond
							((bits? buttons 4)
								;middle button - reset
								(reset-view)
								(reset))
							((bits? buttons 2)
								;right button - zoom out
								(setq center_x (+ center_x (mbfp-offset rx +width zoom))
									center_y (+ center_y (mbfp-offset ry +height zoom))
									zoom (mbfp-mul zoom (mbfp-from-fixed 2.0)))
								(reset))
							(:t
								;left button - zoom in
								(setq center_x (+ center_x (mbfp-offset rx +width zoom))
									center_y (+ center_y (mbfp-offset ry +height zoom))
									zoom (mbfp-mul zoom (mbfp-from-fixed 0.5)))
								(reset))))

					(:t (. *window* :event msg))))

			(+select_task
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(def val :child child)
					(dispatch-job key val)))

			(+select_reply
				;child response
				(defq key (get-long msg (- (length msg) +long_size)))
				(when (defq val (. farm :find key))
					(dispatch-job key val))
				(setq dirty :t)
				(tile *canvas* msg current_color_scheme))

			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(. farm :refresh +retry_timeout)
				(when dirty
					(setq dirty :nil)
					(. *canvas* :swap 0)
					(when (= 0 (length jobs))
						(defq working :nil)
						(. farm :each (lambda (key val)
							(setq working (or working (get :job val)))))
						(unless working (. farm :close)))))))
	;close window and children
	(. farm :close)
	(gui-sub-rpc *window*))
