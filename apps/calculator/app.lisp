;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'close+ 'max+ 'min+)
	(byte 'button+))

(ui-window mywindow ()
	(ui-title-bar _ "Calculator" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-label display (:text "0" :color +argb_white+ :flow_flags flow_flag_align_hright :font (create-font "fonts/OpenSans-Regular.ctf" 24)))
	(ui-grid _ (:grid_width 4 :grid_height 4 :color *env_toolbar_col* :font (create-font "fonts/OpenSans-Regular.ctf" 42))
		(each (lambda (text)
			(component-connect (ui-button _ (:text (if (eql text "C") "AC" text))) +event_button+))
			"789/456*123-0=C+")))

(defun do_lastop ()
	(cond
		((eql lastop "+")
			(setq accum (+ accum num)))
		((eql lastop "-")
			(setq accum (- accum num)))
		((eql lastop "*")
			(setq accum (* accum num)))
		((eql lastop "/")
			(if (/= num 0) (setq accum (/ accum num)))))
	accum)

(defun main ()
	(bind '(x y w h) (apply view-locate (view-pref-size mywindow)))
	(gui-add (view-change mywindow x y w h))
	(defq accum 0 value 0 num 0 lastop nil)
	(while (cond
		((>= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +event_button+)
			(defq op (get :text (view-find-id mywindow (get-long msg ev_msg_action_source_id))))
			(cond
				((eql op "AC")
					(setq accum 0 value 0 num 0 lastop nil))
				((find op "=+-/*")
					(if lastop
						(setq value (do_lastop))
						(setq value num accum num))
					(setq lastop op num 0))
				(t
					(cond
						((= num 0)
							(unless (eql op "0"))
								(setq num (str-to-num op)))
						(t (setq num (str-to-num (cat (str num) op)))))
					(setq value num)))
			(set display :text (str value))
			(view-dirty (view-layout display)))
		((= id +event_close+)
			;close button
			nil)
		((= id +event_min+)
			;min button
			(bind '(x y w h) (apply view-fit (cat (view-get-pos mywindow) (view-pref-size mywindow))))
			(view-change-dirty mywindow x y w h))
		((= id +event_max+)
			;max button
			(bind '(x y w h) (apply view-fit (cat (view-get-pos mywindow) '(512 512))))
			(view-change-dirty mywindow x y w h))
		(t (view-event mywindow msg))))
	(view-hide mywindow))
