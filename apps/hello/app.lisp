;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;add close event
(structure 'event 0
  (byte 'win_close))
(structure 'event 1
  (byte 'click))

;add event id
(defq id t)

;create a window with a label
(ui-tree window (create-window window_flag_close) nil	
  (ui-element button (create-button) ('text "Press Me" 'text_color argb_blue 'color argb_green 'flow_flags flow_flag_fillw  'font (create-font "fonts/OpenSans-Regular.ttf" 18)))
  (ui-element label (create-label) ('text "ChrysaLisp" 'color argb_white 'flow_flags flow_flag_fillw 'font (create-font "fonts/OpenSans-Regular.ttf" 18))))

;set a name to the window
(window-set-title window "Hello")

;bind events
(window-connect-close window event_win_close)
(button-connect-click button event_click) 

;window with a 200w wide padding
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 256 128 (add w 200) h))

;main app loop
(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_close)
			(setq id nil))
    ((eq id event_click)
      (set button 'color argb_red)
      (view-dirty button)
      (set label 'text "The label is red!")
      (view-dirty label))
		(t (view-event window msg))))
