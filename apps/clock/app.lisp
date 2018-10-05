;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;add event id
(defq id t)

(defun make_time ()
  (defun pad (str) 
    (if (eq (length str) 1) (cat "0" str) str))
  (defq sec (div (time) 1000000))
  (defq seconds (mod sec 60))
  (defq minutes (mod (div sec 60) 60))
  (defq hours (mod (div sec 60 60) 24))
  (cat (pad (str hours)) ":" (pad (str minutes)) ":" (pad (str seconds))))

; (debug (time))
(debug (make_time))

;define events we will use
(structure 'event 0
  (byte 'close))

;create a window with a label
(ui-tree window (create-window window_flag_close) nil
  (ui-element label (create-label) ('text (make_time) 'color argb_yellow 'flow_flags flow_flag_fillw
    'font (create-font "fonts/input_mono_regular.ttf" 20))))

;set a name to the window
(window-set-title window "Clock")

;bind events
(window-connect-close window event_close)

;window with at least 200 wide
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 290 16 w h))

;main app loop
(while id
  (cond
    ((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_close)
      (setq id nil))
    (t (view-event window msg))))
