;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	;drop the service entry now !
	(mail-forget entry)
	;few seconds delay till exit !
	;just let any stray debug sends arrive.
	(mail-timeout (elem-get +select_exit select) +rate_exit 0))

(defun action-hvalue ()
	(reset (get :value *hslider*)))

(defun action-clear ()
	(when selected_index
		(step (elem-get selected_index buf_list))
		(setq buf_keys (cat (slice 0 selected_index buf_keys) (slice (inc selected_index) -1 buf_keys)))
		(setq buf_list (cat (slice 0 selected_index buf_list) (slice (inc selected_index) -1 buf_list)))
		(reset (min selected_index (dec (length buf_list))))))

(defun action-clear-all ()
	(each step buf_list)
	(reset))

(defun action-play ()
	(when selected_index
		(play (elem-get selected_index buf_list))))

(defun action-forward ()
	(when selected_index
		(forward (elem-get selected_index buf_list))))

(defun action-pause ()
	(when selected_index
		(pause (elem-get selected_index buf_list))))

(defun action-step ()
	(when selected_index
		(pause (elem-get selected_index buf_list))
		(step (elem-get selected_index buf_list))))

(defun action-play-all ()
	(each play buf_list))

(defun action-forward-all ()
	(each forward buf_list))

(defun action-pause-all ()
	(each pause buf_list))

(defun action-step-all ()
	(each pause buf_list)
	(each step buf_list))
