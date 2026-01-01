;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	;drop the service entry now !
	(mail-forget entry)
	;few seconds delay till exit !
	;just let any stray debug sends arrive.
	(mail-timeout (elem-get select +select_exit) +rate_exit 0))

(defun action-hvalue ()
	(reset (get :value *hslider*)))

(defun action-clear ()
	(when selected_index
		(step (elem-get buf_list selected_index))
		(setq buf_keys (erase buf_keys selected_index (inc selected_index)))
		(setq buf_list (erase buf_list selected_index (inc selected_index)))
		(reset (min selected_index (dec (length buf_list))))))

(defun action-clear-all ()
	(each step buf_list)
	(reset))

(defun action-play ()
	(when selected_index
		(play (elem-get buf_list selected_index))))

(defun action-forward ()
	(when selected_index
		(forward (elem-get buf_list selected_index))))

(defun action-pause ()
	(when selected_index
		(pause (elem-get buf_list selected_index))))

(defun action-step ()
	(when selected_index
		(pause (elem-get buf_list selected_index))
		(step (elem-get buf_list selected_index))))

(defun action-play-all ()
	(each play buf_list))

(defun action-forward-all ()
	(each forward buf_list))

(defun action-pause-all ()
	(each pause buf_list))

(defun action-step-all ()
	(each pause buf_list)
	(each step buf_list))
