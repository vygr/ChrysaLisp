;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-hvalue ()
	(reset (get :value *hslider*)))

(defun action-clear ()
	(when selected_index
		(setq buf_keys (erase buf_keys selected_index (inc selected_index)))
		(setq buf_list (erase buf_list selected_index (inc selected_index)))
		(reset (min selected_index (dec (length buf_list))))))

(defun action-clear-all ()
	(reset))
