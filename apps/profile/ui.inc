;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-hvalue ()
	(reset (get :value *hslider*)))

(defun action-clear ()
	(when selected_index
		(setq buf_keys (cat (slice 0 selected_index buf_keys) (slice (inc selected_index) -1 buf_keys)))
		(setq buf_list (cat (slice 0 selected_index buf_list) (slice (inc selected_index) -1 buf_list)))
		(reset (min selected_index (dec (length buf_list))))))

(defun action-clear-all ()
	(reset))
