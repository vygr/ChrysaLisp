;;;;;;;;;
; texture
;;;;;;;;;

(defun texture-metrics (this)
	; (texture-metrics texture) -> (handle width height)
	(list (get-field this (const texture_handle) +long_size+)
		(get-field this (const texture_width) (const (neg +int_size+)))
		(get-field this (const texture_height) (const (neg +int_size+)))))
