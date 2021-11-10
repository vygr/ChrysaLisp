;;;;;;;;
; reader
;;;;;;;;

(defun pcb-read (data)
	; (pcb-read data) -> pcb
	(defq pcb (elem-get 0 (read (string-stream data) (ascii-code " "))))
	(each! 1 -1 (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(setd paths '())
		;convert wires to vec3-f
		(each (lambda (wire)
			(each (lambda (p)
				(elem-set _ wire (apply fixeds p))) wire)) wires)
		;convert paths to vec3-f
		(each (lambda (wire)
			(each (lambda (p)
				(elem-set _ wire (apply fixeds p))) wire)) paths)
		;convert pads to vec3-f and vec2-f
		(each (lambda (p)
			(bind '(pad_radius pad_gap pad_pos pad_shape) p)
			(elem-set 2 p (apply fixeds pad_pos))
			(each (lambda (p)
				(elem-set _ pad_shape (apply fixeds p))) pad_shape)) pads)) (list pcb))
	pcb)
