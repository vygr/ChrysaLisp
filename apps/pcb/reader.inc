;;;;;;;;
; reader
;;;;;;;;

(defun pcb-read (data)
	; (pcb-read data) -> pcb
	(defq pcb (first (read (string-stream data))))
	(each! (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(setd paths '())
		;convert wires to vec3-f
		(each (lambda (wire)
			(each (lambda (p)
				(elem-set wire (!) (apply fixeds p))) wire)) wires)
		;convert paths to vec3-f
		(each (lambda (wire)
			(each (lambda (p)
				(elem-set wire (!) (apply fixeds p))) wire)) paths)
		;convert pads to vec3-f and vec2-f
		(each (lambda (p)
			(bind '(pad_radius pad_gap pad_pos pad_shape) p)
			(elem-set p 2 (apply fixeds pad_pos))
			(each (lambda (p)
				(elem-set pad_shape (!) (apply fixeds p))) pad_shape)) pads)) (list pcb) 1)
	pcb)
