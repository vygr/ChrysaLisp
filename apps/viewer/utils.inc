;;;;;;;
; utils
;;;;;;;

(import "././edit/utils.inc")

(defun switch-file (file)
	;switch to file
	(populate-vdu file)
	(select-node file))
