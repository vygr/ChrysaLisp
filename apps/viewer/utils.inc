;;;;;;;
; utils
;;;;;;;

(import "././edit/utils.inc")

(defun switch-file (file)
	;switch to file or scratch (:nil)
	(populate-vdu file)
	(select-node file))

(defun open-file (file)
	;open file or scratch (:nil)
	(and file (not (find file *open_files*))
		(push *open_files* file))
	(switch-file file))
