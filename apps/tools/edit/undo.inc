;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defq *global_undo_stack* (list) *global_redo_stack* (list))

(defun action-undo ()
	(if (empty? *global_undo_stack*)
		(. *edit* :undo)
		(progn
			(defq transaction (pop *global_undo_stack*))
			;undo each modified buffer in reverse chronological order
			(reach (lambda ((file mark))
					(unless (eql file *current_file*) (switch-file file))
					(. *edit* :rewind mark))
				transaction)
			(push *global_redo_stack* transaction)))
	(refresh))

(defun action-redo ()
	(if (empty? *global_redo_stack*)
		(. *edit* :redo)
		(progn
			(defq transaction (pop *global_redo_stack*))
			;redo each modified buffer in original chronological order
			(each (lambda ((file mark))
					(unless (eql file *current_file*) (switch-file file))
					(. *edit* :forwardwind mark))
				transaction)
			(push *global_undo_stack* transaction)))
	(refresh))

(defun action-rewind ()
	(. *edit* :rewind)
	;clean up the global stacks for this file since its local history is gone
	(setq *global_undo_stack* (clean-stack *global_undo_stack* *current_file*)
		*global_redo_stack* (clean-stack *global_redo_stack* *current_file*))
	(refresh))

(defun action-forwardwind ()
	(. *edit* :redo)
	;clean up the global stacks for this file since its local history is gone
	(setq *global_undo_stack* (clean-stack *global_undo_stack* *current_file*)
		*global_redo_stack* (clean-stack *global_redo_stack* *current_file*))
	(refresh))