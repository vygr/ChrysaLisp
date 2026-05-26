;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defq *global_undo_stack* (list) *global_redo_stack* (list))

(defun action-undo ()
	(ifn (defq transaction (pop *global_undo_stack*))
		(. *edit* :undo)
		(progn
			;undo each modified buffer in reverse chronological order
			(push *refresh_mode* 1)
			(reach (lambda ((file old_mark &))
					(ifn (eql file *current_file*) (switch-file file))
					(. *edit* :rewind old_mark))
				transaction)
			(push *global_redo_stack* transaction)
			(pop *refresh_mode*)))
	(refresh))

(defun action-redo ()
	(ifn (defq transaction (pop *global_redo_stack*))
		(. *edit* :redo)
		(progn
			;redo each modified buffer in original chronological order
			(push *refresh_mode* 1)
			(each (lambda ((file & new_mark))
					(ifn (eql file *current_file*) (switch-file file))
					(. *edit* :forwardwind new_mark))
				transaction)
			(push *global_undo_stack* transaction)
			(pop *refresh_mode*)))
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