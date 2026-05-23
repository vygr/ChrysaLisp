;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defq *global_undo_stack* (list) *global_redo_stack* (list))

(defun action-undo ()
	(if (empty? *global_undo_stack*)
		(. *edit* :undo)
		(progn
			;undo each modified buffer in reverse chronological order
			(reach (# (unless (eql %0 *current_file*) (switch-file %0))
					(. *edit* :undo))
				(defq transaction (pop *global_undo_stack*)))
			(push *global_redo_stack* transaction)))
	(refresh))

(defun action-redo ()
	(if (empty? *global_redo_stack*)
		(. *edit* :redo)
		(progn
			;redo each modified buffer in original chronological order
			(each (# (unless (eql %0 *current_file*) (switch-file %0))
					(. *edit* :redo))
				(defq transaction (pop *global_redo_stack*)))
			(push *global_undo_stack* transaction)))
	(refresh))

(defun action-rewind ()
	(. *edit* :rewind)
	;clean up the global stacks for this file since its local history is gone
	(setq *global_undo_stack* (clean-stack *global_undo_stack* *current_file*))
	(setq *global_redo_stack* (clean-stack *global_redo_stack* *current_file*))
	(refresh))