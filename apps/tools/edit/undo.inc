;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defq *global_undo_stack* (list) *global_redo_stack* (list))

(defun action-undo ()
	(ifn (defq transaction (pop *global_undo_stack*))
		(. *edit* :undo)
		(progn
			;if the active file is in the transaction, swap it to the end
			(if (defq idx (some (# (if (eql (first %0) *current_file*) (!))) transaction))
				(swap transaction idx (dec (length transaction))))
			;redo each modified buffer ending back where we are
			(push *refresh_mode* +refresh_mode_hidden)
			(each (lambda ((file old_mark &))
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
			;if the active file is in the transaction, swap it to the end
			(if (defq idx (some (# (if (eql (first %0) *current_file*) (!))) transaction))
				(swap transaction idx (dec (length transaction))))
			;redo each modified buffer ending back where we are
			(push *refresh_mode* +refresh_mode_hidden)
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
