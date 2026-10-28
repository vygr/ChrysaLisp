;debug options
(case :nil
(0 (import "lib/debug/frames.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")

;Stores, what is in stores: two sides, each the files of this machine or
;a store, this machine's or another's, as folders, and what is taken on
;one copied to the other, from files into a store, out of one, or from
;store to store with no file between. A folder copied onto one of the
;same name is the two merged. What is in a store can be deleted, and how
;many members are to keep it set. What is done is done by a task of its
;own, ./work.lisp, that says how it is going, and can be cancelled.

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main tip work))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	;what each side can be, the two sides, the one last clicked, the rule for
	;what is there already, and what is being done: the task doing it,
	;what it said it has to do, (count bytes), and a delete asked once
	(defq select (task-mboxes +select_size) *running* :t *work_mbox* (elem-get select +select_work)
		*sources* (list) *sides* (list) *active* 0 *rule* :skip
		*work* :nil *doing* :nil *plan* (list 0 0) *faults* 0 *delete* :nil)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(stores-start)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;the rows sized again now the window has its size
	(stores-start)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_work)
				(dispatch-action work-heard *msg*))
			;a row of a side, its name or its open button
			((and (= (getf *msg* +ev_msg_type) +ev_type_action)
					(>= (defq id (getf *msg* +ev_msg_target_id)) +event_rows) (< id (+ +event_rows (* 4 +rows_max))))
				(defq n (- id +event_rows) s (/ n (* 2 +rows_max)) n (% n (* 2 +rows_max)))
				(dispatch-action action-row s (/ n 2) (% n 2)))
			;must be +select_main
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(gui-sub-rpc *window*))
