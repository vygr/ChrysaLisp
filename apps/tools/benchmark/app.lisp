(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./widgets.inc")

(enums +select 0
	(enum main child_reply tip))

(defq *running* :t
	*chart_list* (list) *bar_list* (list) *mean_accum* (list)
	*best_val* +max_long *worst_val* 0
	*scale_min* +max_long *scale_max* 0
	*margin_percent* 5 *smooth_steps* 100
	*run_count* 0 *child* :nil)

(defun smooth-result (results val)
	(if (> (length (push results val)) *smooth_steps*)
		(setq results (rest results)))
	(list results (/ (reduce (const +) results 0) (length results))))

(defun glide (current target)
	; Smoothly relaxes current towards target using integer division
	(defq diff (- target current) step (/ diff 10))
	(if (and (= step 0) (/= diff 0))
		(setq step (if (> diff 0) 1 -1)))
	(+ current step))

(defun update-scale-custom (chart min_val max_val)
	(defq scale (.-> (get :scale_grid chart) :dirty_all :children)
		scale_range (- max_val min_val))
	(each (lambda (mark)
		; Calculate evenly divided values from min_val to max_val
		(defq val (+ min_val (/ (* scale_range (inc (!))) (length scale))))
		(def mark :text (str val "|"))
		(. mark :constrain :t)) scale))

(defun update-display (vals)
	; Determine range of display (safely check for uninitialized bounds)
	(defq min_val (if (= *scale_min* +max_long) 0 *scale_min*)
		max_val (if (= *scale_max* 0) 0 *scale_max*)
		scale_range (- max_val min_val))
	(if (<= scale_range 0) (setq scale_range 1))

	; Map over charts, bars, and values simultaneously (netmon style)
	(each (lambda (chart bar val)
			; Apply the custom bracketed scale to the chart markers
			(update-scale-custom chart min_val max_val)
			; Set the progress bar offset relative to the minimum scale boundary
			(def bar :maximum scale_range :value (max 0 (- val min_val)))
			(. bar :dirty))
		*chart_list* *bar_list* vals)

	; Redraw window
	(.-> *window* :layout :dirty_all))

(defun main ()
	(defq select (task-mboxes +select_size))
	(def *window* :tip_mbox (elem-get select +select_tip))

	; Retrieve the chart widgets from the UI grid and append progress bars
	(setq *chart_list* (. *charts* :children)
		*bar_list* (map (# (. %0 :add_bar)) *chart_list*))

	; Explicitly set the minimum height of the progress bars
	(each (# (def %0 :min_height 32)) *bar_list*)

	; Position window
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; Start the child benchmark task
	(setq *child* (open-child (cat *app_root* "child.lisp") +kn_call_child))
	
	; Initial empty display
	(update-display (list 0 0 0))

	; Kick off first run by sending reply mailbox to child
	(mail-send *child* (elem-get select +select_child_reply))

	; Main event loop
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_child_reply)
				; Received a new benchmark result from the child
				(defq duration (str-as-num msg))
				(when (> duration 0)
					; Increment run counter
					(++ *run_count*)
					; Calculate smoothed mean
					(bind '(new_accum mean_val) (smooth-result *mean_accum* duration))
					(setq *mean_accum* new_accum)
					; If at a smooth step boundary, reset best/worst to current mean
					(if (= (% *run_count* *smooth_steps*) 0)
						(setq *best_val* mean_val *worst_val* mean_val)
						; Otherwise, update sticky min/max
						(setq *best_val* (min *best_val* duration)
							*worst_val* (max *worst_val* duration)))
					; Calculate dynamic margin (based on the defined percentage of the span, min 500 us)
					(defq margin (max 500 (/ (* (- *worst_val* *best_val*) *margin_percent*) 100))
						target_min (- *best_val* margin) target_max (+ *worst_val* margin))
					; Glide boundaries smoothly to target values (expansion and contraction)
					(if (= *scale_min* +max_long)
						(setq *scale_min* target_min)
						(setq *scale_min* (glide *scale_min* target_min)))
					(if (= *scale_max* 0)
						(setq *scale_max* target_max)
						(setq *scale_max* (glide *scale_max* target_max)))
					; Update UI with consolidated list of values
					(update-display (list mean_val *best_val* *worst_val*))
					; Update title bar with formatted elapsed seconds
					(def *title* :text (cat "Benchmark -> " (time-in-seconds mean_val) " seconds"))
					(.-> *title* :layout :dirty))
				; Poll for the next run if the parent is still active
				(mail-send *child* (elem-get select +select_child_reply)))
			((= idx +select_main)
				(if (= (defq id (getf msg +ev_msg_target_id)) +event_close)
					(setq *running* :nil)
					(. *window* :event msg)))))
	; Clean up and signal child to terminate
	(if *child* (mail-send *child* ""))
	(gui-sub-rpc *window*))