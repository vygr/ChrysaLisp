;jit compile apps native functions
(defq *app_root* (path-to-file))
(jit *app_root* "lisp.vp" '("hit_line" "add_line" "sub_line"))

(import "./app.inc")
(import "./reader.inc")
(import "./router.inc")

(enums +select 0
	(enum main timeout))

(defun route (select reply_mbox prog_mbox
		grid_res vias_cost quant flood_range even_range odd_range data)
	(defq pcb_list (pcb-read data))
	(bind '(width height depth) (first pcb_list))
	(bind '(verb) '(1))
	(defq pcb (Pcb width height depth grid_res verb quant vias_cost flood_range even_range odd_range))
	(each! (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(unless (mail-poll select)
			(task-slice)
			(setq wires '() pads (map (lambda ((radius gap pos shape))
				(Pad radius gap pos shape)) pads))
			(defq track (Track id track_radius via_radius track_gap pads wires))
			(. pcb :add_track track))) (list pcb_list) 1)
	(. pcb :route select reply_mbox prog_mbox)
	(. pcb :close))

(defun main ()
	(defq select (task-mboxes +select_size) running :t)
	(mail-timeout (elem-get select +select_timeout) 5000000 0)
	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, cancel timeout and reply with result
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(apply (const route) (cat
					(list select)
					(getf-> msg +job_reply +job_prog
						+job_grid_res +job_vias_cost +job_quant
						+job_flood_range +job_even_range +job_odd_range)
					(list (slice msg +job_data -1)))))))
	(profile-report "Pcb"))
