;jit compile apps native functions
(import "sys/lisp.inc")
(jit "apps/pcb/" "lisp.vp" '("scan_buckets"))

(import "class/lisp.inc")
;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "./app.inc")
(import "./reader.inc")
(import "./router.inc")

(enums +select 0
	(enum main timeout))

(defun gen-vectors (vec_range x_range y_range)
	;generate range of routing vectors
	(defq out (list) vec_range (n2f vec_range) y (neg y_range))
	(while (<= y y_range)
		(defq x (neg x_range))
		(while (<= x x_range)
			(defq l (vec-length (Vec2-f (n2f x) (n2f y))))
			(and (> l 0.1) (<= l vec_range)
				(push out (Node x y 0)))
			(setq x (inc x)))
		(setq y (inc y)))
	out)

(defun route (mbox data)
	(defq pcb_list (pcb-read data))
	(bind '(width height depth) (elem-get 0 pcb_list))
	(bind '(res verb quant viascost) '(1 1 1 0))
	(bind '(flood_range flood_range_even_layer flood_range_odd_layer) '(2 1 1))
	(defq rfvs (list
			(gen-vectors flood_range flood_range_even_layer flood_range)
			(gen-vectors flood_range flood_range flood_range_odd_layer))
		pcb (Pcb width height depth rfvs res verb quant viascost))
	(each! 1 -1 (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(task-slice)
		(setq wires '() pads (map (lambda ((radius gap pos shape))
			(Pad radius gap pos shape)) pads))
		(defq track (Track id track_radius via_radius track_gap pads wires))
		(. pcb :add_track track)) (list pcb_list))
	(. pcb :route select mbox)
	;(. pcb :print_pcb (file-stream "apps/pcb/data/test4.pcb" +file_open_write))
	(. pcb :close))

(defun main ()
	(defq select (alloc-select +select_size) running t +timeout 5000000)
	(while running
		(mail-timeout (elem-get +select_timeout select) +timeout 0)
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem-get +select_timeout select) 0 0)
				(mail-timeout (elem-get +select_timeout select) 1000000000 0)
				(route (getf msg +job_reply) (slice +job_data -1 msg)))))
	(free-select select)
	(if (get 'profile-report)
		(profile-report "Pcb")))
