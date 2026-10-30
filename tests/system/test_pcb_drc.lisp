(report-header "System: the Pcb design rule check, apps/science/pcb/drc.inc, on boards made to break it")

;each a board as the router prints it: (w h layers), then of each net
;(id track_radius via_radius gap (pad ...) (wire ...) (path ...)), a pad
;(radius gap (x y z) (shape point ...)). A gap is 0.127 throughout, so
;two tracks of radius 0.127 need their centres 0.381 apart
(defq *app_root* "apps/science/pcb/")
(import "apps/science/pcb/reader.inc")
(import "apps/science/pcb/drc.inc")

(defun pd-n (x)
	;a number as the router prints one, a fixed
	(str (n2f x)))
(defun pd-pad (x y z) (cat "(0.4 0.127 (" (pd-n x) " " (pd-n y) " " (pd-n z) ") ())"))
(defun pd-net (id pads paths)
	(cat "(" (str id) " 0.127 0.3 0.127 (" (apply (const cat) (cat (list "") pads)) ") () ("
		(apply (const cat) (cat (list "") (map (lambda (pth)
			(cat "(" (apply (const cat) (cat (list "") (map (lambda ((x y z)) (cat "(" (pd-n x) " " (pd-n y) " " (pd-n z) ")")) pth))) ")"))
			paths))) "))"))
(defun pd-check (&rest nets)
	(pcb-drc (pcb-read (cat "((20 20 2)" (apply (const cat) (cat (list "") nets)) ")"))))
(defun pd-track (id y1 y2 z1 z2)
	;a net across the board, x 2 to 18, pads at its ends
	(pd-net id (list (pd-pad 2 y1 z1) (pd-pad 18 y2 z2)) (list (list (list 2 y1 z1) (list 18 y2 z2)))))

(bind '(s u o) (pd-check (pd-track 1 5 5 0 0) (pd-track 2 6 6 0 0)))
(assert-eq "two tracks 1 apart, clear" 0 (length s))
(assert-list-eq "and each net joined" '() u)
(bind '(s &ignore) (pd-check (pd-track 1 5 5 0 0) (pd-track 2 5.3 5.3 0 0)))
(assert-true "0.3 apart, a short" (> (length s) 0))
(assert-list-eq "between the two nets" '(1 2) (sort (slice (first s) 0 2) (# (- %0 %1))))
(bind '(s &ignore) (pd-check (pd-track 1 5 15 1 1) (pd-track 2 15 5 0 0)))
(assert-eq "crossing on two layers, clear" 0 (length s))
(bind '(s &ignore) (pd-check (pd-track 1 5 15 0 0) (pd-track 2 15 5 0 0)))
(assert-true "crossing on one layer, a short" (> (length s) 0))

;a via goes through every layer
(bind '(s u &ignore) (pd-check
	(pd-net 1 (list (pd-pad 2 10 0) (pd-pad 18 10 1)) (list (list '(2 10 0) '(10 10 0) '(10 10 1) '(18 10 1))))
	(pd-track 2 10.3 10.3 1 1)))
(assert-true "a via by a track of the other layer, a short" (> (length s) 0))
(bind '(s u &ignore) (pd-check
	(pd-net 1 (list (pd-pad 2 10 0) (pd-pad 18 10 1)) (list (list '(2 10 0) '(10 10 0) '(10 10 1) '(18 10 1))))))
(assert-list-eq "and a net that changes layer by it is joined" '() u)

;an SMT pad is its shape filled
(defq pd_smt "(0.0 0.127 (10.0 10.0 0.0) ((-1.0 -1.0)(1.0 -1.0)(1.0 1.0)(-1.0 1.0)(-1.0 -1.0)))")
(bind '(s &ignore) (pd-check (pd-net 1 (list pd_smt) '()) (pd-track 2 10 10 0 0)))
(assert-true "a track across the inside of another net's pad, a short" (> (length s) 0))
(bind '(s &ignore) (pd-check (pd-net 1 (list pd_smt) '()) (pd-track 2 13 13 0 0)))
(assert-eq "and 3 from its middle, 2 from its edge, clear" 0 (length s))
(bind '(& u &ignore) (pd-check (pd-net 1 (list pd_smt (pd-pad 18 10 0)) (list '((10 10 0) (18 10 0))))))
(assert-list-eq "a track from inside its own pad joins it" '() u)

(bind '(& u &ignore) (pd-check (pd-net 1 (list (pd-pad 2 5 0) (pd-pad 18 5 0)) '())))
(assert-list-eq "a net with no track between its pads, unjoined" '(1) u)
(bind '(& u &ignore) (pd-check (pd-net 1 (list (pd-pad 2 5 0) (pd-pad 18 5 0)) (list '((2 5 0) (10 5 0))))))
(assert-list-eq "and with half of one" '(1) u)
(bind '(& u &ignore) (pd-check (pd-net 1 (list (pd-pad 2 5 0) (pd-pad 2 5 1) (pd-pad 18 5 1)) (list '((2 5 1) (18 5 1))))))
(assert-list-eq "a pad through the board, met on one layer, is joined by its hole" '() u)
(bind '(& & o) (pd-check (pd-net 1 (list (pd-pad 2 5 0) (pd-pad 18 5 0)) (list '((2 5 0) (2 25 0) (18 5 0))))))
(assert-list-eq "a track off the board" '(1) o)

;and what the router makes: some nets of test1, routed, have no short
(import "apps/science/pcb/app.inc")
(import "apps/science/pcb/router.inc")
(defq pd_list (pcb-read (load "apps/science/pcb/data/test1.pcb")))
(bind '(pd_w pd_h pd_d) (first pd_list))
(defq pcb (Pcb pd_w pd_h pd_d 1 0 0 2 1 1))
(each! (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(setq pads (map (lambda ((radius gap pos shape)) (Pad radius gap pos shape)) pads))
		(. pcb :add_track (Track id track_radius via_radius track_gap pads '())))
	(list pd_list) 1)
;every pad on the board first, as (. pcb :route) has them
(. pcb :remove_netlist)
(each! (# (. %0 :route)) (list (get :netlist pcb)) 0 8)
(. pcb :print_pcb (defq pd_ss (string-stream (cat ""))))
(bind '(s & o) (pcb-drc (pcb-read (str pd_ss))))
(assert-eq "nets of test1 the router routed, no short" 0 (length s))
(assert-list-eq "and none off the board" '() o)

;the router checks the board it ends with: clean with nets not routed,
;they are not counted, and not when a net's track lies on another's
(defq pd_nets (get :netlist pcb))
(assert-true "the routed nets of test1, the check at the end passes" (. pcb :drc_clean pd_nets))
(defq pd_a (some (# (if (nempty? (get :paths %0)) %0)) pd_nets)
	pd_b (some (# (and (nempty? (get :paths %0)) (not (eql %0 pd_a)) %0)) pd_nets)
	pd_saved (cat (get :paths pd_a)))
(each (# (push (get :paths pd_a) %0)) (get :paths pd_b))
(assert-eq "a net given another's track, the check fails" :nil (. pcb :drc_clean pd_nets))
(clear (get :paths pd_a))
(each (# (push (get :paths pd_a) %0)) pd_saved)
(assert-true "and passes when it is put back" (. pcb :drc_clean pd_nets))
