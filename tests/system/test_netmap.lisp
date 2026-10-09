(report-header "Network Map: its links, their heat and the bedspring, with no desktop")

(import "apps/system/netmap/map.inc")

;the app's own names, as it has them, and the thing a link is drawn with
(defq global_tasks (Fmap 11) links (Fmap 11) top_rate (n2r +quiet) zoom +real_1 changed :nil)
(defun make-bar () :bar)
(defun nm-id (n) (cat (char n +long_size) (char 0 +long_size)))
(defun nm-node (n &rest peers)
	;a node, and what it says its links are to, each (peer sent)
	(def (defq node (env 1)) :key (nm-id n) :system "" :tasks 0
		:pos (reals (n2r (* n 3)) (n2r n) (n2r (- 0 n))) :vel (reals +real_0 +real_0 +real_0)
		:links (map (lambda ((peer sent)) (list (nm-id peer) sent)) peers))
	(. global_tasks :insert (nm-id n) node)
	node)
(defun nm-says (n &rest peers)
	(def (. global_tasks :find (nm-id n)) :links (map (lambda ((peer sent)) (list (nm-id peer) sent)) peers)))
(defun nm-link (a b) (. links :find (cat (nm-id a) (nm-id b))))
(defun nm-dist (a b)
	(defq d (nums-sub (get :pos (. global_tasks :find (nm-id a))) (get :pos (. global_tasks :find (nm-id b)))))
	(sqrt (nums-dot d d)))

;the color of a machine is from its id
(defq nm_sys1 (cat (char 0x1234567 +long_size) (char 0x89abcd +long_size))
	nm_sys2 (cat (char 0x7654321 +long_size) (char 0x1111 +long_size)))
(assert-eq "a color is three parts" 3 (length (machine-color nm_sys1)))
(assert-true "each between none and all of it"
	(every (# (<= +real_0 %0 +real_1)) (cat (machine-color nm_sys1) (machine-color nm_sys2))))
(assert-true "the same machine is the same color every time" (eql (str (machine-color nm_sys1)) (str (machine-color nm_sys1))))
(assert-true "it is bright, one part is all of it" (some (# (= %0 +real_1)) (machine-color nm_sys1)))
(assert-eq "a node that has not said what machine it is on has a color" 3 (length (machine-color "")))

;two machines whose ids come to hues that are next to each other are told apart
(defun nm-apart (a b) (min (defq d (abs (- a b))) (- 360 d)))
(test-cases
	(hue-clear 200 (list 65)) 200
	(hue-clear 58 (list 65)) 138
	(hue-clear 350 (list 10)) 70
	(hue-clear 58 (list)) 58)
(assert-true "with no room left it still gives a hue, it does not go round for ever"
	(<= 0 (hue-clear 5 (list 0 40 80 120 160 200 240 280 320)) 359))
;a Mac, and a network of its own that was added to it, 7 apart, 9 October
(defq machines (list) machine_hues (list)
	nm_mac (hex-decode "8921786E6C1619AE9B9398F23F3CD8F6") nm_net "0axccjvvwn61rhle")
(assert-eq "the hue of one machine is from its id" 65 (machine-hue nm_mac))
(assert-eq "and of another, next to it" 58 (machine-hue nm_net))
(assert-eq "the first there has its own" 65 (machines-heard nm_mac))
(assert-true "the one that comes after is clear of it" (>= (nm-apart 65 (machines-heard nm_net)) +hue_gap))
(assert-eq "and has the same again each time it is heard" (machines-heard nm_net) (machines-heard nm_net))
(assert-eq "the first has not moved" 65 (machines-heard nm_mac))
(assert-eq "there are two machines" 2 (length machines))
;a machine with no node left is gone, and the hue it had is free
(defq nm_keep global_tasks global_tasks (Fmap 11))
(def (defq nm_node (env 1)) :system nm_net)
(. global_tasks :insert "a" nm_node)
(machines-prune)
(assert-list-eq "a machine with no node left is gone" (list nm_net) machines)
(assert-eq "its hue with it" 1 (length machine_hues))
(assert-eq "one that comes back to an empty place has its own hue" 65 (machines-heard nm_mac))
(setq global_tasks nm_keep)

;the links, from what each end says
(nm-node 1 '(2 1000)) (nm-node 2 '(1 500) '(9 77)) (nm-node 3)
(links-gather)
(assert-eq "two nodes that each say the other, one link" 1 (. links :size))
(assert-true "it is made the once, lower id first, and is new" (and (nm-link 1 2) changed))
(assert-eq "what it has carried is what both ends sent" 1500 (get :sent (nm-link 1 2)))
(assert-eq "a link to a node that is not known is not one" :nil (. links :find (cat (nm-id 2) (nm-id 9))))
(assert-eq "it has the bar the app gave it" :bar (get :bar (nm-link 1 2)))
(assert-true "a link that has only just been seen is cold" (= (get :heat (nm-link 1 2)) +real_0))

;heat. A flow comes up smoothly, and goes down
(defq nm_sent 1500 nm_heats (list))
(times 6 (setq nm_sent (+ nm_sent 100000)) (nm-says 1 (list 2 nm_sent)) (nm-says 2 '(1 0))
	(links-gather) (push nm_heats (get :heat (nm-link 1 2))))
(assert-true "a flow that lasts, the heat climbs each poll" (every (# (< %0 %1)) nm_heats (rest nm_heats)))
(assert-true "it does not jump to hot at once" (< (first nm_heats) (const (n2r 0.5))))
(assert-true "and is most of the way there after six" (> (last nm_heats) (const (n2r 0.7))))
(assert-true "it is never more than all" (<= (last nm_heats) +real_1))
(defq nm_top (last nm_heats))
(times 6 (links-gather) (push nm_heats (get :heat (nm-link 1 2))))
(assert-true "the flow stops, the heat falls" (< (last nm_heats) (* nm_top (const (n2r 0.5)))))
(setq nm_sent (+ nm_sent 100)) (nm-says 1 (list 2 nm_sent))
(times 40 (links-gather))
(setq nm_sent (+ nm_sent 500)) (nm-says 1 (list 2 nm_sent)) (links-gather)
(assert-true "a trickle on a quiet network is not hot" (< (get :heat (nm-link 1 2)) (const (n2r 0.1))))

;a link goes when an end stops saying it
(nm-says 1) (nm-says 2)
(setq changed :nil)
(links-gather)
(assert-eq "neither end says it, the link is gone" 0 (. links :size))
(assert-true "and that is a change" changed)

;the bedspring
(nm-says 1 '(2 0)) (nm-says 2 '(1 0))
(links-gather)
(times 400 (spring-step))
(defq nm_linked (nm-dist 1 2) nm_loose (nm-dist 1 3))
(assert-true "two nodes with a link settle about the spring's own length apart"
	(< (* +rest (const (n2r 0.8))) nm_linked (* +rest (const (n2r 1.2)))))
(assert-true "a node with no link is pushed further off than that" (> nm_loose nm_linked))
(defq nm_mid (reduce (# (nums-add %0 (get :pos (. global_tasks :find (nm-id %1))))) '(1 2 3) (reals +real_0 +real_0 +real_0)))
(assert-true "the middle of it all stays in the middle" (< (nums-dot nm_mid nm_mid) (const (n2r 0.0001))))
(assert-true "it is drawn to fit, the furthest node is where it should be"
	(< (abs (- (* zoom (sqrt (max (nums-dot (get :pos (. global_tasks :find (nm-id 3))) (get :pos (. global_tasks :find (nm-id 3))))
		(nums-dot (get :pos (. global_tasks :find (nm-id 1))) (get :pos (. global_tasks :find (nm-id 1))))
		(nums-dot (get :pos (. global_tasks :find (nm-id 2))) (get :pos (. global_tasks :find (nm-id 2))))))) +fit))
		(const (n2r 0.05))))
(defq nm_before (nm-dist 1 2))
(times 400 (spring-step))
(assert-true "left alone it stays where it settled" (< (abs (- (nm-dist 1 2) nm_before)) (const (n2r 0.01))))
;a hot link pulls its ends together
(def (nm-link 1 2) :heat +real_1)
(times 400 (spring-step))
(assert-true "a hot link is shorter" (< (nm-dist 1 2) (* nm_linked (const (n2r 0.8)))))
(assert-true "and its ends are not on top of each other" (> (nm-dist 1 2) (* nm_linked (const (n2r 0.3)))))

;how hard a node works, from how long it says it has been idle
(defun nm-worker () (def (defq node (env 1)) :idle 0 :time 0 :busy +real_0) node)
(defq nm_work (nm-worker))
(node-busy nm_work 5000 1000000)
(assert-true "a node that is first heard of is not at work, there is nothing to set it against"
	(= (get :busy nm_work) +real_0))
(node-busy nm_work 5000 2000000)
(assert-true "one that was not idle at all since, is half way to all of it"
	(= (get :busy nm_work) +real_1/2))
(each (lambda (i) (node-busy nm_work 5000 (+ 3000000 (* i 1000000)))) (range 0 8))
(assert-true "and all but there if it goes on" (> (get :busy nm_work) (const (n2r 0.99))))
(each (lambda (i) (node-busy nm_work (+ 5000 (* (inc i) 1000000)) (+ 11000000 (* i 1000000)))) (range 0 8))
(assert-true "idle all of the time, it falls away to none" (< (get :busy nm_work) (const (n2r 0.01))))
(node-busy nm_work 12000000 19000000)
(assert-true "it is never less than none, a clock is not that good" (>= (get :busy nm_work) +real_0))
(defq nm_old (nm-worker))
(node-busy nm_old 0 0) (node-busy nm_old 0 0)
(assert-true "a node that gives no time is not shown as at work" (= (get :busy nm_old) +real_0))
