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
