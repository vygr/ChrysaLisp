(report-header "Links: what a node knows of each link it has, and a cylinder")

(import "lib/net/links.inc")
(import "lib/math/mesh.inc")

;the tests run on a network of more than one node, or of one
(defq lk_links (net-links) lk_nodes (lisp-nodes))
(assert-true "a link for each other node, or more, or none on a node alone"
	(or (= (length lk_nodes) 1) (nempty? lk_links)))
(assert-true "each is the size of the record" (every (# (= (length %0) +link_size)) lk_links))
(assert-true "the peer of each is a node that is known, or has yet to say"
	(every (# (or (find (getf %0 +link_peer_node) lk_nodes)
		(eql (getf %0 +link_peer_node) (const (str-alloc +node_id_size)))) ) lk_links))
(assert-true "what a link has carried is not less than nothing" (every (# (>= (getf %0 +link_sent) 0)) lk_links))

;mail to a node on the other end of a link is counted on a link. Which
;link is the kernel's to choose, so it is the sum that grows
(when (> (length lk_nodes) 1)
	(defq lk_sum (lambda () (reduce (# (+ %0 (getf %1 +link_sent))) (net-links) 0))
		lk_before (lk_sum) lk_mbox (mail-mbox)
		lk_there (some (# (if (nql %0 (task-nodeid)) %0)) lk_nodes))
	(open-task (str `(mail-send (hex-decode ,(hex-encode lk_mbox)) "back")) lk_there +kn_call_pin 0 (mail-mbox))
	(assert-eq "a task on another node answers" "back" (mail-read-timeout lk_mbox (task-timeout 5)))
	(assert-true "and the links have carried more" (> (lk_sum) lk_before)))

(defq lk_cyl (Mesh-cylinder +real_1 +real_2 8))
(assert-eq "a cylinder of 8 sides, a ring at each end and two middles" 18 (/ (length (. lk_cyl :get_verts)) 4))
(assert-eq "two faces a side and a slice of each end" 32 (/ (length (. lk_cyl :get_tris)) 4))
(assert-eq "a normal for each face" 32 (/ (length (. lk_cyl :get_norms)) 3))
(assert-true "it stands on y, from -1 to 1"
	(every (# (or (= %0 +real_1) (= %0 +real_-1)))
		(map (# (second %0)) (partition (. lk_cyl :get_verts) 4))))
