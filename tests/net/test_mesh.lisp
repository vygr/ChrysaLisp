(report-header "Net: the services make a mesh, who knows whom and who dials")
(import "service/net/mesh.inc")

(defq ms_a "AAAA" ms_b "BBBB" ms_c "CCCC" ms_now 100000000)

;a beacon heard is an address had
(defq ms_peers (Fmap 7) ms_peer (mesh-beacon ms_peers ms_b "192.168.1.2" 3333 :nil :nil ms_now))
(assert-eq "a beacon gives the address" "192.168.1.2" (elem-get ms_peer +mesh_peer_ip))
(assert-eq "and the port" 3333 (elem-get ms_peer +mesh_peer_port))
(assert-true "a peer that only beacons is dialled" (mesh-dial? ms_peer ms_a ms_b :nil ms_now))
(assert-true "and not again at once" (not (mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now 1000000))))
(assert-true "but again if no link has come of it" (mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now +mesh_redial 1)))
(assert-true "and then not till twice as long has gone"
	(not (mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now (* 2 +mesh_redial) 2))))
(assert-true "when it is" (mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now (* 3 +mesh_redial) 2)))
(assert-true "never when there is a link" (not (mesh-dial? ms_peer ms_a ms_b :t (+ ms_now (* 4 +mesh_redial)))))
(assert-true "and a link lost is dialled as if for the first time"
	(and (not (mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now (* 4 +mesh_redial))))
		(mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now (* 5 +mesh_redial)))
		(mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now (* 6 +mesh_redial) 1))))

;two that both beacon and both dial, the lower id makes the link
(defq ms_peers (Fmap 7) ms_peer (mesh-beacon ms_peers ms_b "192.168.1.2" 3333 :t :t ms_now))
(assert-true "of two that both dial, the lower does" (mesh-dial? ms_peer ms_a ms_b :nil ms_now))
(defq ms_peers (Fmap 7) ms_peer (mesh-beacon ms_peers ms_a "192.168.1.1" 3333 :t :t ms_now))
(assert-true "and the higher waits" (not (mesh-dial? ms_peer ms_b ms_a :nil ms_now)))
(assert-true "and still waits" (not (mesh-dial? ms_peer ms_b ms_a :nil (+ ms_now (/ +mesh_patience 2)))))
(assert-true "but not for ever" (mesh-dial? ms_peer ms_b ms_a :nil (+ ms_now +mesh_patience 1)))
;a peer that dials can not, if I do not beacon, it has no way to know where I am
(defq ms_peers (Fmap 7) ms_peer (mesh-beacon ms_peers ms_a "192.168.1.1" 3333 :t :nil ms_now))
(assert-true "the higher dials a peer that can not know where it is" (mesh-dial? ms_peer ms_b ms_a :nil ms_now))

;a hello, what one service says to another
(defq ms_peers (Fmap 7))
(mesh-beacon ms_peers ms_b "192.168.1.2" 3333 :t :t ms_now)
(mesh-peer ms_peers ms_c)
(defq ms_text (mesh-hello ms_peers ms_a ms_now))
(assert-true "a hello says who it is from" (starts-with (cat "CHRYSA_HELLO " ms_a) ms_text))
(assert-true "and the peers it has an address for" (found? ms_text (cat ms_b " 192.168.1.2 3333")))
(assert-true "and not one it has none for" (not (found? ms_text ms_c)))

;C hears it. It learns where B is, and that A dials but does not know where C is
(defq ms_cpeers (Fmap 7))
(assert-eq "a hello heard gives who it was from" ms_a (mesh-heard ms_cpeers ms_c ms_text ms_now))
(assert-eq "a peer of a peer is learned" "192.168.1.2" (elem-get (mesh-peer ms_cpeers ms_b) +mesh_peer_ip))
(assert-true "the one who said it has no address by it" (= 0 (elem-get (mesh-peer ms_cpeers ms_a) +mesh_peer_port)))
(assert-true "and is not dialled, there is nowhere to" (not (mesh-dial? (mesh-peer ms_cpeers ms_a) ms_c ms_a :nil ms_now)))
(assert-true "the peer learned of is dialled, though its id is lower, it does not know of me"
	(mesh-dial? (mesh-peer ms_cpeers ms_b) ms_c ms_b :nil ms_now))
(assert-eq "my own hello is not heard" :nil (mesh-heard ms_cpeers ms_a ms_text ms_now))
(assert-eq "nor what is not a hello" :nil (mesh-heard ms_cpeers ms_c "CHRYSA_BEACON:3333" ms_now))
(assert-eq "nor nothing" :nil (mesh-heard ms_cpeers ms_c "" ms_now))

;B hears A's hello too. A has B in it, so A knows where B is, and A is lower, so B waits
(defq ms_bpeers (Fmap 7))
(mesh-beacon ms_bpeers ms_a "192.168.1.1" 3333 :nil :nil ms_now)
(mesh-heard ms_bpeers ms_b ms_text ms_now)
(assert-true "a peer that has me in its hello knows where I am" (elem-get (mesh-peer ms_bpeers ms_a) +mesh_peer_knows_me))
(assert-true "so the higher of the two waits for it" (not (mesh-dial? (mesh-peer ms_bpeers ms_a) ms_b ms_a :nil ms_now)))
;what was heard first hand is not changed by what another says
(defq ms_text2 (cat "CHRYSA_HELLO " ms_c (ascii-char 10) ms_a " 10.0.0.9 4444" (ascii-char 10)))
(mesh-heard ms_bpeers ms_b ms_text2 ms_now)
(assert-eq "an address had is not changed by a hello" "192.168.1.1" (elem-get (mesh-peer ms_bpeers ms_a) +mesh_peer_ip))
;a beacon after a hello does not undo what the hello said
(mesh-beacon ms_bpeers ms_a "192.168.1.1" 3333 :nil :nil ms_now)
(assert-true "a beacon does not undo what a hello said" (elem-get (mesh-peer ms_bpeers ms_a) +mesh_peer_knows_me))

;a peer that has gone. Its beacon stops, it is still alive for a while, then it is forgotten
(defq ms_peers (Fmap 7) ms_peer (mesh-beacon ms_peers ms_b "192.168.1.2" 3333 :nil :nil ms_now))
(mesh-forget ms_peers (+ ms_now (/ +mesh_alive 2)))
(assert-eq "a peer not heard for a while is still known" 3333 (elem-get ms_peer +mesh_peer_port))
(assert-true "and still in a hello" (found? (mesh-hello ms_peers ms_a (+ ms_now (/ +mesh_alive 2))) ms_b))
(mesh-forget ms_peers (+ ms_now +mesh_alive 1))
(assert-eq "one not heard for long enough is forgotten" 0 (elem-get ms_peer +mesh_peer_port))
(assert-true "and is not dialled" (not (mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now +mesh_alive 2))))
(assert-true "nor in a hello" (not (found? (mesh-hello ms_peers ms_a (+ ms_now +mesh_alive 2)) ms_b)))
(mesh-beacon ms_peers ms_b "192.168.1.7" 3333 :nil :nil (+ ms_now +mesh_alive 3))
(assert-true "its beacon brings it back, and it is dialled at once"
	(mesh-dial? ms_peer ms_a ms_b :nil (+ ms_now +mesh_alive 4)))
;what another says of a peer keeps it alive, but is not passed on
(defq ms_peers (Fmap 7) ms_t2 (+ ms_now 20000000)
	ms_said (cat "CHRYSA_HELLO " ms_c (ascii-char 10) ms_b " 192.168.1.2 3333" (ascii-char 10)))
(mesh-heard ms_peers ms_a ms_said ms_now)
(assert-true "a peer only told of is not in my hello" (not (found? (mesh-hello ms_peers ms_a ms_now) ms_b)))
(mesh-heard ms_peers ms_a ms_said ms_t2)
(mesh-forget ms_peers (+ ms_now +mesh_alive 1))
(assert-eq "word of a peer keeps it" 3333 (elem-get (mesh-peer ms_peers ms_b) +mesh_peer_port))
(mesh-forget ms_peers (+ ms_t2 +mesh_alive 1))
(assert-eq "till the word stops" 0 (elem-get (mesh-peer ms_peers ms_b) +mesh_peer_port))
;a link keeps a peer, though nothing is heard
(defq ms_peers (Fmap 7) ms_peer (mesh-beacon ms_peers ms_b "192.168.1.2" 3333 :nil :nil ms_now))
(mesh-dial? ms_peer ms_a ms_b :t (+ ms_now +mesh_alive))
(mesh-forget ms_peers (+ ms_now +mesh_alive 1))
(assert-eq "a peer there is a link to is not forgotten" 3333 (elem-get ms_peer +mesh_peer_port))

;three machines that all beacon and all dial, every pair has one dialler
(defq ms_ids (list ms_a ms_b ms_c) ms_dials 0)
(each (lambda (me)
	(defq peers (Fmap 7))
	(each (lambda (them)
		(unless (eql me them)
			(if (mesh-dial? (mesh-beacon peers them "10.0.0.1" 3333 :t :t ms_now) me them :nil ms_now)
				(setq ms_dials (inc ms_dials))))) ms_ids)) ms_ids)
(assert-eq "three that all dial make three links, one a pair" 3 ms_dials)

(undef (env) 'ms_peers 'ms_peer 'ms_cpeers 'ms_bpeers 'ms_text 'ms_text2 'ms_said 'ms_t2 'ms_ids 'ms_dials)
