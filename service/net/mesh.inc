;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Net services of a network make it a mesh between themselves.
;
; A TCP link is not a wire between two machines, any machine can
; reach any other. So each service keeps what it knows of the others,
; who they are and where they listen, tells the rest, and links to
; any it has no link to. No machine is then on the way between two
; others, and no application has anything to do with it.
;
; What a service knows of a peer comes from the peer's beacon, heard
; first hand, or from the hello of another service, which is its list
; of peers. Which of a pair makes the link is settled by what each
; knows of the other. If only one knows where the other is, it dials.
; If both do, the lower system id dials, and the other waits, though
; not for ever.
;
; A peer is alive while it is heard from, its beacon or its hello, or
; while another service says it has heard from it. Only what a service
; has heard itself goes in its hello, so word of a peer that has gone
; stops going round, and it is forgotten. A dial that brings no link
; is tried again, each time after twice as long.
;
; All of this is the logic alone, the service does the mail and the
; links, service/net/app_impl.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a peer, what is known of one other system
;build is the id of the boot image it runs, lib/boot/id.inc, as its
;beacon said, "" if it has not said
(enums +mesh_peer 0
	(enum ip port dialler knows_me said_hello t_dialled t_wait t_heard t_seen tries build))

;not again within this of a dial, twice as long after each that brought no
;link, up to 8 times. How long to wait for a peer that should be the one to
;dial. And how long a peer is alive for with nothing heard of it
(defq +mesh_redial 10000000 +mesh_patience 15000000 +mesh_alive 30000000)

(defun mesh-peer (peers sys)
	; (mesh-peer peers sys) -> peer
	;the peer of that system id, a new one with no address if not known
	(or (. peers :find sys)
		(progn (. peers :insert sys (defq peer (list "" 0 :nil :nil :nil 0 0 0 0 0 ""))) peer)))

(defun mesh-beacon (peers sys ip port dials i_beacon now &optional build)
	; (mesh-beacon peers sys ip port dials i_beacon now [build]) -> peer
	;a beacon was heard, first hand, so its address is the one to have.
	;A peer that hears beacons and dials will dial me if I beacon, until a
	;hello from it says for sure. build is the id of its boot image, if
	;its beacon had one
	(defq peer (mesh-peer peers sys))
	(if build (elem-set peer +mesh_peer_build build))
	(elem-set peer +mesh_peer_ip ip)
	(elem-set peer +mesh_peer_port port)
	(elem-set peer +mesh_peer_t_heard now)
	(elem-set peer +mesh_peer_t_seen now)
	(unless (elem-get peer +mesh_peer_said_hello)
		(elem-set peer +mesh_peer_dialler dials)
		(elem-set peer +mesh_peer_knows_me (if (and dials i_beacon) :t)))
	peer)

(defun mesh-hello (peers my_sys now)
	; (mesh-hello peers my_sys now) -> str
	;what is said to the other services. Who I am, then each peer I know
	;the address of and have heard from myself, of late. What another told
	;me is not passed on, or word of a peer that has gone would go round
	;for ever
	(defq out (list "CHRYSA_HELLO " my_sys (ascii-char 10)))
	(. peers :each (lambda (sys peer)
		(if (and (> (elem-get peer +mesh_peer_port) 0)
				(< (- now (elem-get peer +mesh_peer_t_heard)) +mesh_alive))
			(push out sys " " (elem-get peer +mesh_peer_ip) " "
				(str (elem-get peer +mesh_peer_port)) (ascii-char 10)))))
	(apply (const cat) out))

(defun mesh-heard (peers my_sys text now)
	; (mesh-heard peers my_sys text now) -> :nil | sys
	;a hello from another service. It dials, and it knows where I am if I
	;am in its list. A peer of its that I have no address for, I now have.
	;What is not a hello, or is my own, is :nil
	(defq lines (split text (ascii-char 10)) head (split (ifn (first lines) "") " "))
	(when (and (= (length head) 2) (eql (first head) "CHRYSA_HELLO") (nql (second head) my_sys))
		(defq from (mesh-peer peers (second head)) knows :nil)
		(each! (lambda (line)
			(when (= (length (defq parts (split line " "))) 3)
				(bind '(sys ip port) parts)
				(setq port (str-as-num port))
				(cond
					((eql sys my_sys) (setq knows :t))
					((> port 0)
						;it is alive, the one who says so heard it
						(elem-set (defq peer (mesh-peer peers sys)) +mesh_peer_t_seen now)
						(when (= (elem-get peer +mesh_peer_port) 0)
							(elem-set peer +mesh_peer_ip ip)
							(elem-set peer +mesh_peer_port port))))))
			(list lines) 1)
		(elem-set from +mesh_peer_t_heard now)
		(elem-set from +mesh_peer_t_seen now)
		(elem-set from +mesh_peer_said_hello :t)
		(elem-set from +mesh_peer_dialler :t)
		(elem-set from +mesh_peer_knows_me knows)
		(second head)))

(defun mesh-dial? (peer my_sys sys linked now)
	; (mesh-dial? peer my_sys sys linked now) -> :nil | :t
	;is a link to be made to this peer, now ? If so it is noted as dialled
	(cond
		;there is a link
		(linked (elem-set peer +mesh_peer_t_wait 0)
			(elem-set peer +mesh_peer_tries 0)
			(elem-set peer +mesh_peer_t_seen now) :nil)
		;nowhere to dial
		((= (elem-get peer +mesh_peer_port) 0) (elem-set peer +mesh_peer_t_wait 0) :nil)
		;dialled not long ago, it may yet come up. Longer each time it has not
		((< (- now (elem-get peer +mesh_peer_t_dialled))
			(<< +mesh_redial (max 0 (dec (elem-get peer +mesh_peer_tries))))) :nil)
		;both can dial, and it is for the other to. Wait, but not for ever
		((and (elem-get peer +mesh_peer_dialler) (elem-get peer +mesh_peer_knows_me)
				(> (cmp my_sys sys) 0)
				(progn (if (= (elem-get peer +mesh_peer_t_wait) 0) (elem-set peer +mesh_peer_t_wait now))
					(< (- now (elem-get peer +mesh_peer_t_wait)) +mesh_patience))) :nil)
		(:t (elem-set peer +mesh_peer_t_dialled now)
			(elem-set peer +mesh_peer_tries (min 4 (inc (elem-get peer +mesh_peer_tries))))
			(elem-set peer +mesh_peer_t_wait 0) :t)))

(defun mesh-forget (peers now)
	; (mesh-forget peers now) -> peers
	;a peer not heard of for a while has gone. Its address is let go of, so
	;it is not dialled, and it is as one never met. If it comes back its
	;beacon, or word of it, says where it is
	(. peers :each (lambda (sys peer)
		(when (and (> (elem-get peer +mesh_peer_port) 0)
				(> (- now (elem-get peer +mesh_peer_t_seen)) +mesh_alive))
			(each (# (elem-set peer %0 %1))
				(static-q (0 1 2 3 4 5 6 9)) (static-q ("" 0 :nil :nil :nil 0 0 0))))))
	peers)

(defun mesh-beacon-text (port sys inst dials build)
	; (mesh-beacon-text port sys inst dials build) -> str
	;what a beacon says. The port to dial, who it is, which node of it, a D
	;if it hears beacons and dials, and a B and the start of the id of its
	;boot image, if it has one. A beacon from before there were ids has no
	;B, and one that does not know of them takes no notice of it
	(cat "CHRYSA_BEACON:" (str port) ":" sys ":" inst (if dials ":D" "")
		(if (nempty? build) (cat ":B" (slice build 0 (min 16 (length build)))) "")))

(defun mesh-beacon-read (data)
	; (mesh-beacon-read data) -> :nil | (port sys inst dials build)
	;a beacon taken apart, :nil if it is not one
	(when (and (starts-with "CHRYSA_BEACON:" data) (>= (length (defq parts (split data ":"))) 4))
		(defq dials :nil build "")
		(each! (# (cond
				((eql %0 "D") (setq dials :t))
				((starts-with "B" %0) (setq build (rest %0)))))
			(list parts) 4)
		(list (str-as-num (second parts)) (third parts) (elem-get parts 3) dials build)))

(defun mesh-peers-text (peers linked my_build)
	; (mesh-peers-text peers linked my_build) -> str
	;what is known of the peers, a line each, for mesh the command to say:
	;its system id, its address, its port, if there is a link to it, and
	;its build as against this one's, same, differs, or unknown
	(defq out (list))
	(. peers :each (lambda (sys peer)
		(defq build (elem-get peer +mesh_peer_build))
		(push out (cat sys " " (if (eql (elem-get peer +mesh_peer_ip) "") "-" (elem-get peer +mesh_peer_ip))
			" " (str (elem-get peer +mesh_peer_port)) " " (if (find sys linked) "linked" "unlinked") " "
			(cond
				((or (eql build "") (eql my_build "")) "unknown")
				((starts-with build my_build) "same")
				(:t "differs"))
			(ascii-char 10)))))
	(apply (const cat) (cat (list "") out)))
