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
; All of this is the logic alone, the service does the mail and the
; links, service/net/app_impl.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a peer, what is known of one other system
(enums +mesh_peer 0
	(enum ip port dialler knows_me said_hello t_dialled t_wait))

;not again within this of a dial, and how long to wait for a peer that
;should be the one to dial
(defq +mesh_redial 10000000 +mesh_patience 15000000)

(defun mesh-peer (peers sys)
	; (mesh-peer peers sys) -> peer
	;the peer of that system id, a new one with no address if not known
	(or (. peers :find sys)
		(progn (. peers :insert sys (defq peer (list "" 0 :nil :nil :nil 0 0))) peer)))

(defun mesh-beacon (peers sys ip port dials i_beacon)
	; (mesh-beacon peers sys ip port dials i_beacon) -> peer
	;a beacon was heard, first hand, so its address is the one to have.
	;A peer that hears beacons and dials will dial me if I beacon, until a
	;hello from it says for sure
	(defq peer (mesh-peer peers sys))
	(elem-set peer +mesh_peer_ip ip)
	(elem-set peer +mesh_peer_port port)
	(unless (elem-get peer +mesh_peer_said_hello)
		(elem-set peer +mesh_peer_dialler dials)
		(elem-set peer +mesh_peer_knows_me (if (and dials i_beacon) :t)))
	peer)

(defun mesh-hello (peers my_sys)
	; (mesh-hello peers my_sys) -> str
	;what is said to the other services. Who I am, then each peer I know
	;the address of
	(defq out (list "CHRYSA_HELLO " my_sys (ascii-char 10)))
	(. peers :each (lambda (sys peer)
		(if (> (elem-get peer +mesh_peer_port) 0)
			(push out sys " " (elem-get peer +mesh_peer_ip) " "
				(str (elem-get peer +mesh_peer_port)) (ascii-char 10)))))
	(apply (const cat) out))

(defun mesh-heard (peers my_sys text)
	; (mesh-heard peers my_sys text) -> :nil | sys
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
					((and (> port 0) (= (elem-get (defq peer (mesh-peer peers sys)) +mesh_peer_port) 0))
						(elem-set peer +mesh_peer_ip ip)
						(elem-set peer +mesh_peer_port port)))))
			(list lines) 1)
		(elem-set from +mesh_peer_said_hello :t)
		(elem-set from +mesh_peer_dialler :t)
		(elem-set from +mesh_peer_knows_me knows)
		(second head)))

(defun mesh-dial? (peer my_sys sys linked now)
	; (mesh-dial? peer my_sys sys linked now) -> :nil | :t
	;is a link to be made to this peer, now ? If so it is noted as dialled
	(cond
		;there is a link, or nowhere to dial
		((or linked (= (elem-get peer +mesh_peer_port) 0))
			(elem-set peer +mesh_peer_t_wait 0) :nil)
		;dialled not long ago, it may yet come up
		((< (- now (elem-get peer +mesh_peer_t_dialled)) +mesh_redial) :nil)
		;both can dial, and it is for the other to. Wait, but not for ever
		((and (elem-get peer +mesh_peer_dialler) (elem-get peer +mesh_peer_knows_me)
				(> (cmp my_sys sys) 0)
				(progn (if (= (elem-get peer +mesh_peer_t_wait) 0) (elem-set peer +mesh_peer_t_wait now))
					(< (- now (elem-get peer +mesh_peer_t_wait)) +mesh_patience))) :nil)
		(:t (elem-set peer +mesh_peer_t_dialled now)
			(elem-set peer +mesh_peer_t_wait 0) :t)))
