; RPC command types
(enums +net_rpc_type 0
	(enum connect listen accept link beacon discover hello peers refused refusals))

; Common RPC request header
(structure +net_rpc 0
	(netid reply_id)
	(uint type))

; Link request: starts service/net/link with target string
(structure +net_rpc_link +net_rpc_size
	(offset target))

; Connect request: connects to host:port
(structure +net_rpc_connect +net_rpc_size
	(netid client_in_mbox)
	(uint port)
	(offset host))

; Listen request: listens on port
(structure +net_rpc_listen +net_rpc_size
	(netid accept_mbox)
	(uint port))

; Accept request: claims an incoming connection offer
(structure +net_rpc_accept +net_rpc_size
	(netid client_in_mbox)
	(uint conn_handle))

; Beacon request: tells @Net to advertise TCP port via UDP broadcast
(structure +net_rpc_beacon +net_rpc_size
	(uint port))

; Discover request: tells @Net to make a mesh with the other @Net services,
; and with udp not 0 to listen for beacons on UDP 3334 as well
(structure +net_rpc_discover +net_rpc_size
	(uint udp))

; Hello: one @Net service to another, the peers it knows. No reply
(structure +net_rpc_hello +net_rpc_size
	(offset text))

; Peers: what the service knows of the other machines, its reply is the
; reply header and then text, a line a peer, (mesh-peers-text)
(structure +net_rpc_peers +net_rpc_size)

; Refused: a door closed on a machine of another link, where it is and the
; first 11 bytes of its hello, its word and version. No reply
(structure +net_rpc_refused +net_rpc_size
	(offset text))

; Refusals: those the service has been told of, its reply is the reply
; header and then text, a line each: where, the hello's bytes in hex, and
; how many seconds ago
(structure +net_rpc_refusals +net_rpc_size)

; Reply header to connect or accept
(structure +net_rpc_reply 0
	(netid server_in_mbox)
	(uint handle)
	(int status))

; Incoming connection offer sent to accept_mbox
(structure +net_msg_offer 0
	(uint type conn_handle))

(defq +net_type_offer 1)

;;;;;;;;;;;;;;;;;;;;;;
; Client RPC Functions
;;;;;;;;;;;;;;;;;;;;;;

(defun net-service ()
	; (net-service) -> netid | :nil
	(mail-first "@Net"))

(defun ensure-net-service ()
	; (ensure-net-service) -> netid | :nil
	;the Net service of this machine. It is waited for, a desktop or a
	;terminal starts one as it comes up. A node that is neither has none,
	;so one is started, or a link from there would go round the service,
	;and round the key of the machine with it
	(defq svc (net-service) retries 50)
	(while (and (not svc) (> (-- retries) 0))
		(task-sleep 10000)
		(setq svc (net-service)))
	(unless svc
		(open-child "service/net/app.lisp" +kn_call_run)
		(setq retries 100)
		(while (and (not (setq svc (net-service))) (> (-- retries) 0))
			(task-sleep 10000)))
	svc)

(defun net-open-rpc (host port)
	; (net-open-rpc host port) -> (client_in client_out) | :nil
	(defq svc (ensure-net-service) result :nil)
	(when svc
		(defq client_in (in-stream) reply_mbox (mail-mbox)
			req (setf-> (cat (str-alloc +net_rpc_connect_size) host)
				(+net_rpc_type +net_rpc_type_connect)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_connect_client_in_mbox (in-mbox client_in))
				(+net_rpc_connect_port port)))
		(mail-send svc req)
		(defq reply (mail-read-timeout reply_mbox))
		(if (and reply (= (getf reply +net_rpc_reply_status) 0))
			(progn
				(defq server_in_mbox (getf reply +net_rpc_reply_server_in_mbox)
					client_out (out-stream server_in_mbox))
				(setq result (list client_in client_out)))
			(in-set-state client_in +stream_mail_state_aborted)))
	result)

(defun net-listen-rpc (port accept_mbox)
	; (net-listen-rpc port accept_mbox) -> listener_handle | :nil
	(defq svc (ensure-net-service) handle :nil)
	(when svc
		(defq reply_mbox (mail-mbox)
			req (setf-> (str-alloc +net_rpc_listen_size)
				(+net_rpc_type +net_rpc_type_listen)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_listen_accept_mbox accept_mbox)
				(+net_rpc_listen_port port)))
		(mail-send svc req)
		(defq reply (mail-read-timeout reply_mbox))
		(and reply (= (getf reply +net_rpc_reply_status) 0)
			(setq handle (getf reply +net_rpc_reply_handle))))
	handle)

(defun net-accept-rpc (offer_msg)
	; (net-accept-rpc offer_msg) -> (client_in client_out) | :nil
	(defq svc (net-service) result :nil
		conn_handle (getf offer_msg +net_msg_offer_conn_handle))
	(when (and svc (> conn_handle 0))
		(defq client_in (in-stream) reply_mbox (mail-mbox)
			req (setf-> (str-alloc +net_rpc_accept_size)
				(+net_rpc_type +net_rpc_type_accept)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_accept_client_in_mbox (in-mbox client_in))
				(+net_rpc_accept_conn_handle conn_handle)))
		(mail-send svc req)
		(defq reply (mail-read-timeout reply_mbox))
		(if (and reply (= (getf reply +net_rpc_reply_status) 0))
			(progn
				(defq server_in_mbox (getf reply +net_rpc_reply_server_in_mbox)
					client_out (out-stream server_in_mbox))
				(setq result (list client_in client_out)))
			(in-set-state client_in +stream_mail_state_aborted)))
	result)

(defun net-close-rpc (conn)
	; (net-close-rpc (client_in client_out))
	(in-set-state (first conn) +stream_mail_state_aborted))

(defun net-link-rpc (target)
	; (net-link-rpc target) -> status | :nil
	(defq svc (ensure-net-service))
	(if svc
		(progn
			(defq reply_mbox (mail-mbox)
				req (setf-> (cat (str-alloc +net_rpc_link_size) target)
					(+net_rpc_type +net_rpc_type_link)
					(+net_rpc_reply_id reply_mbox)))
			(mail-send svc req)
			(defq reply (mail-read-timeout reply_mbox))
			(if reply (getf reply +net_rpc_reply_status) :nil))
		(mail-send (open-child "service/net/link" +kn_call_pin) target)))

(defun net-beacon-rpc (port)
	; (net-beacon-rpc port) -> status | :nil
	(defq svc (ensure-net-service))
	(when svc
		(defq reply_mbox (mail-mbox)
			req (setf-> (str-alloc +net_rpc_beacon_size)
				(+net_rpc_type +net_rpc_type_beacon)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_beacon_port port)))
		(mail-send svc req)
		(defq reply (mail-read-timeout reply_mbox))
		(if reply (getf reply +net_rpc_reply_status) :nil)))

(defun net-discover-rpc (&optional mesh_only)
	; (net-discover-rpc [mesh_only]) -> status | :nil
	;the service makes a mesh with the others it can reach, and listens
	;for their beacons. With mesh_only :t it does not listen, it has only
	;what the other services tell it
	(defq svc (ensure-net-service))
	(when svc
		(defq reply_mbox (mail-mbox)
			req (setf-> (str-alloc +net_rpc_discover_size)
				(+net_rpc_type +net_rpc_type_discover)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_discover_udp (if mesh_only 0 1))))
		(mail-send svc req)
		(defq reply (mail-read-timeout reply_mbox))
		(if reply (getf reply +net_rpc_reply_status) :nil)))



(defun net-what-is (system)
	; (net-what-is system) -> str
	;what a machine of the mesh says it is, arm64/Darwin, asked of one of
	;its nodes. ? if none of its nodes is seen
	(defq mbox (mail-mbox) nodes (lisp-nodes system))
	(cond
		((empty? nodes) "?")
		(:t (open-task (str `(mail-send (hex-decode ,(hex-encode mbox)) (cat (str (cpu)) "/" (str (os)))))
				(first nodes) +kn_call_pin 0 (mail-mbox))
			(ifn (mail-read-timeout mbox (task-timeout 2)) "no answer"))))

(defun net-refused (where head)
	; (net-refused where head)
	;a door tells the Net service of this machine it closed on another
	;machine for its link: where, host:port, or "dialled in", and the first
	;bytes of its hello
	(if (defq svc (net-service))
		(mail-send svc (cat (setf-> (str-alloc +net_rpc_refused_size)
				(+net_rpc_type +net_rpc_type_refused)) where " " (hex-encode head)))))

(defun net-refused-rpc ()
	; (net-refused-rpc) -> :nil | ((where head seconds) ...)
	;the machines a door closed on lately, for speaking another link: where,
	;the first bytes of the hello it sent, and how long ago. The last 8, of
	;the last 10 minutes
	(when (defq svc (net-service))
		(defq reply_mbox (mail-mbox))
		(mail-send svc (setf-> (str-alloc +net_rpc_refusals_size)
			(+net_rpc_type +net_rpc_type_refusals)
			(+net_rpc_reply_id reply_mbox)))
		(when (defq reply (mail-read-timeout reply_mbox))
			(reduce (lambda (out line)
				(when (= (length (defq parts (split line " "))) 3)
					(push out (list (first parts) (hex-decode (to-upper (second parts))) (str-as-num (third parts)))))
				out) (split (slice reply +net_rpc_reply_size -1) "\n") (list)))))

(defun net-peers-rpc ()
	; (net-peers-rpc) -> :nil | ((system_id ip port linked build link) ...)
	;what the Net service of this machine knows of the others, from their
	;beacons and from each other. linked is "linked" or "unlinked", build
	;is "same", "differs" or "unknown", the boot image they run as against
	;this one's, and link "same", "unknown", or the version of the link
	;they speak if it is another. :nil if the service does not answer
	(when (defq svc (net-service))
		(defq reply_mbox (mail-mbox))
		(mail-send svc (setf-> (str-alloc +net_rpc_peers_size)
			(+net_rpc_type +net_rpc_type_peers)
			(+net_rpc_reply_id reply_mbox)))
		(when (defq reply (mail-read-timeout reply_mbox))
			(reduce (lambda (out line)
				(if (= (length (defq parts (split line " "))) 6) (push out parts))
				out) (split (slice reply +net_rpc_reply_size -1) "\n") (list)))))
