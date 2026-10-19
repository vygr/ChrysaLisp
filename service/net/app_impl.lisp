(import "./app.inc")
(import "./mesh.inc")
(import "./door.inc")
(import "lib/boot/id.inc")

;low-level PII network bindings
(ffi "service/net/lisp_init" net-init)
; (net-init) -> num
(ffi "service/net/lisp_deinit" net-deinit)
; (net-deinit) -> num
(ffi "service/net/lisp_connect" net-connect)
; (net-connect host port) -> handle
(ffi "service/net/lisp_listen" net-listen)
; (net-listen port) -> handle
(ffi "service/net/lisp_accept" net-accept)
; (net-accept handle) -> handle
(ffi "service/net/lisp_send" net-send)
; (net-send handle str) -> bytes_sent
(ffi "service/net/lisp_recv" net-recv)
; (net-recv handle max_len) -> str | :nil
(ffi "service/net/lisp_close" net-close)
; (net-close handle) -> num
(ffi "service/net/lisp_poll" net-poll)
; (net-poll handle) -> flags
(ffi "service/net/lisp_udp_bind" net-udp-bind)
; (net-udp-bind port) -> handle
(ffi "service/net/lisp_udp_send" net-udp-send)
; (net-udp-send handle host port str) -> bytes_sent
(ffi "service/net/lisp_udp_recv" net-udp-recv)
; (net-udp-recv handle max_len) -> (data ip port) | :nil
;a boot image from before this call was written has not got it. The
;service still runs there, with no mesh, and says why if one is asked for.
;It is the state a machine is in between new source and make all boot
(defq *net_can_mesh* (eql :there (catch (progn (ffi "service/net/lisp_links" net-links) :there) :not)))
; (net-links) -> (link ...), a link is the fields of lib/net/links.inc
;and the same of this one, without it a change of key is not acted on
(defq *net_can_drop* (eql :there (catch (progn (ffi "service/net/lisp_drop" net-drop) :there) :not)))
; (net-drop) -> num, the links over the network are told they are closed

(bits +net_poll 0
	(bit in out error))

(enums +select 0
	(enum main timer))

(defq +sleep_min 1000 +sleep_max 20000 +stream_timeout 60000000)
(defq +disco_udp_port 3334 +beacon_interval 2000000
	;where this machine listens for the others, and how long till it tries
	;again for that port when another session of the machine has it
	+mesh_tcp_port 3333 +mesh_retry 10000000
	;a desktop that asked another session for the port asks again this soon
	+mesh_ask 1500000)

(defun session-close (sessions handle)
	(net-close handle)
	(when (defq session (. sessions :find handle))
		(when (eql (get :type session) :stream)
			(in-set-state (get :server_in session) +stream_mail_state_aborted)))
	(. sessions :erase handle))

(defun session-gui? ()
	; (session-gui? ) -> :t | :nil
	;is this session a desktop: a GUI on one of its own nodes. Another
	;session of this machine is not linked to this one, so the nodes of this
	;machine known here are this session's
	(defq mine (lisp-nodes :t))
	(some (# (find (slice (hex-decode (second (split %0 ","))) +long_size (+ +long_size +node_id_size)) mine))
		(mail-enquire "@Gui,")))

(defun net-start-link (key target)
	; (net-start-link key target) -> :nil | net_id
	;a link, ":port" to listen or "host:port" to dial. It is through the
	;door, service/net/door.inc, each end proves it has the key before
	;the link carries anything. A machine with no key has no links over
	;the network, :nil
	(cond
		((not key) (defq child :nil))
		((starts-with ":" target)
			(defq child (open-child "service/net/gate.lisp" +kn_call_pin))
			(if (/= (get-long child 0) 0)
				(mail-send child (cat key (if (eql target ":") "3333" (slice target 1 -1))))))
		(:t (defq child (open-child "service/net/door.lisp" +kn_call_pin))
			(if (/= (get-long child 0) 0) (mail-send child (cat "D" key target)))))
	child)

(defun main ()
	(net-init)
	(defq service (mail-declare (task-mbox) "@Net" "Net Service 0.1")
		select (list (task-mbox) (mail-mbox)) sessions (Fmap 31)
		sleep_time +sleep_min running :t
		beacon_ports (Fset 7)
		beacon_socket 0
		last_beacon_time 0
		disco_socket 0
		;a session with a system id of its own, a subnet, a container, is
		;not this machine's and has none of its key: (node-start) says so
		;to each node of one. The key of this machine, if it has one, is
		;worked out the once, and again when the file is another
		own (not (get '*env_node_sid*))
		;a session told to keep to itself does not join by itself, a run
		;of the tests say. It links when it is asked to, as any does
		solo (get '*env_node_solo*)
		key_age (age +door_key_file)
		net_key (if own (door-key))
		;is this the session that listens for the machine, its member of
		;the mesh, what listens for it, and when to try to be if not
		member :nil gates (list) next_join 0 dropping :nil last_watch_time 0
		;what starts this machine's store, if it has one, for as long as
		;this session is the machine's member: service/storage/auto.lisp
		store_auto :nil
		mesh :nil
		mesh_service :nil
		mesh_peers (Fmap 31)
		last_mesh_time 0
		;the id of the boot image this machine has, to say in the beacon
		my_build (boot-id)
		my_sys_id (hex-encode (system-id))
		my_inst_id (hex-encode (first (lisp-nodes))))
	;what was made as this session joined, the listener, the beacon, the
	;links, let go of: the key has changed, or a desktop of this machine
	;asked for the port
	(defun mesh-leave ()
		(each (# (mail-send %0 "")) gates)
		(when (> beacon_socket 0) (net-close beacon_socket))
		(when (> disco_socket 0) (net-close disco_socket))
		(if mesh_service (mail-forget mesh_service))
		(when store_auto (mail-send store_auto "") (setq store_auto :nil))
		(setq gates (list) beacon_socket 0 beacon_ports (Fset 7) disco_socket 0
			mesh :nil mesh_service :nil mesh_peers (Fmap 31)
			member :nil dropping *net_can_drop*))
	(mail-timeout (elem-get select +select_timer) sleep_time 0)
	(while running
		(let* ((idx (mail-select select)) (msg (mail-read (elem-get select idx))))
			(cond
				; 1. Service RPC Requests
				((= idx +select_main)
					(setq sleep_time +sleep_min)
					(defq reply_id (getf msg +net_rpc_reply_id) type (getf msg +net_rpc_type))
					(case type
						(+net_rpc_type_connect
							(defq port (getf msg +net_rpc_connect_port)
								client_in_mbox (getf msg +net_rpc_connect_client_in_mbox)
								host (slice msg +net_rpc_connect_host -1)
								handle (net-connect host port))
							(if (> handle 0)
								(progn
									(def (defq s (env 1))
										:handle handle :type :connecting
										:client_in_mbox client_in_mbox :reply_id reply_id
										:timestamp (pii-time))
									(. sessions :insert handle s))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_listen
							(defq port (getf msg +net_rpc_listen_port)
								accept_mbox (getf msg +net_rpc_listen_accept_mbox)
								handle (net-listen port))
							(if (> handle 0)
								(progn
									(def (defq s (env 1))
										:handle handle :type :listener
										:accept_mbox accept_mbox :port port)
									(. sessions :insert handle s)
									(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
										(+net_rpc_reply_handle handle)
										(+net_rpc_reply_status 0))))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_accept
							(defq conn_handle (getf msg +net_rpc_accept_conn_handle)
								client_in_mbox (getf msg +net_rpc_accept_client_in_mbox)
								session (. sessions :find conn_handle))
							(if (and session (eql (get :type session) :offer))
								(progn
									(def session :type :stream
										:server_in (in-stream)
										:server_out (out-stream client_in_mbox)
										:state :connected)
									(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
										(+net_rpc_reply_server_in_mbox (in-mbox (get :server_in session)))
										(+net_rpc_reply_handle conn_handle)
										(+net_rpc_reply_status 0))))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_link
							(defq target (slice msg +net_rpc_link_target -1)
								child (net-start-link net_key target))
							;what listens is told to go if the key is ever another
							(and child (starts-with ":" target) (/= (get-long child 0) 0) (push gates child))
							(if (and child (/= (get-long child 0) 0))
								(progn
									(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
										(+net_rpc_reply_handle 0)
										(+net_rpc_reply_status 0))))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_beacon
							(defq port (getf msg +net_rpc_beacon_port))
							(and net_key (= beacon_socket 0)
								(setq beacon_socket (net-udp-bind 0)))
							(if (and net_key (> beacon_socket 0))
								(progn
									(. beacon_ports :insert port)
									(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
										(+net_rpc_reply_handle beacon_socket)
										(+net_rpc_reply_status 0))))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_peers
							(mail-send reply_id (cat (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0) (+net_rpc_reply_status 0))
								(mesh-peers-text mesh_peers
									(if *net_can_mesh* (map (# (hex-encode (slice %0 +node_id_size
										(const (* 2 +node_id_size))))) (net-links)) (list))
									my_build))))
						(+net_rpc_type_hello
							(if mesh (mesh-heard mesh_peers my_sys_id (slice msg +net_rpc_hello_text -1) (pii-time))))
						(+net_rpc_type_discover
							;a machine with no key is not on the network at all
							(setq mesh (if net_key *net_can_mesh*))
							;a service that makes a mesh says so to every machine. @Net is
							;for this machine and is not seen from another, a * name is
							(and mesh (not mesh_service)
								(setq mesh_service (mail-declare (task-mbox) "*Mesh" "Mesh 0.1")))
							(and mesh (= disco_socket 0) (/= (getf msg +net_rpc_discover_udp) 0)
								(setq disco_socket (net-udp-bind +disco_udp_port)))
							(if (and mesh (or (> disco_socket 0) (= (getf msg +net_rpc_discover_udp) 0)))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle disco_socket)
									(+net_rpc_reply_status 0)))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))))
				; 2. I/O Polling Pump
				((= idx +select_timer)
					(defq active :nil now (pii-time))
					; The key, and this machine's place in the mesh
					(when (> (- now last_watch_time) +beacon_interval)
						(setq last_watch_time now)
						;the key is another, or has gone, or has come. What was
						;made with the old one ends: what listens, the beacon, the
						;links. Then, below, it joins those who have the new one,
						;and with none it stays off the network till it has
						(when (and own (/= (defq new_age (age +door_key_file)) key_age))
							(setq key_age new_age net_key (door-key) active :t)
							(mesh-leave)
							(setq next_join 0))
						;a link is told it is closed till none is left to tell
						(if dropping (setq dropping (/= (net-drop) 0) active :t))
						;first come first served: the session that has the port
						;listens for the machine, says so to the network, and looks
						;for the others. One that finds the port taken asks again
						;later, the session that had it may have gone
						(when (and net_key *net_can_mesh* (not solo) (not member) (not dropping) (>= now next_join))
							(defq said (mail-mbox) gate (open-child "service/net/gate.lisp" +kn_call_pin))
							(when (/= (get-long gate 0) 0)
								(mail-send gate (cat net_key (str +mesh_tcp_port) " " (hex-encode said)))
								(cond
									((eql (mail-read-timeout said 2000000) "1")
										(push gates gate)
										(setq member :t mesh :t active :t)
										;the one session that has the port is the one that
										;serves the machine's store
										(unless store_auto
											(setq store_auto (open-child "service/storage/auto.lisp" +kn_call_pin))
											(if (= (get-long store_auto 0) 0) (setq store_auto :nil)))
										(if (= beacon_socket 0) (setq beacon_socket (net-udp-bind 0)))
										(. beacon_ports :insert +mesh_tcp_port)
										(unless mesh_service
											(setq mesh_service (mail-declare (task-mbox) "*Mesh" "Mesh 0.1")))
										(if (= disco_socket 0) (setq disco_socket (net-udp-bind +disco_udp_port))))
									(:t (mail-send gate "")
										(cond
											;another session of this machine has the port. A
											;desktop asks it to step aside, one that is not a
											;desktop gives way, and is asked again soon
											((session-gui?)
												(when (> (defq ask (net-udp-bind 0)) 0)
													(net-udp-send ask "127.0.0.1" +disco_udp_port (cat "YIELD " my_inst_id))
													(net-close ask))
												(setq next_join (+ now +mesh_ask)))
											((setq next_join (+ now +mesh_retry)))))))))
					; Broadcast beacons if advertising
					(and (> beacon_socket 0) (nempty? beacon_ports)
						(when (> (- now last_beacon_time) +beacon_interval)
							(setq last_beacon_time now active :t)
							(. beacon_ports :each (lambda (p)
								;the D says this one hears beacons and dials, so of two
								;that both do, only one need
								(defq beacon_msg (mesh-beacon-text p my_sys_id my_inst_id (> disco_socket 0) my_build))
								(net-udp-send beacon_socket "255.255.255.255" +disco_udp_port beacon_msg)
								(net-udp-send beacon_socket "127.0.0.1" +disco_udp_port beacon_msg)))))
					; Poll incoming beacons if auto-discovery active
					(when (> disco_socket 0)
						(defq pkt :nil)
						(while (and (> disco_socket 0) (setq pkt (net-udp-recv disco_socket 256)))
							(setq active :t)
							(defq data (first pkt) src_ip (second pkt))
							;a desktop of this machine asks for the port: this session
							;is not a desktop, it steps aside, and asks for the port
							;again when it would have, the desktop has it till it goes
							(when (and (starts-with "YIELD " data) (eql src_ip "127.0.0.1")
									(nql (slice data 6 -1) my_inst_id) member (not (session-gui?)))
								(mesh-leave)
								(setq next_join (+ now +mesh_retry)))
							(when (defq beacon (mesh-beacon-read data))
								(bind '(tcp_port peer_sys_id peer_inst_id dials build) beacon)
								;what is heard is noted, the links are made below.
								;Not my own, nor that of another session of this
								;machine: it is this system, there is no link to
								;make to it, and its address is my own
								(unless (or (eql peer_inst_id my_inst_id) (eql peer_sys_id my_sys_id))
									(mesh-beacon mesh_peers peer_sys_id src_ip tcp_port dials
										(nempty? beacon_ports) now build)))))
					; The mesh. A link to each peer there is none to, and a hello to
					; each of the other services
					(when (and mesh (> (- now last_mesh_time) +beacon_interval))
						(setq last_mesh_time now)
						(defq linked (map (# (hex-encode (slice %0 +node_id_size (const (* 2 +node_id_size)))))
							(net-links)))
						(. mesh_peers :each (lambda (sys peer)
							(when (mesh-dial? peer my_sys_id sys (find sys linked) now)
								(setq active :t)
								(net-start-link net_key (cat (elem-get peer +mesh_peer_ip) ":"
									(str (elem-get peer +mesh_peer_port)))))))
						(defq hello (cat (setf-> (str-alloc +net_rpc_hello_size)
								(+net_rpc_type +net_rpc_type_hello)
								(+net_rpc_reply_id (task-mbox)))
							(mesh-hello (mesh-forget mesh_peers now) my_sys_id now)))
						(each (lambda (entry)
							(defq mbox (hex-decode (second (split entry ","))))
							(unless (eql mbox (task-mbox)) (mail-send mbox (cat hello))))
							(mail-enquire "*Mesh,")))
					(. sessions :each (lambda (handle session)
						(case (get :type session)
							(:connecting
								(defq p (net-poll handle))
								(cond
									((bits? p +net_poll_out)
										(def session :type :stream
											:server_in (in-stream)
											:server_out (out-stream (get :client_in_mbox session))
											:state :connected)
										(mail-send (get :reply_id session) (setf-> (str-alloc +net_rpc_reply_size)
											(+net_rpc_reply_server_in_mbox (in-mbox (get :server_in session)))
											(+net_rpc_reply_handle handle)
											(+net_rpc_reply_status 0)))
										(undef session :client_in_mbox :reply_id)
										(setq active :t))
									((or (bits? p +net_poll_error) (> (- now (get :timestamp session)) 10000000))
										(mail-send (get :reply_id session) (setf-> (str-alloc +net_rpc_reply_size)
											(+net_rpc_reply_handle 0)
											(+net_rpc_reply_status -1)))
										(session-close sessions handle)
										(setq active :t))))
							(:listener
								(defq p (net-poll handle))
								(when (bits? p +net_poll_in)
									(defq client_handle (net-accept handle))
									(when (> client_handle 0)
										(def (defq offer (env 1))
											:handle client_handle :type :offer :timestamp now)
										(. sessions :insert client_handle offer)
										(mail-send (get :accept_mbox session) (setf-> (str-alloc +net_msg_offer_size)
											(+net_msg_offer_type +net_type_offer)
											(+net_msg_offer_conn_handle client_handle)))
										(setq active :t))))
							(:offer
								(when (> (- now (get :timestamp session)) 5000000)
									(session-close sessions handle)))
							(:stream
								(defq p (net-poll handle))
								; Check TCP -> Client
								(cond
									((bits? p +net_poll_in)
										(defq chunk (net-recv handle 4096))
										(cond
											((eql chunk :nil)
												(session-close sessions handle)
												(setq active :t))
											((nql chunk "")
												(def session :timestamp now)
												(write-blk (get :server_out session) chunk)
												(stream-flush (get :server_out session))
												(setq active :t))))
									((and (bits? p +net_poll_error) (not (bits? p +net_poll_in)))
										(session-close sessions handle)
										(setq active :t)))
								; Check Client -> TCP
								(when (. sessions :find handle)
									(defq in_state (in-get-state (get :server_in session)))
									(cond
										((or (= in_state +stream_mail_state_aborted)
											 (= in_state +stream_mail_state_stopped)
											 (> (- now (get :timestamp session)) +stream_timeout))
											(session-close sessions handle)
											(setq active :t))
										((mail-poll (list (in-mbox (get :server_in session))))
											(in-next-msg (get :server_in session))
											(defq data (read-avail (get :server_in session)))
											(when (nql data "")
												(def session :timestamp now)
												(net-send handle data)
												(setq active :t)))))))))
					(if active
						(setq sleep_time +sleep_min)
						(setq sleep_time (min +sleep_max (+ sleep_time 1000))))
					(mail-timeout (elem-get select +select_timer) sleep_time 0)))))
	(when (> beacon_socket 0) (net-close beacon_socket))
	(when (> disco_socket 0) (net-close disco_socket))
	(if mesh_service (mail-forget mesh_service))
	(mail-forget service)
	(net-deinit))