(import "./app.inc")
(import "./mesh.inc")

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
; (net-links) -> (system_id ...)

(bits +net_poll 0
	(bit in out error))

(enums +select 0
	(enum main timer))

(defq +sleep_min 1000 +sleep_max 20000 +stream_timeout 60000000)
(defq +disco_udp_port 3334 +beacon_interval 2000000)

(defun session-close (sessions handle)
	(net-close handle)
	(when (defq session (. sessions :find handle))
		(when (eql (get :type session) :stream)
			(in-set-state (get :server_in session) +stream_mail_state_aborted)))
	(. sessions :erase handle))

(defun main ()
	(net-init)
	(defq service (mail-declare (task-mbox) "@Net" "Net Service 0.1")
		select (list (task-mbox) (mail-mbox)) sessions (Fmap 31)
		sleep_time +sleep_min running :t
		beacon_ports (Fset 7)
		beacon_socket 0
		last_beacon_time 0
		disco_socket 0
		mesh :nil
		mesh_peers (Fmap 31)
		last_mesh_time 0
		my_sys_id (hex-encode (system-id))
		my_inst_id (hex-encode (first (lisp-nodes))))
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
								child (open-child "service/net/link" +kn_call_pin))
							(if (/= (get-long child 0) 0)
								(progn
									(mail-send child target)
									(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
										(+net_rpc_reply_handle 0)
										(+net_rpc_reply_status 0))))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_beacon
							(defq port (getf msg +net_rpc_beacon_port))
							(when (= beacon_socket 0)
								(setq beacon_socket (net-udp-bind 0)))
							(if (> beacon_socket 0)
								(progn
									(. beacon_ports :insert port)
									(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
										(+net_rpc_reply_handle beacon_socket)
										(+net_rpc_reply_status 0))))
								(mail-send reply_id (setf-> (str-alloc +net_rpc_reply_size)
									(+net_rpc_reply_handle 0)
									(+net_rpc_reply_status -1)))))
						(+net_rpc_type_hello
							(if mesh (mesh-heard mesh_peers my_sys_id (slice msg +net_rpc_hello_text -1) (pii-time))))
						(+net_rpc_type_discover
							(setq mesh *net_can_mesh*)
							(when (and mesh (= disco_socket 0) (/= (getf msg +net_rpc_discover_udp) 0))
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
					; Broadcast beacons if advertising
					(when (and (> beacon_socket 0) (nempty? beacon_ports))
						(when (> (- now last_beacon_time) +beacon_interval)
							(setq last_beacon_time now active :t)
							(. beacon_ports :each (lambda (p)
								;the D says this one hears beacons and dials, so of two
								;that both do, only one need
								(defq beacon_msg (cat "CHRYSA_BEACON:" (str p) ":" my_sys_id ":" my_inst_id
									(if (> disco_socket 0) ":D" "")))
								(net-udp-send beacon_socket "255.255.255.255" +disco_udp_port beacon_msg)
								(net-udp-send beacon_socket "127.0.0.1" +disco_udp_port beacon_msg)))))
					; Poll incoming beacons if auto-discovery active
					(when (> disco_socket 0)
						(defq pkt :nil)
						(while (setq pkt (net-udp-recv disco_socket 256))
							(setq active :t)
							(defq data (first pkt) src_ip (second pkt))
							(when (starts-with "CHRYSA_BEACON:" data)
								(defq parts (split data ":"))
								(when (>= (length parts) 4)
									(defq tcp_port (str-as-num (elem-get parts 1))
										peer_sys_id (elem-get parts 2)
										peer_inst_id (elem-get parts 3))
									;what is heard is noted, the links are made below
									(unless (eql peer_inst_id my_inst_id)
										(mesh-beacon mesh_peers peer_sys_id src_ip tcp_port
											(and (> (length parts) 4) (eql (elem-get parts 4) "D"))
											(nempty? beacon_ports) now))))))
					; The mesh. A link to each peer there is none to, and a hello to
					; each of the other services
					(when (and mesh (> (- now last_mesh_time) +beacon_interval))
						(setq last_mesh_time now)
						(defq linked (map (const hex-encode) (net-links)))
						(. mesh_peers :each (lambda (sys peer)
							(when (mesh-dial? peer my_sys_id sys (find sys linked) now)
								(setq active :t)
								(defq child (open-child "service/net/link" +kn_call_pin))
								(when (/= (get-long child 0) 0)
									(mail-send child (cat (elem-get peer +mesh_peer_ip) ":"
										(str (elem-get peer +mesh_peer_port))))))))
						(defq hello (cat (setf-> (str-alloc +net_rpc_hello_size)
								(+net_rpc_type +net_rpc_type_hello)
								(+net_rpc_reply_id (task-mbox)))
							(mesh-hello (mesh-forget mesh_peers now) my_sys_id now)))
						(each (lambda (entry)
							(defq mbox (hex-decode (second (split entry ","))))
							(unless (eql mbox (task-mbox)) (mail-send mbox (cat hello))))
							(mail-enquire "@Net,")))
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
	(mail-forget service)
	(net-deinit))