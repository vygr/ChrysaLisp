;;;;;;;;;;;;;;;;;;;;;
; service/net/app.inc
;;;;;;;;;;;;;;;;;;;;;

; RPC command types
(enums +net_rpc_type 0
	(enum connect listen accept))

; Common RPC request header
(structure +net_rpc 0
	(netid reply_id)
	(uint type))

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

; Reply header to connect or accept
(structure +net_rpc_reply 0
	(netid server_in_mbox)
	(uint handle)
	(int status))

; Incoming connection offer sent to accept_mbox
(structure +net_msg_offer 0
	(uint type)
	(uint conn_handle))

(defq +net_type_offer 1)

;;;;;;;;;;;;;;;;;;;;;;
; Client RPC Functions
;;;;;;;;;;;;;;;;;;;;;;

(defun net-service ()
	; (net-service) -> netid | :nil
	(when (defq s (mail-enquire "*Socket,"))
		(hex-decode (second (split (first s) ",")))))

(defun net-open (host port)
	; (net-open host port) -> (list client_in client_out) | :nil
	(defq svc (net-service) result :nil)
	(when svc
		(defq client_in (in-stream)
			reply_mbox (mail-mbox)
			req (setf-> (cat (str-alloc +net_rpc_connect_size) host)
				(+net_rpc_type +net_rpc_type_connect)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_connect_client_in_mbox (in-mbox client_in))
				(+net_rpc_connect_port port)))
		(mail-send svc req)
		(defq reply (mail-read reply_mbox))
		(when (= (getf reply +net_rpc_reply_status) 0)
			(defq server_in_mbox (getf reply +net_rpc_reply_server_in_mbox)
				client_out (out-stream server_in_mbox))
			(setq result (list client_in client_out))))
	result)

(defun net-listen (port accept_mbox)
	; (net-listen port accept_mbox) -> listener_handle | :nil
	(defq svc (net-service) handle :nil)
	(when svc
		(defq reply_mbox (mail-mbox)
			req (setf-> (str-alloc +net_rpc_listen_size)
				(+net_rpc_type +net_rpc_type_listen)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_listen_accept_mbox accept_mbox)
				(+net_rpc_listen_port port)))
		(mail-send svc req)
		(defq reply (mail-read reply_mbox))
		(if (= (getf reply +net_rpc_reply_status) 0)
			(setq handle (getf reply +net_rpc_reply_handle))))
	handle)

(defun net-accept (offer_msg)
	; (net-accept offer_msg) -> (list client_in client_out) | :nil
	(defq svc (net-service)
		conn_handle (getf offer_msg +net_msg_offer_conn_handle)
		result :nil)
	(when (and svc (> conn_handle 0))
		(defq client_in (in-stream)
			reply_mbox (mail-mbox)
			req (setf-> (str-alloc +net_rpc_accept_size)
				(+net_rpc_type +net_rpc_type_accept)
				(+net_rpc_reply_id reply_mbox)
				(+net_rpc_accept_client_in_mbox (in-mbox client_in))
				(+net_rpc_accept_conn_handle conn_handle)))
		(mail-send svc req)
		(defq reply (mail-read reply_mbox))
		(when (= (getf reply +net_rpc_reply_status) 0)
			(defq server_in_mbox (getf reply +net_rpc_reply_server_in_mbox)
				client_out (out-stream server_in_mbox))
			(setq result (list client_in client_out))))
	result)