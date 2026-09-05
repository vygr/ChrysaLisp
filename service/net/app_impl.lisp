(import "./app.inc")
(import "./net.inc")

(enums +select 0
	(enum main timer))

(defq +sleep_min 1000 +sleep_max 20000)

(defun session-close (sessions handle session)
	(net-close handle)
	(. sessions :erase handle))

(defun main ()
	(net-init)
	(defq service (mail-declare (task-mbox) "*Net" "Socket Service 0.1")
		select (list (task-mbox) (mail-mbox)) sessions (Fmap 31)
		sleep_time +sleep_min running :t)
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
									(defq s (env 1))
									(def s :handle handle :type :connecting
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
									(defq s (env 1))
									(def s :handle handle :type :listener
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
									(+net_rpc_reply_status -1)))))))
				; 2. I/O Polling Pump
				((= idx +select_timer)
					(defq active :nil now (pii-time))
					(. sessions :each (lambda (handle session)
						(case (get :type session)
							(:connecting
								(defq p (net-poll handle))
								(cond
									((/= (logand p 2) 0)
										(def session :type :stream
											:server_in (in-stream)
											:server_out (out-stream (get :client_in_mbox session))
											:state :connected)
										(undef session :client_in_mbox)
										(mail-send (get :reply_id session) (setf-> (str-alloc +net_rpc_reply_size)
											(+net_rpc_reply_server_in_mbox (in-mbox (get :server_in session)))
											(+net_rpc_reply_handle handle)
											(+net_rpc_reply_status 0)))
										(undef session :reply_id)
										(setq active :t))
									((or (/= (logand p 4) 0) (> (- now (get :timestamp session)) 10000000))
										(mail-send (get :reply_id session) (setf-> (str-alloc +net_rpc_reply_size)
											(+net_rpc_reply_handle 0)
											(+net_rpc_reply_status -1)))
										(net-close handle)
										(. sessions :erase handle)
										(setq active :t))))
							(:listener
								(defq p (net-poll handle))
								(when (/= (logand p 1) 0)
									(defq client_handle (net-accept handle))
									(when (> client_handle 0)
										(defq offer (env 1))
										(def offer :handle client_handle :type :offer :timestamp now)
										(. sessions :insert client_handle offer)
										(mail-send (get :accept_mbox session) (setf-> (str-alloc +net_msg_offer_size)
											(+net_msg_offer_type +net_type_offer)
											(+net_msg_offer_handle client_handle)))
										(setq active :t))))
							(:offer
								(when (> (- now (get :timestamp session)) 5000000)
									(net-close handle)
									(. sessions :erase handle)))
							(:stream
								(defq p (net-poll handle))
								; Check TCP -> Client
								(cond
									((/= (logand p 1) 0)
										(defq chunk (net-recv handle 4096))
										(cond
											((eql chunk :nil)
												(session-close sessions handle session)
												(setq active :t))
											((nql chunk "")
												(write-blk (get :server_out session) chunk)
												(stream-flush (get :server_out session))
												(setq active :t))))
									((and (/= (logand p 4) 0) (= (logand p 1) 0))
										(session-close sessions handle session)
										(setq active :t)))
								; Check Client -> TCP
								(when (. sessions :find handle)
									(defq in_state (in-get-state (get :server_in session)))
									(cond
										((or (= in_state +stream_mail_state_aborted)
											 (= in_state +stream_mail_state_stopped))
											(session-close sessions handle session)
											(setq active :t))
										((mail-poll (list (in-mbox (get :server_in session))))
											(in-next-msg (get :server_in session))
											(defq data (read-avail (get :server_in session)))
											(when (nql data "")
												(log-msg "[service] client -> TCP send data=" (length data) " bytes")
												(net-send handle data)
												(setq active :t)))))))))
					(if active
						(setq sleep_time +sleep_min)
						(setq sleep_time (min +sleep_max (+ sleep_time 1000))))
					(mail-timeout (elem-get select +select_timer) sleep_time 0)))))
	(mail-forget service)
	(net-deinit))