;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCP Echo Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/utils.inc")
(import "lib/net/ip.inc")
(import "lib/net/tcp.inc")
(import "lib/net/tcp_state.inc")
(import "lib/net/socket.inc")

(defun tcp-echo-server/main (args)
	; Simple TCP echo server
	; Usage: (tcp-echo-server/main '("7777"))

	(defq port (if (>= (length args) 1)
	              (num (elem-get args 0))
	              7777)  ; Default echo port
	      connections (list))  ; List of active connections

	; Initialize TCP
	(tcp/init)

	; Create listening socket
	(defq listen_sock (socket/create sock_stream))

	; Bind to port
	(unless (socket/bind listen-sock port)
		(prin "Failed to bind to port " port)
		(prinl)
		(exit 1))

	; Start listening
	(socket/listen listen-sock
		(lambda (tcb)
			; New connection accepted
			(prin "New connection from "
			      (net/ip-to-string (elem-get tcb :remote-ip))
			      ":" (elem-get tcb :remote-port))
			(prinl)

			; Add to connections list
			(push connections tcb)))

	(prin "TCP echo server listening on port " port)
	(prinl)
	(prin "Press Ctrl+C to stop")
	(prinl)

	; Main loop
	(while t
		; Process each connection
		(each
			(lambda (tcb)
				; Check if we have data
				(when (tcp/is-connected tcb)
					(defq data (tcp/recv_data tcb))
					(when (> (length data) 0)
						(prin "Received " (length data) " bytes from "
						      (net/ip-to-string (elem-get tcb :remote-ip))
						      ":" (elem-get tcb :remote-port))
						(prinl)

						; Echo data back
						(tcp/send-data tcb data)

						(prin "Echoed back")
						(prinl))))
			connections)

		; Remove closed connections
		(setq connections
			(filter (# (not (= (elem-get %0 :state) tcp_state_closed)))
			        connections))

		; Sleep briefly
		(task-sleep 100000)))  ; Sleep 100ms

; Export main function
tcp-echo-server/main
