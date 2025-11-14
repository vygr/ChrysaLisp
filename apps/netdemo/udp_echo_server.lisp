;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UDP Echo Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/utils.inc")
(import "lib/net/ip.inc")
(import "lib/net/udp.inc")
(import "lib/net/socket.inc")

(defun udp-echo-server/main (args)
	; Simple UDP echo server
	; Usage: (udp-echo-server/main '("7777"))

	(defq port (if (>= (length args) 1)
	              (num (elem-get args 0))
	              7777))  ; Default echo port

	; Initialize UDP
	(udp/init)

	; Create UDP socket
	(defq sock (socket/create sock_dgram))

	; Set up data handler
	(socket/set-handler sock
		(lambda (src-ip src-port data)
			(prin "Received " (length data) " bytes from "
			      (net/ip-to-string src-ip) ":" src-port)
			(prinl)

			; Echo data back
			(socket/sendto sock src-ip src-port data)

			(prin "Echoed back to " (net/ip-to-string src-ip) ":" src-port)
			(prinl)))

	; Bind to port
	(if (socket/bind sock port)
		(progn
			(prin "UDP echo server listening on port " port)
			(prinl)
			(prin "Press Ctrl+C to stop")
			(prinl)

			; Main loop (in real implementation, this would be event-driven)
			(while t
				(task-sleep 100000)  ; Sleep 100ms
				))
		(progn
			(prin "Failed to bind to port " port)
			(prinl)
			(exit 1))))

; Export main function
udp-echo-server/main
