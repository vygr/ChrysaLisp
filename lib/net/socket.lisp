;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Socket API - Unified interface for network programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/tcp.lisp")
(import "lib/net/tcp_state.lisp")
(import "lib/net/udp.lisp")

;;;;;;;;;;;;;;;;;;
; Socket Creation
;;;;;;;;;;;;;;;;;;

(defun socket/create (type)
	; Create a new socket
	; Input: type - socket type (sock_stream for TCP, sock_dgram for UDP)
	; Output: socket environment
	(cond
		((= type sock_stream)
			; TCP socket
			(env
				:type sock_stream
				:tcb nil
				:state sock_state_closed
				:data-handler nil))

		((= type sock_dgram)
			; UDP socket
			(env
				:type sock_dgram
				:port nil
				:state sock_state_closed
				:data-handler nil))

		(t nil)))

;;;;;;;;;;;;;;;;;;
; Socket Bind
;;;;;;;;;;;;;;;;;;

(defun socket/bind (sock port)
	; Bind socket to local port
	; Inputs: sock - socket, port - local port number
	; Output: t if success, nil if failed
	(cond
		((= (elem-get sock :type) sock_stream)
			; TCP bind (prepare for listen)
			(elem-set sock :state sock_state_bound)
			(elem-set sock :port port)
			t)

		((= (elem-get sock :type) sock_dgram)
			; UDP bind
			(defq handler (elem-get sock :data_handler))
			(if (udp/bind port handler)
				(progn
					(elem-set sock :port port)
					(elem-set sock :state sock_state_bound)
					t)
				nil))

		(t nil)))

;;;;;;;;;;;;;;;;;;
; Socket Listen (TCP only)
;;;;;;;;;;;;;;;;;;

(defun socket/listen (sock accept_fn)
	; Listen for incoming connections (TCP only)
	; Inputs: sock - socket, accept-fn - callback for new connections
	; Output: t if success, nil if failed
	(if (= (elem-get sock :type) sock_stream)
		(progn
			(defq port (elem_get sock :port))
			(if (tcp/listen port accept-fn)
				(progn
					(elem-set sock :state sock_state_listening)
					t)
				nil))
		nil))

;;;;;;;;;;;;;;;;;;
; Socket Connect (TCP only)
;;;;;;;;;;;;;;;;;;

(defun socket/connect (sock dst-ip dst_port)
	; Connect to remote host (TCP only)
	; Inputs: sock - socket, dst-ip - destination IP, dst-port - destination port
	; Output: t if connection initiated, nil if failed
	(if (= (elem-get sock :type) sock_stream)
		(progn
			(defq tcb (tcp/connect dst-ip dst_port))
			(if tcb
				(progn
					(elem-set sock :tcb tcb)
					(elem-set sock :state sock_state_connecting)
					t)
				nil))
		nil))

;;;;;;;;;;;;;;;;;;
; Socket Send
;;;;;;;;;;;;;;;;;;

(defun socket/send (sock data)
	; Send data via socket
	; Inputs: sock - socket, data - data to send (byte array)
	; Output: t if sent, nil if failed
	(cond
		((= (elem-get sock :type) sock_stream)
			; TCP send
			(defq tcb (elem_get sock :tcb))
			(if tcb
				(tcp/send-data tcb data)
				nil))

		(t nil)))

(defun socket/sendto (sock dst-ip dst_port data)
	; Send data to specific address (UDP)
	; Inputs: sock, dst-ip, dst-port, data
	; Output: t if sent, nil if failed
	(if (= (elem-get sock :type) sock_dgram)
		(progn
			(defq src-port (elem_get sock :port))
			(if src-port
				(udp/send dst-ip src-port dst-port data)
				nil))
		nil))

;;;;;;;;;;;;;;;;;;
; Socket Receive
;;;;;;;;;;;;;;;;;;

(defun socket/recv (sock)
	; Receive data from socket
	; Input: sock - socket
	; Output: byte array of received data or nil
	(cond
		((= (elem-get sock :type) sock_stream)
			; TCP receive
			(defq tcb (elem_get sock :tcb))
			(if tcb
				(tcp/recv-data tcb)
				nil))

		(t nil)))

(defun socket/set-handler (sock handler_fn)
	; Set data handler callback for socket
	; Inputs: sock - socket, handler-fn - callback function
	;   TCP: handler-fn called with (tcb data)
	;   UDP: handler-fn called with (src-ip src-port data)
	(elem-set sock :data-handler handler-fn))

;;;;;;;;;;;;;;;;;;
; Socket Close
;;;;;;;;;;;;;;;;;;

(defun socket/close (sock)
	; Close socket
	; Input: sock - socket
	(cond
		((= (elem-get sock :type) sock_stream)
			; TCP close
			(defq tcb (elem_get sock :tcb))
			(when tcb
				(tcp/close tcb)
				(elem-set sock :tcb nil))
			(elem-set sock :state sock_state_closed))

		((= (elem-get sock :type) sock_dgram)
			; UDP close
			(defq port (elem_get sock :port))
			(when port
				(udp/unbind port)
				(elem-set sock :port nil))
			(elem-set sock :state sock_state_closed)))

	t)

;;;;;;;;;;;;;;;;;;
; Socket Status
;;;;;;;;;;;;;;;;;;

(defun socket/is-connected (sock)
	; Check if socket is connected
	(if (= (elem-get sock :type) sock_stream)
		(progn
			(defq tcb (elem_get sock :tcb))
			(and tcb (tcp/is-connected tcb)))
		nil))

(defun socket/get-state (sock)
	; Get socket state string
	(defq state (elem_get sock :state))
	(cond
		((= state sock_state_closed) "CLOSED")
		((= state sock_state_bound) "BOUND")
		((= state sock_state_listening) "LISTENING")
		((= state sock_state_connecting) "CONNECTING")
		((= state sock_state_connected) "CONNECTED")
		(t "UNKNOWN")))
