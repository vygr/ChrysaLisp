;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ICMP (Internet Control Message Protocol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/packet.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ip.lisp")

;;;;;;;;;;;;;;;;;;
; ICMP Packet Creation
;;;;;;;;;;;;;;;;;;

(defun icmp/create-packet (type code data)
	; Create ICMP packet
	; Inputs:
	;   type - ICMP message type
	;   code - ICMP message code
	;   data - payload data (byte array)
	; Output: ICMP packet (byte array)
	(defq pkt (array))

	; Type
	(elem-set pkt 0 type)

	; Code
	(elem-set pkt 1 code)

	; Checksum (initially 0)
	(net/write-u16 pkt 2 0)

	; Rest of header (zeros for now, will be set by specific message types)
	(net/write-u32 pkt 4 0)

	; Add data
	(each (# (push pkt %0)) data)

	; Calculate and set checksum
	(defq cksum (net/checksum pkt 0 (length pkt)))
	(net/write-u16 pkt 2 cksum)

	pkt)

(defun icmp/create-echo-request (id seq data)
	; Create ICMP echo request (ping)
	; Inputs:
	;   id - identifier (16-bit)
	;   seq - sequence number (16-bit)
	;   data - payload data
	; Output: ICMP echo request packet
	(defq pkt (array))

	; Type (Echo Request)
	(elem-set pkt 0 icmp_echo)

	; Code
	(elem-set pkt 1 0)

	; Checksum (initially 0)
	(net/write-u16 pkt 2 0)

	; Identifier
	(net/write-u16 pkt 4 id)

	; Sequence number
	(net/write-u16 pkt 6 seq)

	; Add data
	(each (# (push pkt %0)) data)

	; Calculate and set checksum
	(defq cksum (net/checksum pkt 0 (length pkt)))
	(net/write-u16 pkt 2 cksum)

	pkt)

(defun icmp/create-echo-reply (id seq data)
	; Create ICMP echo reply (pong)
	; Inputs:
	;   id - identifier (16-bit)
	;   seq - sequence number (16-bit)
	;   data - payload data
	; Output: ICMP echo reply packet
	(defq pkt (array))

	; Type (Echo Reply)
	(elem-set pkt 0 icmp_echo_reply)

	; Code
	(elem-set pkt 1 0)

	; Checksum (initially 0)
	(net/write-u16 pkt 2 0)

	; Identifier
	(net/write-u16 pkt 4 id)

	; Sequence number
	(net/write-u16 pkt 6 seq)

	; Add data
	(each (# (push pkt %0)) data)

	; Calculate and set checksum
	(defq cksum (net/checksum pkt 0 (length pkt)))
	(net/write-u16 pkt 2 cksum)

	pkt)

(defun icmp/create-dest-unreachable (code orig_packet)
	; Create ICMP destination unreachable
	; Inputs:
	;   code - unreachable code
	;   orig_packet - original IP packet (first 8 bytes of data)
	; Output: ICMP dest unreachable packet
	(defq pkt (array))

	; Type (Destination Unreachable)
	(elem-set pkt 0 icmp_dest_unreach)

	; Code
	(elem-set pkt 1 code)

	; Checksum (initially 0)
	(net/write-u16 pkt 2 0)

	; Unused (must be zero)
	(net/write-u32 pkt 4 0)

	; Add original IP header + 8 bytes of data
	(defq copy_len (min 28 (length orig_packet)))
	(defq i 0)
	(while (< i copy_len)
		(push pkt (elem-get orig_packet i))
		(setq i (+ i 1)))

	; Calculate and set checksum
	(defq cksum (net/checksum pkt 0 (length pkt)))
	(net/write-u16 pkt 2 cksum)

	pkt)

;;;;;;;;;;;;;;;;;;
; ICMP Packet Parsing
;;;;;;;;;;;;;;;;;;

(defun icmp/parse (data)
	; Parse ICMP packet
	; Input: data - byte array
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) 8)
		nil
		(env
			:type (elem-get data 0)
			:code (elem-get data 1)
			:checksum (net/read-u16 data 2)
			:rest (net/read-u32 data 4)
			:data (slice data 8 (length data)))))

(defun icmp/verify-checksum (data)
	; Verify ICMP checksum
	; Input: data - ICMP packet byte array
	; Output: t if valid, nil if invalid
	(= (net/checksum data 0 (length data)) 0xFFFF))

;;;;;;;;;;;;;;;;;;
; ICMP Echo (Ping)
;;;;;;;;;;;;;;;;;;

(defq *icmp-echo-handlers* (list))  ; List of echo reply handlers

(defun icmp/send-ping (dst_ip id seq data)
	; Send ICMP echo request (ping)
	; Inputs:
	;   dst_ip - destination IP (4-byte array)
	;   id - identifier
	;   seq - sequence number
	;   data - payload data
	; Output: IP packet or nil
	(defq icmp_pkt (icmp/create-echo-request id seq data))
	(ip/send-packet dst_ip ip_proto_icmp icmp_pkt))

(defun icmp/register-echo-handler (handler_fn)
	; Register handler for ICMP echo replies
	; Input: handler_fn - function to call with (src_ip id seq data)
	(push *icmp-echo-handlers* handler_fn))

;;;;;;;;;;;;;;;;;;
; ICMP Packet Processing
;;;;;;;;;;;;;;;;;;

(defun icmp/handle-echo-request (src_ip dst_ip icmp_pkt)
	; Handle ICMP echo request
	; Inputs: src_ip, dst_ip - IP addresses, icmp_pkt - parsed ICMP packet
	; Output: ICMP echo reply packet or nil
	(defq id (ash (elem-get icmp_pkt :rest) -16)
	      seq (logand (elem-get icmp_pkt :rest) 0xFFFF)
	      reply (icmp/create-echo-reply id seq (elem-get icmp_pkt :data)))

	; Send reply back to source
	(ip/send-packet src_ip ip_proto_icmp reply))

(defun icmp/handle-echo-reply (src_ip dst_ip icmp_pkt)
	; Handle ICMP echo reply
	; Inputs: src_ip, dst_ip - IP addresses, icmp_pkt - parsed ICMP packet
	(defq id (ash (elem-get icmp_pkt :rest) -16)
	      seq (logand (elem-get icmp_pkt :rest) 0xFFFF))

	; Call all registered handlers
	(each (# (%0 src_ip id seq (elem-get icmp_pkt :data)))
	      *icmp-echo-handlers*))

(defun icmp/handle-dest-unreachable (src_ip dst_ip icmp_pkt)
	; Handle ICMP destination unreachable
	; Inputs: src_ip, dst_ip - IP addresses, icmp_pkt - parsed ICMP packet
	; (Could notify transport layers about unreachable destination)
	nil)

(defun icmp/process (src_ip dst_ip data)
	; Process incoming ICMP packet
	; Inputs: src_ip, dst_ip - IP addresses (4-byte arrays), data - ICMP packet
	; Output: t if processed, nil if error
	(defq icmp_pkt (icmp/parse data))

	(if (and icmp_pkt (icmp/verify-checksum data))
		(progn
			(cond
				((= (elem-get icmp_pkt :type) icmp_echo)
					; Echo request - send reply
					(icmp/handle-echo-request src_ip dst_ip icmp_pkt)
					t)

				((= (elem-get icmp_pkt :type) icmp_echo_reply)
					; Echo reply - notify handlers
					(icmp/handle-echo-reply src_ip dst_ip icmp_pkt)
					t)

				((= (elem-get icmp_pkt :type) icmp_dest_unreach)
					; Destination unreachable
					(icmp/handle-dest-unreachable src_ip dst_ip icmp_pkt)
					t)

				(t
					; Unknown ICMP type
					nil)))
		; Invalid packet
		nil))

;;;;;;;;;;;;;;;;;;
; Initialize ICMP
;;;;;;;;;;;;;;;;;;

(defun icmp/init ()
	; Initialize ICMP layer
	; Register ICMP as IP protocol handler
	(ip/register-handler ip_proto_icmp icmp/process))
