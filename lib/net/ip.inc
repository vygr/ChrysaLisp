;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IP (Internet Protocol) Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./consts.inc")
(import "./packet.inc")
(import "./utils.inc")

;;;;;;;;;;;;;;;;;;
; IP State
;;;;;;;;;;;;;;;;;;

(defq *ip_id_counter* 0)  ; Packet ID counter
(defq *ip_our_addr* (array 0 0 0 0))  ; Our IP address
(defq *ip_netmask* (array 255 255 255 0))  ; Our netmask
(defq *ip_gateway* (array 0 0 0 0))  ; Default gateway

(defun ip/init (ip netmask gateway)
	; Initialize IP layer
	; Inputs: ip, netmask, gateway - 4-byte arrays
	(setq *ip_our_addr* (apply array ip))
	(setq *ip_netmask* (apply array netmask))
	(setq *ip_gateway* (apply array gateway))
	(setq *ip_id_counter* (net/random-range 1 65536)))

(defun ip/get-addr ()
	; Get our IP address
	*ip_our_addr*)

;;;;;;;;;;;;;;;;;;
; IP Header Creation
;;;;;;;;;;;;;;;;;;

(defun ip/create-header (src_ip dst_ip protocol data_len &optional ttl df_flag)
	; Create IP header
	; Inputs:
	;   src_ip, dst_ip - 4-byte arrays
	;   protocol - protocol number (TCP=6, UDP=17, ICMP=1)
	;   data_len - length of data following header
	;   ttl - time to live (optional, default 64)
	;   df_flag - don't fragment flag (optional, default nil)
	; Output: byte array of IP header
	(defq hdr (array)
	      total_len (+ ip_hdr_min_len data_len)
	      id *ip_id_counter*
	      flags_offset (if df_flag ip_flag_df 0))

	; Increment ID for next packet
	(setq *ip_id_counter* (logand (+ *ip_id_counter* 1) 0xFFFF))

	; Version (4) and IHL (5 = 20 bytes)
	(elem-set hdr 0 0x45)

	; Type of Service
	(elem-set hdr 1 0)

	; Total length
	(net/write-u16 hdr 2 total_len)

	; Identification
	(net/write-u16 hdr 4 id)

	; Flags and fragment offset
	(net/write-u16 hdr 6 flags_offset)

	; Time to Live
	(elem-set hdr 8 (or ttl ip_default_ttl))

	; Protocol
	(elem-set hdr 9 protocol)

	; Checksum (will be calculated)
	(net/write-u16 hdr 10 0)

	; Source IP
	(each (# (elem-set hdr (+ 12 %1) %0)) src_ip)

	; Destination IP
	(each (# (elem-set hdr (+ 16 %1) %0)) dst_ip)

	; Calculate and set checksum
	(defq cksum (net/checksum hdr 0 ip_hdr_min_len))
	(net/write-u16 hdr 10 cksum)

	hdr)

;;;;;;;;;;;;;;;;;;
; IP Packet Parsing
;;;;;;;;;;;;;;;;;;

(defun ip/parse (data)
	; Parse IP packet
	; Input: data - byte array
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) ip_hdr_min_len)
		nil
		(progn
			(defq ver_ihl (elem-get data 0)
			      version (ash ver_ihl -4)
			      ihl (logand ver_ihl 0x0F)
			      hdr_len (* ihl 4)
			      total_len (net/read-u16 data 2))

			; Validate
			(if (and (= version 4)
			         (>= ihl 5)
			         (<= total_len (length data)))
				(env
					:version version
					:ihl ihl
					:header-len hdr_len
					:tos (elem-get data 1)
					:total-len total_len
					:id (net/read-u16 data 4)
					:flags (ash (net/read-u16 data 6) -13)
					:frag-offset (logand (net/read-u16 data 6) 0x1FFF)
					:ttl (elem-get data 8)
					:protocol (elem-get data 9)
					:checksum (net/read-u16 data 10)
					:src-ip (slice data 12 16)
					:dst-ip (slice data 16 20)
					:options (if (> hdr_len ip_hdr_min_len)
					            (slice data ip_hdr_min_len hdr_len)
					            (array))
					:data (slice data hdr_len total_len))
				nil))))

(defun ip/verify-checksum (data)
	; Verify IP header checksum
	; Input: data - IP packet byte array
	; Output: t if valid, nil if invalid
	(if (< (length data) ip_hdr_min_len)
		nil
		(progn
			(defq ver_ihl (elem-get data 0)
			      ihl (logand ver_ihl 0x0F)
			      hdr_len (* ihl 4))
			(= (net/checksum data 0 hdr_len) 0xFFFF))))

;;;;;;;;;;;;;;;;;;
; IP Routing
;;;;;;;;;;;;;;;;;;

(defun ip/is-local (dst_ip)
	; Check if destination IP is on local network
	; Input: dst_ip - 4-byte array
	; Output: t if local, nil if needs gateway
	(net/ip-in-subnet dst_ip *ip_netmask* *ip_our_addr*))

(defun ip/next-hop (dst_ip)
	; Determine next hop for destination
	; Input: dst_ip - 4-byte array
	; Output: next hop IP address (4-byte array)
	(if (ip/is-local dst_ip)
		dst_ip
		*ip_gateway*))

;;;;;;;;;;;;;;;;;;
; IP Packet Sending
;;;;;;;;;;;;;;;;;;

(defun ip/send-packet (dst_ip protocol data &optional ttl)
	; Send IP packet
	; Inputs:
	;   dst_ip - 4-byte array
	;   protocol - protocol number
	;   data - payload data (byte array)
	;   ttl - optional time to live
	; Output: complete packet (IP header + data) or nil
	(defq hdr (ip/create-header *ip_our_addr* dst_ip
	                            protocol (length data) ttl)
	      packet (array))

	; Combine header and data
	(each (# (push packet %0)) hdr)
	(each (# (push packet %0)) data)

	packet)

;;;;;;;;;;;;;;;;;;
; IP Packet Reception
;;;;;;;;;;;;;;;;;;

(defq *ip_protocol_handlers* (env 16))  ; Protocol handler registry

(defun ip/register-handler (protocol handler_fn)
	; Register handler for IP protocol
	; Inputs:
	;   protocol - protocol number
	;   handler_fn - function to call with (src_ip dst_ip data)
	(elem-set *ip_protocol_handlers* protocol handler_fn))

(defun ip/process (packet)
	; Process incoming IP packet
	; Input: packet - raw IP packet (byte array)
	; Output: t if processed, nil if error
	(defq ip_pkt (ip/parse packet))

	(if (and ip_pkt (ip/verify-checksum packet))
		(progn
			; Check if packet is for us
			(when (or (every eql *ip_our_addr* (elem-get ip_pkt :dst-ip))
			          ; Broadcast check could go here
			          (every eql (array 255 255 255 255) (elem-get ip_pkt :dst-ip)))

				; Find protocol handler
				(defq handler (elem-get *ip_protocol_handlers*
				                        (elem-get ip_pkt :protocol)))

				(if handler
					(progn
						; Call protocol handler
						(handler (elem-get ip_pkt :src-ip)
						         (elem-get ip_pkt :dst-ip)
						         (elem-get ip_pkt :data))
						t)
					; No handler for this protocol
					nil)))
		; Invalid packet
		nil))

;;;;;;;;;;;;;;;;;;
; IP Fragmentation (Simplified)
;;;;;;;;;;;;;;;;;;

(defun ip/fragment (data mtu)
	; Fragment IP data into MTU-sized chunks
	; Inputs:
	;   data - byte array to fragment
	;   mtu - maximum transmission unit
	; Output: list of fragments
	(defq fragments (list)
	      offset 0
	      data-len (length data)
	      max-frag-size (- mtu ip_hdr_min_len))

	(while (< offset data-len)
		(defq frag-len (min max-frag-size (- data-len offset))
		      frag (slice data offset (+ offset frag-len)))
		(push fragments frag)
		(setq offset (+ offset frag-len)))

	fragments)
