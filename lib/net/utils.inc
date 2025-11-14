;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./consts.inc")
(import "./packet.inc")

;;;;;;;;;;;;;;;;;;
; Checksum Calculation
;;;;;;;;;;;;;;;;;;

(defun net/checksum (data start len)
	; Calculate Internet checksum (RFC 1071)
	; Inputs:
	;   data - byte array or string
	;   start - starting offset
	;   len - length to checksum
	; Output:
	;   16-bit checksum value
	(defq sum 0 i start end (+ start len))

	; Sum 16-bit words
	(while (< (+ i 1) end)
		(setq sum (+ sum
			(logior
				(ash (elem-get data i) 8)
				(elem-get data (+ i 1)))))
		(setq i (+ i 2)))

	; Add odd byte if present
	(when (< i end)
		(setq sum (+ sum (ash (elem-get data i) 8))))

	; Fold 32-bit sum to 16 bits
	(while (> sum 0xFFFF)
		(setq sum (+ (logand sum 0xFFFF) (ash sum -16))))

	; Return one's complement
	(logxor sum 0xFFFF))

(defun net/verify-checksum (data start len)
	; Verify checksum is correct (should be 0xFFFF when included)
	(= (net/checksum data start len) 0xFFFF))

;;;;;;;;;;;;;;;;;;
; IP Address Utilities
;;;;;;;;;;;;;;;;;;

(defun net/ip-to-string (ip_bytes)
	; Convert 4-byte IP address to string "A.B.C.D"
	; Input: ip_bytes - array or string of 4 bytes
	; Output: string representation
	(str (elem-get ip_bytes 0) "."
	     (elem-get ip_bytes 1) "."
	     (elem-get ip_bytes 2) "."
	     (elem-get ip_bytes 3)))

(defun net/string-to-ip (ip_str)
	; Convert string "A.B.C.D" to 4-byte array
	; Input: ip_str - string like "192.168.1.1"
	; Output: array of 4 bytes
	(defq parts (split ip_str "."))
	(if (= (length parts) 4)
		(apply array (map (# (num %0)) parts))
		nil))

(defun net/mac-to-string (mac_bytes)
	; Convert 6-byte MAC address to string "XX:XX:XX:XX:XX:XX"
	(defq parts (map (# (str-to-hex (array %0)))
	                 (list (elem-get mac_bytes 0)
	                       (elem-get mac_bytes 1)
	                       (elem-get mac_bytes 2)
	                       (elem-get mac_bytes 3)
	                       (elem-get mac_bytes 4)
	                       (elem-get mac_bytes 5))))
	(join ":" parts))

(defun net/string-to-mac (mac_str)
	; Convert string "XX:XX:XX:XX:XX:XX" to 6-byte array
	(defq parts (split mac_str ":"))
	(if (= (length parts) 6)
		(apply array (map (# (hex-to-num %0)) parts))
		nil))

(defun net/ip-in-subnet (ip netmask network)
	; Check if IP address is in subnet
	; All inputs are 4-byte arrays
	(defq ip_val (logand
		(logior (ash (elem-get ip 0) 24)
		        (ash (elem-get ip 1) 16)
		        (ash (elem-get ip 2) 8)
		        (elem-get ip 3)))
	      mask_val (logand
		(logior (ash (elem-get netmask 0) 24)
		        (ash (elem-get netmask 1) 16)
		        (ash (elem-get netmask 2) 8)
		        (elem-get netmask 3)))
	      net_val (logand
		(logior (ash (elem-get network 0) 24)
		        (ash (elem-get network 1) 16)
		        (ash (elem-get network 2) 8)
		        (elem-get network 3))))
	(= (logand ip_val mask_val) (logand net_val mask_val)))

;;;;;;;;;;;;;;;;;;
; Byte Order Conversion
;;;;;;;;;;;;;;;;;;

(defun net/htons (val)
	; Host to network short (16-bit)
	(logior
		(ash (logand val 0x00FF) 8)
		(ash (logand val 0xFF00) -8)))

(defun net/htonl (val)
	; Host to network long (32-bit)
	(logior
		(ash (logand val 0x000000FF) 24)
		(ash (logand val 0x0000FF00) 8)
		(ash (logand val 0x00FF0000) -8)
		(ash (logand val 0xFF000000) -24)))

(defun net/ntohs (val) (net/htons val))
(defun net/ntohl (val) (net/htonl val))

;;;;;;;;;;;;;;;;;;
; Packet Buffer Utilities
;;;;;;;;;;;;;;;;;;

(defun net/write-u16 (buf offset val)
	; Write 16-bit value in network byte order
	(elem-set buf offset (ash val -8))
	(elem-set buf (+ offset 1) (logand val 0xFF)))

(defun net/write-u32 (buf offset val)
	; Write 32-bit value in network byte order
	(elem-set buf offset (ash val -24))
	(elem-set buf (+ offset 1) (logand (ash val -16) 0xFF))
	(elem-set buf (+ offset 2) (logand (ash val -8) 0xFF))
	(elem-set buf (+ offset 3) (logand val 0xFF)))

(defun net/read-u16 (buf offset)
	; Read 16-bit value from network byte order
	(logior
		(ash (elem-get buf offset) 8)
		(elem-get buf (+ offset 1))))

(defun net/read-u32 (buf offset)
	; Read 32-bit value from network byte order
	(logior
		(ash (elem-get buf offset) 24)
		(ash (elem-get buf (+ offset 1)) 16)
		(ash (elem-get buf (+ offset 2)) 8)
		(elem-get buf (+ offset 3))))

(defun net/copy-bytes (src src_off dst dst_off len)
	; Copy bytes from src to dst
	(defq i 0)
	(while (< i len)
		(elem-set dst (+ dst_off i) (elem-get src (+ src_off i)))
		(setq i (+ i 1))))

;;;;;;;;;;;;;;;;;;
; Random Number Generation
;;;;;;;;;;;;;;;;;;

(defq *net_random_state* (time))

(defun net/random ()
	; Simple LCG random number generator for sequence numbers
	(setq *net_random_state*
		(logand (+ (* *net_random_state* 1103515245) 12345) 0xFFFFFFFF))
	*net_random_state*)

(defun net/random-range (min max)
	; Random number in range [min, max)
	(+ min (mod (net/random) (- max min))))
