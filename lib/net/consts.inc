;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Protocol Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Ethernet Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'eth_addr_len 6)          ; MAC address length
(defcvar 'eth_hdr_len 14)          ; Ethernet header length
(defcvar 'eth_crc_len 4)           ; CRC length
(defcvar 'eth_min_len 64)          ; Minimum frame length
(defcvar 'eth_max_len 1518)        ; Maximum frame length (including CRC)
(defcvar 'eth_mtu 1500)            ; Maximum Transmission Unit

; Ethernet frame types (EtherType)
(defcvar 'eth_type_ip 0x0800)      ; IPv4
(defcvar 'eth_type_arp 0x0806)     ; ARP
(defcvar 'eth_type_ipv6 0x86DD)    ; IPv6

;;;;;;;;;;;;;;;;;;
; ARP Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'arp_hrd_ethernet 1)      ; Ethernet hardware type
(defcvar 'arp_op_request 1)        ; ARP request
(defcvar 'arp_op_reply 2)          ; ARP reply

(defcvar 'arp_cache_size 64)       ; ARP cache entries
(defcvar 'arp_cache_timeout 300000000) ; 5 minutes in microseconds

;;;;;;;;;;;;;;;;;;
; IP Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'ip_addr_len 4)           ; IPv4 address length
(defcvar 'ip_hdr_min_len 20)       ; Minimum IP header length
(defcvar 'ip_version 4)            ; IPv4 version
(defcvar 'ip_default_ttl 64)       ; Default Time To Live

; IP protocol numbers
(defcvar 'ip_proto_icmp 1)         ; ICMP
(defcvar 'ip_proto_tcp 6)          ; TCP
(defcvar 'ip_proto_udp 17)         ; UDP

; IP fragmentation flags
(defcvar 'ip_flag_df 0x4000)       ; Don't Fragment
(defcvar 'ip_flag_mf 0x2000)       ; More Fragments
(defcvar 'ip_frag_offset_mask 0x1FFF) ; Fragment offset mask

;;;;;;;;;;;;;;;;;;
; ICMP Constants
;;;;;;;;;;;;;;;;;;

; ICMP message types
(defcvar 'icmp_echo_reply 0)       ; Echo reply (pong)
(defcvar 'icmp_dest_unreach 3)     ; Destination unreachable
(defcvar 'icmp_source_quench 4)    ; Source quench
(defcvar 'icmp_redirect 5)         ; Redirect
(defcvar 'icmp_echo 8)             ; Echo request (ping)
(defcvar 'icmp_time_exceeded 11)   ; Time exceeded
(defcvar 'icmp_param_problem 12)   ; Parameter problem

; ICMP codes for Destination Unreachable
(defcvar 'icmp_net_unreach 0)
(defcvar 'icmp_host_unreach 1)
(defcvar 'icmp_proto_unreach 2)
(defcvar 'icmp_port_unreach 3)
(defcvar 'icmp_frag_needed 4)
(defcvar 'icmp_source_route_failed 5)

;;;;;;;;;;;;;;;;;;
; UDP Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'udp_hdr_len 8)           ; UDP header length

;;;;;;;;;;;;;;;;;;
; TCP Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'tcp_hdr_min_len 20)      ; Minimum TCP header length

; TCP flags
(defcvar 'tcp_flag_fin 0x01)       ; Finish
(defcvar 'tcp_flag_syn 0x02)       ; Synchronize
(defcvar 'tcp_flag_rst 0x04)       ; Reset
(defcvar 'tcp_flag_psh 0x08)       ; Push
(defcvar 'tcp_flag_ack 0x10)       ; Acknowledgment
(defcvar 'tcp_flag_urg 0x20)       ; Urgent

; TCP states
(defcvar 'tcp_state_closed 0)
(defcvar 'tcp_state_listen 1)
(defcvar 'tcp_state_syn_sent 2)
(defcvar 'tcp_state_syn_received 3)
(defcvar 'tcp_state_established 4)
(defcvar 'tcp_state_fin_wait_1 5)
(defcvar 'tcp_state_fin_wait_2 6)
(defcvar 'tcp_state_close_wait 7)
(defcvar 'tcp_state_closing 8)
(defcvar 'tcp_state_last_ack 9)
(defcvar 'tcp_state_time_wait 10)

; TCP timer values (in microseconds)
(defcvar 'tcp_timeout_init 3000000)     ; 3 seconds initial RTO
(defcvar 'tcp_timeout_min 1000000)      ; 1 second minimum RTO
(defcvar 'tcp_timeout_max 60000000)     ; 60 seconds maximum RTO
(defcvar 'tcp_time_wait_timeout 120000000) ; 2 minutes
(defcvar 'tcp_keepalive_timeout 7200000000) ; 2 hours

; TCP window sizes
(defcvar 'tcp_default_window 65535) ; Default receive window
(defcvar 'tcp_max_window 65535)     ; Maximum window size (without scaling)

; TCP connection limits
(defcvar 'tcp_max_connections 256)  ; Maximum concurrent connections
(defcvar 'tcp_max_retries 12)       ; Maximum retransmission attempts
(defcvar 'tcp_syn_retries 5)        ; SYN retries

;;;;;;;;;;;;;;;;;;
; Socket Constants
;;;;;;;;;;;;;;;;;;

; Socket types
(defcvar 'sock_stream 1)            ; TCP socket
(defcvar 'sock_dgram 2)             ; UDP socket
(defcvar 'sock_raw 3)               ; Raw IP socket

; Socket states
(defcvar 'sock_state_closed 0)
(defcvar 'sock_state_bound 1)
(defcvar 'sock_state_listening 2)
(defcvar 'sock_state_connecting 3)
(defcvar 'sock_state_connected 4)

; Socket options
(defcvar 'so_reuseaddr 0x0004)
(defcvar 'so_keepalive 0x0008)
(defcvar 'so_broadcast 0x0020)
(defcvar 'so_rcvbuf 0x1002)
(defcvar 'so_sndbuf 0x1001)

;;;;;;;;;;;;;;;;;;
; Packet Buffer Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'pkt_pool_size 512)        ; Number of packet buffers
(defcvar 'pkt_buffer_size 2048)     ; Size of each packet buffer
(defcvar 'pkt_max_headers 128)      ; Maximum header space

;;;;;;;;;;;;;;;;;;
; Network Utilities
;;;;;;;;;;;;;;;;;;

; Convert 32-bit IP address to network byte order (big-endian)
(defun htonl (val)
	; For little-endian architectures
	(logior
		(ash (logand val 0x000000FF) 24)
		(ash (logand val 0x0000FF00) 8)
		(ash (logand val 0x00FF0000) -8)
		(ash (logand val 0xFF000000) -24)))

; Convert 16-bit value to network byte order
(defun htons (val)
	(logior
		(ash (logand val 0x00FF) 8)
		(ash (logand val 0xFF00) -8)))

; Network to host conversions (same as host to network for these)
(defun ntohl (val) (htonl val))
(defun ntohs (val) (htons val))
