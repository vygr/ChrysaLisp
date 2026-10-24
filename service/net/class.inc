;;;;;;;;;;;;;;;;;;;;
; host net PII class
;;;;;;;;;;;;;;;;;;;;

(include "lib/asm/class.inc")

(def-class :host_net :nil
	(dec-method :init :nil :virtual () (:r0))
	(dec-method :deinit :nil :virtual () (:r0))
	(dec-method :connect :nil :virtual (:r0 :r1) (:r0))
	(dec-method :listen :nil :virtual (:r0) (:r0))
	(dec-method :accept :nil :virtual (:r0) (:r0))
	(dec-method :send :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :recv :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :close :nil :virtual (:r0) (:r0))
	(dec-method :poll :nil :virtual (:r0) (:r0))
	(dec-method :udp_bind :nil :virtual (:r0) (:r0))
	(dec-method :udp_send :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :udp_recv :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :lisp_init service/net/lisp_init :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_deinit service/net/lisp_deinit :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_connect service/net/lisp_connect :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_listen service/net/lisp_listen :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_accept service/net/lisp_accept :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_send service/net/lisp_send :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r13-:r14 :f0-:f15))
	(dec-method :lisp_recv service/net/lisp_recv :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_close service/net/lisp_close :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r13-:r14 :f0-:f15))
	(dec-method :lisp_poll service/net/lisp_poll :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r13-:r14 :f0-:f15))
	(dec-method :lisp_udp_bind service/net/lisp_udp_bind :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_udp_send service/net/lisp_udp_send :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_udp_recv service/net/lisp_udp_recv :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_links service/net/lisp_links :static (:r0 :r1) (:r0 :r1) (:r1-:r5 :f0-:f15))
	(dec-method :lisp_drop service/net/lisp_drop :static (:r0 :r1) (:r0 :r1) (:r1-:r4 :r14 :f0-:f15))
	(dec-method :link service/net/link :static :nil :nil (:r0-:r14 :f0-:f15))
	(dec-method :conn service/net/conn :static :nil :nil (:r0-:r7 :r13-:r14 :f0-:f15))
	(dec-method :in service/net/in :static :nil :nil (:r0-:r14 :f0-:f15))
	(dec-method :out service/net/out :static :nil :nil (:r0-:r14 :f0-:f15))
	(dec-method :send_all service/net/send_all :static (:r0 :r1 :r2 :r3) (:r0) (:r0-:r2 :r13-:r14 :f0-:f15))
	(dec-method :recv_all service/net/recv_all :static (:r0 :r1 :r2 :r3) (:r0) (:r0-:r14 :f0-:f15)))

(defun host-net-call (%0 %1 &optional %2 %3 %4 %5)
	;%0 = class name (:host_net)
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	;%4 = dispatch reg
	;%5 = temp reg
	(setd %4 :r14 %5 :r13)
	(fn-bind 'sys/statics/statics %4)
	(vp-cpy-ir %4 statics_sys_load_host_net_funcs %4)
	(abi-call-table %0 %1 %4 %2 %3 %5))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; network link wire format
;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +net_wire_type 0
	(enum none ping frag))

(structure +net_wire_hdr 0
	(uint length type))

(structure +net_ping_pkt 0
	(uint wire_length wire_type)
	(nodeid peer_node_id)
	(nodeid peer_system_id)
	;pad is 0, so nothing of this node's memory goes out
	(uint task_count pad)
	(align))

;what follows the header of a ping on the wire, the start of a link's
;record, lk_node, its ids and its load. It is the wire's own size, the
;record has more that is not sent
(defq +net_ping_payload_size (- +net_ping_pkt_size +net_wire_hdr_size))

(structure +net_frag_pkt 0
	(uint wire_length wire_type)
	(netid dest src)
	;pad is 0, as the ping's
	(uint length offset total pad)
	(align))
