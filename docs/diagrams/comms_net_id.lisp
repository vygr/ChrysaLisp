;docs/gui/comms.md: what a net_id is made of
(defq s (dia-scene) blue 0xffd7e6f7 gold 0xffffe8a8)
(dia-put s 'mbox (dia-box '("mailbox_id" "64 bits") 160 48 blue 12 :first) 0 20)
(dia-put s 'node (dia-box '("node_id" "128 bits") 320 48 gold 12 :first) 160 20)
(dia-say s "net_id" 0 12 12 :t 0xff202428)
(dia-say s "which mailbox, on that node" 0 88 11)
(dia-say s "which node, anywhere in the network: routed to first" 160 88 11)
(diagram "comms_net_id" (dia-scene-doc s))
