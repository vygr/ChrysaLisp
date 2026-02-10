(import "sys/list/struct.inc")

;task flags
(bits +tk_flag 0
	(bit aborted))

(structure +tk_node +ln_node_size
	(ptr stack stack_top mbox_addr lisp)
	(long time)
	(uint priority mbox_id flags)
	(align))
