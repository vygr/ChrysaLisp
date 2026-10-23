(import "././list/struct.inc")

;task flags
(bits +tk_flag 0
	(bit aborted))

(structure +tk_node +ln_node_size
	;dying is not 0 while the task is destroying an object, and is then
	;the notes of those put by to be done after it, 1 for none: each note
	;the next note and the object, (:obj :destroy). print is the lists the
	;task is printing, 0 for none: each a note on the stack of its print,
	;the next note and the list, (:list :print)
	;server is the function server of another system the task binds
	;functions of first, 0 for none, (:sys_load :bind)
	(ptr stack stack_top mbox_addr lisp dying print server)
	(long time)
	(uint priority mbox_id flags)
	(align))
