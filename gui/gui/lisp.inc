;gui events for Lisp level (getf)

(enums +ev_type 0
	(enum mouse key action gui))

(structure +ev_msg 0
	(ulong target_id type))

(structure +ev_msg_mouse +ev_msg_size
	(uint buttons)
	(int x y rx ry))

(structure +ev_msg_key +ev_msg_size
	(uint keycode key))

(structure +ev_msg_action +ev_msg_size
	(ulong source_id))

(structure +ev_msg_gui +ev_msg_size)
