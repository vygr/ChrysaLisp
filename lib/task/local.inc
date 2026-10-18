;;;;;;;;;;;;;;;;;;;;
; local tasks object
;;;;;;;;;;;;;;;;;;;;

;A Farm, lib/task/farm.inc, whose children are a herd on the nodes of this
;machine. It says where they go and how many: each node in turn, and as
;many as the nodes call for, more as nodes join. The rest is the farm's

(import "./farm.inc")

;module
(env-push)

(defun herd-want (herd_max herd_init herd_growth nodes)
	;how many workers a herd of these nodes is to have
	(max 1 (min herd_max (+ herd_init (* herd_growth (dec (length nodes)))))))

(defclass Local (create destroy &optional herd_max herd_init herd_growth) (Farm create destroy 0)
	; (Local fnc_create fnc_destroy [herd_max herd_init herd_growth]) -> local
	;a herd of worker tasks on the nodes of this machine, those that share
	;its file system. The herd is herd_init workers, and herd_growth more
	;for each node other than this one, and no more than herd_max. They are
	;started at once, on each node in turn.
	(setd herd_max +max_int herd_init 1 herd_growth 1)
	(lower :herd_max :herd_init :herd_growth)
	(defq network_nodes (lisp-nodes :t) mutated :nil now (pii-time) e (penv))
	(. this :grow_to (herd-want herd_max herd_init herd_growth network_nodes))

	(defmethod :nodes ()
		; (. local :nodes) -> nodes
		;the nodes of this machine
		(lisp-nodes :t))

	(defmethod :place (key nodes)
		; (. local :place key nodes) -> nodes
		;each child on the next of the nodes in turn
		(list (elem-get nodes (% key (length nodes)))))

	(defmethod :grown ()
		; (. local :grown) -> local
		;more nodes may have joined, the herd grown to what they call for
		(raise :herd_max :herd_init :herd_growth)
		(. this :grow_to (herd-want herd_max herd_init herd_growth network_nodes)))

	(defmethod :reply (msg)
		; (. local :reply msg) -> :nil | val
		;what a kernel answers when it is asked to start a worker: its
		;entry, to keep the id in, :nil if none was started. The nodes of
		;a herd are this machine's, there is none of them to leave
		(if (/= (get-long (getf msg +kn_msg_reply_id) 0) 0)
			(. this :find (getf msg +kn_msg_key))))
	)

;module
(export-classes '(Local))
(env-pop)
