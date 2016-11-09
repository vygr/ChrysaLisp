"list structures"

(def-struct 'lh_list)
	(ptr 'lh_list_head)
	(ptr 'lh_list_tail)
	(ptr 'lh_list_tailpred)
(def-struct-end)

(def-struct 'ln_node)
	(ptr 'ln_node_succ)
	(ptr 'ln_node_pred)
(def-struct-end)

(def-struct 'ln_fnode)
	(ptr 'ln_fnode_next)
(def-struct-end)

(def-struct 'obj 'ln_fnode)
	(ptr 'obj_vtable)
(def-struct-end)

(def-struct 'local 'obj)
	(byte 'local_b1 'local_b2)
	(ubyte 'local_ub1 'local_ub2)
	(short 'local_s1 'local_s2)
	(ushort 'local_us1 'local_us2)
	(int 'local_i1 'local_i2)
	(uint 'local_ui1 'local_ui2)
	(long 'local_l1 'local_l2)
	(ulong 'local_ul1 'local_ul2)
	(ptr 'local_inst 'local_args 'local_value)
	(byte 'local_b3)
	(offset 'local_path)
(def-struct-end)
