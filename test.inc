"list Structures"

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

(def-struct 'view 'obj)
	(byte 'view_b1 'view_b2)
	(ubyte 'view_ub1 'view_ub2)
	(short 'view_s1 'view_s2)
	(ushort 'view_us1 'view_us2)
	(int 'view_i1 'view_i2)
	(uint 'view_ui1 'view_ui2)
	(long 'view_l1 'view_l2)
	(ulong 'view_ul1 'view_ul2)
	(ptr 'view_inst 'view_args 'view_value)
	(byte 'view_b3)
	(offset 'view_path)
(def-struct-end)
