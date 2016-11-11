%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_vector.ninc'

def_func class/unordered_set/copy
	;inputs
	;r0 = unordered_set object
	;r1 = num buckets
	;outputs
	;r0 = unordered_set object
	;r1 = unordered_set copy
	;trashes
	;all but r0, r4

	vp_cpy r0, r9
	f_call unordered_set, create, {[r0 + unordered_set_key_callback], r1}, {r0}
	vp_push r0
	f_call unordered_set, for_each, {r9, $callback, r4}, {_, _}
	vp_pop r1
	vp_cpy [r0 + unordered_set_length], r2
	vp_cpy r2, [r1 + unordered_set_length]
	vp_ret

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	vp_cpy [r0], r2
	f_call ref, ref, {[r1]}
	vp_push r0
	f_call unordered_set, get_bucket, {r2, r0}, {r0}
	vp_pop r1
	f_jmp vector, push_back, {r0, r1}

def_func_end
