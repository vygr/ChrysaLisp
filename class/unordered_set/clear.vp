%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_vector.ninc'

def_func class/unordered_set/clear
	;inputs
	;r0 = unordered_set object
	;outputs
	;r0 = unordered_set object
	;trashes
	;all but r0, r4

	;clear all buckets
	vp_push r0
	d_call vector, get_length, {[r0 + unordered_set_buckets]}, {r1}
	f_call vector, for_each, {r0, 0, r1, $callback, 0}, {r1}
	vp_pop r0
	vp_cpy r1, [r0 + unordered_set_length]
	vp_ret

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	f_call vector, clear, {[r1]}
	vp_cpy 1, r1
	vp_ret

def_func_end
