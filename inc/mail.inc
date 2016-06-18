%ifndef MAIL_1234
%define MAIL_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

	def_class sys_mail
	def_method statics, sys/mail_statics
	def_method init, sys/mail_init
	def_method deinit, sys/mail_deinit
	def_method alloc, sys/mail_alloc, static, {}, {r0}
	def_method alloc_parcel, sys/mail_alloc_parcel, static, {r0}, {r0}
	def_method send, sys/mail_send, static, {r0}
	def_method read, sys/mail_read, static, {r0}, {r0}
	def_method try_read, sys/mail_try_read, static, {r0}, {r0}
	def_method select, sys/mail_select, static, {r0, r1}, {r0}
	def_method mymail, sys/mail_mymail, static, {}, {r0}
	def_method init_mailbox, sys/mail_init_mailbox, static, {r0}
	def_method in, sys/mail_in
	def_method out, sys/mail_out

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

	mail_data_size	equ 512

	def_structure mailbox, lh_list
		struct mailbox_parcel_list, lh_list
		ptr mailbox_tcb
	def_structure_end

	def_structure id
		ulong id_mbox
		ulong id_cpu
	def_structure_end

	def_structure msg, ln_node
		ulong msg_length
		struct msg_dest, id
		struct msg_parcel_id, id
		ulong msg_parcel_size
		ulong msg_parcel_total
		ulong msg_parcel_frag
		offset msg_header_size
		struct msg_data, mail_data
	def_structure_end

	def_structure ml_statics
		struct ml_statics_offchip_list, lh_list
		struct ml_statics_heap, hp_heap
		ptr ml_statics_kernel_mailbox
		ptr ml_statics_in_mailbox
		ptr ml_statics_out_mailbox
		ulong ml_statics_parcel_id
	def_structure_end

;;;;;;;;;;;;;
; mail macros
;;;;;;;;;;;;;

	%macro ml_init 3
		;inputs
		;%1 = mailbox
		;%2 = temp
		;%3 = temp
		;outputs
		;%1 = mailbox
		;trashes
		;%2, %3

		vp_cpy_cl 0, [%1 + mailbox_tcb]
		lh_init %1, %2
		vp_lea [%1 + mailbox_parcel_list], %2
		lh_init %2, %3
	%endmacro

	%macro ml_temp_create 2
		;inputs
		;%1 = temp
		;%2 = temp
		;outputs
		;r4 = mailbox
		;trashes
		;%1, %2

		vp_sub mailbox_size, r4
		ml_init r4, %1, %2
	%endmacro

	%macro ml_temp_destroy 0
		vp_add mailbox_size, r4
	%endmacro

%endif
