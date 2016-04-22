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
	def_method alloc, sys/mail_alloc, static, '', 'r0'
	def_method alloc_parcel, sys/mail_alloc_parcel, static, 'r0', 'r0'
	def_method send, sys/mail_send, static, 'r0'
	def_method read, sys/mail_read, static, 'r0', 'r0'
	def_method try_read, sys/mail_try_read, static, 'r0', 'r0'
	def_method select, sys/mail_select, static, 'r0, r1', 'r0'
	def_method mymail, sys/mail_mymail, static, '', 'r0'
	def_method in, sys/mail_in
	def_method out, sys/mail_out

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

	mailbox_id_size	equ 16
	mail_data_size	equ 512

	def_structure	ml_mailbox, lh_list
		def_struct	ml_mailbox_parcel_list, lh_list
		def_long	ml_mailbox_tcb
	def_structure_end

	def_structure	ml_msg, ln_node
		def_long	ml_msg_length
		def_struct	ml_msg_dest, mailbox_id
		def_struct	ml_msg_parcel_id, mailbox_id
		def_long	ml_msg_parcel_size
		def_long	ml_msg_parcel_total
		def_long	ml_msg_parcel_frag
		def_struct	ml_msg_header_size, null
		def_struct	ml_msg_data, mail_data
	def_structure_end

	def_structure	ml_statics
		def_struct	ml_statics_offchip_list, lh_list
		def_struct	ml_statics_heap, hp_heap
		def_long	ml_statics_kernel_mailbox
		def_long	ml_statics_in_mailbox
		def_long	ml_statics_out_mailbox
		def_long	ml_statics_parcel_id
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

		vp_cpy_cl 0, [%1 + ml_mailbox_tcb]
		lh_init %1, %2
		vp_lea [%1 + ml_mailbox_parcel_list], %2
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

		vp_sub ml_mailbox_size, r4
		ml_init r4, %1, %2
	%endmacro

	%macro ml_temp_destroy 0
		vp_add ml_mailbox_size, r4
	%endmacro

%endif
