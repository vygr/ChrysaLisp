%ifndef MAIL_1234
	%define MAIL_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

	def_class mail
	def_method statics, sys/mail_statics
	def_method init, sys/mail_init
	def_method deinit, sys/mail_deinit
	def_method alloc, sys/mail_alloc
	def_method parcel, sys/mail_alloc_parcel
	def_method send, sys/mail_send
	def_method read, sys/mail_read
	def_method mymail, sys/mail_mymail
	def_method in, sys/mail_in
	def_method out, sys/mail_out

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

	struc ml_mailbox
		ml_mailbox_list:		resb lh_list_size
		ml_mailbox_parcel_list:	resb lh_list_size
		ml_mailbox_tcb:			resq 1
	endstruc

	struc ml_msg
		ml_msg_node:			resb ln_node_size
		ml_msg_length:			resq 1
		ml_msg_dest:			resq 2
		ml_msg_parcel_id:		resq 2
		ml_msg_parcel_size:		resq 1
		ml_msg_parcel_total:	resq 1
		ml_msg_parcel_frag:		resq 1
		ml_msg_data:			resb 512
	endstruc

	struc ml_statics
		ml_statics_offchip_list:	resb lh_list_size
		ml_statics_heap:			resb hp_heap_size
		ml_statics_kernel_mailbox:	resq 1
		ml_statics_in_mailbox:		resq 1
		ml_statics_out_mailbox:		resq 1
		ml_statics_parcel_id:		resq 1
	endstruc

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

		vp_cpy 0, qword[%1 + ml_mailbox_tcb]
		lh_init %1, %2
		vp_lea [%1 + ml_mailbox_parcel_list], %2
		lh_init %2, %3
	%endmacro

	%macro ml_check 2
		;inputs
		;%1 = mailbox
		;%2 = temp
		;outputs
		;%1 = mailbox
		;%2 = 0 if no mail

		lh_is_empty %1, %2
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
