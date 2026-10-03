(report-header "Struct Edges: field wrap around, offsets, enums and bits")

(structure +se 0
	(byte b)
	(ubyte ub)
	(short s)
	(ushort us)
	(int i)
	(uint ui)
	(long l))

(defq se_obj (str-alloc +se_size))

; --- sizes, note str-alloc does not clear the memory it gives ---
(test-cases
	+se_size 24
	(length se_obj) 24
	(length (str-alloc 0)) 0)

; --- signed fields wrap, unsigned fields hold the full range ---
(setf-> se_obj (+se_b 255) (+se_ub 255) (+se_s -1) (+se_us -1) (+se_i -1) (+se_ui -1) (+se_l -1))
(test-cases
	(getf se_obj +se_b) -1
	(getf se_obj +se_ub) 255
	(getf se_obj +se_s) -1
	(getf se_obj +se_us) 65535
	(getf se_obj +se_i) -1
	(getf se_obj +se_ui) 4294967295
	(getf se_obj +se_l) -1)

; --- a value too big for the field keeps only its low bits ---
(setf-> se_obj (+se_b 256) (+se_ub 256) (+se_s 32768) (+se_us 65536) (+se_i 2147483648))
(test-cases
	(getf se_obj +se_b) 0
	(getf se_obj +se_ub) 0
	(getf se_obj +se_s) -32768
	(getf se_obj +se_us) 0
	(getf se_obj +se_i) -2147483648
	;the fields beside them are untouched
	(getf se_obj +se_ui) 4294967295
	(getf se_obj +se_l) -1)

; --- offsets, a field is aligned to its own size ---
(structure +se_base 4 (int a))
(structure +se_pad 0 (byte a) (long b))
(structure +se_sub 0 (struct s 16) (byte t))
(structure +se_none 0)
(test-cases
	;a structure can start at an offset
	(list +se_base_a +se_base_size) '(4 8)
	(list +se_pad_a +se_pad_b +se_pad_size) '(0 8 16)
	(list +se_sub_s +se_sub_t +se_sub_size) '(0 16 17)
	+se_none_size 0)

; --- enums and bits ---
(enums +se_en 0 (enum a b c))
(enums +se_eo 5 (enum a b))
(bits +se_bt 0 (bit a b c))
(test-cases
	(list +se_en_a +se_en_b +se_en_c) '(0 1 2)
	;an enum can start at any value, and has a size
	(list +se_eo_a +se_eo_b +se_eo_size) '(5 6 7)
	(list +se_bt_a +se_bt_b +se_bt_c) '(1 2 4)
	(bits? 5 1) :t
	(bits? 5 2) :nil
	;any one of several masks will do
	(bits? 5 1 2) :t
	(bits? 0 0) :nil
	(bit-mask 1 4) 5)

; --- align ---
(test-cases
	(align 0 8) 0		(align 1 8) 8
	(align 8 8) 8		(align 9 8) 16
	(align 5 1) 5)
