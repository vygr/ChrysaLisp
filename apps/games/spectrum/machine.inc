;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the machine: memory, keys, tapes, snapshots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

(ffi (cat *app_root* "zx_run") zx-run)
; (zx-run machine) -> +zx_stop_xxx
(ffi (cat *app_root* "zx_render") zx-render)
; (zx-render canvas machine flash zoom) -> :nil
(ffi (cat *app_root* "zx_poke") zx-poke)
; (zx-poke machine addr data) -> :nil

(defun zx-zeros (n)
	; (zx-zeros n) -> str
	;n bytes of 0, n a power of 2
	(defq z (char 0))
	(while (< (length z) n) (setq z (cat z z)))
	z)

(defun zx-peek (m addr)
	; (zx-peek machine addr) -> byte
	(code (elem-get m (+ +zx_size (logand addr 0xffff)))))

(defun zx-peek16 (m addr)
	(logior (zx-peek m addr) (<< (zx-peek m (inc addr)) 8)))

(defun zx-key-row (m row bits)
	; (zx-key-row machine row bits)
	;a half row of the matrix, a bit 0 a key held
	(obj-set m (+ +str_data +zx_keys row) +type_ubyte bits))

(defun zx-reset (m)
	; (zx-reset machine) -> machine
	;the RAM cleared and the Z80 as at power on, the ROM as it is
	(zx-poke m 0x4000 (slice (zx-zeros 0x10000) 0 0xc000))
	(setf-> m (+zx_ts 0) (+zx_af 0xffff) (+zx_sp 0xffff) (+zx_pc 0)
		(+zx_bc 0) (+zx_de 0) (+zx_hl 0) (+zx_ix 0) (+zx_iy 0)
		(+zx_af2 0xffff) (+zx_bc2 0) (+zx_de2 0) (+zx_hl2 0)
		(+zx_i 0) (+zx_r 0) (+zx_r7 0) (+zx_im 0) (+zx_iff1 0) (+zx_iff2 0)
		(+zx_halted 0) (+zx_border 7) (+zx_kemp 0) (+zx_ear 0) (+zx_beep 0) (+zx_trap 0))
	(each (# (zx-key-row m %0 0xff)) (range 0 8))
	m)

(defun zx-machine (rom)
	; (zx-machine rom) -> machine
	(defq m (slice (zx-zeros 0x20000) 0 +zx_machine_size))
	(zx-poke m 0 rom)
	(zx-reset m))

;;;;;;;;;;
; the keys
;;;;;;;;;;

;the matrix, a half row a list, bit 0 first, and the host's keys on it
(defq +zx_matrix ''(
	(+sc_lshift +sc_z +sc_x +sc_c +sc_v)
	(+sc_a +sc_s +sc_d +sc_f +sc_g)
	(+sc_q +sc_w +sc_e +sc_r +sc_t)
	(+sc_1 +sc_2 +sc_3 +sc_4 +sc_5)
	(+sc_0 +sc_9 +sc_8 +sc_7 +sc_6)
	(+sc_p +sc_o +sc_i +sc_u +sc_y)
	(+sc_return +sc_l +sc_k +sc_j +sc_h)
	(+sc_space +sc_lctrl +sc_m +sc_n +sc_b)))

(defun zx-key-map ()
	; (zx-key-map) -> fmap
	;scan code -> ((row bit) ...), the keys of the Spectrum it holds
	(defq out (Fmap 31))
	(each (lambda (keys)
			(defq row (!))
			(each (lambda (key)
					(. out :insert (eval key) (list (list row (<< 1 (!))))))
				keys))
		+zx_matrix)
	;the other shift, Alt as SYMBOL SHIFT too, and DELETE as CAPS SHIFT 0
	(. out :insert +sc_rshift (. out :find +sc_lshift))
	(each (# (. out :insert %0 (. out :find +sc_lctrl))) (list +sc_rctrl +sc_lalt +sc_ralt))
	(. out :insert +sc_backspace (list (list 0 1) (list 4 1)))
	out)

;the joystick, Kempston, its bits
(defq +zx_joystick ''((+sc_right 1) (+sc_left 2) (+sc_down 4) (+sc_up 8) (+sc_tab 16)))

(defun zx-joy-map ()
	(defq out (Fmap 7))
	(each (lambda ((key bit)) (. out :insert (eval key) bit)) +zx_joystick)
	out)

(defun zx-keys (m held key_map joy_map)
	; (zx-keys machine held key_map joy_map)
	;the matrix and joystick of the scan codes held
	(defq rows (map (lambda (_) 0xff) (range 0 8)) joy 0)
	(each (lambda (scode)
			(each (lambda ((row bit)) (elem-set rows row (logand (elem-get rows row) (lognot bit))))
				(ifn (. key_map :find scode) '()))
			(setq joy (logior joy (ifn (. joy_map :find scode) 0))))
		held)
	(each (# (zx-key-row m (!) %0)) rows)
	(setf m +zx_kemp joy))

;;;;;;;
; tapes
;;;;;;;

(defun zx-byte (data i)
	(code (elem-get data i)))

(defun zx-word (data i)
	(logior (zx-byte data i) (<< (zx-byte data (inc i)) 8)))

(defun zx-tap-blocks (data)
	; (zx-tap-blocks data) -> (block ...)
	;each block its flag byte, its bytes and its check sum
	(defq out (list) i 0)
	(while (<= (+ i 2) (length data))
		(defq n (zx-word data i) e (min (length data) (+ i 2 n)))
		(push out (slice data (+ i 2) e))
		(setq i e))
	out)

(defun zx-tzx-blocks (data)
	; (zx-tzx-blocks data) -> (block ...)
	;the data blocks of a TZX, standard and turbo, the rest passed over
	(defq out (list) i 10 len (length data))
	(and (> len 10) (eql (slice data 0 7) "ZXTape!")
		(while (< i len)
			(defq id (zx-byte data i) n :nil)
			(case id
				(0x10 (setq n (zx-word data (+ i 3)))
					(push out (slice data (+ i 5) (min len (+ i 5 n))))
					(setq i (+ i 5 n)))
				(0x11 (setq n (logior (zx-word data (+ i 16)) (<< (zx-byte data (+ i 18)) 16)))
					(push out (slice data (+ i 19) (min len (+ i 19 n))))
					(setq i (+ i 19 n)))
				(0x12 (setq i (+ i 5)))
				(0x13 (setq i (+ i 2 (* 2 (zx-byte data (inc i))))))
				(0x14 (setq i (+ i 11 (logior (zx-word data (+ i 8)) (<< (zx-byte data (+ i 10)) 16)))))
				(0x15 (setq i (+ i 9 (logior (zx-word data (+ i 6)) (<< (zx-byte data (+ i 8)) 16)))))
				((0x18 0x19) (setq i (+ i 5 (logior (zx-word data (inc i)) (<< (zx-word data (+ i 3)) 16)))))
				(0x20 (setq i (+ i 3)))
				(0x21 (setq i (+ i 2 (zx-byte data (inc i)))))
				((0x22 0x25 0x27) (setq i (inc i)))
				((0x23 0x24) (setq i (+ i 3)))
				(0x26 (setq i (+ i 3 (* 2 (zx-word data (inc i))))))
				(0x28 (setq i (+ i 3 (zx-word data (inc i)))))
				(0x2a (setq i (+ i 5)))
				(0x2b (setq i (+ i 6)))
				(0x30 (setq i (+ i 2 (zx-byte data (inc i)))))
				(0x31 (setq i (+ i 3 (zx-byte data (+ i 2)))))
				(0x32 (setq i (+ i 3 (zx-word data (inc i)))))
				(0x33 (setq i (+ i 2 (* 3 (zx-byte data (inc i))))))
				(0x35 (setq i (+ i 21 (logior (zx-word data (+ i 17)) (<< (zx-word data (+ i 19)) 16)))))
				(0x5a (setq i (+ i 10)))
				(:t (setq i len)))))
	out)

(defun zx-tape-trap (m tape)
	; (zx-tape-trap machine tape)
	;the ROM's LD-BYTES done at once from the next block of the tape, a
	;list (blocks next): A the flag, IX where, DE how many, carry set to
	;load else verify. Carry set as it returns if all went well. It ends
	;by LD-RET, which enables interrupts. Entered past its EX AF,AF', the
	;flag and carry are in AF'
	(bind '(blocks next) tape)
	(defq ask (if (= (getf m +zx_pc) +zx_ld_bytes2) (getf m +zx_af2) (getf m +zx_af))
		af (getf m +zx_af) a (>> ask 8) f (logand af 0xfe)
		ix (getf m +zx_ix) de (getf m +zx_de) block (elem-get blocks next))
	(when block
		(elem-set tape 1 (inc next))
		(and (> (length block) 1) (= (zx-byte block 0) a)
			(cond
				((= 0 (logand ask 1)) (setq f (logior f 1)))
				(:t (defq n (min de (- (length block) 2)))
					(zx-poke m ix (slice block 1 (inc n)))
					(setq ix (logand (+ ix n) 0xffff) de (- de n))
					(if (= de 0) (setq f (logior f 1)))))))
	(defq sp (getf m +zx_sp))
	(setf-> m (+zx_af (logior (<< a 8) f)) (+zx_ix ix) (+zx_de de)
		(+zx_pc (zx-peek16 m sp)) (+zx_sp (logand (+ sp 2) 0xffff))
		(+zx_iff1 1) (+zx_iff2 1)))

;;;;;;;;;;;
; snapshots
;;;;;;;;;;;

(defun zx-sna (m data)
	; (zx-sna machine data) -> :t | :nil
	;a .sna of a 48K, PC on the stack
	(when (>= (length data) (+ 27 0xc000))
		(zx-reset m)
		(zx-poke m 0x4000 (slice data 27 (+ 27 0xc000)))
		(defq sp (zx-word data 23))
		(setf-> m (+zx_i (zx-byte data 0)) (+zx_hl2 (zx-word data 1)) (+zx_de2 (zx-word data 3))
			(+zx_bc2 (zx-word data 5)) (+zx_af2 (zx-word data 7)) (+zx_hl (zx-word data 9))
			(+zx_de (zx-word data 11)) (+zx_bc (zx-word data 13)) (+zx_iy (zx-word data 15))
			(+zx_ix (zx-word data 17)) (+zx_iff1 (logand (>> (zx-byte data 19) 2) 1))
			(+zx_iff2 (logand (>> (zx-byte data 19) 2) 1))
			(+zx_r (zx-byte data 20)) (+zx_r7 (logand (zx-byte data 20) 0x80))
			(+zx_af (zx-word data 21)) (+zx_im (logand (zx-byte data 25) 3))
			(+zx_border (logand (zx-byte data 26) 7))
			(+zx_pc (zx-peek16 m sp)) (+zx_sp (logand (+ sp 2) 0xffff)))
		:t))

(defun zx-unrle (data i e size)
	; (zx-unrle data start end size) -> str
	;a .z80's ED ED n b runs, size bytes of it at most
	(defq out (list) n 0)
	(while (and (< i e) (< n size))
		(cond
			((and (< (+ i 3) e) (= (zx-byte data i) 0xed) (= (zx-byte data (inc i)) 0xed))
				(defq run (zx-byte data (+ i 2)) b (char (zx-byte data (+ i 3))))
				(times run (push out b))
				(setq n (+ n run) i (+ i 4)))
			(:t (push out (elem-get data i))
				(setq n (inc n) i (inc i)))))
	(apply (const cat) out))

(defun zx-z80 (m data)
	; (zx-z80 machine data) -> :t | :nil
	;a .z80 of a 48K, version 1, 2 or 3
	(when (>= (length data) 30)
		(defq pc (zx-word data 6) flags1 (zx-byte data 12) ok :t)
		(if (= flags1 0xff) (setq flags1 1))
		(cond
			((/= pc 0)
				;version 1, the 48K after the header
				(zx-reset m)
				(zx-poke m 0x4000 (if (/= 0 (logand flags1 0x20))
					(zx-unrle data 30 (length data) 0xc000)
					(slice data 30 (min (length data) (+ 30 0xc000))))))
			(:t (defq extra (zx-word data 30) hw (zx-byte data 34) i (+ 32 extra))
				(setq pc (zx-word data 32))
				;48K only, 0 and 1 of version 2, 0 1 and 3 of version 3
				(cond
					((or (and (= extra 23) (> hw 1)) (and (/= extra 23) (> hw 3) (/= hw 3)) (= hw 2))
						(setq ok :nil))
					(:t (zx-reset m)
						(while (<= (+ i 3) (length data))
							(defq len (zx-word data i) page (zx-byte data (+ i 2)) i (+ i 3)
								at (case page (8 0x4000) (4 0x8000) (5 0xc000) (:t :nil))
								e (if (= len 0xffff) (+ i 0x4000) (+ i len)))
							(if at (zx-poke m at (if (= len 0xffff)
								(slice data i (min e (length data)))
								(zx-unrle data i (min e (length data)) 0x4000))))
							(setq i e))))))
		(when ok
			(setf-> m (+zx_af (logior (<< (zx-byte data 0) 8) (zx-byte data 1)))
				(+zx_bc (zx-word data 2)) (+zx_hl (zx-word data 4)) (+zx_pc pc)
				(+zx_sp (zx-word data 8)) (+zx_i (zx-byte data 10))
				(+zx_r (zx-byte data 11)) (+zx_r7 (<< (logand flags1 1) 7))
				(+zx_border (logand (>> flags1 1) 7))
				(+zx_de (zx-word data 13)) (+zx_bc2 (zx-word data 15)) (+zx_de2 (zx-word data 17))
				(+zx_hl2 (zx-word data 19)) (+zx_af2 (logior (<< (zx-byte data 21) 8) (zx-byte data 22)))
				(+zx_iy (zx-word data 23)) (+zx_ix (zx-word data 25))
				(+zx_iff1 (if (= 0 (zx-byte data 27)) 0 1)) (+zx_iff2 (if (= 0 (zx-byte data 28)) 0 1))
				(+zx_im (logand (zx-byte data 29) 3))))
		ok))
