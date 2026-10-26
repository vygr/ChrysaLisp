;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VP binary: a function's VP as bytes, to be translated quickly
;
; A function is its name, how many labels it has, how many instructions,
; and the instructions. An instruction is its op, its place in
; +vp_emit_ops, how many operands it has, and each operand: a tag and
; what follows it. A number is unsigned, 7 bits a byte, low first, the
; top bit of a byte saying another follows; a signed one is that of its
; zigzag, 0 -1 1 -2 2 ... as 0 1 2 3 4 ...
;
;	reg			a byte, :r0 to :rsp 0 to 15, :f0 to :f15 16 to 31
;	num			signed, a num of 64 bits
;	label		its number: where it is
;	label_pc	its number: where it is less where this instruction is
;	label_diff	two numbers: where the first is less where the second is
;	string		its length and its bytes
;	pc_label	its number: where this instruction is less where it is
;	name		a keyword that is not a register, the class and method a
;				call-i names, its length and its bytes
;
; Labels are numbered from 0 in the order they are first seen. There is
; no other operand: not a fixed, a real, nor a form still to be worked
; out, docs/vm/vp_vm.md, A bundle of VP.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././asm/regs.inc")
(import "././asm/ops.inc")
(import "./vpb_ops.inc")
(import "././boot/image.inc")

;module
(env-push)

(enums +vpb 0
	(enum reg num label label_pc label_diff string name pc_label))

;the encoder, lib/trans/vpb_encode.vp, of its file or of the boot image,
;the VP64 one carries it so an install encodes with it, and its maps:
;each op's number and each register's, 0 to 31
(defq +vpb_encoder (if (or (> (age (cat (load-path) "lib/trans/vpb_encode")) 0) (image-has? "lib/trans/vpb_encode"))
		(ffi "lib/trans/vpb_encode"))
	+vpb_ops_env (reduce (# (def %0 %1 (!)) %0) +vpb_all_ops (env 101))
	+vpb_regs_env (reduce (# (def %0 %1 (!)) %0) (cat +vp_rregs +vp_fregs) (env 31)))

(defun vpb-uint (out n)
	;n as 64 bits unsigned
	(while (/= 0 (>> n 7))
		(write-char out (logior 0x80 (logand n 0x7f)))
		(setq n (>> n 7)))
	(write-char out n))

(defun vpb-encode (entry)
	; (vpb-encode entry) -> str
	;the VP binary of a bundle's entry, (name (progn inst ...) ...), its
	;instructions by the encoder. An op or operand it does not know, a form
	;that is not a num nor a label's, is an error of the source that made it
	(unless +vpb_encoder (throw "No VP binary encoder here, make install makes it !" (first entry)))
	(defq body (string-stream (cat "")) name (first entry))
	(unless (defq counts (+vpb_encoder entry body +vpb_ops_env +vpb_regs_env (env 31) 'quote '- '*pc*))
		(throw "VP binary: an op or operand the encoder does not know !" name))
	(defq out (string-stream (cat "")))
	(vpb-uint out (length name))
	(write-blk out name)
	(vpb-uint out (>> counts 32))
	(vpb-uint out (logand counts 0xffffffff))
	(write-blk out (str body))
	(str out))

(defun vpb-decode (data)
	; (vpb-decode str) -> (name (progn inst ...))
	;the entry the VP binary was made of, its labels named again
	(defq at 0
		byte (lambda () (code data 1 (- (setq at (inc at)) 1)))
		uint (lambda ()
			(defq n 0 s 0 b 0x80)
			(while (/= 0 (logand b 0x80))
				(setq b (byte) n (logior n (<< (logand b 0x7f) s)) s (+ s 7)))
			n)
		int (lambda () (defq z (uint)) (logxor (>> z 1) (neg (logand z 1)))))
	(defq n (uint) name (slice data at (setq at (+ at n)))
		nlabels (uint) ninsts (uint)
		labels (map (# (sym (cat "_" (str %0)))) (range 0 nlabels))
		code (list 'progn))
	(times ninsts
		(defq inst (list (elem-get +vpb_all_ops (byte))))
		(times (uint)
			(push inst (case (byte)
				(+vpb_reg (defq r (byte)) (if (< r 16) (elem-get +vp_rregs r) (elem-get +vp_fregs (- r 16))))
				(+vpb_num (int))
				(+vpb_label
					(defq l (elem-get labels (uint)))
					(if (eql (first inst) 'emit-label) (list 'quote l) l))
				(+vpb_label_pc (list '- (elem-get labels (uint)) '*pc*))
				(+vpb_label_diff (list '- (elem-get labels (uint)) (elem-get labels (uint))))
				(+vpb_string (defq n (uint)) (slice data at (setq at (+ at n))))
				(+vpb_name (defq n (uint)) (sym (slice data at (setq at (+ at n)))))
				(+vpb_pc_label (list '- '*pc* (elem-get labels (uint))))
				(:t (throw "VP binary: no such tag !" at)))))
		(push code inst))
	(list name code))

(defun vpb-bundle (bins)
	; (vpb-bundle bins) -> str
	;the VP binary of the functions of a file, each after its length
	(defq out (string-stream (cat "")))
	(each (lambda (bin) (vpb-uint out (length bin)) (write-blk out bin)) bins)
	(str out))

(defun vpb-part (data at)
	; (vpb-part str at) -> :nil | (start end)
	;a part of the str at at, its length first, 7 bits a byte, low first:
	;where it starts and ends, :nil if the str ends before it does, as one
	;from another node may
	(defq n 0 s 0 b 0x80 e (length data))
	(while (and (/= 0 (logand b 0x80)) (< at e) (< s 63))
		(setq b (code data 1 at) at (inc at) n (logior n (<< (logand b 0x7f) s)) s (+ s 7)))
	(and (= 0 (logand b 0x80)) (>= n 0) (<= (+ at n) e) (list at (+ at n))))

(defun vpb-unbundle (data)
	; (vpb-unbundle str) -> (bin ...)
	;the binaries of a bundle, as many as are whole
	(defq at 0 bins (list))
	(while (and (< at (length data)) (defq part (vpb-part data at)))
		(push bins (apply (const slice) (cat (list data) part)))
		(setq at (second part)))
	bins)

(defun vpb-name (bin)
	; (vpb-name bin) -> str
	;the name of the function of a VP binary, "" if it is not whole
	(if (defq part (vpb-part bin 0)) (apply (const slice) (cat (list bin) part)) ""))

;module
(export-symbols '(vpb-encode vpb-decode vpb-bundle vpb-unbundle vpb-name))
(env-pop)
