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
;	num			signed
;	label		its number: where it is
;	label_pc	its number: where it is less where this instruction is
;	label_diff	two numbers: where the first is less where the second is
;	string		its length and its bytes
;	pc_label	its number: where this instruction is less where it is
;	name		a keyword that is not a register, the class and method a
;				call-i names, its length and its bytes
;
; Labels are numbered from 0 in the order they are first seen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././asm/regs.inc")
(import "././asm/ops.inc")
(import "./vpb_ops.inc")

;module
(env-push)

(enums +vpb 0
	(enum reg num label label_pc label_diff string name pc_label))

(defq +vpb_ops (reduce (# (. %0 :insert %1 (!))) +vpb_all_ops (Fmap 101)))

(defun vpb-uint (out n)
	;n as 64 bits unsigned
	(while (/= 0 (>> n 7))
		(write-char out (logior 0x80 (logand n 0x7f)))
		(setq n (>> n 7)))
	(write-char out n))

(defun vpb-int (out n) (vpb-uint out (logxor (<< n 1) (>>> n 63))))

(defun vpb-encode (entry)
	; (vpb-encode entry) -> str
	;the VP binary of a bundle's entry, (name (progn inst ...) ...)
	(defq body (string-stream (cat "")) labels (Fmap 101) nlabels 0 insts 0
		label (lambda (l)
			(unless (defq i (. labels :find l))
				(. labels :insert l (setq i nlabels))
				(setq nlabels (inc nlabels)))
			i))
	(each! (lambda (inst)
			(unless (defq op (. +vpb_ops :find (first inst)))
				(throw "VP binary: no such op !" inst))
			(write-char body op)
			(vpb-uint body (dec (length inst)))
			(each! (lambda (o)
					(cond
						((defq r (vp-rreg? o))
							(write-char body (list +vpb_reg r)))
						((defq r (vp-freg? o))
							(write-char body (list +vpb_reg (+ 16 r))))
						((num? o)
							(write-char body +vpb_num)
							(vpb-int body o))
						((and (sym? o) (starts-with ":" o))
							(write-char body +vpb_name)
							(vpb-uint body (length o))
							(write-blk body o))
						((sym? o)
							(write-char body +vpb_label)
							(vpb-uint body (label o)))
						((str? o)
							(write-char body +vpb_string)
							(vpb-uint body (length o))
							(write-blk body o))
						((and (list? o) (eql (first o) 'quote) (sym? (second o)))
							(write-char body +vpb_label)
							(vpb-uint body (label (second o))))
						((and (list? o) (= (length o) 3) (eql (first o) '-) (eql (second o) '*pc*) (sym? (third o)))
							(write-char body +vpb_pc_label)
							(vpb-uint body (label (third o))))
						((and (list? o) (= (length o) 3) (eql (first o) '-) (sym? (second o)))
							(cond
								((eql (third o) '*pc*)
									(write-char body +vpb_label_pc)
									(vpb-uint body (label (second o))))
								((sym? (third o))
									(write-char body +vpb_label_diff)
									(vpb-uint body (label (second o)))
									(vpb-uint body (label (third o))))
								((throw "VP binary: no such operand !" o))))
						;a constant still to be worked out, as the compile can give
						;it, (+ 24 1), the number it comes to
						((num? (defq v (eval o)))
							(write-char body +vpb_num)
							(vpb-int body v))
						((throw "VP binary: no such operand !" o))))
				(list inst) 1)
			(setq insts (inc insts)))
		(list (second entry)) 1)
	(defq out (string-stream (cat "")) name (first entry))
	(vpb-uint out (length name))
	(write-blk out name)
	(vpb-uint out nlabels)
	(vpb-uint out insts)
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

(defun vpb-unbundle (data)
	; (vpb-unbundle str) -> (bin ...)
	(defq at 0 bins (list))
	(while (< at (length data))
		(defq n 0 s 0 b 0x80)
		(while (/= 0 (logand b 0x80))
			(setq b (code data 1 at) at (inc at) n (logior n (<< (logand b 0x7f) s)) s (+ s 7)))
		(push bins (slice data at (setq at (+ at n)))))
	bins)

(defun vpb-name (bin)
	; (vpb-name bin) -> str
	;the name of the function of a VP binary
	(defq at 0 n 0 s 0 b 0x80)
	(while (/= 0 (logand b 0x80))
		(setq b (code bin 1 at) at (inc at) n (logior n (<< (logand b 0x7f) s)) s (+ s 7)))
	(slice bin at (+ at n)))

;module
(export-symbols '(vpb-encode vpb-decode vpb-bundle vpb-unbundle vpb-name))
(env-pop)
