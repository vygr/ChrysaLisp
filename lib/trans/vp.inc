;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;the translator in VP of the target, lib/trans/<cpu>.vp, VP binary in,
;lib/trans/vpb.inc, native out. A platform has it made by its build, and
;the VP64 boot image has those to each native CPU, so an install
;translates with them. It translates any VP: one it gives back nothing
;for is a fault in it
(import "./vpb.inc")
(import "././boot/image.inc")
;the translator, of its file or of the boot image. A constant, put in where
;it is used: a variable of :nil is not prebound, and a task that calls
;(emit-translate) from its own environment would not find it
(defq +vpb_native (progn
	(defq vpb_name (trans-name *cpu* *abi*))
	(if (or (> (age (cat (load-path) vpb_name)) 0) (image-has? vpb_name)) (ffi vpb_name))))

(defun vp-translate-bin (bin)
	; (vp-translate-bin bin) -> func_binary
	;a function's VP binary to this platform's native code
	(unless +vpb_native (throw "No translator to this CPU here, make install makes them !" *cpu*))
	(ifn (+vpb_native bin)
		(throw (cat "The " *cpu* " translator failed on this function !") (vpb-name bin))))

(defun emit-translate (emit_code)
	; (emit-translate emit_code) -> func_binary
	(vp-translate-bin (vpb-encode (list "" emit_code))))

(defun vp-translate-out (name out)
	;a function's native code saved in obj/<cpu>/<abi>/ by its name. Its VP
	;is once for every CPU, in the binary bundle of its file in
	;obj/vp/<mode>/, where the loader finds it if the native is not there.
	;The header has 16 bits for the length, and for where the links and
	;paths are
	(if (> (length out) 0xffff) (throw "Function too big, over 64KB !" name))
	(save out (cat "obj/" *cpu* "/" *abi* "/" name))
	(if (> *build_verb* 0)
		(print "-> " (cat "obj/" *cpu* "/" *abi* "/" name) " (" (length out) ")")))

(defun vp-translate-save (entry)
	; (vp-translate-save entry) -> bin
	;a build's: a function's VP to this platform's native code, saved in
	;obj/<cpu>/<abi>/ by its name. An entry is the name, the instructions,
	;and the forms after them. Its VP binary is given back, for the binary
	;bundle beside the text
	(defq bin (vpb-encode entry))
	(vp-translate-out (first entry) (vp-translate-bin bin))
	bin)

(defun vp-translate-file (file dir)
	; (vp-translate-file file dir)
	;a build's translate stage: the bundle of the source file, as the VP
	;stage left it in dir, obj/vp/<mode>/, each of its functions to this
	;platform's native code. From the binary bundle beside it if it is as
	;new, else from the text
	(defq bins (cat dir file "b"))
	(cond
		((and (> (age bins) 0) (>= (age bins) (age (cat dir file))))
			(each (# (vp-translate-out (vpb-name %0) (vp-translate-bin %0))) (vpb-unbundle (load bins))))
		(:t (defq stream (file-stream (cat dir file)))
			(while (defq entry (first (read stream)))
				(vp-translate-save entry)))))

;module
(export-symbols '(emit-translate vp-translate-save vp-translate-bin vp-translate-file trans-name
	vpb-encode vpb-encode-lisp vpb-decode vpb-bundle vpb-unbundle vpb-name))
(env-pop)
