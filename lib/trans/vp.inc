;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;the translator in VP of the target, lib/trans/<cpu>.vp, VP text in,
;lib/trans/vp_parse.vp, native out. A platform has it made by its build, and
;the VP64 boot image has those to each native CPU, so an install
;translates with them. It translates any VP: one it gives back nothing
;for is a fault in it
(import "././boot/image.inc")
;the translator, of its file or of the boot image. A constant, put in where
;it is used: a variable of :nil is not prebound, and a task that calls
;(emit-translate) from its own environment would not find it
(defq +vpb_native (progn
	(defq vpb_name (trans-name *cpu* *abi*))
	(if (or (> (age (cat (load-path) vpb_name)) 0) (image-has? vpb_name)) (ffi vpb_name))))

(defun vp-portable (form)
	; (vp-portable form) -> form
	;an arg of an instruction as it can be written and read back. A constant
	;the Lisp that made it left as a call of a function, (+ 24 1) with +
	;the function and not the symbol, would be written #add: it is worked
	;out. One with anything but numbers in it can not be, and is thrown
	;each list of it copied, with the list it is in and where, outer ones
	;first, then each worked out after all those in it, from the last
	(if (list? form)
		(progn
			(defq lists (list (list (defq top (cat form)) :nil 0)) i -1)
			(while (< (++ i) (length lists))
				(defq l (first (elem-get lists i)))
				(each (lambda (e)
						(when (list? e)
							(elem-set l (!) (defq c (cat e)))
							(push lists (list c l (!)))))
					l))
			(reach (lambda ((l up at))
					(when (and (nempty? l) (func? (first l)))
						(unless (some! (const num?) (list l) :t 1)
							(throw "VP not portable, a call of a function in an instruction !" l))
						(setq l (eval l)))
					(if up (elem-set up at l) (setq top l)))
				lists)
			top)
		form))

(defun vp-code-text (entry)
	; (vp-code-text entry) -> str
	;an entry's instructions and the forms after them, as text that reads
	;back the same: a constant the Lisp left as a call is worked out
	(cat "(" (join (map (# (str (if (and (list? %0) (some (const list?) %0))
			(cat (list (first %0)) (map! (const vp-portable) (list %0) 1 -1)) %0)))
			(second entry)) "\n") ")"
		(apply (const cat) (cat (list "") (map! (# (cat "\n" (str %0))) (list entry) 2 -1)))))

(defun vp-entry-text (entry)
	; (vp-entry-text entry) -> str
	;an entry as a bundle in obj/vp/ has it
	(cat "(\q" (first entry) "\q\n"
		(vp-code-text entry) ")\n"))

(defun vp-translate-text (text)
	; (vp-translate-text text) -> func_binary
	;a function's VP text, its entry of a bundle, to this platform's native
	;code
	(unless +vpb_native (throw "No translator to this CPU here, make install makes them !" *cpu*))
	(ifn (+vpb_native text)
		(throw (cat "The " *cpu* " translator failed on this function !")
			(first (first (read (string-stream text)))))))

(defun emit-translate (emit_code)
	; (emit-translate emit_code) -> func_binary
	;instructions with no name, as they print: what is given has only
	;numbers, names, strings and labels, as text reads back
	(vp-translate-text (str (list "" emit_code))))

(defun vp-translate-out (name out)
	;a function's native code saved in obj/<cpu>/<abi>/ by its name. Its VP
	;is once for every CPU, in the bundle of its file in obj/vp/, where
	;the loader finds it if the native is not there.
	;The header has 16 bits for the length, and for where the links and
	;paths are
	(if (> (length out) 0xffff) (throw "Function too big, over 64KB !" name))
	(save out (cat "obj/" *cpu* "/" *abi* "/" name))
	(if (> *build_verb* 0)
		(print "-> " (cat "obj/" *cpu* "/" *abi* "/" name) " (" (length out) ")")))

(defun vp-translate-save (entry &optional text)
	; (vp-translate-save entry [text])
	;a build's: a function's VP, as text, (vp-entry-text) if not given, to
	;this platform's native code, saved in obj/<cpu>/<abi>/ by its name
	(vp-translate-out (first entry) (vp-translate-text (ifn text (vp-entry-text entry)))))

(defun vp-translate-file (file dir)
	; (vp-translate-file file dir)
	;a build's translate stage: the bundle of the source file, as the VP
	;stage left it in dir, obj/vp/, each of its functions to this
	;platform's native code. Each entry as read prints as VP text
	(defq stream (file-stream (cat dir file)))
	(while (defq entry (first (read stream)))
		(vp-translate-save entry (str entry))))

;module
(export-symbols '(emit-translate vp-translate-save vp-translate-text vp-translate-file trans-name
	vp-portable vp-code-text vp-entry-text))
(env-pop)
