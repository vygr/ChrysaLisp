;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/boot/id.inc - what a boot image was built from, as one number
;
; A boot image is not the same bytes on one CPU as on another, so two
; machines can not tell if they run the same system by their images. What
; the images were built from is the same, the VP source and what it
; includes. Its hash is written beside the image when the image is made,
; obj/<cpu>/<abi>/sys/boot_id, and is the same on every CPU for the same
; source. A node says it to the others, the Net service's beacon has it,
; and one that differs is a machine that needs to be updated, or built.
;
; It is of the VP source alone. The Lisp a node loads as it runs is not in
; it, that is the tree, and a matter for sync.
;
; The files it is of are written beside it too, boot_files, so the id the
; tree would give now can be worked out with no build, and a boot image
; made from other source than the tree has is known, (boot-id-tree).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././crypto/sha256.inc")

(defun boot-id-of (files)
	; (boot-id-of files) -> hex
	;the id of those source files, whatever order they are given in. The
	;name of each and the hash of what is in it, all hashed
	(defq ctx (sha256-start))
	(each (lambda (file)
		(sha256-add ctx (cat file "\n"
			(sha256 (ifn (load file) "")) "\n")))
		(sort (cat files)))
	(to-lower (hex-encode (sha256-end ctx))))

(defun boot-id-file (cpu abi)
	; (boot-id-file cpu abi) -> path
	(cat "obj/" cpu "/" abi "/sys/boot_id"))

(defun boot-files-file (cpu abi)
	; (boot-files-file cpu abi) -> path
	(cat "obj/" cpu "/" abi "/sys/boot_files"))

(defun boot-id-tree ()
	; (boot-id-tree) -> :nil | hex
	;the id of the files this node's boot image was made from, as the tree
	;has them now, :nil if it was made before their names were kept. Not
	;the boot image's own id, it was made from other source than is here
	(when (defq text (load (cat (load-path) "sys/boot_files")))
		(boot-id-of (filter (const nempty?) (split text "\n")))))

(defun boot-id ()
	; (boot-id) -> hex | ""
	;the id of the boot image this node's machine has, "" if it was made
	;before there was one
	(trim (ifn (load (cat (load-path) "sys/boot_id")) "") (const (char-class " \t\r\n"))))
