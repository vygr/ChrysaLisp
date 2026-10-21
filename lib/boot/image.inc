;;;;;;;;;;;;;;;;;;
; boot image tools
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(import "././asm/lisp.inc")

(enums +blob 0
	(enum body links refs))

;what a boot image holds of all the functions there are. One that is not in
;it is bound from its file under obj/ the first time it is asked for, so the
;image need only hold what an install binds before it has built anything:
;the kernel and Lisp, in these folders, and these few besides, the font,
;pixmap and path classes Lisp's boot asks for, the crypto a machine's key is
;made with and the Net service's sockets. What they call comes in with them
(defq +boot_folders ''("sys/" "class/")
	+boot_extras ''(gui/font/flush gui/path/create gui/path/type gui/path/vtable
		gui/pixmap/as_argb gui/pixmap/deinit gui/pixmap/from_argb32 gui/pixmap/lisp_as_argb
		gui/pixmap/lisp_from_argb32 gui/pixmap/lisp_read gui/pixmap/lisp_shared
		gui/pixmap/lisp_to_argb32 gui/pixmap/lisp_write gui/pixmap/to_argb gui/pixmap/to_argb32
		gui/pixmap/vtable gui/view/get_prop gui/view/ref_prop
		lib/crypto/gf25519_mul lib/crypto/random_bytes lib/crypto/sha256_blocks lib/crypto/sha512_blocks
		service/net/lisp_accept service/net/lisp_close service/net/lisp_connect service/net/lisp_listen
		service/net/lisp_poll service/net/lisp_recv service/net/lisp_send))

(defun boot-funcs (funcs &optional all)
	; (boot-funcs funcs [all]) -> funcs
	;those of these functions a boot image holds, every one of them with all
	(if all funcs
		(filter (lambda (fn_path)
				(or (some (# (starts-with %0 fn_path)) +boot_folders) (find (sym fn_path) +boot_extras)))
			funcs)))

(defun func-obj-path (%0)
	(sym (cat "obj/" *cpu* "/" *abi* "/" %0)))

(defun func-refs (fobj)
	; (func-refs fobj) -> ([sym] ...)
	(defq paths (list)
		lo (get-short fobj +fn_header_links)
		po (get-short fobj +fn_header_paths))
	(while (/= lo po)
		(push paths (sym (get-cstr fobj (+ (get-long fobj lo) lo))))
		(setq lo (+ lo +ptr_size)))
	paths)

(defun func-load (name)
	; (func-load name) -> (body links refs)
	;cache loading of function blobs etc
	(or (get name *blob_map*)
		(progn
			(unless (defq fobj (load (func-obj-path name)))
				(throw "No such file !" (func-obj-path name)))
			(defq links_offset (get-short fobj +fn_header_links)
				paths_offset (get-short fobj +fn_header_paths)
				body (cat (char -1 +ptr_size) (char paths_offset +short_size)
					(slice fobj +fn_header_entry links_offset))
				links (slice fobj links_offset paths_offset))
			(def *blob_map* name (list body links (func-refs fobj))))))

(defun boot-image (&optional *funcs* *abi* *cpu*)
	(setd *funcs* (list) *abi* (abi) *cpu* (cpu))
	(defq *blob_map* (env 31) zero (cat (char 0 +ptr_size) (char 0 +int_size)))
	(unless (list?? *funcs*) (setq *funcs* (list *funcs*)))
	;all functions required and requested
	(defq boot_funcs (merge (list
		;must be first function !
		'sys/load/init
		;must be second function !
		'sys/statics/statics
		;must be included !
		'sys/kernel/kernel)
		(map (const sym) *funcs*)))
	;load refs recursively
	(each-mergeable
		(# (merge boot_funcs (elem-get (func-load %0) +blob_refs)))
		boot_funcs)
	;sort into order, leaving the init/statics first !
	(sort boot_funcs (const cmp) 2)
	;list of function blobs and links in order
	;list of offsets of header and link sections
	(defq blobs (map (# (get %0 *blob_map*)) boot_funcs)
		header_offsets (list) links_offsets (list))
	(reduce (lambda (offset (body links refs))
		(push header_offsets offset)
		(push links_offsets (setq offset (+ offset (length body))))
		(+ offset (length links))) blobs 0)
	;create new link sections with offsets to header path
	(each (lambda (blob u)
		(elem-set blob +blob_links (apply (const cat) (push (map
			(# (char (-
				(+ (elem-get header_offsets (find %0 boot_funcs)) +fn_header_pathname)
				(+ u (* (!) +ptr_size))) +ptr_size)) (elem-get blob +blob_refs)) "")))) blobs links_offsets)
	;build list of all sections of boot image
	(defq stream (file-stream (func-obj-path 'sys/boot_image) +file_open_write)
		blobs (push (reduce (# (push %0
			(elem-get %1 +blob_body)
			(elem-get %1 +blob_links))) blobs (list)) zero)
		boot_len (reduce (# (+ %0 (length %1))) blobs 0))
	;save size in boot image
	(elem-set blobs 0 (cat (char boot_len +long_size) (slice (first blobs) +long_size -1)))
	;write out boot image
	(each (# (write-blk stream %0)) blobs)
	(print "-> " (func-obj-path 'sys/boot_image) " (" boot_len ")")
	:nil)

;module
(export-symbols '(boot-image boot-funcs func-obj-path))
(env-pop)
