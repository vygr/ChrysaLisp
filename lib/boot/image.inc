;;;;;;;;;;;;;;;;;;
; boot image tools
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(import "././asm/lisp.inc")

(enums +blob 0
	(enum body links refs))

(defun func-obj-path (_)
	(sym (cat "obj/" *cpu* "/" *abi* "/" _)))

(defun func-refs (fobj)
	; (func-refs fobj) -> (list [sym] ...)
	(defq paths (list)
		lo (get-short fobj +fn_header_links)
		po (get-short fobj +fn_header_paths))
	(while (/= lo po)
		(push paths (sym (get-cstr fobj (+ (get-long fobj lo) lo))))
		(setq lo (+ lo +ptr_size)))
	paths)

(defun func-load (name)
	; (func-load name) -> (list body links refs)
	;cache loading of function blobs etc
	(or (get name *fparts*)
		(progn
			(unless (defq fobj (load (func-obj-path name)))
				(throw "No such file !" (func-obj-path name)))
			(defq links_offset (get-short fobj +fn_header_links)
				paths_offset (get-short fobj +fn_header_paths)
				body (cat (char -1 +ptr_size) (char paths_offset +short_size)
					(slice +fn_header_entry links_offset fobj))
				links (slice links_offset paths_offset fobj))
			(def *fparts* name (list body links (func-refs fobj))))))

(defun boot-image (&optional *funcs* *abi* *cpu*)
	(setd *funcs* (list) *abi* (abi) *cpu* (cpu))
	(defq *fparts* (env 31) z (cat (char 0 +ptr_size) (char 0 +int_size)))
	(unless (list? *funcs*) (setq *funcs* (list *funcs*)))
	(defq boot_funcs (list
		;must be first function !
		'sys/load/init
		;must be second function !
		'sys/statics/statics
		;must be included !
		'sys/kernel/kernel))
	;load up all extra functions requested
	(merge-obj boot_funcs (map sym *funcs*))
	;load all dependents
	(each-mergeable
		(# (merge-obj boot_funcs (elem-get +blob_refs (func-load %0))))
		boot_funcs)
	;sort into order, leaving the init/statics first !
	(sort cmp boot_funcs 2)
	;list of function blobs and links in order
	;list of offsets of header and link sections
	;and offset of new strings section
	(defq blobs (map (# (get %0 *fparts*)) boot_funcs)
		boot_paths (list) boot_paths_offsets (list)
		header_offsets (list) links_offsets (list)
		boot_paths_offset (+ (length z)
			(reduce (lambda (offset (body links refs))
				(push header_offsets offset)
				(push links_offsets (setq offset (+ offset (length body))))
				(+ offset (length links))) blobs 0)))
	;list of paths that will appear in boot image paths section
	(each (lambda (func)
		(each (lambda (ref)
			(unless (find ref boot_funcs) (merge-obj boot_paths (list ref))))
			(elem-get +blob_refs (get func *fparts*)))) boot_funcs)
	;list of boot paths offsets
	(reduce (# (push boot_paths_offsets %0) (+ %0 (length %1) 1)) boot_paths 0)
	;create new link sections with offsets to header path or boot paths
	(each (# (defq u (elem-get _ links_offsets))
		(elem-set +blob_links %0 (apply cat (push (map (#
			(char (-
				(if (defq i (find %0 boot_funcs))
					(+ (elem-get i header_offsets) +fn_header_pathname)
					(+ (elem-get (find %0 boot_paths) boot_paths_offsets) boot_paths_offset))
				(+ u (* _ +ptr_size))) +ptr_size)) (elem-get +blob_refs %0)) "")))) blobs)
	;build list of all sections of boot image
	;concatenate all sections and write out
	(defq stream (file-stream (func-obj-path 'sys/boot_image) +file_open_write)
		blobs (reduce (# (push %0 (cat %1 (ascii-char 0)))) boot_paths
			(push (reduce (# (push %0 (elem-get +blob_body %1) (elem-get +blob_links %1))) blobs (list)) z))
		boot_len (reduce (# (+ %0 (length %1))) blobs 0))
	;save size in boot image
	(elem-set 0 blobs (cat (char boot_len +long_size) (slice +long_size -1 (elem-get 0 blobs))))
	(each (# (write stream %0)) blobs)
	(print "-> " (func-obj-path 'sys/boot_image) " (" boot_len ")") :nil)

;module
(export-symbols
	boot-image func-obj-path)
(env-pop)
