;;;;;;;;;;;;
; cpm loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")

;module
(env-push)

(bits +cpm_flag 0
	(bit rle lz4))

(structure +cpm 0
	(uint ident)		;ident .CPM or .FLM
	(uint flags)		;flags
	(uint version)		;version number
	(int type)			;image type
	(uint width)		;image w
	(uint height)		;image h
	(uint reserved_1))	;reserved 1

(defun CPM-info (stream)
	; (CPM-info stream) -> (width height type) | (-1 -1 -1)
	(defq out '(-1 -1 -1))
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (and (= (length header) +cpm_size)
				(find (char (getf header +cpm_ident) +int_size) '("MPC." "MLF.")))
			(setq out (getf-> header +cpm_width +cpm_height +cpm_type))))
	out)

(defun cpm-load-stage (lib fn downstream_mbox src &rest args)
	(defq in_src (if (str? src)
		`(progn (mail-send (hex-decode ,(hex-encode src)) (in-mbox (defq in (in-stream)))) in)
		`(obj-ref ,(weak-ref src))))
	(str `(progn (import ,lib)
		(,fn ,in_src (out-stream (hex-decode ,(hex-encode downstream_mbox))) ~args))))

(defun cpm-save-stage (lib fn handshake_mbox dst &rest args)
	(defq out_dst (if (str? dst)
		`(out-stream (hex-decode ,(hex-encode dst)))
		`(obj-ref ,(weak-ref dst))))
	(str `(progn (import ,lib)
		(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox (defq in (in-stream))))
		(,fn in ,out_dst ~args)
		,(ifn (str? dst) `(mail-send (hex-decode ,(hex-encode handshake_mbox)) :t)))))

(defun cpm-load-stage-pixmap (pixmap type handshake_mbox)
	(str `(progn (import "gui/pixmap/lisp.inc")
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(in-mbox (defq pixmap (obj-ref ,(weak-ref pixmap)) in (in-stream))))
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(str (weak-ref (pixmap-read pixmap in ,type)))))))

(defun cpm-save-stage-pixmap (pixmap downstream_mbox type)
	(str `(progn (import "gui/pixmap/lisp.inc")
		(pixmap-write (pixmap-as-argb (obj-ref ,(weak-ref pixmap)))
			(out-stream (hex-decode ,(hex-encode downstream_mbox))) ,type))))

(defun CPM-load (stream)
	; (CPM-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (and (= (length header) +cpm_size)
				(find (char (getf header +cpm_ident) +int_size) '("MPC." "MLF.")))
			(bind '(flags ver w h type) (getf-> header +cpm_flags +cpm_version +cpm_width +cpm_height +cpm_type))
			(defq rle :nil lz4 :nil)
			(if (= ver 1)
				(setq rle :t)
				(setq rle (bits? flags +cpm_flag_rle)
					lz4 (bits? flags +cpm_flag_lz4)))
			(defq canvas (Canvas w h 1) num_bits type
				pixmap (getf canvas +canvas_pixmap 0))
			; 12 and 15 bit images are padded to 16 num_bits in the stream
			(if (or (= type 12) (= type 15)) (setq num_bits 16))
			; save stream type for any film players !
			(setf-> pixmap (+pixmap_stream_type type 0) (+pixmap_type 32 0))
			; define stages in data-flow order (stream -> ... -> pixmap)
			(defq stages (list))
			(if lz4 (push stages (# (cpm-load-stage "lib/streams/lz4.inc" 'lz4-decompress %0 %1))))
			(if rle (push stages (# (cpm-load-stage "lib/streams/rle.inc" 'rle-decompress %0 %1 num_bits 8 (* w h)))))
			(cond
				((empty? stages)
					(ifn (pixmap-read pixmap stream type) (setq canvas :nil)))
				(:t (defq handshake_mbox (mail-mbox))
					(open-child (cpm-load-stage-pixmap pixmap type handshake_mbox) +kn_call_pin)
					(each! (lambda (stage)
						(open-child (stage (mail-read handshake_mbox) handshake_mbox) +kn_call_pin))
						(list stages) -1 1)
					(open-child ((first stages) (mail-read handshake_mbox) stream) +kn_call_pin)
					(if (eql (mail-read handshake_mbox) (const (str :nil))) (setq canvas :nil))))))
	canvas)

(defun CPM-save (canvas stream type &optional rle lz4 ident)
	; (CPM-save canvas stream type [rle lz4 ident]) -> canvas
	(setd ident "MPC.")
	(defq pixmap (getf canvas +canvas_pixmap 0) num_bits type
		flags (+ (if rle +cpm_flag_rle 0) (if lz4 +cpm_flag_lz4 0)))
	(if (or (= type 12) (= type 15)) (setq num_bits 16))
	;header
	(write-blk stream (setf-> (str-alloc +cpm_size)
		(+cpm_ident (code ident 4))
		(+cpm_flags flags)
		(+cpm_version 2)
		(+cpm_type type)
		(+cpm_width (getf pixmap +pixmap_width 0))
		(+cpm_height (getf pixmap +pixmap_height 0))
		(+cpm_reserved_1 0)))
	;define stages in data-flow order (pixmap -> ... -> stream)
	(defq stages (list))
	(if rle (push stages (# (cpm-save-stage "lib/streams/rle.inc" 'rle-compress %0 %1 num_bits 8))))
	(if lz4 (push stages (# (cpm-save-stage "lib/streams/lz4.inc" 'lz4-compress %0 %1))))
	(cond
		((empty? stages)
			(pixmap-write (pixmap-as-argb pixmap) stream type))
		(:t (defq handshake_mbox (mail-mbox))
			(open-child ((last stages) handshake_mbox stream) +kn_call_pin)
			(each! (lambda (stage)
				(open-child (stage handshake_mbox (mail-read handshake_mbox)) +kn_call_pin))
				(list stages) -2 0)
			(open-child (cpm-save-stage-pixmap pixmap (mail-read handshake_mbox) type) +kn_call_pin)
			(mail-read handshake_mbox)))
	canvas)

;module
(export-symbols '(CPM-load CPM-info CPM-save))
(env-pop)