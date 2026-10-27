;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flm, our own film: written a frame at a time, and read as a reel of
; packets that the flm codec makes frames of
;
; The file is the header of a .cpm image, ident MLF., version 3, its
; reserved word the frame rate in hundredths of a frame a second, and
; then packets, each its length, 4 bytes, and its bytes. The first is the
; first frame, whole, as the pixels of a .cpm are, run length and lz4 as
; its flags say. Each after it is only what changed from the frame before,
; runs of pixels to keep and runs to draw, gui/pixmap/flm.vp.
;
; Writing. (flm-open) sets the pipe up, (flm-add) pumps a frame in, and
; (flm-close) ends it. The encoder is a task of its own on this node, fed
; by a stream, so the next frame can be on its way while the last is
; encoded, docs/ai_digest/async_pipelines.md. The toflm command and the
; Raymarch film are made so.
;
; Reading. (flm-reel stream) has the packets found, where each is and how
; long, and (flm-packet) gives one. (flm-codec reel) owns a canvas and
; (flm-decode codec packet index) puts a packet on it: packet 0 is a whole
; frame, the rest each the next. (flm-frame codec n) gives frame n, from
; the one it gave last or from the first. lib/media/reel.inc is what a player uses,
; this is the flm kind of it. A film of an older version, with no packets,
; is made this one by (flm-convert), toflm -c.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././image/cpm.inc")
(import "./reel.inc")
(import "././streams/lz4.inc")
(import "././streams/rle.inc")

;module
(env-push)

(defq +flm_version 3 +flm_rate 30 +flm_header 28)

(defun flm-bits (format)
	;the bits a pixel of a format takes in the stream
	(if (or (= format 12) (= format 15)) 16 format))

;;;;;;;;;
; writing
;;;;;;;;;

(defun flm-packet-write (stream data)
	;a packet, its length and its bytes
	(write-blk stream (cat (char (length data) 4) data)))

(defun flm-encoder (stream format rate)
	;the state of a film being written to a stream
	(defq film (env 1))
	(def film :stream stream :format format :rate rate :num_bits (flm-bits format)
		:last (memory-stream) :next (memory-stream) :pixels 0 :frames 0)
	film)

(defun flm-encode-frame (film canvas)
	;the next frame of the film, a packet written to its stream
	(defq stream (get :stream film) format (get :format film) num_bits (get :num_bits film)
		last (get :last film) next (get :next film) pixels (get :pixels film)
		frames (get :frames film) pixmap (getf canvas +canvas_pixmap 0)
		out (string-stream (cat "")))
	(cond
		((= frames 0)
			;the first frame, whole, as a .cpm image is saved: its header
			;made this version's, and its pixels the first packet
			(CPM-save canvas out format :t :t "MLF.")
			(defq whole (str out) header (slice whole 0 +flm_header))
			(write-blk stream (cat (slice header 0 8) (char +flm_version 4) (slice header 12 24)
				(char (n2i (* (get :rate film) 100)) 4)))
			(flm-packet-write stream (slice whole +flm_header -1))
			(def film :pixels (* (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0)))
			(pixmap-write (pixmap-as-argb pixmap) last format))
		(:t ;what has changed since the frame before
			(stream-seek last 0 0)
			(stream-seek next 0 0)
			(pixmap-write (pixmap-as-argb pixmap) next format)
			(stream-seek next 0 0)
			(defq p_state (array 0 0) c_state (array 0 0) w_state (array 0 0)
				mode :none count 0 draw_buf (list) at 0)
			(times pixels
				;the encoder is on the node of what makes the film, the GUI's
				;for an app: it gives way every so many pixels, or a frame
				;held the node, the mouse and all, for the whole of it
				(if (= (logand (++ at) 16383) 0) (task-slice))
				(defq p (read-bits last p_state num_bits)
					c (read-bits next c_state num_bits))
				(if (= p -1) (setq p 0))
				(if (= c -1) (setq c 0))
				(cond
					((= p c)
						(when (eql mode :draw)
							(when (> count 0)
								(write-bits out w_state count 8)
								(each (# (write-bits out w_state %0 num_bits)) draw_buf)
								(clear draw_buf))
							(setq count 0 mode :skip))
						(setq mode :skip)
						(when (= (++ count) 128)
							(write-bits out w_state (- 256 128) 8)
							(setq count 0)))
					(:t (when (eql mode :skip)
							(if (> count 0)
								(write-bits out w_state (- 256 count) 8))
							(setq count 0 mode :draw))
						(setq mode :draw)
						(push draw_buf c)
						(when (= (++ count) 127)
							(write-bits out w_state 127 8)
							(each (# (write-bits out w_state %0 num_bits)) draw_buf)
							(clear draw_buf)
							(setq count 0)))))
			;what is left of the last run
			(if (eql mode :draw)
				(when (> count 0)
					(write-bits out w_state count 8)
					(each (# (write-bits out w_state %0 num_bits)) draw_buf)))
			(if (eql mode :skip)
				(when (> count 0)
					(write-bits out w_state (- 256 count) 8)))
			;a frame ends on a whole byte, and is a packet
			(flush-bits out w_state)
			(flm-packet-write stream (str out))
			;this frame is the one the next is compared with
			(def film :last next :next last)))
	(def film :frames (inc frames))
	film)

(defun flm-encode (in stream format rate)
	;the encoder task. The feed is the size of a frame, then frame after
	;frame of pixels, till it ends
	(defq width (read-int in) height (read-int in) frames 0)
	(when (and width height (> width 0) (> height 0))
		(defq canvas (Canvas width height 1) pixmap (getf canvas +canvas_pixmap 0)
			film (flm-encoder stream format rate))
		(setf pixmap +pixmap_type 32 0)
		(while (pixmap-read pixmap in format)
			(flm-encode-frame film canvas)
			(setq frames (inc frames))
			(task-slice)))
	(stream-flush stream)
	frames)

(defun flm-stage (stream format rate handshake)
	;the source of the encoder task, it says where its feed is, makes the
	;film, and says how many frames there were
	(str `(progn (import "gui/lisp.inc") (import "lib/media/flm.inc")
		(mail-send (hex-decode ,(hex-encode handshake)) (in-mbox (defq in (in-stream))))
		(mail-send (hex-decode ,(hex-encode handshake))
			(str (flm-encode in (obj-ref ,(weak-ref stream)) ,format ,rate))))))

(defun flm-open (stream format &optional rate)
	; (flm-open stream format [rate]) -> film
	;a film to be written to a stream, format is the bits of a pixel,
	;1, 8, 12, 15, 16, 24 or 32, and rate the frames a second, 30 if not
	;said. The encoder is started, and waits for frames
	(defq film (env 1) handshake (mail-mbox))
	(open-child (flm-stage stream format (ifn rate +flm_rate) handshake) +kn_call_pin)
	(def film :stream stream :format format :handshake handshake :frames 0
		:feed (out-stream (mail-read handshake)))
	film)

(defun flm-add (film canvas)
	; (flm-add film canvas) -> film
	;the next frame of the film, what is on the canvas now. It must be
	;the size of the first. The pixmap of the canvas is left as argb.
	(defq feed (get :feed film) pixmap (getf canvas +canvas_pixmap 0))
	(when (= (get :frames film) 0)
		(write-int feed (list (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0))))
	(pixmap-write (pixmap-as-argb pixmap) feed (get :format film))
	(def film :frames (inc (get :frames film)))
	film)

(defun flm-close (film)
	; (flm-close film) -> frames
	;no more frames. The feed is flushed and let go, which is the end of
	;it for the encoder, and when it has written the last of the film it
	;says how many frames it made, which is the result
	(stream-flush (get :feed film))
	(undef film :feed)
	(defq frames (str-as-num (mail-read (get :handshake film))))
	(undef film :stream)
	frames)

;;;;;;;;;
; reading
;;;;;;;;;

(defun flm-head (stream)
	; (flm-head stream) -> :nil | (version flags type width height rate)
	;what the header of a film says, rate in frames a second
	(and (defq header (read-blk stream +flm_header)) (= (length header) +flm_header)
		(eql (slice header 0 4) "MLF.")
		(list (code header 4 8) (code header 4 4) (code header 4 12) (code header 4 16) (code header 4 20)
			;the rate only this version has, the word was something else before
			(if (or (/= (code header 4 8) +flm_version) (= (defq hundredths (code header 4 24)) 0))
				+flm_rate (/ (n2f hundredths) 100.0)))))

(defun flm-reel (stream &optional few)
	; (flm-reel stream [few]) -> :nil | reel
	;a film to be read from a stream that can seek: its header, and where
	;each packet is and how long, found by their lengths. :nil if it is no
	;film of this version, :version says which it is
	(stream-seek stream 0 0)
	(when (defq head (flm-head stream))
		(bind '(version flags format width height rate) head)
		(defq reel (Emap))
		(. reel :insert :version version)
		(when (= version +flm_version)
			(defq offsets (list) lengths (list) at +flm_header)
			;read-blk gives :nil at the end, and :nil is 4 long, the letters of
			;its name, so it is looked for first
			(while (and (defq size (read-blk stream 4)) (= (length size) 4)
					(>= (defq len (code size 4 0)) 0))
				(push offsets (+ at 4))
				(push lengths len)
				(setq at (+ at 4 len))
				(stream-seek stream at 0))
			(scatter reel :stream stream :flags flags :format format :width width :height height
				:rate rate :offsets offsets :lengths lengths :frames (length offsets)))
		reel))

(defun flm-packet (reel index)
	; (flm-packet reel index) -> :nil | str
	;the bytes of a packet of the film
	(when (< -1 index (. reel :find :frames))
		(defq stream (. reel :find :stream))
		(stream-seek stream (elem-get (. reel :find :offsets) index) 0)
		(read-blk stream (elem-get (. reel :find :lengths) index))))

(defun flm-codec (reel &optional scale)
	; (flm-codec reel [scale]) -> codec
	;what makes frames of the packets of a film, on a canvas of its own
	;the native of a delta frame is taken as the codec is made, not as this
	;is read: this is read by every task that loads a picture, and one that
	;named a function the system had not been built with yet never got as
	;far as building it, gui/pixmap/flm.vp
	;a scale only if it is of the whole size: a frame is drawn on the one
	;before by its runs, as wide as the film
	(defq w (. reel :find :width) h (. reel :find :height) scale (ifn scale 1))
	(if (or (/= 0 (% w scale)) (/= 0 (% h scale))) (setq scale 1))
	(scatter (Emap) :canvas (Canvas (/ w scale) (/ h scale) scale) :scale scale
		:format (. reel :find :format) :flags (. reel :find :flags)
		:delta (ffi "gui/pixmap/lisp_flm_delta") :reel reel :at -1))

(defun flm-decode (codec packet index)
	; (flm-decode codec packet index) -> :nil | canvas
	;the frame of a packet on the codec's canvas: packet 0 is a whole frame,
	;each after it is drawn on the frame before it
	(defq canvas (. codec :find :canvas) pixmap (getf canvas +canvas_pixmap 0)
		format (. codec :find :format) in (string-stream packet))
	(cond
		((= index 0)
			;the whole frame, its lz4 and run lengths undone here, in this
			;task, from the packet it was given. As stages of tasks of their
			;own, the first was given the stream by a weak reference, and the
			;stream, this function's, was gone before it had read it all
			(defq flags (. codec :find :flags))
			(setf pixmap +pixmap_type 32 0)
			(when (/= 0 (logand flags 2))
				(lz4-decompress in (defq out (memory-stream)))
				(stream-seek out 0 0)
				(setq in out))
			(when (/= 0 (logand flags 1))
				(rle-decompress in (defq out (memory-stream)) (flm-bits format) 8
					(* (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0)))
				(stream-seek out 0 0)
				(setq in out))
			(if (pixmap-read pixmap in format) canvas))
		;(delta pixmap stream type) -> :nil | pixmap, the frame before drawn
		;on with the runs of the packet
		(((. codec :find :delta) pixmap in format) canvas)))

(defun flm-frame (codec n)
	; (flm-frame codec n) -> :nil | canvas
	;frame n, each packet after the frame given last, or after the first
	;if n is before it
	(defq reel (. codec :find :reel) at (. codec :find :at) canvas :nil)
	(if (< n at) (setq at -1))
	(if (= n at) (setq canvas (. codec :find :canvas)))
	(while (< at n)
		(setq at (inc at) canvas (flm-decode codec (flm-packet reel at) at)))
	(. codec :insert :at at)
	canvas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a film shown as a picture, its first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun FLM-info (stream)
	; (FLM-info stream) -> (width height type) | (-1 -1 -1)
	(if (defq head (flm-head stream))
		(list (elem-get head 3) (elem-get head 4) (elem-get head 2))
		(list -1 -1 -1)))

(defun FLM-load (stream)
	; (FLM-load stream) -> :nil | canvas
	(and (defq reel (flm-reel stream)) (> (ifn (. reel :find :frames) 0) 0)
		(flm-decode (flm-codec reel) (flm-packet reel 0) 0)))

(canvas-loader ".flm" FLM-info FLM-load)

;and played as a reel
(reel-kind ".flm" flm-reel flm-packet flm-codec flm-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a film of an older version, made this one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flm-convert (in out &optional rate)
	; (flm-convert in out [rate]) -> :nil | frames
	;a film of version 1 or 2 read, and written to out as this version: a
	;whole first frame and then each frame after it, found by its runs.
	;Those have no packets, a frame ends where its runs have covered every
	;pixel. :nil if in is not such a film, or of a format whose pixels are
	;not whole bytes
	(stream-seek in 0 0)
	(when (and (defq head (flm-head in)) (< (first head) +flm_version)
			(= (logand (flm-bits (third head)) 7) 0))
		(bind '(& & format width height &ignore) head)
		(stream-seek in 0 0)
		(when (defq first_frame (CPM-load in))
			;what follows the first frame, every delta frame
			(defq parts (list) part :nil)
			(while (nempty? (setq part (ifn (read-blk in 1048576) ""))) (push parts part))
			(defq rest (apply (const cat) (cat (list "") parts)))
			;the first frame written this version's, as the encoder does
			(defq film (flm-encoder out format (ifn rate +flm_rate)))
			(flm-encode-frame film first_frame)
			;then each frame, its runs walked till they cover every pixel
			(defq pixels (* width height) bytes (/ (flm-bits format) 8) at 0 frames 1)
			(while (< at (length rest))
				(defq start at count 0)
				(while (and (< count pixels) (< at (length rest)))
					(defq run (code rest 1 at))
					(setq at (inc at))
					(cond
						((>= run 128) (setq count (+ count (- 256 run))))
						(:t (setq count (+ count run) at (+ at (* run bytes))))))
				(flm-packet-write out (slice rest start (min at (length rest))))
				(setq frames (inc frames)))
			(stream-flush out)
			frames)))

;module
(export-symbols '(flm-open flm-add flm-close flm-encode flm-reel flm-packet flm-codec flm-decode flm-frame flm-convert FLM-info FLM-load))
(env-pop)
