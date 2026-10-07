;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flm, a film, written to a stream a frame at a time
;
; Frames are pumped in, each a canvas, and the film comes out of the
; stream. (flm-open) sets the pipe up, (flm-add) pumps a frame in, and
; (flm-close) ends it, when the film is flushed and whole. So a film
; is written as its frames are made, straight from the canvas each is
; drawn on, with no file for each frame. The toflm command is this,
; with the frames loaded from files.
;
; It is an async local pipeline, as the load and save of a .cpm image
; are, docs/ai_digest/async_pipelines.md. The encoder is a task of its
; own on this node, fed by a stream, and the stream the film goes to is
; the sink. (flm-add) only writes the pixels of the frame to the feed,
; so the next frame can be on its way while the last is encoded, and
; the feed holds the maker back if the encoder falls behind.
;
; The first frame is a .cpm image. Each frame after it is only what has
; changed from the one before, runs of pixels to skip and runs to draw.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././image/cpm.inc")

(defun flm-encoder (stream format)
	;the state of a film being written to a stream
	(defq film (env 1))
	(def film :stream stream :format format
		:num_bits (if (or (= format 12) (= format 15)) 16 format)
		:last (memory-stream) :next (memory-stream) :pixels 0 :frames 0)
	film)

(defun flm-encode-frame (film canvas)
	;the next frame of the film, written to its stream
	(defq stream (get :stream film) format (get :format film) num_bits (get :num_bits film)
		last (get :last film) next (get :next film) pixels (get :pixels film)
		frames (get :frames film) pixmap (getf canvas +canvas_pixmap 0))
	(cond
		((= frames 0)
			;the first frame, whole, as a .cpm image is saved
			(CPM-save canvas stream format :t :t "MLF.")
			(def film :pixels (* (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0)))
			(pixmap-write (pixmap-as-argb pixmap) last format))
		(:t ;what has changed since the frame before
			(stream-seek last 0 0)
			(stream-seek next 0 0)
			(pixmap-write (pixmap-as-argb pixmap) next format)
			(stream-seek next 0 0)
			(defq p_state (array 0 0) c_state (array 0 0) w_state (array 0 0)
				mode :none count 0 draw_buf (list))
			(times pixels
				(defq p (read-bits last p_state num_bits)
					c (read-bits next c_state num_bits))
				(if (= p -1) (setq p 0))
				(if (= c -1) (setq c 0))
				(cond
					((= p c)
						(if (eql mode :draw)
							(progn
								(when (> count 0)
									(write-bits stream w_state count 8)
									(each (# (write-bits stream w_state %0 num_bits)) draw_buf)
									(clear draw_buf))
								(setq count 0 mode :skip)))
						(setq mode :skip)
						(when (= (++ count) 128)
							(write-bits stream w_state (- 256 128) 8)
							(setq count 0)))
					(:t (if (eql mode :skip)
							(progn
								(if (> count 0)
									(write-bits stream w_state (- 256 count) 8))
								(setq count 0 mode :draw)))
						(setq mode :draw)
						(push draw_buf c)
						(when (= (++ count) 127)
							(write-bits stream w_state 127 8)
							(each (# (write-bits stream w_state %0 num_bits)) draw_buf)
							(clear draw_buf)
							(setq count 0)))))
			;what is left of the last run
			(if (eql mode :draw)
				(when (> count 0)
					(write-bits stream w_state count 8)
					(each (# (write-bits stream w_state %0 num_bits)) draw_buf)))
			(if (eql mode :skip)
				(when (> count 0)
					(write-bits stream w_state (- 256 count) 8)))
			;a frame ends on a whole byte
			(flush-bits stream w_state)
			;this frame is the one the next is compared with
			(def film :last next :next last)))
	(def film :frames (inc frames))
	film)

(defun flm-encode (in stream format)
	;the encoder task. The feed is the size of a frame, then frame after
	;frame of pixels, till it ends
	(defq width (read-int in) height (read-int in) frames 0)
	(when (and width height (> width 0) (> height 0))
		(defq canvas (Canvas width height 1) pixmap (getf canvas +canvas_pixmap 0)
			film (flm-encoder stream format))
		(setf pixmap +pixmap_type 32 0)
		(while (pixmap-read pixmap in format)
			(flm-encode-frame film canvas)
			(setq frames (inc frames))
			(task-slice)))
	(stream-flush stream)
	frames)

(defun flm-stage (stream format handshake)
	;the source of the encoder task, it says where its feed is, makes the
	;film, and says how many frames there were
	(str `(progn (import "gui/lisp.inc") (import "lib/streams/flm.inc")
		(mail-send (hex-decode ,(hex-encode handshake)) (in-mbox (defq in (in-stream))))
		(mail-send (hex-decode ,(hex-encode handshake))
			(str (flm-encode in (obj-ref ,(weak-ref stream)) ,format))))))

(defun flm-open (stream format)
	; (flm-open stream format) -> film
	;a film to be written to a stream, format is the bits of a pixel,
	;1, 8, 12, 15, 16, 24 or 32. The encoder is started, and waits for
	;frames
	(defq film (env 1) handshake (mail-mbox))
	(open-child (flm-stage stream format handshake) +kn_call_pin)
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
