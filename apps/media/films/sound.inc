;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the sound of a film: its AAC, AC-3 or E-AC-3 track decoded a packet at
; a time, a little ahead of what is heard, into a stream of the Audio
; service, 16 bit stereo at its rate. What has been heard is the film's clock, and the
; pictures are kept to it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/audio/app.inc")
(import "lib/media/aac.inc")
(import "lib/media/ac3.inc")

;how far ahead of what is heard the stream is kept, in seconds
(defq +sound_ahead 0.25)

(defun sound-of (reel)
	; (sound-of reel) -> :nil | sound
	;the sound track of a film of an mp4 that there is a decoder for here:
	;AAC-LC, its packets of 1024 samples, each after the one before; AC-3
	;and E-AC-3, of 1536, a block of 256 late. Each made two channels
	(when (defq tracks (. reel :find :tracks))
		(defq track (some (# (and (eql (. %0 :find :kind) "soun") (find (. %0 :find :codec) '("mp4a" "ec-3" "ac-3")) %0)) tracks))
		(when track
			(mp4-tables track)
			(defq sound (scatter (Emap) :track track :stream (. reel :find :stream)
				:start (. track :find :start) :handle :nil :written 0 :left 0 :time 0.0
				;the native that makes 16 bit samples, taken now, not as this is read
				:s16 (ffi "lib/media/media_s16")))
			(cond
				((eql (. track :find :codec) "mp4a")
					(and (defq config (aac-track-config track)) (= (first config) 2)
						;a lambda finds nothing of where it was made, what it wants is
						;in the sound
						(scatter sound :rate (third config) :size 1024 :delay 1024 :config config
							:make (lambda (sound) (aac-decoder (. sound :find :config)))
							:decode (lambda (decoder packet) (aac-decode decoder packet)))))
				(:t (stream-seek (. sound :find :stream) (elem-get (. track :find :offsets) 0) 0)
					(defq head (ac3-header (bits-of (read-blk (. sound :find :stream) 16))))
					(and head (scatter sound :rate (. head :find :rate) :size (* 256 (. head :find :blocks)) :delay 256 :head head
						:make (lambda (sound) (ac3-decoder (. sound :find :head)))
						:decode (lambda (decoder packet) (ac3-stereo decoder (ac3-frame decoder packet))))))))))

(defun sound-stop (sound)
	; (sound-stop sound)
	(when (and sound (. sound :find :handle))
		(audio-stream-close-rpc (. sound :find :handle))
		(. sound :insert :handle :nil)))

(defun sound-packet (sound n)
	;the samples of packet n of the track, a list of reals a channel
	(defq track (. sound :find :track) stream (. sound :find :stream))
	(stream-seek stream (elem-get (. track :find :offsets) n) 0)
	((. sound :find :decode) (. sound :find :decoder) (read-blk stream (elem-get (. track :find :sizes) n))))

(defun sound-start (sound time)
	; (sound-start sound seconds)
	;heard from a time of the film: the packet whose samples have it,
	;decoded after the one before, which only fills the overlap, and what
	;is before the time in it left out. A packet's samples come so many
	;late, its delay: those of packet q are from q by its size less that
	(sound-stop sound)
	(defq rate (. sound :find :rate) size (. sound :find :size) delay (. sound :find :delay)
		sample (+ (max 0 (+ (n2i (* time (n2f rate))) (. sound :find :start))) delay)
		packet (/ sample size))
	(. sound :insert :decoder ((. sound :find :make) sound))
	(and (> packet 0) (< (dec packet) (. (. sound :find :track) :find :samples)) (sound-packet sound (dec packet)))
	(scatter sound :next packet :drop (- sample (* packet size)) :time time
		:written 0 :left 0 :anchor (pii-time) :handle (audio-stream-open-rpc rate 2)))

(defun sound-pcm (sound left right from)
	;16 bit stereo, left first, from a frame on, lib/media/lisp.vp media_s16
	((. sound :find :s16) (str-alloc (* 4 (- (length left) from))) (list left right) from))

(defun sound-feed (sound)
	; (sound-feed sound)
	;packets decoded and written till the stream is far enough ahead of
	;what is heard
	(when (defq handle (. sound :find :handle))
		(defq rate (. sound :find :rate) packets (. (. sound :find :track) :find :samples)
			left (ifn (audio-stream-left-rpc handle) 0) written (. sound :find :written)
			ahead (n2i (* +sound_ahead (n2f rate))) next (. sound :find :next))
		(. sound :insert :left left)
		(while (and (< left ahead) (< next packets))
			(defq chans (sound-packet sound next) drop (. sound :find :drop)
				data (sound-pcm sound (first chans) (ifn (second chans) (first chans)) drop))
			(audio-stream-write-rpc handle data)
			(defq got (- (length (first chans)) drop))
			(setq written (+ written got) left (+ left got) next (inc next))
			(. sound :insert :drop 0))
		(scatter sound :written written :next next)))

(defun sound-clock (sound)
	; (sound-clock sound) -> seconds
	;the time of the film that is being heard: where it started, and what
	;has been heard of what was written since
	(+ (. sound :find :time)
		(/ (n2f (- (. sound :find :written) (. sound :find :left))) (n2f (. sound :find :rate)))))

(defun sound-smooth (sound)
	; (sound-smooth sound) -> seconds
	;the time the pictures are shown at: of the system's clock from an
	;anchor, as what is heard is told in steps, as the mixer takes a
	;buffer of 20ms or more, and a film of 60 a second shown by them would
	;jerk. Anchored again where nothing has been heard yet, and where the
	;two are more than 100ms apart
	(defq now (pii-time) heard (sound-clock sound)
		;in ms, a fixed number holds no more than 2^31 of the us of a film
		smooth (+ (. sound :find :time) (/ (n2f (/ (- now (. sound :find :anchor)) 1000)) 1000.0)))
	(cond
		((or (= (. sound :find :written) (. sound :find :left)) (> (abs (- smooth heard)) 0.1))
			(. sound :insert :anchor (- now (* 1000 (n2i (* (- heard (. sound :find :time)) 1000.0)))))
			heard)
		(smooth)))
