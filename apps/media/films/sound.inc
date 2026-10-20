;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the sound of a film: its AAC, AC-3 or E-AC-3 track decoded a packet at
; a time, a little ahead of what is heard, into a stream of the Audio
; service, 16 bit stereo at its rate, or an MP3's or a FLAC's frames,
; its reel's packets. What has been heard is the film's clock, and the pictures are
; kept to it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/audio/app.inc")
(import "lib/media/aac.inc")
(import "lib/media/ac3.inc")
(import "lib/media/mp3.inc")
(import "lib/media/flac.inc")

;how far ahead of what is heard the stream is kept, in seconds
(defq +sound_ahead 0.25)

(defun sound-of (reel)
	; (sound-of reel) -> :nil | sound
	;the sound track of a film of an mp4 that there is a decoder for here:
	;AAC-LC, its packets of 1024 samples, each after the one before, and
	;HE-AAC's of 2048 at twice the rate; AC-3
	;and E-AC-3, of 1536, a block of 256 late. Of an MP3 its frames, of
	;1152 or 576, as they are heard, the packets of its reel, and those
	;before one decoded first fill the reservoir. Each made two channels
	(when (defq tracks (. reel :find :tracks))
		(defq track (some (# (and (eql (. %0 :find :kind) "soun") (find (. %0 :find :codec) '("mp4a" "ec-3" "ac-3" "mp3" "flac")) %0)) tracks))
		(when track
			(unless (find (. track :find :codec) '("mp3" "flac")) (mp4-tables track))
			(defq sound (scatter (Emap) :track track :stream (. reel :find :stream) :reel reel
				:start (. track :find :start) :handle :nil :written 0 :left 0 :time 0.0 :before 1
				;the native that makes 16 bit samples, taken now, not as this is read
				:s16 (ffi "lib/media/media_s16")))
			(cond
				((eql (. track :find :codec) "mp3")
					;of an MP3 its reel's packets, of a film's track its own, its first
					;frame's header what it is
					(defq own (. reel :find :head) head (or own
						(progn (stream-seek (. sound :find :stream) (elem-get (. track :find :offsets) 0) 0)
							(mp3-header (ifn (read-blk (. sound :find :stream) 4) "") 0))))
					(and head (scatter sound :rate (. head :find :rate) :size (. head :find :samples) :delay 0 :before 3
						:make (lambda (sound) (scatter (mp3-decoder) :samples (. sound :find :size)))
						:read (if own (lambda (sound n) (reel-packet (. sound :find :reel) n)))
						;a frame not read is heard as nothing
						:decode (lambda (decoder packet)
							(defq pcm (if packet (catch (mp3-frame decoder packet) :t)))
							(if (list? pcm) pcm (list (defq quiet (fill (reals) (. decoder :find :samples) (n2r 0))) quiet))))))
				((eql (. track :find :codec) "flac")
					;a FLAC's frames as they are heard, lossless, of a FLAC its reel's
					;packets, of a film's track its own, its metadata the track's setup
					(defq own (. reel :find :info) info (or own (and (. track :find :private) (flac-info (string-stream (. track :find :private))))))
					(and info (scatter sound :rate (. info :find :rate) :size (. info :find :max_block) :delay 0 :before 0 :info info
						:make (lambda (sound) (. sound :find :info))
						:read (if own (lambda (sound n) (reel-packet (. sound :find :reel) n)))
						;a frame not read is heard as nothing
						:decode (lambda (info packet)
							(defq got (if packet (catch (flac-frame info packet) :t)))
							(if (list? got) (flac-pcm info (first got))
								(list (defq quiet (fill (reals) (. info :find :max_block) (n2r 0))) quiet))))))
				((eql (. track :find :codec) "mp4a")
					(and (defq config (aac-track-config track)) (= (first config) 2)
						;HE-AAC's SBR at twice the rate of its core, packets of 2048 and
						;the track's start in the core's time; a lambda finds nothing of
						;where it was made, what it wants is in the sound
						;of a core of 24000 or less SBR may be there unsaid, its first
						;packet decoded to see
						(defq times (cond
							((elem-get config 4) 2)
							((> (third config) 24000) 1)
							((progn (stream-seek (. sound :find :stream) (elem-get (. track :find :offsets) 0) 0)
								(defq got (catch (aac-decode (aac-decoder config) (read-blk (. sound :find :stream) (elem-get (. track :find :sizes) 0))) :t))
								(and (list? got) (= (length (first got)) 2048))) 2)
							(1)))
						(scatter sound :rate (* times (third config)) :size (* times 1024) :delay (* times 1024)
							:start (* times (. sound :find :start)) :config config
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
	((. sound :find :decode) (. sound :find :decoder)
		(if (. sound :find :read) ((. sound :find :read) sound n)
			(stream-seek stream (elem-get (. track :find :offsets) n) 0)
			(read-blk stream (elem-get (. track :find :sizes) n)))))

(defun sound-start (sound time)
	; (sound-start sound seconds)
	;heard from a time of the film: the packet whose samples have it,
	;decoded after those before, :before of them, which only fill the
	;overlap, and what is before the time in it left out. A packet's
	;samples come so many late, its delay: those of packet q are from q by
	;its size less that
	(sound-stop sound)
	;the whole seconds by the rate as numbers, a fixed product is no more
	;than 2^31, 13 hours of 44100 a second, and a book is longer
	(defq rate (. sound :find :rate) size (. sound :find :size) delay (. sound :find :delay)
		whole (n2i (max 0.0 time))
		sample (+ (max 0 (+ (* whole rate) (n2i (* (- time (n2f whole)) (n2f rate))) (. sound :find :start))) delay)
		packet (/ sample size))
	(. sound :insert :decoder ((. sound :find :make) sound))
	(each (# (sound-packet sound %0))
		(range (max 0 (- packet (. sound :find :before))) (min packet (. (. sound :find :track) :find :samples))))
	(scatter sound :next packet :drop (- sample (* packet size)) :time time
		:written 0 :left 0 :anchor (pii-time) :began (pii-time) :handle (audio-stream-open-rpc rate 2)))

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
	;jerk. For its first second what is heard is not believed, the mixer
	;takes in a buffer it has yet to play, and it reads 200ms ahead. Then
	;drawn to what is heard by no more than 2ms at a time, which is not
	;seen, and never back. Anchored again where nothing has been written
	;yet, and where the two are half a second apart, after a stall
	(defq now (pii-time) heard (sound-clock sound) time (. sound :find :time)
		;in ms, a fixed number holds no more than 2^31 of the us of a film
		smooth (+ time (/ (n2f (/ (- now (. sound :find :anchor)) 1000)) 1000.0)))
	(cond
		((= (. sound :find :written) 0) time)
		((> (abs (- smooth heard)) 0.5)
			(. sound :insert :anchor (- now (* 1000 (n2i (* (- heard time) 1000.0)))))
			heard)
		((< (- now (. sound :find :began)) 1000000) smooth)
		(:t (defq to (+ smooth (max -0.002 (min 0.002 (- heard smooth)))))
			(. sound :insert :anchor (- now (* 1000 (n2i (* (- to time) 1000.0)))))
			to)))
