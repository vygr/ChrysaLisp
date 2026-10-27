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
(import "lib/media/adpcm.inc")

;how far ahead of what is heard the stream is kept, in seconds, and how
;many of the frames an MP3's sound decoded last it keeps for its pictures,
;more than are written ahead
(defq +sound_ahead 0.25 +sound_kept 32)

(defun sound-track (reel)
	; (sound-track reel) -> :nil | track
	;the first sound track of a reel there is a decoder for here
	(if (defq tracks (. reel :find :tracks))
		(some (# (and (eql (. %0 :find :kind) "soun")
			(find (. %0 :find :codec) '("mp4a" "ec-3" "ac-3" "mp3" "flac" "pcm" "adpcm_ms" "adpcm_ima")) %0)) tracks)))

(defun sound-avi (reel track)
	; (sound-avi reel track) -> :nil | sound
	;the sound of an AVI that is not MP3: its chunks one stream, (avi-track-stream),
	;cut into packets of a size, each read where it is. PCM of 16 bits and two
	;channels is written as it is, the rest made reals. AC-3 its frames,
	;each of the size its first says
	(defq in (avi-track-stream reel track) bytes (stream-seek in 0 2) codec (. track :find :codec)
		channels (max 1 (. track :find :channels)) align (max 1 (. track :find :block_align))
		bits (. track :find :bits) extra (ifn (. track :find :extra) "")
		;of a block, the samples a block it says, or worked out from its size
		per (if (>= (length extra) 2) (code extra 2 0) 0)
		sound (scatter (Emap) :stream (. reel :find :stream) :reel reel :in in
			:start 0 :handle :nil :written 0 :left 0 :time 0.0 :before 0 :delay 0
			:rate (. track :find :sample_rate) :channels channels :bits bits
			:s16 (ffi "lib/media/media_s16")
			:read (lambda (sound n)
				(stream-seek (. sound :find :in) (* n (. sound :find :packet)) 0)
				(read-blk (. sound :find :in) (. sound :find :packet)))))
	(cond
		((eql codec "pcm")
			(scatter sound :size 1024 :packet (* 1024 align)
				:make (lambda (sound) sound)
				:decode (lambda (sound packet)
					(cond
						((not packet) (list (defq quiet (fill (reals) 1024 (n2r 0))) quiet))
						((and (= (. sound :find :bits) 16) (= (. sound :find :channels) 2)) packet)
						((adpcm-pcm packet (. sound :find :channels) (. sound :find :bits)))))))
		((eql codec "adpcm_ms")
			(scatter sound :size (if (> per 0) per (+ 2 (/ (* 2 (- align (* 7 channels))) channels))) :packet align
				:coefs (adpcm-ms-coefs extra)
				:make (lambda (sound) sound)
				:decode (lambda (sound packet)
					(if packet (adpcm-ms-block packet (. sound :find :channels) (. sound :find :coefs))
						(list (defq quiet (fill (reals) (. sound :find :size) (n2r 0))) quiet)))))
		((eql codec "adpcm_ima")
			(scatter sound :size (if (> per 0) per (inc (/ (* 2 (- align (* 4 channels))) channels))) :packet align
				:make (lambda (sound) sound)
				:decode (lambda (sound packet)
					(if packet (adpcm-ima-block packet (. sound :find :channels))
						(list (defq quiet (fill (reals) (. sound :find :size) (n2r 0))) quiet)))))
		((eql codec "ac-3")
			(stream-seek in 0 0)
			(when (defq head (ac3-header (bits-of (ifn (read-blk in 16) ""))))
				(scatter sound :rate (. head :find :rate) :size (* 256 (. head :find :blocks)) :delay 256 :head head
					:packet (. head :find :size)
					:make (lambda (sound) (ac3-decoder (. sound :find :head)))
					:decode (lambda (decoder packet) (ac3-stereo decoder (ac3-frame decoder packet)))))))
	;how many packets, the last may be short
	(when (. sound :find :make)
		(. sound :insert :track (scatter (Emap) :kind "soun" :codec codec :start 0
			:samples (/ (+ bytes (dec (. sound :find :packet))) (. sound :find :packet))))
		sound))


(defun sound-of (reel)
	; (sound-of reel) -> :nil | sound
	;the sound track of a film of an mp4 that there is a decoder for here:
	;AAC-LC, its packets of 1024 samples, each after the one before, and
	;HE-AAC's of 2048 at twice the rate; AC-3
	;and E-AC-3, of 1536, a block of 256 late. Of an MP3 its frames, of
	;1152 or 576, as they are heard, the packets of its reel, and those
	;before one decoded first fill the reservoir. Each made two channels
	;of an AVI, its sound one stream of its chunks: an MP3's frames found in
	;it as in a file of MP3, a reel of its own, the rest packets of a size
	(and (defq track (sound-track reel)) (. track :find :avi)
		(eql (. track :find :codec) "mp3")
		(setq track (when (defq sub (mp3-reel (avi-track-stream reel track)))
			(. sub :insert :kind (some (# (if (eql (first %0) ".mp3") %0)) *reel_kinds*))
			(setq reel sub)
			(sound-track sub))))
	(when track
		(cond
			((. track :find :avi) (sound-avi reel track))
			(track
			(unless (find (. track :find :codec) '("mp3" "flac")) (mp4-tables track))
			(defq sound (scatter (Emap) :track track :stream (. reel :find :stream) :reel reel
				:start (. track :find :start) :handle :nil :written 0 :left 0 :time 0.0 :before 1
				;the native that makes 16 bit samples, taken now, not as this is read
				:s16 (ffi "lib/media/media_s16")))
			;of a reel read as it is played its packets are had by their times
			(if (. reel :find :track_packet)
				(. sound :insert :read (lambda (sound n)
					((. (. sound :find :reel) :find :track_packet) (. sound :find :reel) (. sound :find :track) n))))
			(cond
				((eql (. track :find :codec) "mp3")
					;of an MP3 its reel's packets, of a film's track its own, its first
					;frame's header what it is
					(defq own (. reel :find :head) head (or own
						(progn (stream-seek (. sound :find :stream) (elem-get (. track :find :offsets) 0) 0)
							(mp3-header (ifn (read-blk (. sound :find :stream) 4) "") 0))))
					;the frames before one decoded first till the reservoir holds the 511
					;bytes a frame may begin back
					(and head (scatter sound :rate (. head :find :rate) :size (. head :find :samples) :delay 0
						:before (mp3-before (. head :find :size))
						:make (lambda (sound) (scatter (mp3-decoder) :samples (. sound :find :size)))
						:read (if own (lambda (sound n) (reel-packet (. sound :find :reel) n)))
						;of an MP3 its pictures are its frames, kept as they are decoded
						:kept (if own (map (lambda (_) :nil) (range 0 +sound_kept)))
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
						:decode (lambda (decoder packet) (ac3-stereo decoder (ac3-frame decoder packet)))))))))))


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
			;of PCM of 16 bit stereo, the packet as it is
			(defq chans (sound-packet sound next) drop (. sound :find :drop)
				kept (. sound :find :kept)
				data (if (str? chans) (slice chans (* 4 drop) -1)
					(sound-pcm sound (first chans) (ifn (second chans) (first chans)) drop)))
			(audio-stream-write-rpc handle data)
			(if kept (elem-set kept (% next +sound_kept) (list next chans)))
			(defq got (if (str? chans) (- (/ (length chans) 4) drop) (- (length (first chans)) drop)))
			(setq written (+ written got) left (+ left got) next (inc next))
			(. sound :insert :drop 0))
		(scatter sound :written written :next next)))

(defun sound-kept (sound n)
	; (sound-kept sound n) -> :nil | (left right)
	;the samples of packet n, of a sound that keeps them, if it decoded it
	;lately
	(and (defq kept (. sound :find :kept))
		(defq got (elem-get kept (% n +sound_kept)))
		(= (first got) n)
		(second got)))

(defun sound-clock (sound)
	; (sound-clock sound) -> seconds
	;the time of the film that is being heard: where it started, and what
	;has been heard of what was written since, its whole seconds as numbers:
	;a fixed number divided is shifted up first, no more than 2^31 samples,
	;13 hours of 44100, and a book is longer
	(defq heard (- (. sound :find :written) (. sound :find :left)) rate (. sound :find :rate))
	(+ (. sound :find :time) (n2f (/ heard rate)) (/ (n2f (% heard rate)) (n2f rate))))

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
