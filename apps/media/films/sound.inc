;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the sound of a film: its AAC track decoded a packet at a time, a little
; ahead of what is heard, into a stream of the Audio service, 16 bit
; stereo at its rate. What has been heard is the film's clock, and the
; pictures are kept to it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/audio/app.inc")
(import "lib/media/aac.inc")

;how far ahead of what is heard the stream is kept, in seconds
(defq +sound_ahead 0.25)

(defun sound-of (reel)
	; (sound-of reel) -> :nil | sound
	;the AAC-LC track of a film of an mp4, if it has one and it is that
	(and (defq tracks (. reel :find :tracks))
		(defq track (some (# (and (eql (. %0 :find :kind) "soun") (eql (. %0 :find :codec) "mp4a") %0)) tracks))
		(progn (mp4-tables track) (defq config (aac-track-config track)))
		(= (first config) 2)
		(scatter (Emap) :track track :config config :rate (third config) :stream (. reel :find :stream)
			:start (. track :find :start) :handle :nil :written 0 :left 0 :time 0.0)))

(defun sound-stop (sound)
	; (sound-stop sound)
	(when (and sound (. sound :find :handle))
		(audio-stream-close-rpc (. sound :find :handle))
		(. sound :insert :handle :nil)))

(defun sound-packet (sound n)
	;the samples of packet n of the track, a list of reals a channel
	(defq track (. sound :find :track) stream (. sound :find :stream))
	(stream-seek stream (elem-get (. track :find :offsets) n) 0)
	(aac-decode (. sound :find :decoder) (read-blk stream (elem-get (. track :find :sizes) n))))

(defun sound-start (sound time)
	; (sound-start sound seconds)
	;heard from a time of the film: the packet that has it decoded after the
	;one before, which only fills the overlap, so what comes out first is
	;that packet's, and what is before the time in it left out
	(sound-stop sound)
	(defq rate (. sound :find :rate) sample (+ (n2i (* time (n2f rate))) (. sound :find :start))
		packet (inc (/ (max sample 0) 1024)))
	(. sound :insert :decoder (aac-decoder (. sound :find :config)))
	(if (< (dec packet) (. (. sound :find :track) :find :samples)) (sound-packet sound (dec packet)))
	(scatter sound :next packet :drop (- (max sample 0) (* (dec packet) 1024)) :time time
		:written 0 :left 0 :anchor (pii-time) :handle (audio-stream-open-rpc rate 2)))

(defun sound-pcm (left right from)
	;16 bit stereo, left first, from a frame on
	(apply (const cat) (map! (lambda (l r)
			(cat (char (logand (max -32768 (min 32767 (n2i l))) 0xffff) 2)
				(char (logand (max -32768 (min 32767 (n2i r))) 0xffff) 2)))
		(list left right) from -1)))

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
				data (sound-pcm (first chans) (ifn (second chans) (first chans)) drop))
			(audio-stream-write-rpc handle data)
			(setq written (+ written (- 1024 drop)) left (+ left (- 1024 drop)) next (inc next))
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
