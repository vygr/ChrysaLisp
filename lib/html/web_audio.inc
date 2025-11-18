;; Web Audio API
;; HTML5 Audio and Web Audio API implementation for ChrysaLisp

;; AudioContext - Main audio processing graph
(defclass audio-context nil
	(defq
		state "suspended"  ; "suspended", "running", "closed"
		destination nil
		sample_rate 44100
		current_time 0.0
		nodes (list)))

(defmethod :init audio-context ()
	(setq this-state "suspended"
		this-sample_rate 44100
		this-current_time 0.0
		this-nodes (list)
		this-destination (audio-destination-node :init this))
	this)

(defmethod :get-state audio-context ()
	; Get current state
	; audioCtx.state
	this-state)

(defmethod :resume audio-context ()
	; Resume audio processing
	; audioCtx.resume()
	(setq this-state "running")
	:nil)

(defmethod :suspend audio-context ()
	; Suspend audio processing
	; audioCtx.suspend()
	(setq this-state "suspended")
	:nil)

(defmethod :close audio-context ()
	; Close audio context
	; audioCtx.close()
	(setq this-state "closed")
	:nil)

(defmethod :get-destination audio-context ()
	; Get destination node (speakers)
	; audioCtx.destination
	this-destination)

(defmethod :get-sample-rate audio-context ()
	; Get sample rate
	; audioCtx.sampleRate
	this-sample_rate)

(defmethod :get-current-time audio-context ()
	; Get current time
	; audioCtx.currentTime
	this-current_time)

(defmethod :createOscillator audio-context ()
	; Create an oscillator node
	; var osc = audioCtx.createOscillator()
	(defq osc (oscillator-node :init this))
	(push this-nodes osc)
	osc)

(defmethod :createGain audio-context ()
	; Create a gain (volume) node
	; var gain = audioCtx.createGain()
	(defq gain (gain-node :init this))
	(push this-nodes gain)
	gain)

(defmethod :createBufferSource audio-context ()
	; Create a buffer source node
	; var source = audioCtx.createBufferSource()
	(defq source (buffer-source-node :init this))
	(push this-nodes source)
	source)

;; Base Audio Node
(defclass audio-node nil
	(defq
		context nil
		connections (list)))

(defmethod :init audio-node (ctx)
	(setq this-context ctx
		this-connections (list))
	this)

(defmethod :connect audio-node (destination)
	; Connect this node to another node
	; node.connect(destination)
	(push this-connections destination)
	destination)

(defmethod :disconnect audio-node ()
	; Disconnect all connections
	; node.disconnect()
	(setq this-connections (list))
	:nil)

;; Oscillator Node
(defclass oscillator-node audio-node
	(defq
		type "sine"  ; "sine", "square", "sawtooth", "triangle"
		frequency 440.0
		detune 0.0
		started :nil
		stopped :nil
		audio_handle :nil  ; Handle to synthesized audio file
		temp_file :nil))   ; Path to temp WAV file

(defmethod :init oscillator-node (ctx)
	(. (super) :init ctx)
	(setq this-type "sine"
		this-frequency 440.0
		this-detune 0.0
		this-started :nil
		this-stopped :nil
		this-audio_handle :nil
		this-temp_file :nil)
	this)

(defmethod :set-type oscillator-node (wave-type)
	; Set oscillator type
	; osc.type = "sine"
	(setq this-type wave-type)
	:nil)

(defmethod :get-type oscillator-node ()
	; Get oscillator type
	this-type)

(defmethod :set-frequency oscillator-node (freq)
	; Set frequency
	; osc.frequency.value = 440
	(setq this-frequency freq)
	:nil)

(defmethod :get-frequency oscillator-node ()
	; Get frequency
	this-frequency)

(defmethod :set-detune oscillator-node (cents)
	; Set detune in cents
	; osc.detune.value = 100
	(setq this-detune cents)
	:nil)

(defmethod :get-detune oscillator-node ()
	; Get detune
	this-detune)

(defmethod :start oscillator-node (&optional when)
	; Start oscillator
	; osc.start()
	(setq this-started :t)

	; Try to synthesize and play real audio
	(catch
		(progn
			(import "lib/audio/synthesis.inc")
			(import "lib/audio/simple.inc")

			; Generate temp filename
			(defq timestamp (time))
			(setq this-temp_file (cat "/tmp/osc_" timestamp "_" this-frequency ".wav"))

			; Synthesize tone (default 1 second duration)
			(defq duration 1.0)
			(when (synth-tone this-type this-frequency duration this-temp_file)
				; Load and play the synthesized audio
				(setq this-audio_handle (audio-load this-temp_file))
				(when (and this-audio_handle (>= this-audio_handle 0))
					(audio-play this-audio_handle)
					(print "[AUDIO] Oscillator started: " this-type " at " this-frequency "Hz (real audio)"))))
		(lambda (e)
			; Fallback to mock
			(print "[AUDIO] Oscillator started (mock): " this-type " at " this-frequency "Hz - " e)))
	:nil)

(defmethod :stop oscillator-node (&optional when)
	; Stop oscillator
	; osc.stop()
	(setq this-stopped :t)

	; Try to stop real audio
	(catch
		(progn
			(import "lib/audio/simple.inc")
			(when (and this-audio_handle (>= this-audio_handle 0))
				(audio-stop this-audio_handle)
				(print "[AUDIO] Oscillator stopped (real audio)")))
		(lambda (e)
			; Fallback to mock
			(print "[AUDIO] Oscillator stopped (mock)")))
	:nil)

;; Gain Node
(defclass gain-node audio-node
	(defq
		gain 1.0))

(defmethod :init gain-node (ctx)
	(. (super) :init ctx)
	(setq this-gain 1.0)
	this)

(defmethod :set-gain gain-node (value)
	; Set gain value
	; gain.gain.value = 0.5
	(setq this-gain value)
	:nil)

(defmethod :get-gain gain-node ()
	; Get gain value
	this-gain)

;; Audio Destination Node (speakers)
(defclass audio-destination-node audio-node)

(defmethod :init audio-destination-node (ctx)
	(. (super) :init ctx)
	this)

;; Buffer Source Node
(defclass buffer-source-node audio-node
	(defq
		buffer nil
		loop :nil
		playback_rate 1.0
		started :nil))

(defmethod :init buffer-source-node (ctx)
	(. (super) :init ctx)
	(setq this-buffer nil
		this-loop :nil
		this-playback_rate 1.0
		this-started :nil)
	this)

(defmethod :set-buffer buffer-source-node (buf)
	; Set audio buffer
	; source.buffer = buffer
	(setq this-buffer buf)
	:nil)

(defmethod :set-loop buffer-source-node (loop-enable)
	; Enable/disable looping
	; source.loop = true
	(setq this-loop loop-enable)
	:nil)

(defmethod :start buffer-source-node (&optional when)
	; Start playback
	; source.start()
	(setq this-started :t)
	(print "[AUDIO] Buffer source started")
	:nil)

;; HTML5 Audio Element
(defclass html-audio-element nil
	(defq
		src ""
		volume 1.0
		paused :t
		loop :nil
		autoplay :nil
		current_time 0.0
		duration 0.0
		handle :nil))  ; Audio handle from native system

(defmethod :init html-audio-element (source)
	(setq this-src (if source source "")
		this-volume 1.0
		this-paused :t
		this-loop :nil
		this-autoplay :nil
		this-current_time 0.0
		this-duration 0.0
		this-handle :nil)
	this)

(defmethod :play html-audio-element ()
	; Start playback
	; audio.play()
	(setq this-paused :nil)

	; Try to use native audio if available
	(catch
		(progn
			(import "lib/audio/simple.inc")
			; Load if not already loaded
			(unless this-handle
				(setq this-handle (audio-load this-src)))
			; Play the audio
			(when (and this-handle (>= this-handle 0))
				(audio-play this-handle)
				(print "[AUDIO] Playing: " this-src " (handle: " this-handle ")")))
		(lambda (e)
			; Fallback to mock playback
			(print "[AUDIO] Playing (mock): " this-src)))
	:nil)

(defmethod :pause html-audio-element ()
	; Pause playback
	; audio.pause()
	(setq this-paused :t)

	; Try to use native audio if available
	(catch
		(progn
			(import "lib/audio/simple.inc")
			(when (and this-handle (>= this-handle 0))
				(audio-pause this-handle)
				(print "[AUDIO] Paused: " this-src)))
		(lambda (e)
			; Fallback to mock
			(print "[AUDIO] Paused (mock): " this-src)))
	:nil)

(defmethod :load html-audio-element ()
	; Load audio file
	; audio.load()

	; Try to use native audio if available
	(catch
		(progn
			(import "lib/audio/simple.inc")
			(setq this-handle (audio-load this-src))
			(when (and this-handle (>= this-handle 0))
				(print "[AUDIO] Loaded: " this-src " (handle: " this-handle ")")))
		(lambda (e)
			; Fallback to mock
			(print "[AUDIO] Loading (mock): " this-src)))
	:nil)

(defmethod :set-volume html-audio-element (vol)
	; Set volume (0.0 to 1.0)
	; audio.volume = 0.5
	(setq this-volume (max 0.0 (min 1.0 vol)))
	:nil)

(defmethod :get-volume html-audio-element ()
	; Get volume
	this-volume)

(defmethod :set-current-time html-audio-element (time)
	; Set playback position
	; audio.currentTime = 10.5
	(setq this-current_time time)
	:nil)

(defmethod :get-current-time html-audio-element ()
	; Get playback position
	this-current_time)

(defmethod :get-duration html-audio-element ()
	; Get duration
	this-duration)

(defmethod :get-paused html-audio-element ()
	; Check if paused
	this-paused)

;; Helper Functions

(defun AudioContext ()
	; Create new AudioContext
	; var audioCtx = new AudioContext()
	(audio-context :init))

(defun create-html-audio (&optional src)
	; Create HTML5 audio element
	; var audio = new Audio("sound.wav")
	(html-audio-element :init src))

