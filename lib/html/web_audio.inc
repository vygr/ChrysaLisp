;; Web Audio API
;; HTML5 Audio and Web Audio API implementation for ChrysaLisp

(import "lib/class/class.inc")

;; AudioContext - Main audio processing graph
(defclass audio-context () :nil
	(def this :state "suspended")  ; "suspended", "running", "closed"
	(def this :destination :nil)
	(def this :sample_rate 44100)
	(def this :current_time 0.0)
	(def this :nodes (list))

	(defmethod :init ()
		(def this :state "suspended")
		(def this :sample_rate 44100)
		(def this :current_time 0.0)
		(def this :nodes (list))
		(def this :destination (audio-destination-node :init this))
		this)

	(defmethod :get-state ()
		; Get current state
		; audioCtx.state
		(get :state this))

	(defmethod :resume ()
		; Resume audio processing
		; audioCtx.resume()
		(def this :state "running")
		:nil)

	(defmethod :suspend ()
		; Suspend audio processing
		; audioCtx.suspend()
		(def this :state "suspended")
		:nil)

	(defmethod :close ()
		; Close audio context
		; audioCtx.close()
		(def this :state "closed")
		:nil)

	(defmethod :get-destination ()
		; Get destination node (speakers)
		; audioCtx.destination
		(get :destination this))

	(defmethod :get-sample-rate ()
		; Get sample rate
		; audioCtx.sampleRate
		(get :sample_rate this))

	(defmethod :get-current-time ()
		; Get current time
		; audioCtx.currentTime
		(get :current_time this))

	(defmethod :createOscillator ()
		; Create an oscillator node
		; var osc = audioCtx.createOscillator()
		(defq osc (oscillator-node :init this))
		(defq nodes (get :nodes this))
		(push nodes osc)
		(def this :nodes nodes)
		osc)

	(defmethod :createGain ()
		; Create a gain (volume) node
		; var gain = audioCtx.createGain()
		(defq gain (gain-node :init this))
		(defq nodes (get :nodes this))
		(push nodes gain)
		(def this :nodes nodes)
		gain)

	(defmethod :createBufferSource ()
		; Create a buffer source node
		; var source = audioCtx.createBufferSource()
		(defq source (buffer-source-node :init this))
		(defq nodes (get :nodes this))
		(push nodes source)
		(def this :nodes nodes)
		source))

;; Base Audio Node
(defclass audio-node () :nil
	(def this :context :nil)
	(def this :connections (list))

	(defmethod :init (ctx)
		(def this :context ctx)
		(def this :connections (list))
		this)

	(defmethod :connect (destination)
		; Connect this node to another node
		; node.connect(destination)
		(defq connections (get :connections this))
		(push connections destination)
		(def this :connections connections)
		destination)

	(defmethod :disconnect ()
		; Disconnect all connections
		; node.disconnect()
		(def this :connections (list))
		:nil))

;; Oscillator Node
(defclass oscillator-node () (audio-node)
	(def this :type "sine")  ; "sine", "square", "sawtooth", "triangle"
	(def this :frequency 440.0)
	(def this :detune 0.0)
	(def this :started :nil)
	(def this :stopped :nil)
	(def this :audio_handle :nil)  ; Handle to synthesized audio file
	(def this :temp_file :nil)   ; Path to temp WAV file

	(defmethod :init (ctx)
		((const (super)) this ctx)
		(def this :type "sine")
		(def this :frequency 440.0)
		(def this :detune 0.0)
		(def this :started :nil)
		(def this :stopped :nil)
		(def this :audio_handle :nil)
		(def this :temp_file :nil)
		this)

	(defmethod :set-type (wave-type)
		; Set oscillator type
		; osc.type = "sine"
		(def this :type wave-type)
		:nil)

	(defmethod :get-type ()
		; Get oscillator type
		(get :type this))

	(defmethod :set-frequency (freq)
		; Set frequency
		; osc.frequency.value = 440
		(def this :frequency freq)
		:nil)

	(defmethod :get-frequency ()
		; Get frequency
		(get :frequency this))

	(defmethod :set-detune (cents)
		; Set detune in cents
		; osc.detune.value = 100
		(def this :detune cents)
		:nil)

	(defmethod :get-detune ()
		; Get detune
		(get :detune this))

	(defmethod :start (&optional when)
		; Start oscillator
		; osc.start()
		(def this :started :t)

		; Try to synthesize and play real audio
		(catch
			(progn
				(import "lib/audio/synthesis.inc")
				(import "lib/audio/simple.inc")

				; Generate temp filename
				(defq timestamp (time))
				(defq frequency (get :frequency this))
				(defq type (get :type this))
				(defq temp_file (cat "/tmp/osc_" timestamp "_" frequency ".wav"))
				(def this :temp_file temp_file)

				; Synthesize tone (default 1 second duration)
				(defq duration 1.0)
				(when (synth-tone type frequency duration temp_file)
					; Load and play the synthesized audio
					(defq audio_handle (audio-load temp_file))
					(def this :audio_handle audio_handle)
					(when (and audio_handle (>= audio_handle 0))
						(audio-play audio_handle)
						(print "[AUDIO] Oscillator started: " type " at " frequency "Hz (real audio)"))))
			(lambda (e)
				; Fallback to mock
				(defq frequency (get :frequency this))
				(defq type (get :type this))
				(print "[AUDIO] Oscillator started (mock): " type " at " frequency "Hz - " e)))
		:nil)

	(defmethod :stop (&optional when)
		; Stop oscillator
		; osc.stop()
		(def this :stopped :t)

		; Try to stop real audio
		(catch
			(progn
				(import "lib/audio/simple.inc")
				(defq audio_handle (get :audio_handle this))
				(when (and audio_handle (>= audio_handle 0))
					(audio-stop audio_handle)
					(print "[AUDIO] Oscillator stopped (real audio)")))
			(lambda (e)
				; Fallback to mock
				(print "[AUDIO] Oscillator stopped (mock)")))
		:nil))

;; Gain Node
(defclass gain-node () (audio-node)
	(def this :gain 1.0)

	(defmethod :init (ctx)
		((const (super)) this ctx)
		(def this :gain 1.0)
		this)

	(defmethod :set-gain (value)
		; Set gain value
		; gain.gain.value = 0.5
		(def this :gain value)
		:nil)

	(defmethod :get-gain ()
		; Get gain value
		(get :gain this)))

;; Audio Destination Node (speakers)
(defclass audio-destination-node () (audio-node)
	(defmethod :init (ctx)
		((const (super)) this ctx)
		this))

;; Buffer Source Node
(defclass buffer-source-node () (audio-node)
	(def this :buffer :nil)
	(def this :loop :nil)
	(def this :playback_rate 1.0)
	(def this :started :nil)

	(defmethod :init (ctx)
		((const (super)) this ctx)
		(def this :buffer :nil)
		(def this :loop :nil)
		(def this :playback_rate 1.0)
		(def this :started :nil)
		this)

	(defmethod :set-buffer (buf)
		; Set audio buffer
		; source.buffer = buffer
		(def this :buffer buf)
		:nil)

	(defmethod :set-loop (loop-enable)
		; Enable/disable looping
		; source.loop = true
		(def this :loop loop-enable)
		:nil)

	(defmethod :start (&optional when)
		; Start playback
		; source.start()
		(def this :started :t)
		(print "[AUDIO] Buffer source started")
		:nil))

;; HTML5 Audio Element
(defclass html-audio-element () :nil
	(def this :src "")
	(def this :volume 1.0)
	(def this :paused :t)
	(def this :loop :nil)
	(def this :autoplay :nil)
	(def this :current_time 0.0)
	(def this :duration 0.0)
	(def this :handle :nil)  ; Audio handle from native system

	(defmethod :init (source)
		(def this :src (if source source ""))
		(def this :volume 1.0)
		(def this :paused :t)
		(def this :loop :nil)
		(def this :autoplay :nil)
		(def this :current_time 0.0)
		(def this :duration 0.0)
		(def this :handle :nil)
		this)

	(defmethod :play ()
		; Start playback
		; audio.play()
		(def this :paused :nil)

		; Try to use native audio if available
		(catch
			(progn
				(import "lib/audio/simple.inc")
				; Load if not already loaded
				(defq handle (get :handle this))
				(unless handle
					(defq src (get :src this))
					(defq handle (audio-load src))
					(def this :handle handle))
				; Play the audio
				(when (and handle (>= handle 0))
					(audio-play handle)
					(defq src (get :src this))
					(print "[AUDIO] Playing: " src " (handle: " handle ")")))
			(lambda (e)
				; Fallback to mock playback
				(defq src (get :src this))
				(print "[AUDIO] Playing (mock): " src)))
		:nil)

	(defmethod :pause ()
		; Pause playback
		; audio.pause()
		(def this :paused :t)

		; Try to use native audio if available
		(catch
			(progn
				(import "lib/audio/simple.inc")
				(defq handle (get :handle this))
				(when (and handle (>= handle 0))
					(audio-pause handle)
					(defq src (get :src this))
					(print "[AUDIO] Paused: " src)))
			(lambda (e)
				; Fallback to mock
				(defq src (get :src this))
				(print "[AUDIO] Paused (mock): " src)))
		:nil)

	(defmethod :load ()
		; Load audio file
		; audio.load()

		; Try to use native audio if available
		(catch
			(progn
				(import "lib/audio/simple.inc")
				(defq src (get :src this))
				(defq handle (audio-load src))
				(def this :handle handle)
				(when (and handle (>= handle 0))
					(print "[AUDIO] Loaded: " src " (handle: " handle ")")))
			(lambda (e)
				; Fallback to mock
				(defq src (get :src this))
				(print "[AUDIO] Loading (mock): " src)))
		:nil)

	(defmethod :set-volume (vol)
		; Set volume (0.0 to 1.0)
		; audio.volume = 0.5
		(def this :volume (max 0.0 (min 1.0 vol)))
		:nil)

	(defmethod :get-volume ()
		; Get volume
		(get :volume this))

	(defmethod :set-current-time (time)
		; Set playback position
		; audio.currentTime = 10.5
		(def this :current_time time)
		:nil)

	(defmethod :get-current-time ()
		; Get playback position
		(get :current_time this))

	(defmethod :get-duration ()
		; Get duration
		(get :duration this))

	(defmethod :get-paused ()
		; Check if paused
		(get :paused this)))

;; Helper Functions

(defun AudioContext ()
	; Create new AudioContext
	; var audioCtx = new AudioContext()
	(audio-context :init))

(defun create-html-audio (&optional src)
	; Create HTML5 audio element
	; var audio = new Audio("sound.wav")
	(html-audio-element :init src))
