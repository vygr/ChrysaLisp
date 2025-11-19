
;; Web Audio API Tests
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/script.inc")

(deftest-suite "Web Audio API Tests")

; Test 1: Create AudioContext
(deftest "Create AudioContext"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq hasContext (if audioCtx :t :nil))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "hasContext")))

; Test 2: AudioContext state
(deftest "AudioContext State"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq state (. audioCtx :get-state))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "suspended" (. ctx :get-global "state")))

; Test 3: Resume AudioContext
(deftest "Resume AudioContext"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(. audioCtx :resume)
			(defq state (. audioCtx :get-state))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "running" (. ctx :get-global "state")))

; Test 4: Suspend AudioContext
(deftest "Suspend AudioContext"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(. audioCtx :resume)
			(. audioCtx :suspend)
			(defq state (. audioCtx :get-state))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "suspended" (. ctx :get-global "state")))

; Test 5: Create Oscillator
(deftest "Create Oscillator"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(defq hasOsc (if osc :t :nil))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "hasOsc")))

; Test 6: Oscillator type
(deftest "Oscillator Type"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(. osc :set-type \"sine\")
			(defq type (. osc :get-type))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "sine" (. ctx :get-global "type")))

; Test 7: Oscillator frequency
(deftest "Oscillator Frequency"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(. osc :set-frequency 440)
			(defq freq (. osc :get-frequency))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq 440 (. ctx :get-global "freq")))

; Test 8: Create GainNode
(deftest "Create GainNode"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq gain (. audioCtx :createGain))
			(defq hasGain (if gain :t :nil))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "hasGain")))

; Test 9: GainNode value
(deftest "GainNode Value"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq gain (. audioCtx :createGain))
			(. gain :set-gain 0.5)
			(defq value (. gain :get-gain))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq 0.5 (. ctx :get-global "value")))

; Test 10: Connect nodes
(deftest "Connect Nodes"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(defq gain (. audioCtx :createGain))
			(. osc :connect gain)
			(defq connected :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "connected")))

; Test 11: Connect to destination
(deftest "Connect To Destination"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(defq dest (. audioCtx :get-destination))
			(. osc :connect dest)
			(defq hasDest (if dest :t :nil))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "hasDest")))

; Test 12: Start oscillator
(deftest "Start Oscillator"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(. osc :start)
			(defq started :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "started")))

; Test 13: Stop oscillator
(deftest "Stop Oscillator"
	(defq html "
		<script>
			(defq audioCtx (AudioContext))
			(defq osc (. audioCtx :createOscillator))
			(. osc :start)
			(. osc :stop)
			(defq stopped :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "stopped")))

; Test 14: Create Audio element
(deftest "Create Audio Element"
	(defq html "<audio src=\"sound.wav\" id=\"myAudio\"></audio>")
	(defq doc (parse-html html))
	(defq audios (. doc :get-elements-by-tag-name "audio"))

	(assert-eq 1 (length audios))
	(defq audio (elem 0 audios))
	(assert-eq "sound.wav" (. audio :get-attribute "src")))

; Test 15: Audio element playback control
(deftest "Audio Element Playback"
	(defq html "
		<audio id=\"myAudio\" src=\"sound.wav\"></audio>
		<script>
			(defq audio (. document :get-element-by-id \"myAudio\"))
			(. audio :play)
			(defq playing :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "playing")))

; Test 16: Audio element pause
(deftest "Audio Element Pause"
	(defq html "
		<audio id=\"myAudio\" src=\"sound.wav\"></audio>
		<script>
			(defq audio (. document :get-element-by-id \"myAudio\"))
			(. audio :play)
			(. audio :pause)
			(defq paused :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "paused")))

; Test 17: Audio element volume
(deftest "Audio Element Volume"
	(defq html "
		<audio id=\"myAudio\" src=\"sound.wav\"></audio>
		<script>
			(defq audio (. document :get-element-by-id \"myAudio\"))
			(. audio :set-volume 0.7)
			(defq vol (. audio :get-volume))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq 0.7 (. ctx :get-global "vol")))

; Test 18: HTML5 Audio with autoplay
(deftest "Audio With Autoplay Attribute"
	(defq html "<audio src=\"music.mp3\" autoplay></audio>")
	(defq doc (parse-html html))
	(defq audios (. doc :get-elements-by-tag-name "audio"))

	(assert-eq 1 (length audios))
	(defq audio (elem 0 audios))
	(assert-eq :t (. audio :has-attribute "autoplay")))

; Test 19: HTML5 Audio with loop
(deftest "Audio With Loop Attribute"
	(defq html "<audio src=\"music.mp3\" loop></audio>")
	(defq doc (parse-html html))
	(defq audios (. doc :get-elements-by-tag-name "audio"))

	(assert-eq 1 (length audios))
	(defq audio (elem 0 audios))
	(assert-eq :t (. audio :has-attribute "loop")))

; Test 20: Audio onclick integration
(deftest "Audio Click Integration"
	(defq html "
		<audio id=\"myAudio\" src=\"sound.wav\"></audio>
		<button id=\"playBtn\" onclick=\"(. (. document :get-element-by-id \\\"myAudio\\\") :play)\">Play</button>
		<script>
			(defq audio (. document :get-element-by-id \"myAudio\"))
			(defq button (. document :get-element-by-id \"playBtn\"))
			(defq setup :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :t (. ctx :get-global "setup")))


; Report test results
(test-report)
