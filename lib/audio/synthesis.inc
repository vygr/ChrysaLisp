;; Audio Synthesis Library
;; Generate waveforms for oscillators

(import "lib/math/math.inc")

;; Constants
(defq
	SAMPLE_RATE 44100
	TWO_PI (* 2.0 3.14159265359))

;; Waveform Generation

(defun generate-sine-wave (frequency duration &optional (amplitude 0.5))
	; Generate sine wave samples
	; Returns list of samples (-1.0 to 1.0)
	(defq samples (list)
		num-samples (* SAMPLE_RATE duration)
		angular-freq (/ (* TWO_PI frequency) SAMPLE_RATE))

	(times num-samples
		(lambda (i)
			(defq phase (* i angular-freq))
			(defq sample (* amplitude (sin phase)))
			(push samples sample)))

	samples)

(defun generate-square-wave (frequency duration &optional (amplitude 0.5))
	; Generate square wave samples
	; Returns list of samples (-1.0 to 1.0)
	(defq samples (list)
		num-samples (* SAMPLE_RATE duration)
		period (/ SAMPLE_RATE frequency)
		half-period (/ period 2.0))

	(times num-samples
		(lambda (i)
			(defq phase (mod i period))
			(defq sample (if (< phase half-period) amplitude (neg amplitude)))
			(push samples sample)))

	samples)

(defun generate-sawtooth-wave (frequency duration &optional (amplitude 0.5))
	; Generate sawtooth wave samples
	; Returns list of samples (-1.0 to 1.0)
	(defq samples (list)
		num-samples (* SAMPLE_RATE duration)
		period (/ SAMPLE_RATE frequency))

	(times num-samples
		(lambda (i)
			(defq phase (mod i period))
			(defq normalized (/ phase period))  ; 0.0 to 1.0
			(defq sample (* amplitude (- (* 2.0 normalized) 1.0)))  ; -1.0 to 1.0
			(push samples sample)))

	samples)

(defun generate-triangle-wave (frequency duration &optional (amplitude 0.5))
	; Generate triangle wave samples
	; Returns list of samples (-1.0 to 1.0)
	(defq samples (list)
		num-samples (* SAMPLE_RATE duration)
		period (/ SAMPLE_RATE frequency))

	(times num-samples
		(lambda (i)
			(defq phase (mod i period))
			(defq normalized (/ phase period))  ; 0.0 to 1.0
			; Triangle: rises from -1 to 1, then falls from 1 to -1
			(defq sample
				(if (< normalized 0.5)
					(* amplitude (- (* 4.0 normalized) 1.0))  ; Rising
					(* amplitude (- 3.0 (* 4.0 normalized)))))  ; Falling
			(push samples sample)))

	samples)

;; WAV File Generation

(defun float-to-int16 (sample)
	; Convert float sample (-1.0 to 1.0) to 16-bit int (-32768 to 32767)
	(defq scaled (* sample 32767.0))
	(max -32768 (min 32767 (int scaled))))

(defun int16-to-bytes (value)
	; Convert 16-bit int to little-endian bytes
	(defq low (logand value 0xFF))
	(defq high (logand (>> value 8) 0xFF))
	(list low high))

(defun int32-to-bytes (value)
	; Convert 32-bit int to little-endian bytes
	(defq b0 (logand value 0xFF))
	(defq b1 (logand (>> value 8) 0xFF))
	(defq b2 (logand (>> value 16) 0xFF))
	(defq b3 (logand (>> value 24) 0xFF))
	(list b0 b1 b2 b3))

(defun samples-to-wav-data (samples sample-rate)
	; Convert audio samples to WAV file format (as byte list)
	; samples: list of float values (-1.0 to 1.0)
	; Returns: list of bytes representing WAV file

	(defq num-samples (length samples)
		num-channels 1  ; Mono
		bits-per-sample 16
		byte-rate (* sample-rate num-channels (/ bits-per-sample 8))
		block-align (* num-channels (/ bits-per-sample 8))
		data-size (* num-samples num-channels (/ bits-per-sample 8))
		file-size (+ 36 data-size))

	; Build WAV file byte array
	(defq wav-data (list))

	; RIFF header
	(append wav-data (list 82 73 70 70))  ; "RIFF"
	(append wav-data (int32-to-bytes (- file-size 8)))  ; File size - 8
	(append wav-data (list 87 65 86 69))  ; "WAVE"

	; fmt chunk
	(append wav-data (list 102 109 116 32))  ; "fmt "
	(append wav-data (int32-to-bytes 16))  ; Chunk size
	(append wav-data (list 1 0))  ; Audio format (1 = PCM)
	(append wav-data (list num-channels 0))  ; Number of channels
	(append wav-data (int32-to-bytes sample-rate))  ; Sample rate
	(append wav-data (int32-to-bytes byte-rate))  ; Byte rate
	(append wav-data (list block-align 0))  ; Block align
	(append wav-data (list bits-per-sample 0))  ; Bits per sample

	; data chunk
	(append wav-data (list 100 97 116 97))  ; "data"
	(append wav-data (int32-to-bytes data-size))  ; Data size

	; Sample data
	(each! 0 -1 (lambda (sample)
		(defq int-sample (float-to-int16 sample))
		(append wav-data (int16-to-bytes int-sample)))
		samples)

	wav-data)

(defun write-wav-file (filename samples sample-rate)
	; Write samples to WAV file
	; samples: list of float values (-1.0 to 1.0)
	; sample-rate: samples per second (e.g., 44100)

	(defq wav-data (samples-to-wav-data samples sample-rate))

	; Write to file
	(when (defq stream (file-stream filename +file_open_write))
		; Convert byte list to string and write
		(defq byte-string (apply cat (map (lambda (b) (char b)) wav-data)))
		(write stream byte-string)
		(close stream)
		:t))

;; High-level synthesizer functions

(defun synth-tone (waveform frequency duration &optional (filename "/tmp/synth_tone.wav"))
	; Synthesize a tone and save to file
	; waveform: "sine", "square", "sawtooth", "triangle"
	; frequency: Hz (e.g., 440 for A4)
	; duration: seconds
	; Returns: filename on success, :nil on error

	(defq samples
		(cond
			((= waveform "sine") (generate-sine-wave frequency duration))
			((= waveform "square") (generate-square-wave frequency duration))
			((= waveform "sawtooth") (generate-sawtooth-wave frequency duration))
			((= waveform "triangle") (generate-triangle-wave frequency duration))
			(:t (generate-sine-wave frequency duration))))  ; Default to sine

	(when (write-wav-file filename samples SAMPLE_RATE)
		filename))

