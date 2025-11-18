;; Simple Audio Library
;; High-level wrapper for ChrysaLisp's audio RPC system

(import "service/audio/app.inc")

;; Audio Handle Management
(defclass simple-audio nil
	(defq
		loaded_files (env)  ; Map of filepath -> handle
		playing (list)))

(defmethod :init simple-audio ()
	(setq this-loaded_files (env)
		this-playing (list))
	this)

(defmethod :load simple-audio (filepath)
	; Load audio file and return handle
	; Returns handle or :nil on error
	(when (and filepath (path? filepath))
		; Check if already loaded
		(defq handle (get this-loaded_files (sym filepath)))
		(unless handle
			; Load new file
			(setq handle (audio-add-rpc filepath))
			(when (and handle (>= handle 0))
				(set-insert this-loaded_files (sym filepath) handle)
				handle))
		handle))

(defmethod :play simple-audio (filepath-or-handle)
	; Play audio file or handle
	(defq handle :nil)
	(cond
		; If it's a number, use as handle
		((num? filepath-or-handle)
			(setq handle filepath-or-handle))

		; If it's a string, load and get handle
		((str? filepath-or-handle)
			(setq handle (. this :load filepath-or-handle))))

	(when (and handle (>= handle 0))
		(audio-play-rpc handle)
		(push this-playing handle)
		handle))

(defmethod :pause simple-audio (handle)
	; Pause playback
	(when (and handle (>= handle 0))
		(audio-change-rpc handle 1)))  ; 1 = pause

(defmethod :resume simple-audio (handle)
	; Resume playback
	(when (and handle (>= handle 0))
		(audio-change-rpc handle 0)))  ; 0 = resume

(defmethod :stop simple-audio (handle)
	; Stop playback
	(when (and handle (>= handle 0))
		(audio-change-rpc handle -1)))  ; -1 = stop

(defmethod :unload simple-audio (filepath)
	; Unload audio file
	(defq handle (get this-loaded_files (sym filepath)))
	(when handle
		(audio-remove-rpc handle)
		(set-remove this-loaded_files (sym filepath))))

(defmethod :unload-all simple-audio ()
	; Unload all audio files
	(each! 0 -1 (lambda ((key value))
		(audio-remove-rpc value))
		(tolist this-loaded_files))
	(setq this-loaded_files (env)))

;; Global audio manager
(defq *audio-manager* (simple-audio :init))

;; Convenience functions
(defun audio-load (filepath)
	; Load audio file
	; (audio-load "sound.wav") -> handle
	(. *audio-manager* :load filepath))

(defun audio-play (filepath-or-handle)
	; Play audio file or handle
	; (audio-play "sound.wav") or (audio-play handle)
	(. *audio-manager* :play filepath-or-handle))

(defun audio-pause (handle)
	; Pause playback
	; (audio-pause handle)
	(. *audio-manager* :pause handle))

(defun audio-resume (handle)
	; Resume playback
	; (audio-resume handle)
	(. *audio-manager* :resume handle))

(defun audio-stop (handle)
	; Stop playback
	; (audio-stop handle)
	(. *audio-manager* :stop handle))

