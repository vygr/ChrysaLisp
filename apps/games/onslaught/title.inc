;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Title Sequence
; Flying-in letters, dropping sword, dripping blood
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/audio/app.inc")
(import "./enums.inc")
(import "./components.inc")
(import "./sprite.inc")
(import "./widgets.inc")

(unless (def? '*app_root*)
	(defq *app_root* (path-to-file)))

(defq
	*title_letter_idx* 0
	*title_delay* 0
	*title_state* :letters ; :letters, :delay, :sword, :drip, :done
	*snd_explode* :nil
	*snd_clash* :nil
	*snd_drip* :nil)

(defun init-title-audio ()
	(catch (setq *snd_explode* (audio-add-rpc (cat *app_root* "sfx/explode.wav"))) :nil)
	(catch (setq *snd_clash* (audio-add-rpc (cat *app_root* "sfx/clash1.wav"))) :nil)
	(catch (setq *snd_drip* (audio-add-rpc (cat *app_root* "sfx/drip.wav"))) :nil))

(defun play-title-sfx (handle)
	(when handle
		(catch (audio-play-rpc handle) :nil)))

; Vertically centered in the 168-pixel play area (0 to 168):
; (168 - 32) / 2 = 68 pixels from top, exactly 68 pixels above panel!
(defq +title_letter_destinations
	''((0 68)
	  (36 68)
	  (72 68)
	  (108 68)
	  (144 68)
	  (180 68)
	  (216 68)
	  (252 68)
	  (288 68)))

(defun get-xy-outer (w h)
	; Pick a random start point just outside the screen
	(case (random 4)
		(0 (list (- (random (+ +game_width (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ +game_width (* 2 w))) w) +game_height))
		(2 (list (neg w) (- (random (+ +game_height (* 2 h))) h)))
		(3 (list +game_width (- (random (+ +game_height (* 2 h))) h)))))

(defun spawn-title-letter (idx)
	(bind '(tx ty) (elem-get +title_letter_destinations idx))
	(bind '(sx sy) (get-xy-outer 32 32))
	(defq sp (TitleLetter idx sx sy))
	(. sp :fly_to tx ty 16 (# (on-title-letter-arrived %0)))
	(. *layer_sprites* :add_front sp)
	(. sp :dirty)
	sp)

(defun on-title-letter-arrived (sp)
	(play-title-sfx *snd_explode*)
	(setq *title_letter_idx* (+ *title_letter_idx* 1))
	(if (< *title_letter_idx* 9)
		(spawn-title-letter *title_letter_idx*)
		(progn
			(setq *title_state* :delay)
			(setq *title_delay* 16))))

(defun spawn-title-sword ()
	; Drops through center of 'A' (x=144..176) to y=52 (hilt at 52, tip at 116)
	(defq sp (TitleSword 152 -64))
	(. sp :drop_to 52 14 (# (on-title-sword-arrived %0)))
	(. *layer_fx* :add_back sp)
	(. sp :dirty)
	sp)

(defun on-title-sword-arrived (sp)
	(play-title-sfx *snd_clash*)
	(setq *title_state* :drip)
	(spawn-title-drip))

(defun spawn-title-drip ()
	; Drips from sword tip (y=116) down to top of status panel (y=168)
	(defq sp (TitleBloodDrip 152 116))
	(. sp :set_drip 116 168 2
		(lambda (&rest _) (play-title-sfx *snd_drip*))
		(lambda (&rest _) (on-title-drip-done)))
	(. *layer_fx* :add_front sp)
	(. sp :dirty)
	sp)

(defun on-title-drip-done ()
	(setq *title_state* :done))

(defun title-sequence-start (&optional layer)
	(setq *title_letter_idx* 0
		*title_delay* 0
		*title_state* :letters)
	(init-title-audio)
	(spawn-title-letter 0))

(defun title-sequence-update (layer dt)
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_sprites* :children)))
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_fx* :children)))
	(when (eql *title_state* :delay)
		(setq *title_delay* (- *title_delay* 1))
		(when (<= *title_delay* 0)
			(setq *title_state* :sword)
			(spawn-title-sword))))
