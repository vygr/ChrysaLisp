;a film, or a sound, of any kind there is a reel for, lib/media/reel.inc:
;a frame a tenth of the way in small, the first is often black, and how
;long it is; of a reel read only for its start, its first frame. Decoded
;at the least size that is no narrower than the preview, the codec let go
;of once it is copied
(file-previewer
	(lambda (file) (some (# (ends-with (first %0) file)) *reel_kinds*))
	(lambda (file most_w most_h)
		;as much as shows its start, a big film is not read all through
		(when (defq reel (reel-open file (name-stream file) :t))
			(defq scale (max 1 (min 4 (/ (ifn (. reel :find :width) most_w) most_w)))
				view (Flow) rate (. reel :find :rate) frame :nil)
			(when (defq codec (reel-codec reel scale))
				(if (defq canvas (reel-frame codec (if (. reel :find :partial) 0 (/ (. reel :find :frames) 10))))
					(setq frame (. (canvas-fit canvas most_w most_h) :swap 0)))
				(reel-close codec))
			(def view :flow_flags +flow_down_fill :min_width most_w)
			(if frame (. view :add_child frame))
			(when (and (> rate 0.0) (not (. reel :find :partial)))
				(def (defq label (Label)) :text (reel-clock (/ (n2f (. reel :find :frames)) rate)))
				(. view :add_child label))
			(if (nempty? (. view :children)) view))))
