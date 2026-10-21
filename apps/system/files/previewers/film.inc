;a film, or a sound, of any kind there is a reel for, lib/media/reel.inc:
;its first frame small, and how long it is. Decoded at the least size that
;is no narrower than the preview, the codec let go of once it is copied
(file-previewer
	(lambda (file) (some (# (ends-with (first %0) file)) *reel_kinds*))
	(lambda (file most_w most_h)
		(when (defq reel (reel-open file))
			(defq scale (max 1 (min 4 (/ (ifn (. reel :find :width) most_w) most_w)))
				view (Flow) rate (. reel :find :rate) frame :nil)
			(when (defq codec (reel-codec reel scale))
				(if (defq canvas (reel-frame codec 0))
					(setq frame (. (canvas-fit canvas most_w most_h) :swap 0)))
				(reel-close codec))
			(def view :flow_flags +flow_down_fill :min_width most_w)
			(if frame (. view :add_child frame))
			(when (> rate 0.0)
				(defq s (n2i (/ (n2f (. reel :find :frames)) rate)))
				(def (defq label (Label)) :text (cat (str (/ s 3600)) ":" (pad (str (% (/ s 60) 60)) 2 "0")
					":" (pad (str (% s 60)) 2 "0")))
				(. view :add_child label))
			(if (nempty? (. view :children)) view))))
