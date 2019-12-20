;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(defq id t index 0
	frames (map (lambda (_) (canvas-load (cat "apps/background/orangewp1280x1080.cpm") load_flag_shared)) (range 1 2)))

(ui-tree view (create-view) nil
  (ui-element backdrop (create-backdrop)
  ('color argb_black 'ink_color argb_white)
    (ui-element frame (elem 0 frames))))

(defq screen (penv (gui-add view)))

(while id
	(bind '(_ _ screen_width screen_height) (view-get-bounds screen))
	(defq index 0
		frame (elem index frames))
	(view-set-bounds frame 0 0 1280 1080)
	(view-add-back view frame)
	(view-change-dirty view 0 0 screen_width screen_height)
	(task-sleep 1000000))

(view-hide view)
