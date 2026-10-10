;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-minimise ()
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	;as big as shows all of the board, or as will fit
	(bind '(cw ch) (canvas-size))
	(def *image_scroll* :min_width cw :min_height ch)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h)
	(def *image_scroll* :min_width +min_width :min_height +min_height))

(defun action-undo () (. *board* :undo))
(defun action-redo () (. *board* :redo))
(defun action-clear () (. *board* :clear))
(defun action-crop () (. *board* :crop))

(defun action-new ()
	;an empty board, of the size the size field says
	(bind '(w h) (field-size))
	(. *board* :set_doc (cwb-doc w h))
	(setq *file* :nil)
	(board-resized))

(defun action-size ()
	;the board is the size the size field says, what is on it stays
	(bind '(w h) (field-size))
	(defq doc (. *board* :get_doc))
	(unless (and (= w (. doc :find :width)) (= h (. doc :find :height)))
		(. doc :insert :width w)
		(. doc :insert :height h)
		(board-resized)))

(defun action-zoom-in () (step-zoom 1))
(defun action-zoom-out () (step-zoom -1))

(defun action-ink ()
	(def *board* :color (elem-get *palette* (. *ink_toolbar* :get_selected)))
	;what is selected takes it
	(if (nempty? (. *board* :get_selected))
		(. *board* :style :stroke (get :color *board*))))

(defun action-radius ()
	(def *board* :width (elem-get *widths* (. *radius_toolbar* :get_selected)))
	(if (nempty? (. *board* :get_selected))
		(. *board* :style :width (get :width *board*))))

(defun action-mode ()
	(def *board* :mode (elem-get *modes* (. *mode_toolbar* :get_selected)))
	;out of select mode what was selected is let go of. It has no handles
	;to show that it is, and the colours and widths would change it
	(unless (eql (get :mode *board*) :select) (. *board* :select (list)))
	(. *board* :touch +board_dirty_overlay))

(defun action-style ()
	(setq *style* (elem-get *styles* (. *style_toolbar* :get_selected)))
	(. (. *board* :get_doc) :insert :style *style*)
	;the paper is another paper, the document on it is as it was
	(setq *paper_dirty* :t)
	(. *board* :touch +board_dirty_overlay))

(defun action-snap ()
	(setq *snap* (not *snap*))
	(def *board* :snap (if *snap* (n2f (. (. *board* :get_doc) :find :grid)) 0.0))
	(toolbar-states *snap_toolbar* (list *snap* *snap_angle*)))

(defun action-snap-angle ()
	(setq *snap_angle* (not *snap_angle*))
	(def *board* :snap_angle (if *snap_angle* (const (/ +fp_pi 12.0)) 0.0))
	(toolbar-states *snap_toolbar* (list *snap* *snap_angle*)))

(defun action-text ()
	;the words of the text field are put down where the pen next goes down
	(. *mode_toolbar* :set_selected (find :text *modes*))
	(action-mode))

(defun action-arc ()
	;what the protractors draw
	(setq *arc* (elem-get *arcs* (. *arc_toolbar* :get_selected)))
	(each (# (if (Protractor? %0) (def %0 :mode *arc*))) (. (. *board* :get_stage) :get_actors)))

(defun add-instrument (instrument)
	;in the middle of what shows of the board, each a little off the last
	;so that two are not one on the other
	(defq others (length (filter (const Instrument?) (. (. *board* :get_stage) :get_actors))))
	(bind '(x y) (get :origin instrument))
	(def instrument :mode *arc* :origin (list (+ x (* 30.0 (n2f others))) (+ y (* 30.0 (n2f others)))))
	(. (. *board* :get_stage) :add instrument)
	(. *board* :touch +board_dirty_overlay))

(defun action-ruler () (add-instrument (apply (const Ruler) (cat (list *board*) (view-middle)))))
(defun action-protractor () (add-instrument (apply (const Protractor) (cat (list *board*) (view-middle)))))
(defun action-set-square () (add-instrument (apply (const Setsquare) (cat (list *board*) (view-middle)))))

(defun action-group () (. *board* :group))
(defun action-ungroup () (. *board* :ungroup))
(defun action-delete () (. *board* :delete))
(defun action-duplicate () (. *board* :duplicate))
(defun action-to-front () (. *board* :order :t))
(defun action-to-back () (. *board* :order :nil))
(defun action-align-left () (. *board* :align :left))
(defun action-align-hcenter () (. *board* :align :hcenter))
(defun action-align-right () (. *board* :align :right))
(defun action-align-top () (. *board* :align :top))
(defun action-align-vcenter () (. *board* :align :vcenter))
(defun action-align-bottom () (. *board* :align :bottom))
(defun action-select-all () (. *board* :select_all))
(defun action-select-none () (. *board* :select (list)))

(defun action-save ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :t *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Save Whiteboard..." "|" ".")))

(defun action-load ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :nil *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Load Whiteboard..." "|" "." "|" ".cwb")))
