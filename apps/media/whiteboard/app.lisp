;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./widgets.inc")
(import "lib/cwb/stripes.inc")
(import "lib/cwb/palette.inc")
(import "lib/cwb/swatches.inc")

;The Whiteboard. What is on the board is a document of shapes,
;lib/cwb/doc.inc, a .cwb file. All that changes it is the board,
;lib/cwb/board.inc, which is given pointers and has methods. This file is
;the window round it: toolbars that call those methods, a view that makes
;the mouse a pointer, ./view.inc, and two canvases the board draws on, the
;document on one and what is over it, what is being drawn or moved, the
;handles, a ruler, on the other.
;
;A hand that taps on nothing opens a palette on the board, where it tapped,
;lib/cwb/palette.inc. It can do what the toolbars do, and what it changes
;of the board the toolbars are made to show, (sync-ui).
;
;A document that takes this task a while to draw, some thousands of shapes,
;is drawn by the nodes of the machine in stripes, lib/cwb/stripes.inc,
;straight onto the pixels of the first canvas, which are in shared memory
;where the host has it. This task does not wait while they do.

(enums +select 0
	(enum main picker timer tip share))

;the zooms there are, stepped through, so that in and then out is where it was
;the zooms it comes to rest at, each twice the last: at one of these the
;lines of the paper are as they should be, the coarse ones full and the
;fine ones half, and between two of them they are part way. So it is
;never left between, (zoom-magnet)
(defq +zooms ''(0.125 0.25 0.5 1.0 2.0 4.0 8.0))

(defq *zoom* 1.0 *style* :grid
	;the things of a document are anywhere. The paper is a view of some of
	;that, of the size the document says, and is not what they are on:
	;*pan_x* *pan_y* is the point of the document at its top left, and
	;*zoom* how big it is seen. *zoom_to* is where the zoom is going,
	;(zoom x y), a little each tick till it is there, and about what point
	*pan_x* 0.0 *pan_y* 0.0 *zoom_to* :nil
	;and *turn*, how many quarters the board is turned, 0 to 3, the way
	;from the x axis to the y axis. It turns about the middle of the
	;paper, and a paper that is wider than tall is taller than wide when
	;it is turned one or three. Chris: "Full canvas rotation is a rae
	;thing to do and that mainly for people wanting to change from
	;horizontal to vertical", "set the magnets for angles on the canvas
	;as 90 degrees, full locks"
	*turn* 0
	;how far what is seen has yet to go for the turns of a wheel there
	;have been, pixels: a wheel says so many steps, and it glides them
	*pan_go* (list 0.0 0.0) *snapped_as* :nil
	;the bars: where all there is to see begins, which a bar's 0 is; when
	;one was last moved by hand, while that is going on they are left as
	;they are; and whether they are behind what is seen now
	*bars_from* (list 0.0 0.0) *bars_at* 0 *bars_stale* :t
	;the palette has asked for the window back, done on the next tick,
	;not in the middle of what the palette is doing
	*full_go* :nil
	;what the window was, while the board is all there is on the screen:
	;where it was and how big, what else was in it, its edge, and the
	;size of the paper. :nil when it is a window, (full-screen) in ui.inc.
	;It is here and not there: what is in that file is read as a module,
	;and a thing of it that changes is not there to find afterwards
	*full* :nil
	*file* :nil *picker_mbox* :nil *picker_mode* :nil *running* :t
	*committed* :nil *overlay* :nil *flight* :nil *paper* :nil *paper_dirty* :t *flight_used* :nil
	*canvas_size* :nil
	;the size of the paper on the screen, not turned, :nil for the
	;document's. It is what the frame in the window has room for, and the
	;zoom is what is seen on it, (view-follow)
	*view_size* :nil
	;what is in flight behind the document, *flight_b*, a canvas of its own
	;between the document's and the paper. Two layers in flight, Chris:
	;"pull to front and push to back, by multiple users at the same time,
	;so we will have to have 2 inflight layers, back/front and the
	;committed layer in the middle". It is drawn each time while it has
	;anything, it is not moved as a picture as the front one is
	*flight_b* :nil *flight_b_used* :nil
	;the canvas of what is in flight is in a view the size of the paper,
	;*flight_clip*, which is what is stacked with the others. While a hand
	;only moves what is in flight the canvas is not drawn again, it is
	;moved in that view, and the view cuts off what goes past the paper.
	;*flight_at* is what was drawn on it and where each thing then was
	*flight_clip* :nil *flight_at* :nil
	;When a canvas that has been moved is to be drawn on again it must go
	;back to where it began and have another picture, and the screen can
	;be made between those two, a canvas is shown with a call that lets
	;other tasks run: the old picture where it began, for a moment, a
	;thing let go of seen back where it was taken up. So there are two.
	;The new picture is drawn on the one that is not seen, *flight_spare*,
	;and shown there, and then the two change places, which is one step
	*flight_spare* :nil
	;An instrument has a canvas of its own, as big as it is, and what puts
	;the screen together lays it over the rest. It is drawn when it is
	;turned, sized, or says another angle, and not when it is moved, nor
	;when anything else is: then its canvas is only put where it now is.
	;*tools* is one of these for each instrument on the board, back first
	*tools* (list)
	;and what is over the instruments, a palette, the rings where fingers
	;are, is on a canvas in front of them all
	*top* :nil *top_used* :nil *top_show* :nil
	;is only a part of the document's picture drawn again when only a part
	;changed, (draw-damage). It is if a part of a canvas can be made clear,
	;(. canvas :clear), which a system from before that was written can not
	*damage_parts* (not (eql :t (catch (ffi "gui/canvas/lisp_clear") :t)))
	;what was in flight and has been let go, while the nodes have yet to
	;draw the document with it in: *landing* is waiting for a frame to be
	;begun, *landed* is in the frame they are drawing. Both are still drawn
	;with what is in flight, or a thing let go would be nowhere till the
	;frame came, a blink. *flown* is what was in flight when last looked
	*flown* (list) *landing* (list) *landed* (list)
	rate (/ 1000000 60)
	;the board has this much round it, on the screen, each side, that is
	;not the document: an instrument lies half off the paper as a ruler
	;does on a desk, and is seen there. What is over the document, the
	;second canvas, covers it too
	*margin* 240
	;a document that takes this task longer than that to draw is drawn in
	;stripes, and one of fewer shapes than that is tried by this task again
	*farm_ms* 40 *farm_shapes* 2000
	*stripes* (Stripes *board*) *local_ms* 0 *framing* :nil *again* :nil
	*farm_fails* 0 *kept* 0)

;The lights of the bars, Chris: "might need the toolbar to indicate who's
;in that mode ? do we just have some colored dots on the button like other
;indicators ?", and then "a clear 6 element strip of small squares (grid)
;under each button symbol, so they are always there, just transparent when
;not indicating a device". Each device has its square and its colour, the
;same as its dots on the menus, "mouse first, then touch, then 4 pens
;(teacher then 3 students)", (. board :device_slot). Two rows, the main
;button, left, tip or touch, over the second, right, barrel or the other
;end, Chris: "We need to distinguish between the buttons of a device if
;they can be bound to different modes". Made with the bars,
;before the window is laid out, so the bars are the size they are from
;the first and only colours change after
(defq *dots_of* :nil +strip_cells 6)

(defclass Lights () (View)
	; (Lights) -> lights
	;the squares of a button, a lit one its device's colour inside a line of
	;black, Chris: "especially if we can keep a 1 pixel black line around
	;them", 5 pixels a square, 4 apart, so two side by side share a line. A
	;clear one is not drawn, and the view is not opaque, the button is
	;drawn under it
	(def this :lit (list) :min_width (inc (* +strip_cells 4)) :min_height 9)

	(defmethod :layout ()
		; (. lights :layout) -> lights
		;never opaque: a View is, laid out with a solid paper, and it has
		;the bar's, so what is under the clear squares was not drawn
		(. this :set_flags 0 +view_flag_opaque))

	(defmethod :draw ()
		; (. lights :draw) -> lights
		(each (lambda (slot)
				(defq x (* (% slot +strip_cells) 4) y (* (/ slot +strip_cells) 4))
				(.-> this
					(:ctx_set_color +argb_black) (:ctx_filled_box x y 5 5)
					(:ctx_set_color (elem-get +bindings_device_cols (% slot +strip_cells)))
					(:ctx_filled_box (inc x) (inc y) 3 3)))
			(get :lit this))
		this)
	)

(defun button-strip (button)
	; (button-strip button) -> lights
	;the squares of a button, under its symbol, made the first time
	(unless (defq strip (get :strip button))
		(def button :flow_flags (bit-mask +flow_flag_down +flow_flag_align_hcenter))
		(. button :add_child (setq strip (Lights)))
		(def button :strip strip))
	strip)

(defun button-lights (button slots)
	;the squares of the devices bound to what the button binds lit, the
	;button drawn again under them when they change
	(defq strip (button-strip button) lit (sort (filter (# (< %0 (* +strip_cells 2))) slots) (const -)))
	(unless (eql (str lit) (str (get :lit strip)))
		(def strip :lit lit)
		(.-> button :layout :dirty_all)))

(defun binder-dots ()
	;each button of the binders, the devices bound to what it binds, when
	;what they are bound to has changed, and its strip the first time
	(defq rules (get :rules (. *board* :get_bindings))
		as (str rules (get :snap *board*) (get :snap_angle *board*) (get :keep_shape *board*)
			(get :mode *board*) (get :pen *board*) (get :width *board*) (. (get :seen *board*) :size)))
	(unless (eql as *dots_of*)
		(setq *dots_of* as)
		;each device and button by its place, what it is bound to and what
		;it uses, its own or the board's, (. board :device_uses)
		(defq devices (. *board* :device_uses))
		(defq slots-of (lambda (key value)
			(defq out (list))
			(each (lambda ((what slot uses))
					(if (eql (. uses :find key) value) (push out slot)))
				devices)
			out))
		(each (# (button-lights %0 (slots-of :mode (elem-get *modes* (!))))) (. *mode_toolbar* :children))
		(each (# (button-lights %0 (slots-of :width (elem-get *widths* (!))))) (. *radius_toolbar* :children))
		;each pen lit for the device buttons that hold it
		(each (# (button-lights %0 (slots-of :pen (!)))) (. *ink_toolbar* :children))
		;a switch is lit for a device that has it on, its own or the board's
		(each (lambda (button key)
				(button-lights button (reduce (lambda (out (what slot &ignore))
					(if (. *board* :switch_of what key) (push out slot) out)) devices (list))))
			(. *snap_toolbar* :children) '(:snap :snap_angle :keep_shape))))

;the board's pens are the ink bar's, the one list, and a slot holds one by
;its number; the rings show them. The mouse and a finger hold the first,
;black, till they hold another. No menu opens from a tap on the board, a
;tap there is a dot: menus float, made from the toolbar, (action-menu).
;Chris: "A tap on the canvas did leave a dot as I didn't have a pop up
;menu on the canvas"
;the menus offer the widths the toolbar does
(def *board* :pens *palette* :pen 0 :on_pens (# (pens-shown)) :menu_widths *widths*)
;a ring menu opens the swatches, a menu of the inks to fill a pen with
(def *board* :on_swatches (# (swatches-open %0 %1)))

(defun zoom-level (zoom)
	; (zoom-level zoom) -> (level part)
	;a zoom as how many times 1 was doubled to get to the rest zoom at
	;or under it, less than 0 for halved, and how far it is from that
	;one to the next, 0 up to 1
	(defq level 0)
	(while (>= zoom 2.0) (setq zoom (/ zoom 2.0) level (inc level)))
	(while (< zoom 1.0) (setq zoom (* zoom 2.0) level (dec level)))
	(list level (- zoom 1.0)))

(defun zoom-magnet (zoom)
	; (zoom-magnet zoom) -> zoom
	;the zoom to rest at that is nearest, of the zooms there are
	(defq best (first +zooms))
	(each (# (if (< (abs (- (/ zoom %0) 1.0)) (abs (- (/ zoom best) 1.0))) (setq best %0))) +zooms)
	best)

(defun snap-gap ()
	; (snap-gap) -> gap
	;what a thing goes to when the snap is on: every line of the paper
	;that shows at the zoom it rests at, the fine ones, at half, as well
	;as the coarse. Chris: "Snap are gird lines, should be to the 5mm !"
	;So half the grid of the document, halved again for each time the
	;zoom is doubled
	(bind '(level &ignore) (zoom-level (zoom-magnet (if *zoom_to* (first *zoom_to*) *zoom*))))
	(defq gap (* 0.5 (n2f (. (. *board* :get_doc) :find :grid))))
	(times (abs level) (setq gap (if (> level 0) (/ gap 2.0) (* gap 2.0))))
	gap)

;what is kept for an instrument: it, the canvas it is on and one not
;seen to draw the next on, what it was when it was drawn, where on the
;canvas its own 0 0 is, the part of a pixel it was drawn at, and where it
;was when this last looked
(enums +tv 0
	(enum tool canvas spare key px py fx fy ox oy))

(defun sync-ui ()
	;the bars are binders and show no state of their own: each button has a
	;dot for each device that is bound to what it binds, (binder-dots). The
	;grid a device snaps to is what the paper has at the zoom there is, and
	;so is the board's, which a menu may have put on as the document's
	(def *board* :snap_gap (snap-gap))
	(defq on (/= (get :snap *board*) 0.0))
	(and on (/= (get :snap *board*) (snap-gap)) (def *board* :snap (snap-gap)))
	(binder-dots))

(defun doc-size ()
	; (doc-size) -> (width height)
	;the size of the document, not turned
	(defq doc (. *board* :get_doc))
	(list (max 1 (. doc :find :width)) (max 1 (. doc :find :height))))

(defun paper-size ()
	; (paper-size) -> (width height)
	;the size of the paper on the screen, not turned: what the frame has
	;room for, or before it has been seen, the document's
	(ifn *view_size* (doc-size)))

(defun canvas-size ()
	; (canvas-size) -> (width height)
	;the size of the paper on the screen, the other way up if the board is
	;turned a quarter or three. The zoom does not make it bigger, it makes
	;what is seen on it bigger
	(bind '(w h) (paper-size))
	(if (= (logand *turn* 1) 1) (list h w) (list w h)))

(defun frame-size ()
	; (frame-size) -> (width height)
	;what the frame in the window has room for, less its bars, as it lays
	;the paper out, view.inc
	(bind '(w h) (. *image_scroll* :get_size))
	(if (get :bare *image_scroll*) (list w h)
		(list (- w (first (. (get :vslider *image_scroll*) :get_constraint)))
			(- h (second (. (get :hslider *image_scroll*) :get_constraint))))))

(defun turn-matrix (turn)
	; (turn-matrix turn) -> :nil | matrix
	;so many quarters round, about 0 0, as numbers that are exactly so
	(case (logand turn 3)
		(1 (Mat3x2 0.0 -1.0 0.0 1.0 0.0 0.0))
		(2 (Mat3x2 -1.0 0.0 0.0 0.0 -1.0 0.0))
		(3 (Mat3x2 0.0 1.0 0.0 -1.0 0.0 0.0))
		(:t :nil)))

(defun turn-angle ()
	; (turn-angle) -> angle
	(* (n2f *turn*) +fp_hpi))

(defun view-turned ()
	; (view-turned) -> :nil | matrix
	;from the paper as it would be not turned, to the paper as it is:
	;turned about its middle, which is the middle of both
	(when (/= *turn* 0)
		(bind '(cw ch) (canvas-size))
		(bind '(pw ph) (paper-size))
		(Mat3x2-mul (Mat3x2-translate (/ (n2f cw) 2.0) (/ (n2f ch) 2.0))
			(Mat3x2-mul (turn-matrix *turn*)
				(Mat3x2-translate (/ (n2f pw) -2.0) (/ (n2f ph) -2.0))))))

(defun view-unturn (cx cy)
	; (view-unturn cx cy) -> (x y)
	;a point of the paper as a point of the paper not turned
	(Mat3x2-point (Mat3x2-invert (view-turned)) cx cy))

(defun field-size ()
	; (field-size) -> (width height)
	;what the size field says, width x height, or the size the board is
	(defq doc (. *board* :get_doc)
		nums (filter (# (> %0 0)) (map (# (ifn (str-to-num %0) 0))
			(split (. *size_field* :get_text) (const (char-class " xX,*"))))))
	(if (= (length nums) 2)
		(list (min 16384 (n2i (first nums))) (min 16384 (n2i (second nums))))
		(list (. doc :find :width) (. doc :find :height))))

(defun view-matrix ()
	; (view-matrix) -> :nil | matrix
	;from the document to the canvas: what is at the pan is at its top
	;left, it is as big as the zoom, and then it is turned
	(Mat3x2-mul (view-turned)
		(cond
			((and (= *zoom* 1.0) (= *pan_x* 0.0) (= *pan_y* 0.0)) :nil)
			(:t (Mat3x2-mul (Mat3x2-scale *zoom*) (Mat3x2-translate (neg *pan_x*) (neg *pan_y*)))))))

(defun over-matrix ()
	; (over-matrix) -> :nil | matrix
	;from the document to the canvas of what is over it, which has the
	;margin round it
	(if (= *margin* 0) (view-matrix)
		(Mat3x2-mul (Mat3x2-translate *margin* *margin*) (view-matrix))))

(defun view-seen ()
	; (view-seen) -> (x y x1 y1)
	;the box of the document that shows, round the corners of what shows
	;taken back into the document, turned or not. New menus and
	;instruments are put in it, (. board :place_new)
	(bind '(cw ch) (canvas-size))
	(defq inv (Mat3x2-invert (view-matrix))
		pts (map (lambda ((x y)) (Mat3x2-point inv x y)) (list (list 0.0 0.0) (list (n2f cw) 0.0) (list (n2f cw) (n2f ch)) (list 0.0 (n2f ch))))
		xs (map (const first) pts) ys (map (const second) pts))
	(list (reduce min xs) (reduce min ys) (reduce max xs) (reduce max ys)))

(defun view-middle ()
	; (view-middle) -> (x y)
	;the point of the document that is in the middle of the paper, which
	;is the middle of what shows: the paper is always in the middle of it
	(bind '(cw ch) (canvas-size))
	(Mat3x2-point (Mat3x2-invert (view-matrix)) (/ (n2f cw) 2.0) (/ (n2f ch) 2.0)))

(defun tools-stack ()
	;every canvas there is, taken out and put back in the order they lie
	;in, each where it was. One added goes behind those there. From the
	;front: *top*, the instruments, *overlay*, and the three of the document
	(defq all (cat (list *top*) (rmap (# (elem-get %0 +tv_canvas)) *tools*) (list *overlay*)
			(list *flight_clip* *committed* *flight_b* *paper*))
		all (filter (const identity) all) bounds (map (# (. %0 :get_bounds)) all))
	(each (# (. %0 :sub)) all)
	(each (# (. *backdrop* :add_child %0)) all)
	(each (lambda (view (x y w h)) (. view :set_bounds x y w h)) all bounds)
	(. *backdrop* :dirty_all))

(defun tool-show (entry)
	; (tool-show entry) -> :nil | :t
	;an instrument is seen where it is and as it is. Is its canvas another
	;one now, to be put in its place among the rest
	(defq tool (elem-get entry +tv_tool))
	(unless (get :body tool) (. tool :built))
	(. tool :readout)
	(bind '(ox oy) (get :origin tool))
	;where its own 0 0 is on the screen, the pixel, and the part of one
	;over, to an eighth. A canvas is put at whole pixels
	(bind '(sx sy) (Mat3x2-point (over-matrix) ox oy))
	(defq
		ix (n2i (floor (+ sx 0.5))) iy (n2i (floor (+ sy 0.5)))
		fx (/ (n2f (n2i (floor (+ (* (- sx (n2f ix)) 8.0) 0.5)))) 8.0)
		fy (/ (n2f (n2i (floor (+ (* (- sy (n2f iy)) 8.0) 0.5)))) 8.0)
		;how big it is on the screen, not its size and the zoom apart: it
		;is the same picture at any zoom, it stays the size it is there
		;and the angle it is at on the screen, its own and the board's
		key (str (list (+ (get :angle tool) (turn-angle)) (get :length tool) (n2i (* (get :size tool) *zoom* 4096.0)) (get :mode tool)
			(first (get :said tool))))
		canvas (elem-get entry +tv_canvas)
		same (and canvas (eql key (elem-get entry +tv_key)))
		moved (not (and (eql ox (elem-get entry +tv_ox)) (eql oy (elem-get entry +tv_oy))))
		exact (and (eql fx (elem-get entry +tv_fx)) (eql fy (elem-get entry +tv_fy))))
	(elem-set entry +tv_ox ox)
	(elem-set entry +tv_oy oy)
	(cond
		((and same (or exact moved))
			;as it was drawn, or it has only been moved: the canvas is put
			;where it is now. While it moves it may be up to half a pixel
			;from true. When it has come to rest, the next time this looks,
			;it is drawn again just where it is
			(bind '(x y w h) (. canvas :get_bounds))
			(defq nx (- ix (elem-get entry +tv_px)) ny (- iy (elem-get entry +tv_py)))
			(unless (and (= x nx) (= y ny)) (. canvas :change_dirty nx ny w h))
			:nil)
		(:t ;drawn. The box round it as it is turned and sized, with a
			;little room, is how big the canvas is, and where in that its
			;own 0 0 falls is kept: turned about that, it stays on the
			;one pixel
			(bind '(bx by bx1 by1) (Mat3x2-box (third (cwb-flat (get :body tool)))
				(Mat3x2-mul (turn-matrix *turn*) (Mat3x2-mul (Mat3x2-scale *zoom*)
					(Mat3x2-mul (Mat3x2-rot (get :angle tool)) (Mat3x2-scale (get :size tool)))))))
			(defq x0 (- (n2i (floor (+ bx fx))) 3) y0 (- (n2i (floor (+ by fy))) 3)
				;in steps of 32, so that one turned a little is as big as it was
				w (logand (+ (- (n2i (floor (+ bx1 fx))) x0) 4 31) -32)
				h (logand (+ (- (n2i (floor (+ by1 fy))) y0) 4 31) -32)
				px (neg x0) py (neg y0) spare (elem-get entry +tv_spare)
				onto (if (and spare (eql (str (slice (. spare :get_bounds) 2 4)) (str (list w h)))) spare
					(Canvas w h 1)))
			(. onto :set_canvas_flags +canvas_flag_antialias)
			(def onto :paper 0)
			(. onto :fill 0)
			(. tool :draw onto (Mat3x2-mul (Mat3x2-translate (+ (n2f px) fx) (+ (n2f py) fy))
				(Mat3x2-mul (turn-matrix *turn*) (Mat3x2-mul (Mat3x2-scale *zoom*) (Mat3x2-translate (neg ox) (neg oy))))))
			;its picture is shown while it is not seen. That call lets
			;other tasks run, the screen may be made then: so the one
			;that is seen is left where it is, as it is, till every
			;instrument has its new picture, and then they change places
			;all at once, (draw-tools). Chris: "keep the old canvas around
			;until the new one is read and then switch over"
			((const (ffi "gui/canvas/lisp_swap")) onto +swap_write)
			(. onto :set_bounds (- ix px) (- iy py) w h)
			(elem-set entry +tv_canvas onto)
			(elem-set entry +tv_spare canvas)
			(elem-set entry +tv_key key)
			(elem-set entry +tv_px px)
			(elem-set entry +tv_py py)
			(elem-set entry +tv_fx fx)
			(elem-set entry +tv_fy fy)
			:t)))

(defun draw-tools ()
	;the instruments on the board, each on its canvas, in the order they
	;lie in. One that has gone has its canvas taken away
	(defq was *tools* restack :nil)
	(setq *tools* (map (lambda (tool)
			(or (some (# (if (eql (elem-get %0 +tv_tool) tool) %0)) was)
				(list tool :nil :nil :nil 0 0 :nil :nil :nil :nil)))
		(filter (const Instrument?) (. (. *board* :get_stage) :get_actors))))
	(each (lambda (entry)
		(unless (find entry *tools*)
			(setq restack :t)
			(if (elem-get entry +tv_canvas) (. (elem-get entry +tv_canvas) :sub))))
		was)
	(unless (and (= (length was) (length *tools*)) (every (const eql) was *tools*)) (setq restack :t))
	;each that is drawn again is drawn where it is not seen, the one that
	;is seen still there. Then, with nothing between that lets the screen
	;be made, the old ones are taken out and the new put in, in their order
	(defq swapped (list))
	(each (lambda (entry)
		(defq old (elem-get entry +tv_canvas))
		(when (tool-show entry)
			(push swapped (list old (elem-get entry +tv_canvas)))
			(setq restack :t)))
		*tools*)
	(each (lambda ((old new))
		(if old (. old :sub))
		(. *backdrop* :add_child new))
		swapped)
	(if restack (tools-stack)))

(defun board-resized ()
	;the board is another size on the screen, the document's or the zoom
	;has changed. New canvases of that size take the place of those there
	;were, and everything is drawn again. From the back:
	;	*paper*		the paper, drawn when it changes and not else
	;	*committed*	the document, one picture, clear where nothing is.
	;				It is not drawn again while a thing is moved or drawn
	;	*flight_b*	what is in flight behind the document, taken to the back
	;	*flight*	what is in flight in front of it: a thing as it is
	;				moved, a line as it is drawn
	;	*overlay*	the handles, with the margin round it
	;	*tools*		a canvas for each instrument, as big as it is, put
	;				where it lies, (draw-tools)
	;	*top*		a palette, and the rings where fingers are
	(bind '(w h) (setq *canvas_size* (canvas-size)))
	;the grid the board snaps to at this zoom, (snap-gap)
	(def *board* :snap_gap (snap-gap))
	;the pixels of the one the nodes can reach are let go of by name, they
	;last till then, whatever becomes of this task
	(when *committed* (. *committed* :sub) (. *committed* :free))
	(each (# (if %0 (. %0 :sub))) (list *overlay* *flight_clip* *flight_b* *paper* *top*))
	;the pixels of the document's are shared, if the host has that, for the nodes
	(setq *committed* (ifn (canvas-shared w h 1) (Canvas w h 1))
		*overlay* (Canvas (+ w *margin* *margin*) (+ h *margin* *margin*) 1)
		*top* (Canvas (+ w *margin* *margin*) (+ h *margin* *margin*) 1) *top_used* :t
		*flight* (Canvas w h 1) *paper* (Backdrop) *paper_dirty* :t *flight_used* :nil
		*flight_clip* (View) *flight_at* :nil *flight_spare* (Canvas w h 1)
		*flight_b* (Canvas w h 1) *flight_b_used* :nil)
	(. *flight_spare* :set_canvas_flags +canvas_flag_antialias)
	(def *flight_spare* :paper 0)
	;a view with no colour of its own has its parent's, and covers what is under it
	(def *flight_clip* :paper 0)
	(. *flight_clip* :add_child *flight*)
	(. *flight* :set_bounds 0 0 w h)
	(each (# (. %0 :set_canvas_flags +canvas_flag_antialias) (def %0 :paper 0))
		(list *committed* *overlay* *flight* *flight_b* *top*))
	(. *flight* :fill 0)
	(. *flight_b* :fill 0)
	(. *top* :fill 0)
	;a child that is added goes behind those there are, so the front one first
	(.-> *backdrop* (:add_child *top*) (:add_child *overlay*) (:add_child *flight_clip*) (:add_child *committed*)
		(:add_child *flight_b*) (:add_child *paper*))
	;the document's, and the two behind it, are in from the corner by the margin
	(each (# (. %0 :set_bounds *margin* *margin* w h)) (list *committed* *flight_clip* *flight_b* *paper*))
	(def *board* :zoom *zoom*)
	(def *board_view* :zoom *zoom* :margin *margin* :inv (Mat3x2-invert (over-matrix)))
	(defq doc (. *board* :get_doc))
	(. *size_field* :set_text (cat (str (. doc :find :width)) "x" (str (. doc :find :height))))
	(. *board_stack* :change 0 0 (+ w *margin* *margin*) (+ h *margin* *margin*))
	(each (# (. %0 :set_bounds *margin* *margin* w h)) (list *committed* *flight_clip* *flight_b* *paper*))
	(.-> *image_scroll* :layout :dirty_all)
	;the canvases of the instruments are in front of the new ones still
	(if (nempty? *tools*) (tools-stack))
	(def *board* :view_box (view-seen))
	(. *board* :touch (+ +board_dirty_doc +board_dirty_overlay)))

(defun bars-update ()
	;the bars say what is seen of all there is to see. All there is, each
	;way, is from the first of what is drawn or of the view, whichever is
	;first, to the last of either: so there is never an end to come to,
	;a view moved out past the drawing is part of what there is. The
	;thumb is the view. In pixels of the paper as it is seen, turned too
	(bind '(cw ch) (canvas-size))
	(defq box (if (defq drawn (cwb-bounds (apply (const cat) (cat (list (list)) (map (const third) (cwb-layers (. *board* :get_doc)))))))
			(Mat3x2-box drawn (view-matrix)) (list 0.0 0.0 (n2f cw) (n2f ch)))
		x0 (min 0.0 (first box)) y0 (min 0.0 (second box))
		x1 (max (n2f cw) (third box)) y1 (max (n2f ch) (elem-get box 3)))
	(def (get :hslider *image_scroll*) :maximum (max 0 (n2i (- (- x1 x0) (n2f cw)))) :portion cw :value (max 0 (n2i (neg x0))))
	(def (get :vslider *image_scroll*) :maximum (max 0 (n2i (- (- y1 y0) (n2f ch)))) :portion ch :value (max 0 (n2i (neg y0))))
	;where the thumbs were put, to know how far one is moved from there
	(setq *bars_from* (list (get :value (get :hslider *image_scroll*)) (get :value (get :vslider *image_scroll*))) *bars_stale* :nil)
	(. (get :hslider *image_scroll*) :dirty_all)
	(. (get :vslider *image_scroll*) :dirty_all))

(defun bars-tick ()
	;the bars are set if they are behind, and no hand is on one
	(and *bars_stale* (> (- (pii-time) *bars_at*) 400000) (bars-update)))

(defun view-changed ()
	;the pan or the zoom is another: the board and its view are told,
	;the snap is the grid there is at that zoom, and all of it is drawn
	;again, the paper too. What is in flight is drawn, not moved as a
	;picture, it is in another place on its canvas
	(def *board* :zoom *zoom* :snap_gap (snap-gap))
	(def *board* :view_turn (turn-angle) :view_box (view-seen))
	(def *board_view* :zoom *zoom* :inv (Mat3x2-invert (over-matrix)))
	;what a hand holds is in flight and stays in the hand, Chris: "If I
	;hold a shape, and somone else moves the canvas, my shape comes with
	;me !" Each pointer that is down is told to the board where it is now
	(. *board_view* :resend)
	(if (/= (get :snap *board*) 0.0) (def *board* :snap (snap-gap)))
	(setq *paper_dirty* :t *flight_at* :nil *bars_stale* :t)
	(. *board* :touch (+ +board_dirty_doc +board_dirty_overlay)))

(defun tools-held ()
	; (tools-held) -> ((instrument x y on) ...)
	;where each instrument is on the screen, and how big it is there.
	;An instrument is not of the document, it lies on the glass over
	;it: what is seen goes by under it, and gets bigger and smaller
	;under it, and it stays where it is and the size it is. Chris:
	;"Instrument move with the whell, that's definately not wanted."
	;A palette that is open lies on the glass too, it has a place and
	;no size of its own
	;Where it is, is from the middle of the paper: that is what stays
	;where it is in the window when the paper is another size, turned
	(defq m (view-matrix))
	(bind '(cw ch) (canvas-size))
	(map (lambda (tool)
			(bind '(ox oy) (get :origin tool))
			(bind '(sx sy) (Mat3x2-point m ox oy))
			;where, how big, and at what angle, on the screen
			(list tool (- sx (/ (n2f cw) 2.0)) (- sy (/ (n2f ch) 2.0)) (if (Instrument? tool) (* (get :size tool) *zoom*))
				(if (Instrument? tool) (+ (get :angle tool) (turn-angle)))))
		(filter (# (or (Instrument? %0) (Palette? %0) (Swatches? %0))) (. (. *board* :get_stage) :get_actors))))

(defun tools-hold (held)
	;each is put back where it was on the screen, the size it was there,
	;now that the pan and the zoom are another, (tools-held)
	(defq back (Mat3x2-invert (view-matrix)))
	(bind '(cw ch) (canvas-size))
	(each (lambda ((tool sx sy on angle))
		(def tool :origin (Mat3x2-point back (+ sx (/ (n2f cw) 2.0)) (+ sy (/ (n2f ch) 2.0))))
		(if on (def tool :size (/ on *zoom*)))
		(if angle (def tool :angle (- angle (turn-angle)))))
		held))

(defun view-follow ()
	;the paper is the size of the frame, Chris: "a hand resize should
	;resize it too". What is in the middle of what is seen stays there,
	;and what lies on the glass where it is. Not while the board is all
	;of the screen, the document is the screen's size then
	(bind '(fw fh) (frame-size))
	(unless (or *full* (<= fw 0) (<= fh 0) (eql (str (list fw fh)) (str (canvas-size))))
		(bind '(mx my) (view-middle))
		(defq held (tools-held))
		(setq *view_size* (if (= (logand *turn* 1) 1) (list fh fw) (list fw fh)))
		(bind '(uw uh) *view_size*)
		(setq *pan_x* (- mx (/ (n2f uw) (* 2.0 *zoom*))) *pan_y* (- my (/ (n2f uh) (* 2.0 *zoom*))))
		(tools-hold held)
		(view-changed)))

(defun tools-snap (going)
	;with the snap on, each instrument has the 0 of its marks on the lines
	;of the paper, (:snap_zero). An instrument stays where it is on the
	;screen while the paper goes by under it and is zoomed under it, so it
	;is put on the lines again when that has come to rest, and when the
	;snap is put on. Not while the paper is on its way, going says so, nor
	;while a hand has hold of anything: it would be pulled from the hand.
	;It is done the once for what is seen as it then is, *snapped_as*
	(defq step (get :snap *board*) as (list *pan_x* *pan_y* *zoom* *turn* step))
	(unless (or (= step 0.0) (some (const identity) going) (. *board* :busy?)
			(and *snapped_as* (every (const =) as *snapped_as*)))
		(setq *snapped_as* as)
		(each (# (. %0 :snap_zero)) (filter (const Instrument?) (. (. *board* :get_stage) :get_actors)))
		(. *board* :touch +board_dirty_overlay))
	;with it off, the next time it is put on is a time to do it
	(if (= step 0.0) (setq *snapped_as* :nil)))

(defun pan-by (dx dy)
	;what is seen is moved by that many pixels of the screen, as far as
	;is asked: the things of a document are anywhere, there is no edge
	(defq held (tools-held))
	;so many pixels of the paper as it is seen: of the paper not turned,
	;the other way round by as much
	(bind '(dx dy) (Mat3x2-point (turn-matrix (- 4 *turn*)) dx dy))
	(setq *pan_x* (- *pan_x* (/ dx *zoom*)) *pan_y* (- *pan_y* (/ dy *zoom*)))
	(tools-hold held)
	(view-changed))

(defun view-home ()
	;the middle of the document is in the middle of the paper again, at
	;the zoom there is. Not while anybody is drawing a line. The
	;instruments go with the document, where they lie on it, Chris: "when
	;I hit min, the instruments move..."
	(unless (. *board* :busy?)
		(bind '(dw dh) (doc-size))
		(bind '(pw ph) (paper-size))
		(setq *pan_go* (list 0.0 0.0)
			*pan_x* (- (/ (n2f dw) 2.0) (/ (n2f pw) (* 2.0 *zoom*)))
			*pan_y* (- (/ (n2f dh) 2.0) (/ (n2f ph) (* 2.0 *zoom*))))
		(view-changed)))

(defun bars-moved ()
	;a bar was moved by hand: what is seen is where its thumb says, from
	;where all there is began when the bars were last set. They are not
	;set again while it goes on, a thumb is not to change under the hand
	(cond
		;not while anybody has something in hand on the board, what is
		;seen is everybody's: the bar is put back where it was
		((. *board* :busy?) (setq *bars_at* 0 *bars_stale* :t))
		(:t (setq *bars_at* (pii-time))
			;by how far the thumbs have gone since they were last looked at
			(bind '(hx vy) *bars_from*)
			(defq hv (get :value (get :hslider *image_scroll*)) vv (get :value (get :vslider *image_scroll*)))
			(setq *bars_from* (list hv vv))
			(pan-by (- hx hv) (- vy vv)))))

(defun pan-glide (dx dy)
	;so many pixels more to go, for a turn of a wheel: gone a part of
	;the way each tick, (pan-tick), not in steps the size of a turn,
	;which are the size of the paper's lines and look like a snap
	(setq *pan_go* (list (+ (first *pan_go*) (n2f dx)) (+ (second *pan_go*) (n2f dy)))))

(defun pan-tick ()
	; (pan-tick) -> :t | :nil
	;a part of the way there is still to go. Is there more
	(bind '(gx gy) *pan_go*)
	;somebody has taken something in hand: it goes no further
	(and (or (/= gx 0.0) (/= gy 0.0)) (. *board* :busy?)
		(setq *pan_go* (list 0.0 0.0) gx 0.0 gy 0.0))
	(when (or (/= gx 0.0) (/= gy 0.0))
		(defq mx (if (< (abs gx) 1.0) gx (* gx 0.3)) my (if (< (abs gy) 1.0) gy (* gy 0.3)))
		(setq *pan_go* (list (- gx mx) (- gy my)))
		(pan-by mx my)
		:t))

(defun zoom-set (zoom cx cy)
	;the zoom is that, and the point of the document that is at cx cy of
	;the paper stays there
	(setq zoom (max (first +zooms) (min (last +zooms) (n2f zoom))))
	(unless (= zoom *zoom*)
		;the point of the paper, as one of the paper not turned
		(bind '(cx cy) (view-unturn cx cy))
		(defq dx (+ *pan_x* (/ cx *zoom*)) dy (+ *pan_y* (/ cy *zoom*)) held (tools-held))
		(setq *zoom* zoom *pan_x* (- dx (/ cx zoom)) *pan_y* (- dy (/ cy zoom)))
		(tools-hold held)
		(view-changed)))

(defun view-turn (by)
	;the board is turned so many quarters, about the middle of the
	;paper, which is made the other way up if it is one or three. Not
	;while anybody is drawing. What lies on the glass stays where it is
	;and as it is. It is a jump, Chris: "the action and the final jump of
	;the canvas is enough in this use case, it more of a gesture than a
	;micro control action."
	(unless (. *board* :busy?)
		(defq held (tools-held) doc (. *board* :get_doc))
		(bind '(cw ch) (canvas-size))
		;the point of the document in the middle of the paper stays there
		(bind '(mx my) (view-middle))
		(setq *turn* (logand (+ *turn* by 4) 3))
		;the paper is the frame's, which has not turned: its sides change over
		(and *view_size* (= (logand by 1) 1) (setq *view_size* (reverse *view_size*)))
		;a board that is all of the screen has a paper the size of the
		;screen whichever way up: the document's is its sides changed over
		(when *full*
			(defq w (. doc :find :width) h (. doc :find :height))
			(.-> doc (:insert :width h) (:insert :height w)))
		(bind '(cw ch) (canvas-size))
		(bind '(nx ny) (Mat3x2-point (view-matrix) mx my))
		(bind '(dx dy) (Mat3x2-point (turn-matrix (- 4 *turn*)) (- nx (/ (n2f cw) 2.0)) (- ny (/ (n2f ch) 2.0))))
		(setq *pan_x* (+ *pan_x* (/ dx *zoom*)) *pan_y* (+ *pan_y* (/ dy *zoom*)))
		(tools-hold held)
		(board-resized)
		(view-changed)))

(defun zoom-pinch (zoom cx cy)
	;as two fingers ask, as they move. Near a zoom it would rest at it is
	;that one, they are felt as an instrument feels its angles
	(setq *zoom_to* :nil)
	(defq rest (zoom-magnet zoom))
	(zoom-set (if (< (abs (- (/ (n2f zoom) rest) 1.0)) 0.06) rest zoom) cx cy))

(defun zoom-go (zoom cx cy)
	;the zoom is to be that, by way of what is between: a little of the
	;way each tick, (zoom-tick). Not a jump, Chris: "ease to the magnet on
	;release. it's better than a jump."
	(setq *zoom_to* (list (max (first +zooms) (min (last +zooms) (n2f zoom))) cx cy))
	(if (/= (get :snap *board*) 0.0) (def *board* :snap (snap-gap))))

(defun zoom-rest (cx cy)
	;the fingers have let go: it goes to the zoom it rests at that is nearest
	(zoom-go (zoom-magnet *zoom*) cx cy))

(defun zoom-tick ()
	; (zoom-tick) -> :t | :nil
	;a step of the way to where the zoom is going. Is it still going
	(when *zoom_to*
		(bind '(zoom cx cy) *zoom_to*)
		;an eighth of what is left each tick, most of a second in all:
		;slow enough for the lines of the paper to be seen to change
		(defq next (+ *zoom* (* (- zoom *zoom*) 0.125)))
		(cond
			;or, somebody has taken something in hand, all of it at once:
			;what is seen is not to move under them, nor is the paper to
			;be left with its lines part way while they draw
			((or (< (abs (- (/ next zoom) 1.0)) 0.01) (. *board* :busy?))
				(setq *zoom_to* :nil)
				(zoom-set zoom cx cy))
			(:t (zoom-set next cx cy)))
		:t))

(defun step-zoom (step)
	;to the next zoom up or down, twice or half, what is in the middle
	;of the paper stays there
	(defq zoom (zoom-magnet (if *zoom_to* (first *zoom_to*) *zoom*))
		at (ifn (find zoom +zooms) (find 1.0 +zooms)))
	(bind '(cw ch) (canvas-size))
	;not while anybody has something in hand on the board
	(unless (. *board* :busy?)
		(zoom-go (elem-get +zooms (max 0 (min (dec (length +zooms)) (+ at step)))) (/ cw 2) (/ ch 2))))

;the wheel, the middle button and two fingers move what is seen, two
;fingers zoom it, and when they let go it comes to rest, view.inc
(def *board_view* :on_pan pan-by :on_wheel pan-glide :on_zoom zoom-pinch :on_rest zoom-rest :on_turn view-turn)
(def *image_scroll* :on_bar bars-moved)

(defun draw-paper ()
	;what is behind the document to work on, not part of it: a colour,
	;and lines by a style. It is the paper's, not the document's, and is
	;drawn as the document is seen, moved and zoomed, so a thing on a
	;line stays on it. Chris, of his wall panels: "as you zoom the 'half'
	;bright grid lines gradualy became fully opaque and took over". So
	;three rulings, each half the gap of the last: the coarse one full,
	;the next from half to full as the zoom goes from one it rests at to
	;the next, and the finest from nothing to half. At rest that is full,
	;half and none, and at the next rest it is the same with each ruling
	;in the place of the one before
	(defq doc (. *board* :get_doc) back (. doc :find :background)
		ink (logand +cwb_paper_ink 0xffffff))
	(bind '(level part) (zoom-level *zoom*))
	(defq gap (* (n2f (. doc :find :grid)) (+ 1.0 part))
		shade (# (+ (<< (max 0 (min 255 (n2i (* %0 255.0)))) 24) ink)))
	(def *paper* :paper (if (= (logand back 0xff000000) 0xff000000) back +cwb_paper_col)
		:style (if (eql *style* :plain) :nil *style*)
		;where the document's 0 0 is seen. A grid turned a quarter is
		;the same grid, lines that go across go up and down
		:origin_x (first (Mat3x2-point (view-matrix) 0 0)) :origin_y (second (Mat3x2-point (view-matrix) 0 0))
		:turned (if (= (logand *turn* 1) 1) :t)
		:spacing gap :ink (shade 1.0)
		:rules (list (list (/ gap 4.0) (shade (* 0.5 part))) (list (/ gap 2.0) (shade (+ 0.5 (* 0.5 part))))
			(list gap (shade 1.0))))
	(. *paper* :dirty))

(defun draw-local (m)
	;all of the document, by this task, and how long it took. It is not
	;yet shown
	(defq start (pii-time))
	(. *board* :take_appended)
	;clear where nothing is, the paper is behind it
	(. *committed* :fill 0)
	(. *board* :draw *committed* m)
	(setq *local_ms* (/ (- (pii-time) start) 1000)))

(defun draw-over ()
	;what is over the document, on its canvas, which has the margin round
	;it. The handles, and the box that is dragged out, are only seen
	;where the document is. An instrument or a palette is not of the
	;document, and is seen in the margin too. What is in flight is not
	;here, it has a canvas of its own, (draw-flight)
	(bind '(w h) (canvas-size))
	(. *overlay* :fill 0)
	(. *overlay* :set_clip *margin* *margin* (+ *margin* w) (+ *margin* h))
	;the lines and handles of what is selected are here, in front, unless
	;it has been taken to the back: then they are with it, (draw-flight)
	(. *board* :draw_overlay *overlay* (over-matrix) :t (get :float_back *board*))
	(. *overlay* :set_clip 0 0 (+ w *margin* *margin*) (+ h *margin* *margin*))
	;an instrument is not drawn here, it has a canvas of its own, in
	;front of this one. What is on the stage and is not one, a palette,
	;and the rings where fingers are, are on the canvas in front of those,
	;which is drawn only if it has something on it, or had
	(draw-tools)
	(defq used (or (get :show_contacts *board*)
		(some (# (not (Instrument? %0))) (. (. *board* :get_stage) :get_actors))))
	(when (or used *top_used*)
		(. *top* :fill 0)
		(. *board* :draw_actors *top* (over-matrix) (# (not (Instrument? %0))))
		(setq *top_show* :t))
	(setq *top_used* used))

(defun tools-cull (&optional seen)
	; (tools-cull [seen]) -> num
	;an instrument let go of, none of which shows, is put away: one
	;dragged off the edge and let go. It is how one is thrown away, by a
	;hand and nothing else. One left out of sight by what is seen, a min,
	;a pan, a zoom, a window made smaller, is still there, Chris: "if an
	;instrument then is outside the window it should disappear ?", no.
	;While it is held it can go off and come back. seen is the part of the desk that shows, (x y x1 y1) of
	;where the canvases lie, worked out from the frame if not given. How
	;many went
	(defq gone 0)
	(unless seen
		(bind '(fw fh) (. *image_scroll* :get_size))
		(bind '(sx sy sw sh) (. *board_stack* :get_bounds))
		(defq bw (first (. (get :vslider *image_scroll*) :get_size)) bh (second (. (get :hslider *image_scroll*) :get_size)))
		;a frame not yet laid out shows nothing, and nothing is to go for that
		(and (> fw bw) (> fh bh)
			(setq seen (list (max 0 (neg sx)) (max 0 (neg sy)) (min sw (- fw bw sx)) (min sh (- fh bh sy))))))
	(when seen
		(bind '(vx vy vx1 vy1) seen)
		(each (lambda (entry)
			(defq tool (elem-get entry +tv_tool) canvas (elem-get entry +tv_canvas))
			(defq held (> (. tool :held) 0) let_go (and (not held) (get :was_held tool)))
			(def tool :was_held held)
			(when (and canvas let_go)
				(bind '(x y w h) (. canvas :get_bounds))
				(unless (and (< x vx1) (< vx (+ x w)) (< y vy1) (< vy (+ y h)))
					(. (. *board* :get_stage) :sub tool)
					(setq gone (inc gone)))))
			(cat *tools*))
		(if (> gone 0) (. *board* :touch +board_dirty_overlay)))
	gone)

(defun flight-front ()
	; (flight-front) -> items
	;what is selected and in flight in front of the document
	(filter (# (not (. *board* :back? (elem-get %0 +cwb_id)))) (. *board* :selected_items)))

(defun flight-shift ()
	; (flight-shift) -> :nil | (x y)
	;has what is in flight only been moved, all of it by the same, since
	;it was drawn on its canvas: then how far, in pixels of the canvas. A
	;thousand things dragged are then drawn once, as a hand takes them, and
	;not again each time they move. Only while a hand has hold of them, when
	;nothing else of them is changing, and nothing is being drawn
	(when (and *flight_at* (= *turn* 0) (= (. (get :temp *board*) :size) 0)
			(or (. (get :things *board*) :holding) (/= (. (get :handles *board*) :held) 0)))
		(bind '(zoom ids ms ox oy & ds) *flight_at*)
		(defq now (flight-front))
		(when (and (= zoom *zoom*) (nempty? now) (= (length now) (length ids)))
			(defq ident (const (fixeds 1.0 0.0 0.0 0.0 1.0 0.0))
				m0 (ifn (elem-get (first now) +cwb_m) ident) o0 (ifn (first ms) ident)
				dx (- (elem-get m0 2) (elem-get o0 2)) dy (- (elem-get m0 5) (elem-get o0 5)))
			;and each is the shape it was: an end of a line that is dragged
			;changes the line, its path, and nothing of where it is
			(if (every (lambda (item id m d)
					(defq n (ifn (elem-get item +cwb_m) ident) o (ifn m ident))
					(and (= (elem-get item +cwb_id) id) (eql (elem-get item +cwb_d) d)
						(= (elem-get n 0) (elem-get o 0)) (= (elem-get n 1) (elem-get o 1))
						(= (elem-get n 3) (elem-get o 3)) (= (elem-get n 4) (elem-get o 4))
						(= (- (elem-get n 2) (elem-get o 2)) dx) (= (- (elem-get n 5) (elem-get o 5)) dy)))
					now ids ms ds)
				(list (- (n2i (floor (+ (* dx *zoom*) 0.5))) ox) (- (n2i (floor (+ (* dy *zoom*) 0.5))) oy))))))

(defun draw-flight-back ()
	; (draw-flight-back) -> :t | :nil
	;what is in flight behind the document, on its canvas, with its box and
	;handles, as a thing taken to the back has them there. Drawn each time
	;while it has anything, and once more to clear it. Was it drawn
	(defq down (filter (# (. *board* :back? %0)) (cat *landing* *landed*))
		flying (or (get :float_back *board*) (nempty? down)))
	(when (or flying *flight_b_used*)
		(defq m (view-matrix))
		(. *flight_b* :fill 0)
		(when (nempty? down)
			(setq down (cwb-id-set down))
			(each (lambda ((name flags items))
				(unless (bits? flags 1) (cwb-draw-items *flight_b* (cwb-pick items down) m)))
				(cwb-layers (. *board* :get_doc))))
		(when (get :float_back *board*)
			(. *board* :draw_flight *flight_b* m :back)
			(. *board* :draw_selected *flight_b* m))
		(setq *flight_b_used* flying)
		:t))

(defun draw-flight ()
	; (draw-flight) -> :t | :nil
	;what is in flight in front of the document, on its canvas. Is the
	;canvas not what it was: it has something on it now, or had and has
	;not. What is behind is (draw-flight-back)
	(defq flying (. *board* :in_flight?)
		down (filter (# (not (. *board* :back? %0))) (cat *landing* *landed*)))
	(bind '(w h) (canvas-size))
	(defq shift (and flying (empty? down) (catch (flight-shift) :t)))
	(cond
		((list? shift)
			;only moved: the canvas is put where they now are, as it is
			;where it was and where it now is are told to the GUI as changed
			(unless (eql (str shift) (str (slice (. *flight* :get_bounds) 0 2)))
				(. *flight* :change_dirty (first shift) (second shift) w h))
			:nil)
		((or flying *flight_used* (nempty? down))
		;What is in flight may be partly off the paper, or all off it, and
		;be dragged back on. So that all of it is on the canvas, to be moved
		;with it, it is drawn moved over by what brings it all on, ox oy,
		;and the canvas put as far the other way. If it is too big for
		;that, or other things are drawn with it, it is drawn where it is,
		;and then it is not moved as a picture, (flight-shift), but drawn
		;each time
		(defq now (if flying (flight-front) (list)) ox 0 oy 0 whole :nil
			quiet (and (nempty? now) (empty? down) (= (. (get :temp *board*) :size) 0)))
		(when (and quiet (defq box (cwb-bounds now)))
			(bind '(bx by bx1 by1) (Mat3x2-box box (view-matrix)))
			(defq room 4
				bx (- (n2i (floor bx)) room) by (- (n2i (floor by)) room)
				bx1 (+ (n2i (floor bx1)) room 1) by1 (+ (n2i (floor by1)) room 1))
			(and (<= (- bx1 bx) w) (<= (- by1 by) h)
				(setq whole :t ox (cond ((< bx 0) (neg bx)) ((> bx1 w) (- w bx1)) (:t 0))
					oy (cond ((< by 0) (neg by)) ((> by1 h) (- h by1)) (:t 0)))))
		;on the canvas that is seen, if it is where it is to be, or on the other
		(defq m (if (and (= ox 0) (= oy 0)) (view-matrix) (Mat3x2-mul (Mat3x2-translate ox oy) (view-matrix)))
			moved (not (eql (str (. *flight* :get_bounds)) (str (list (neg ox) (neg oy) w h))))
			onto (if moved *flight_spare* *flight*))
		(. onto :fill 0)
		(when (nempty? down)
			(setq down (cwb-id-set down))
			(each (lambda ((name flags items))
				(unless (bits? flags 1) (cwb-draw-items onto (cwb-pick items down) m)))
				(cwb-layers (. *board* :get_doc))))
		(if flying (. *board* :draw_flight onto m :front))
		;what is on it, where each thing was when it was put there, and how
		;far over it was drawn
		(setq *flight_used* (or flying (nempty? down))
			*flight_at* (if whole
				(list *zoom* (map (# (elem-get %0 +cwb_id)) now) (map (# (elem-get %0 +cwb_m)) now) ox oy :nil
					(map (# (elem-get %0 +cwb_d)) now))))
		(cond
			(moved
				;its picture is shown while it is not seen, and then it is
				;put where the other was, and the other taken out
				((const (ffi "gui/canvas/lisp_swap")) onto +swap_write)
				(. *flight* :sub)
				(. *flight_clip* :add_child onto)
				(. onto :set_bounds (neg ox) (neg oy) w h)
				(. *flight* :set_bounds 0 0 w h)
				(. *flight_clip* :dirty_all)
				(setq *flight_spare* *flight* *flight* onto)
				:nil)
			(:t :t)))))

(defun draw-damage (boxes m)
	;only these parts of the document's picture, boxes of the document,
	;by this task: each is made clear and what of the document is in it
	;drawn, cut to it. A thing taken up or put down on a board of ten
	;thousand is then drawn at once, and not when the nodes have been round.
	;The parts that were drawn, in pixels of the canvas, for (show-parts)
	(bind '(w h) (canvas-size))
	(defq doc (. *board* :get_doc) skip (get :floating *board*) parts (list))
	(each (lambda ((x y x1 y1))
		;in pixels of the canvas, and a little more for the soft edge of a line
		(bind '(x y x1 y1) (Mat3x2-box (list x y x1 y1) m))
		(defq cx (max 0 (- (n2i (floor x)) 3)) cy (max 0 (- (n2i (floor y)) 3))
			cx1 (min w (+ (n2i (floor x1)) 4)) cy1 (min h (+ (n2i (floor y1)) 4)))
		(when (and (< cx cx1) (< cy cy1))
			(. *committed* :clear 0 cx cy (- cx1 cx) (- cy1 cy))
			(. *committed* :set_clip cx cy cx1 cy1)
			(cwb-draw *committed* doc m (list (n2f cx) (n2f cy) (n2f cx1) (n2f cy1)) skip)
			(push parts (list cx cy cx1 cy1))))
		boxes)
	(. *committed* :set_clip 0 0 w h)
	parts)

(defun show-parts (canvas parts)
	;the pixels of a canvas are shown, and only these parts of it are told
	;to the GUI as changed, (. view :add_dirty), so only they are drawn
	;again on the screen. (. canvas :swap) tells it all of the canvas
	(when ((const (ffi "gui/canvas/lisp_swap")) canvas +swap_write)
		(each (lambda ((x y x1 y1)) (. canvas :add_dirty x y (- x1 x) (- y1 y))) parts)))

(defun farm? ()
	;is the document one for the nodes to draw: it took this task a while
	;last time, the pixels are where they can reach, and it has not kept
	;going wrong
	;and it is seen from 0 0 at 1: the nodes do not know of a pan, and
	;draw rows of a canvas that is the document's own size
	(and (> *local_ms* *farm_ms*) (< *farm_fails* 3) (/= (canvas-key *committed*) 0)
		(= *zoom* 1.0) (= *pan_x* 0.0) (= *pan_y* 0.0) (= *turn* 0)))

(defun redraw ()
	;draw what has changed. All of the document; or only what was put on
	;top of it, on what is there; and what is over it
	;a paper that is another size, made so or put back by an undo, has
	;new canvases first
	(unless (eql (str (canvas-size)) (str *canvas_size*)) (board-resized))
	(defq m (view-matrix) doc_dirty (. *board* :dirty? +board_dirty_doc)
		append_dirty (. *board* :dirty? +board_dirty_append) show :nil show_over :nil show_under :nil show_back :nil show_parts :nil
		;all of the document's picture, or only parts of it
		damage (if doc_dirty (. *board* :take_damage)))
	(unless *damage_parts* (setq damage :all))
	;the paper, when it is another paper
	(defq show_paper *paper_dirty*)
	(when *paper_dirty*
		(setq *paper_dirty* :nil)
		(draw-paper))
	;what keeps the nodes' copies in step is told of every change
	(if (or doc_dirty append_dirty) (. *stripes* :note))
	;what has been let go of since this last looked is landing
	;what is drawn is another, the bars may be
	(if (or doc_dirty append_dirty) (setq *bars_stale* :t))
	(when doc_dirty
		(defq now (get :floating *board*) held (cwb-id-set now))
		(setq *landing* (cat *landing* (filter (# (not (cwb-id? held %0))) *flown*)) *flown* (cat now)))
	(cond
		((and *framing* (or doc_dirty append_dirty))
			;the nodes are drawing it as it was, it is drawn again when they have
			(setq *again* :t))
		((and doc_dirty (list? damage))
			;only where a few things were taken up or put down
			(if (nempty? damage) (setq show_parts (draw-damage damage m)))
			(setq *landing* (list)))
		((and doc_dirty (farm?) (. *stripes* :cheap?)
				;they draw the shapes and no paper, on pixels made clear for them.
				;What shows is as it was till they have done
				(progn (. *committed* :fill 0) (. *stripes* :frame *committed* *zoom* :nil)))
			;the nodes draw it, (frame-done) when they have. What was
			;landing is in this frame
			(. *board* :take_appended)
			(setq *framing* :t *landed* *landing* *landing* (list)))
		(doc_dirty
			(draw-local m)
			(setq show :t *landing* (list) *landed* (list))
			;the nodes are started, or brought in step, for the next time
			(if (farm?) (.-> *stripes* :start :warm)))
		(append_dirty
			(cwb-draw-items *committed* (. *board* :take_appended) m)
			(setq show :t)))
	(when (. *board* :dirty? +board_dirty_overlay)
		(draw-over)
		(if (draw-flight) (setq show_under :t))
		(if (draw-flight-back) (setq show_back :t))
		(sync-ui)
		(setq show_over :t))
	;shown last, when all else is done
	(if show (. *committed* :swap +swap_write))
	(if show_parts (show-parts *committed* show_parts))
	(if show_under (. *flight* :swap +swap_write))
	(if show_back (. *flight_b* :swap +swap_write))
	(if show_over (. *overlay* :swap +swap_write))
	(when *top_show*
		(setq *top_show* :nil)
		(. *top* :swap +swap_write)))

(defun frame-done (said)
	;what the stripes said of what came to them: :done, the nodes have
	;drawn a frame, :failed, one of them could not, or :warm or :nil
	(case said
		(:done
			(setq *framing* :nil *farm_fails* 0)
			;a document that is now small is this task's again
			(if (< (get :drawn *stripes*) *farm_shapes*) (setq *local_ms* 0))
			(when *again*
				(setq *again* :nil)
				(. *board* :touch +board_dirty_doc))
			;what landed in that frame is in the document's picture now,
			;and is no longer drawn with what is in flight
			(when (nempty? *landed*)
				(setq *landed* (list))
				(. *board* :touch +board_dirty_overlay))
			(. *committed* :swap +swap_write))
		(:failed
			;this task draws it, and the nodes are left alone if they keep at it
			(setq *framing* :nil *again* :nil *farm_fails* (inc *farm_fails*))
			(if (>= *farm_fails* 3) (. *stripes* :close))
			(setq *landing* (list) *landed* (list))
			(. *board* :touch +board_dirty_overlay)
			(draw-local (view-matrix))
			(. *committed* :swap +swap_write))))

(defun frame-watch ()
	;a frame the nodes have had for three seconds is not coming. They are
	;let go, this task draws it, and they are started again when next wanted
	(when (and *framing* (> (- (pii-time) (get :stamp *stripes*)) 3000000))
		(. *stripes* :close)
		(frame-done :failed)))

(defun stage-to-doc ()
	;what lies on the board that is not of the document is kept with it,
	;Chris: "menus and instruments are part of the docs state. So they
	;come and go as docs are loaded". An instrument is what it is, where,
	;turned how far, how long, and what it draws; a floating menu, and a
	;menu of swatches, where it is, Chris: "pallete should stay with the doc
	;state ! like the ring menu etc."
	(defq actors (. (. *board* :get_stage) :get_actors))
	(.-> (. *board* :get_doc)
		(:insert :tools (map (# (list (cond ((Ruler? %0) :ruler) ((Circle? %0) :circle) ((Protractor? %0) :protractor)
				((Setsquare45? %0) :set_square45) (:t :set_square))
				(get :origin %0) (get :angle %0) (. %0 :extent) (get :mode %0)))
			(filter (const Instrument?) actors)))
		(:insert :menus (map (# (get :origin %0)) (filter (# (get :floating %0)) (palettes *board*))))
		(:insert :swatches (map (# (get :origin %0)) (swatches-on *board*)))
		;the inks of the pens
		(:insert :inks (cat *palette*))))

(defun view-to-doc ()
	;how the document is seen now is kept with it, when it is saved
	;the pan as it would be on a paper the document's size, what a file
	;says, the same middle of what is seen
	(bind '(dw dh) (doc-size))
	(bind '(pw ph) (paper-size))
	(defq k (* 2.0 *zoom*))
	(. (. *board* :get_doc) :insert :view (list (+ *pan_x* (/ (n2f (- pw dw)) k)) (+ *pan_y* (/ (n2f (- ph dh)) k)) *zoom* *turn*))
	(stage-to-doc))

(defun stage-from-doc (&optional tools menus)
	;the instruments and floating menus of the document, and none other.
	;tools and menus if it has none, from a state kept before they were
	;the document's
	(defq doc (. *board* :get_doc) stage (. *board* :get_stage))
	(if (defq kept (. doc :find :tools)) (setq tools kept))
	(if (defq kept (. doc :find :menus)) (setq menus kept))
	;the inks of its pens
	(swatches-close)
	;or those of a new one, if it has none
	(pens-refill (if (and (list? (defq inks (. doc :find :inks))) (= (length inks) (length *palette*)) (every (const num?) inks))
		inks *pens_default*))
	(each (# (. stage :sub %0)) (filter (# (or (Instrument? %0) (and (Palette? %0) (get :floating %0)))) (. stage :get_actors)))
	(each (lambda (tool)
		(when (and (list? tool) (= (length tool) 5))
			(bind '(kind origin angle extent mode) tool)
			(when (defq at (find kind '(:ruler :protractor :circle :set_square :set_square45)))
				(defq tool ((elem-get (list Ruler Protractor Circle Setsquare Setsquare45) at)
					*board* (first origin) (second origin)))
				(def tool :angle angle :mode mode)
				(. tool :set_extent extent)
				(. stage :add tool))))
		(ifn tools (list)))
	;a menu opens from the time the board was last told. Not yet told
	;it is 0, the first tick is too long after that for the numbers of
	;its opening, and it was never drawn, though it took the pointers
	;that went down on it, an instrument under it could not be moved
	(. *board* :tick (pii-time))
	(each (lambda (at) (and (list? at) (= (length at) 2) (every (const num?) at)
			(palette-float *board* (first at) (second at))))
		(ifn menus (list)))
	(each (lambda (at) (and (list? at) (= (length at) 2) (every (const num?) at)
			(swatches-open (first at) (second at))))
		(ifn (. doc :find :swatches) (list)))
	(. *board* :touch +board_dirty_overlay))

(defun board-capture ()
	; (board-capture) -> settings
	;all that is not the document and a new document makes as it began,
	;for its undo to put back, (. board :whole_step): what every device is
	;bound to, the board's own settings, the pens' inks, and the
	;instruments and menus, which are kept with the document
	(view-to-doc)
	(defq doc (. *board* :get_doc))
	(list (get :bindings *board*) (get :settings *board*)
		(map (# (get %0 *board*)) '(:mode :pen :width :snap :snap_angle :keep_shape))
		(map (# (. doc :find %0)) '(:tools :menus :swatches :inks)) *style*
		(list *zoom* *pan_x* *pan_y*)))

(defun board-restore (settings)
	;as they were, (board-capture)
	(bind '(bindings pointer_settings vals doc_vals style seen) settings)
	(def *board* :bindings bindings :settings pointer_settings)
	(each (# (def *board* %0 %1)) '(:mode :pen :width :snap :snap_angle :keep_shape) vals)
	(defq doc (. *board* :get_doc))
	(each (# (. doc :insert %0 %1)) '(:tools :menus :swatches :inks) doc_vals)
	;and its paper
	(setq *style* style *paper_dirty* :t)
	(. doc :insert :style style)
	(. *style_toolbar* :set_selected (find style *styles*))
	;and how it was seen
	(bind '(zoom px py) seen)
	(setq *zoom* zoom *zoom_to* :nil *pan_x* px *pan_y* py)
	(view-changed)
	(stage-from-doc)
	(setq *dots_of* :nil)
	(sync-ui))

;a new document is a whole step: its undo puts back all it made as it began,
;(. board :whole_step)
(def *board* :on_capture board-capture :on_restore board-restore)
(defun view-from-doc ()
	;a document is seen as it was when it was saved, one that does not
	;say from 0 0 at 1. Its zoom is one it rests at
	(defq view (. (. *board* :get_doc) :find :view))
	(setq *zoom_to* :nil *pan_go* (list 0.0 0.0) *pan_x* 0.0 *pan_y* 0.0 *zoom* 1.0 *turn* 0)
	(when (and (list? view) (>= (length view) 3) (every (const num?) view))
		(setq *pan_x* (n2f (first view)) *pan_y* (n2f (second view)) *zoom* (zoom-magnet (n2f (third view))))
		;one kept before a board could be turned has no turn
		(if (> (length view) 3) (setq *turn* (logand (n2i (elem-get view 3)) 3))))
	;a file's pan is of a paper the document's size, the paper is the
	;frame's: the same middle of what is seen, Chris: "I opened the
	;test.cwb, and it didn't center on load"
	(bind '(dw dh) (doc-size))
	(bind '(pw ph) (paper-size))
	(defq k (* 2.0 *zoom*))
	(setq *pan_x* (- *pan_x* (/ (n2f (- pw dw)) k)) *pan_y* (- *pan_y* (/ (n2f (- ph dh)) k))))

(defun board-save (file)
	(setq *file* (cat (slice file 0 (if (defq i (rfind "." file)) (dec i) -1)) ".cwb"))
	(. (. *board* :get_doc) :insert :style *style*)
	;the paper is made the size of what is on it, so the file, shown as a
	;picture, is all that was drawn and no more
	(. *board* :fit)
	;what was drawn is the paper now, and is seen in the middle of what shows
	(view-home)
	(view-to-doc)
	(cwb-save (. *board* :get_doc) (file-stream *file* +file_open_write))
	(board-resized)
	(view-changed))

(defun board-load (file)
	(when (and (ends-with ".cwb" file) (defq doc (cwb-load (file-stream file))))
		(setq *file* file)
		(. *board* :set_doc doc)
		;the paper is the size of what is on it, whatever size the file said
		(. *board* :fit)
		(. *board* :forget)
		(view-from-doc)
		(stage-from-doc)
		;it is worked on as it was last
		(when (defq at (find (. doc :find :style) *styles*))
			(setq *style* (elem-get *styles* at))
			(. *style_toolbar* :set_selected at))
		(board-resized)
		(view-changed)))

;import actions and bindings
(import "./share.inc")
(import "./actions.inc")
(import "./config.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	;the mailboxes of the stripes are waited on after this task's own
	(defq select (cat (task-mboxes +select_size) (. *stripes* :mboxes)) *id* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	;it is there to be invited to a board that is shared, and has back what
	;it put by if it was closed while joined to one
	(share-declare :nil)
	(share-kept-back)
	;a corner keeps the shape of what it sizes, till the lock is put off
	(board-resized)
	;as it was when it was last closed, if it has been: where it was and
	;how big, as far as that is on the screen there now is
	;the strips of the binders, before the window is sized
	(binder-dots)
	(defq config (catch (config-load) (progn (prin _) (print) :t))
		;and no smaller than its bars ask, a window kept narrower than they
		;have grown would have their ends cut off
		place (if (list? config)
			(progn (bind '(cx cy cw ch) (slice config 0 4))
				(bind '(pw ph) (. *window* :pref_size))
				(view-fit cx cy (max cw pw) (max ch ph)))
			(apply view-locate (. *window* :pref_size))))
	(bind '(x y w h) place)
	(gui-add-front-rpc (. *window* :change x y w h))
	;it opens where the view was, or on the corner of the document, a
	;little of what is round it showing
	(.-> *image_scroll* :layout :dirty_all)

	;main event loop
	(mail-timeout (elem-get select +select_timer) rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((>= idx +select_size)
				;from the nodes that draw in stripes
				(frame-done (. *stripes* :handle (- idx +select_size) *msg*)))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) rate 0)
				;the words that are put down are those in the text field now
				(def *board* :text (. *text_field* :get_text))
				;what is on the board that moves by itself is told the time
				(. *board* :tick (pii-time))
				;and the zoom, if it is on its way somewhere, and what is
				;seen, if a wheel has it going
				(when *full_go*
					(setq *full_go* :nil)
					(full-window))
				(view-follow)
				(tools-snap (list (zoom-tick) (pan-tick)))
				(bars-tick)
				(tools-cull)
				(redraw)
				;the boards this is shared with are sent what has changed
				(share-tick)
				;once a second the nodes that draw are looked to
				(when (> (setq *kept* (inc *kept*)) 60)
					(setq *kept* 0)
					(frame-watch)
					(. *stripes* :keep)))
			((= idx +select_share)
				;from a board this is shared with
				(share-message *msg*))
			((= idx +select_picker)
				;save/load picker response
				(setq *msg* (trim *msg*))
				(mail-send *picker_mbox* "")
				(setq *picker_mbox* :nil)
				(cond
					;closed picker
					((eql *msg* ""))
					(*picker_mode* (board-save *msg*))
					(:t (board-load *msg*))))
			;must be gui event to main mailbox
			((. *window* :dispatch *msg*))
			(:t ;gui event
				(. *window* :event *msg*))))
	;close window. It is kept as a window, not as all of the screen. A
	;session is left, and this board's own document is what is kept
	(share-leave)
	(share-forget)
	(catch (progn (full-window) (config-save)) (progn (prin _) (print) :t))
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(. *stripes* :close)
	(gui-sub-rpc *window*)
	(if *committed* (. *committed* :free))
	(profile-report "Whiteboard App"))
