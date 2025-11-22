;Configuration and bookmark management for Fractal Explorer

(defq *config_version* 2
	*config_file* "fractalexplorer.cfg"
	*bookmarks_file* "fractalexplorer_bookmarks.tre")

;Configuration structure
(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:fractal_type +fractal_julia
		:color_scheme +color_scheme_rainbow
		:center_x (mbfp-from-fixed 0.0)
		:center_y (mbfp-from-fixed 0.0)
		:zoom (mbfp-from-fixed 1.5)
		:param1 (mbfp-from-fixed -0.4)
		:param2 (mbfp-from-fixed 0.6)
		:param3 (mbfp-from-fixed 2.0)
		:smooth_coloring :nil
		:orbit_trap +orbit_trap_none
		:animation_enabled :nil
		:bookmarks (list)))

;Load configuration from file
(defun config-load ()
	(defq cfg (config-default))
	(when (and (file-exists *config_file*)
			   (defq stream (file-stream *config_file*)))
		(defq loaded (tree-load stream))
		(when (and loaded (= (. loaded :find :version) *config_version*))
			(setq cfg loaded)))
	cfg)

;Save configuration to file
(defun config-save (config)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream config)))

;Bookmark management

(defun bookmark-create (name ftype cx cy z p1 p2 p3 cscheme)
	(scatter (Emap)
		:name name
		:fractal_type ftype
		:center_x cx
		:center_y cy
		:zoom z
		:param1 p1
		:param2 p2
		:param3 p3
		:color_scheme cscheme))

(defun bookmark-save (bookmarks)
	(when (defq stream (file-stream *bookmarks_file* +file_open_write))
		(tree-save stream (scatter (Emap) :version *config_version* :bookmarks bookmarks))))

(defun bookmark-load ()
	(if (and (file-exists *bookmarks_file*)
			 (defq stream (file-stream *bookmarks_file*)))
		(progn
			(defq data (tree-load stream))
			(if (and data (= (. data :find :version) *config_version*))
				(. data :find :bookmarks)
				(list)))
		(list)))

(defun bookmark-add (bookmarks name ftype cx cy z p1 p2 p3 cscheme)
	(push bookmarks (bookmark-create name ftype cx cy z p1 p2 p3 cscheme))
	bookmarks)

(defun bookmark-apply (bookmark)
	;returns (list ftype cx cy z p1 p2 p3 cscheme)
	(list
		(. bookmark :find :fractal_type)
		(. bookmark :find :center_x)
		(. bookmark :find :center_y)
		(. bookmark :find :zoom)
		(. bookmark :find :param1)
		(. bookmark :find :param2)
		(. bookmark :find :param3)
		(. bookmark :find :color_scheme)))

;Animation helpers

(defq +anim_none 0
	+anim_zoom_in 1
	+anim_zoom_out 2
	+anim_rotate_params 3
	+anim_explore 4)

(defun anim-step-zoom (cx cy zoom direction steps current)
	;returns new zoom value
	(defq factor (if (= direction +anim_zoom_in)
		(mbfp-from-fixed 0.95)
		(mbfp-from-fixed 1.05)))
	(mbfp-mul zoom factor))

(defun anim-step-params (p1 p2 p3 steps current)
	;rotate Julia parameters in a circle
	(defq angle (/ (* current 6.28318) steps)
		radius (mbfp-from-fixed 0.7))
	(list
		(mbfp-mul radius (<< (fix (* (sin angle) 1000000)) (- +mbfp_shift 20)))
		(mbfp-mul radius (<< (fix (* (cos angle) 1000000)) (- +mbfp_shift 20)))
		p3))

;Statistics tracking

(defun stats-create ()
	(scatter (Emap)
		:render_start 0
		:render_end 0
		:render_time 0
		:zoom_level "1.0"
		:coordinates "0.0, 0.0"
		:fractal_name "Julia Set"))

(defun stats-update (stats ftype cx cy zoom start_time end_time)
	(def stats
		:render_start start_time
		:render_end end_time
		:render_time (- end_time start_time)
		:zoom_level (str (/ 1000000 (max 1 (>>> zoom (- +mbfp_shift 20)))))
		:coordinates (cat (str (>>> cx (- +mbfp_shift 20))) ", " (str (>>> cy (- +mbfp_shift 20))))
		:fractal_name (elem-get
			'("Julia Set" "Burning Ship" "Newton" "Tricorn" "Phoenix"
			  "Lyapunov" "Mandelbrot^3" "Mandelbrot^4" "Mandelbox"
			  "Celtic" "Buffalo" "Perpendicular" "Heart" "Magnet")
			ftype))
	stats)

;Keyboard shortcut helpers

(defq +key_pan_step 20)

(defun key-pan (cx cy zoom dx dy)
	;pan the view by dx, dy pixels
	(list
		(+ cx (mbfp-offset dx 800 zoom))
		(+ cy (mbfp-offset dy 800 zoom))))
