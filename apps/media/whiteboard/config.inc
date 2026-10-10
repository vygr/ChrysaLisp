;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

;What the board was when it was closed is what it is when it is opened:
;where its window was and how big, what the toolbars were set to, the
;zoom and where the view was, the instruments that were out, and what was
;drawn. In the user's folder, as every app's is.

(import "service/lock/app.inc")

(defq +config_version 1 +config_file (cat *env_home* "whiteboard.tre"))

(defun config-state ()
	; (config-state) -> emap
	;what the board is now
	(defq doc (. *board* :get_doc))
	(. doc :insert :style *style*)
	(bind '(x y w h) (. *window* :get_bounds))
	(scatter (Emap) :version +config_version
		:x x :y y :width w :height h
		:zoom *zoom*
		:scroll_x (ifn (get :value (get :hslider *image_scroll*)) 0)
		:scroll_y (ifn (get :value (get :vslider *image_scroll*)) 0)
		:mode (. *mode_toolbar* :get_selected) :ink (. *ink_toolbar* :get_selected)
		:radius (. *radius_toolbar* :get_selected) :style (. *style_toolbar* :get_selected)
		:snap *snap* :snap_angle *snap_angle* :keep_shape (get :keep_shape *board*)
		:text (. *text_field* :get_text) :file *file*
		;an instrument is what it is, where, turned how far, how long, and what it draws
		:tools (map (# (list (cond ((Ruler? %0) :ruler) ((Circle? %0) :circle) ((Protractor? %0) :protractor) (:t :set_square))
				(get :origin %0) (get :angle %0) (. %0 :extent) (get :mode %0)))
			(filter (const Instrument?) (. (. *board* :get_stage) :get_actors)))
		:doc (cwb-out doc)))

(defun config-apply (config)
	; (config-apply config) -> :nil | (x y width height scroll_x scroll_y)
	;the board is made what it was. Where its window was and its view in
	;it, :nil if this is not what a board was
	(when (and (Emap? config) (eql (. config :find :version) +config_version))
		(bind '(x y w h sx sy zoom mode ink radius style snap snap_angle keep_shape text file tools data)
			(gather config :x :y :width :height :scroll_x :scroll_y :zoom :mode :ink :radius :style
				:snap :snap_angle :keep_shape :text :file :tools :doc))
		(when (and data (defq doc (cwb-in data)))
			(. *board* :set_doc doc))
		(if (find zoom +zooms) (setq *zoom* zoom))
		(setq *file* (if (str? file) file) *snap* (if snap :t) *snap_angle* (if snap_angle :t))
		(def *board* :snap (if *snap* (n2f (. (. *board* :get_doc) :find :grid)) 0.0)
			:snap_angle (if *snap_angle* (const (/ +fp_pi 12.0)) 0.0)
			:keep_shape (if keep_shape :t))
		(toolbar-states *snap_toolbar* (list *snap* *snap_angle* (get :keep_shape *board*)))
		(when (and (num? style) (< -1 style (length *styles*)))
			(. *style_toolbar* :set_selected style)
			(setq *style* (elem-get *styles* style) *paper_dirty* :t))
		(when (and (num? ink) (< -1 ink (length *palette*)))
			(. *ink_toolbar* :set_selected ink)
			(def *board* :color (elem-get *palette* ink)))
		(when (and (num? radius) (< -1 radius (length *widths*)))
			(. *radius_toolbar* :set_selected radius)
			(def *board* :width (elem-get *widths* radius)))
		(when (and (num? mode) (< -1 mode (length *modes*)))
			(. *mode_toolbar* :set_selected mode)
			(def *board* :mode (elem-get *modes* mode)))
		(when (str? text)
			(. *text_field* :set_text text)
			(def *board* :text text))
		(each (lambda (tool)
			(when (and (list? tool) (= (length tool) 5))
				(bind '(kind origin angle extent mode) tool)
				(when (defq at (find kind '(:ruler :protractor :circle :set_square)))
					(defq tool ((elem-get (list Ruler Protractor Circle Setsquare) at)
						*board* (first origin) (second origin)))
					(def tool :angle angle :mode mode)
					(. tool :set_extent extent)
					(. (. *board* :get_stage) :add tool))))
			(ifn tools (list)))
		(board-resized)
		(if (every (const num?) (list x y w h sx sy)) (list x y w h sx sy))))

(defun config-load ()
	; (config-load) -> :nil | (x y width height scroll_x scroll_y)
	(config-apply (with-read-lock +config_file
		(catch (tree-load (file-stream +config_file)) :t))))

(defun config-save ()
	(defq config (config-state))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) config)))
