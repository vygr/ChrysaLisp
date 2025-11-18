;Export functionality for Fractal Explorer

;Simple PPM export (can be converted to PNG externally)
(defun export-to-ppm (canvas filename width height)
	;exports canvas to PPM format (plain text)
	(when (defq stream (file-stream filename +file_open_write))
		;PPM header
		(write-line stream (cat "P3"))
		(write-line stream (cat (str width) " " (str height)))
		(write-line stream (cat "255"))

		;pixel data
		(defq y 0)
		(while (< y height)
			(defq x 0)
			(while (< x width)
				;read pixel color from canvas
				(defq color (. canvas :get_pixel x y))
				(defq r (logand (>>> color 16) 0xff))
				(defq g (logand (>>> color 8) 0xff))
				(defq b (logand color 0xff))
				(write-line stream (cat (str r) " " (str g) " " (str b)))
				(++ x))
			(++ y)
			(task-slice))
		:t))

;Export configuration to file
(defun export-config-to-file (filename fractal_type cx cy zoom p1 p2 p3 cscheme)
	(when (defq stream (file-stream filename +file_open_write))
		(defq config (scatter (Emap)
			:fractal_type fractal_type
			:center_x cx
			:center_y cy
			:zoom zoom
			:param1 p1
			:param2 p2
			:param3 p3
			:color_scheme cscheme
			:timestamp (pii-time)))
		(tree-save stream config)
		:t))

;Import configuration from file
(defun import-config-from-file (filename)
	(when (and (file-exists filename)
			   (defq stream (file-stream filename)))
		(defq config (tree-load stream))
		(when config
			(list
				(. config :find :fractal_type)
				(. config :find :center_x)
				(. config :find :center_y)
				(. config :find :zoom)
				(. config :find :param1)
				(. config :find :param2)
				(. config :find :param3)
				(. config :find :color_scheme)))))

;Create a frame filename for animation export
(defun get-animation-frame-filename (base_name frame_number)
	(cat base_name "_frame_" (str (+ 10000 frame_number)) ".ppm"))

;Export animation sequence info
(defun export-animation-info (filename start_params end_params total_frames)
	(when (defq stream (file-stream filename +file_open_write))
		(write-line stream (cat "# Fractal Explorer Animation Sequence"))
		(write-line stream (cat "# Total Frames: " (str total_frames)))
		(write-line stream (cat "# Start Time: " (str (pii-time))))
		(tree-save stream (scatter (Emap)
			:start_params start_params
			:end_params end_params
			:total_frames total_frames))
		:t))
