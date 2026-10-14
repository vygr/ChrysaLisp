(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/math/obj.inc")
(import "lib/gpu/vp.inc")
(import "lib/gpu/gui.inc")
(import "lib/files/zip.inc")

;A model of a .obj, with its .mtl and the pictures its materials have,
;turned with the mouse and drawn by the GPU of the GUI, or by native code
;here if there is none. A .zip of them is opened as well, the .obj in it
;found and the rest taken from it. It starts on a cube of its own, see
;data/

(enums +event +event_user
	(enum open mode))

;the timer is last, what is first in the list is what is read first
(enums +select 0
	(enum main picker timer))

(defq +size 512 +timer_rate (/ 1000000 30) +near +real_1 +far (n2r 20)
	vertex (shader-load (cat *app_root* "model_vertex.shader"))
	pixel (shader-load (cat *app_root* "model_lit.shader"))
	;the light is up, to the left, and round to our side
	sun (reals (n2r -0.4) (n2r 0.5) (n2r 0.77))
	lens (Mat4x4-frustum (n2r -0.5) (n2r 0.5) (n2r 0.5) (n2r -0.5) +near +far)
	select :nil id :t picker :nil
	;how it is turned, how far away, and where the mouse was when pressed
	yaw (n2r 0.6) pitch (n2r 0.4) away (n2r 3.2) drag :nil
	;the parts of the model, a material each: (verts tris canvas tint mesh)
	parts (list) name "" triangles 0
	;on the GPU, the pair, and is it wanted; here, the pipeline
	gpu_pair :nil gpu_want :t pipeline :nil cpu_time 0 cpu_frames 0)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Model" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *tools* ()
			(ui-buttons (+sym_open) +event_open))
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font *env_body_font* :color *env_toolbar2_col*)) :connect +event_mode)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-backdrop _ (:style :plain :color +argb_black :min_width +size :min_height +size)
		(ui-element *canvas* (Canvas +size +size 1) (:color 0))))

(ui-tool-tips *tools* '("open a model, an .obj or a .zip of one"))

(defun say (text)
	(unless (eql (get :text *status*) text)
		(def *status* :text text)
		(.-> *status* :layout :dirty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a model, from files or a zip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zip-text (data entry)
	(defq out (string-stream (cat "")))
	(if (zip-read data entry out) (str out)))

(defun model-source (path)
	; (model-source path) -> :nil | (obj_text fetch)
	;the text of the .obj, and a function that gives the bytes of a file
	;it names, beside it, or in the same zip
	(cond
		((ends-with ".zip" path)
			(when (and (defq data (load path)) (defq entries (zip-list data))
					(defq obj (some (# (if (ends-with ".obj" (first %0)) %0)) entries)))
				(defq folder (slice (first obj) 0 (inc (ifn (rfind "/" (first obj)) -1))))
				(list (zip-text data obj)
					(lambda (name)
						(if (defq entry (or (zip-find entries (cat folder name)) (zip-find entries name)))
							(zip-text data entry))))))
		((defq text (load path))
			(defq folder (slice path 0 (inc (ifn (rfind "/" path) -1))))
			(list text (lambda (name) (load (cat folder name)))))))

(defun model-picture (name bytes)
	; (model-picture name bytes) -> :nil | canvas
	;a picture of a kind a canvas loads, from its bytes
	(and bytes (defq kind (canvas-loader-of (to-lower name)))
		(defq canvas ((third kind) (string-stream bytes)))
		(. canvas :swap +swap_write)))

(defun model-plain ()
	; (model-plain) -> canvas
	;a picture of one white pixel, for a material that has none: its
	;colour is the tint
	(.-> (Canvas 1 1 1) (:fill +argb_white) (:swap +swap_write)))

(defun model-load (path)
	; (model-load path) -> :nil | :t
	;the model of that file, its parts, each a material, in place of what
	;there was
	(when (defq source (model-source path))
		(bind '(text fetch) source)
		(defq obj (obj-read (string-stream text)) materials (Fmap 11) fit (obj-fit obj) out (list) count 0)
		(each (lambda (library)
				(if (defq mtl (fetch library))
					(. (mtl-read (string-stream mtl)) :each (lambda (k v) (. materials :insert k v)))))
			(. obj :find :libraries))
		(each (lambda ((material corners))
				(defq said (. materials :find material) picture :nil
					tint (if said (. said :find :colour) (list (n2r 0.8) (n2r 0.8) (n2r 0.8))))
				(when (and said (defq file (. said :find :map)))
					;a picture's name may have a folder of another machine in it
					(defq file (last (split file (const (char-class "/\\")))))
					(if (setq picture (model-picture file (fetch file)))
						(setq tint (list +real_1 +real_1 +real_1))))
				(defq verts (obj-flat obj corners fit) n (/ (length verts) 8))
				(setq count (+ count (/ n 3)))
				(push out (list verts (apply (const nums) (range 0 n)) (ifn picture (model-plain))
					(apply (const reals) tint) :nil)))
			(. obj :find :groups))
		(when (nempty? out)
			;what the GPU kept of the last is let go of
			(each (# (if (elem-get %0 4) (canvas-mesh-destroy (elem-get %0 4)))) parts)
			(setq parts out triangles count name (last (split path "/")))
			:t)))

;;;;;;;;;;;
; drawing
;;;;;;;;;;;

(defun model-matrix ()
	;how the model is turned, and it is away down the view
	(mat4x4-mul (Mat4x4-translate +real_0 +real_0 (neg away))
		(mat4x4-mul (Mat4x4-rotx pitch) (Mat4x4-roty yaw))))

(defun draw-gpu (vvals)
	; (draw-gpu vvals) -> :t | :nil
	;a frame by the GPU, a draw a part. :nil if it is to be drawn here
	(and gpu_want (not gpu_pair)
		(unless (setq gpu_pair (shader-gui-pair vertex pixel))
			(setq gpu_want :nil)
			(. *mode* :set_selected 0)))
	(when (and gpu_want gpu_pair (every (# (shader-gui-texture (elem-get %0 2))) parts))
		(each (# (unless (elem-get %0 4) (elem-set %0 4 (shader-gui-mesh (shader-verts-str (first %0)))))) parts)
		(defq drawn (shader-gui-frame *canvas* (map (lambda ((verts tris canvas tint mesh))
				(list gpu_pair (ifn mesh 0) (shader-pack vertex vvals)
					(shader-pack pixel (list (list 'tint tint) (list 'sun sun))) (list (shader-gui-texture canvas))))
			parts)))
		(cond
			((eql drawn :error)
				(setq gpu_pair :nil gpu_want :nil)
				(. *mode* :set_selected 0)
				:nil)
			(drawn :t)
			;busy with the last frame, or a picture not there yet, a frame of
			;the GUI is wanted
			(:t (. *status* :dirty) :t))))

(defun draw-cpu (vvals)
	;a frame by native code, here, a part after another into the one depth
	(unless pipeline (setq pipeline (shader-vp-pipeline vertex pixel)))
	(defq began (pii-time) depth (shader-vp-depth +size +size) pixmap (getf *canvas* +canvas_pixmap 0))
	(. *canvas* :fill +argb_black)
	(each (lambda ((verts tris canvas tint mesh))
			(shader-vp-draw-tris pipeline verts tris pixmap depth vvals
				(list (list 'tint tint) (list 'sun sun) (list 'surface (getf canvas +canvas_pixmap 0)))))
		parts)
	(. *canvas* :swap +swap_write)
	(setq cpu_time (+ cpu_time (- (pii-time) began)) cpu_frames (inc cpu_frames)))

(defun draw-frame ()
	(defq vvals (list (list 'model (model-matrix)) (list 'lens lens)))
	(cond
		((empty? parts))
		((draw-gpu vvals))
		((draw-cpu vvals))))

(defun picker-open ()
	;the Files app's picker, for the model to open
	(if picker (mail-send picker ""))
	(mail-send (setq picker (open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Open model..." "|" ".")))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *mode* :set_selected 1)
	(if (model-load (cat *app_root* "data/cube.obj"))
		(say (cat name ", " (str triangles) " triangles, " (str (length parts)) " materials. Drag to turn, the wheel for nearer.")))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_open) (picker-open))
					((= id +event_mode)
						(setq gpu_want (= (. *mode* :get_selected) 1)))
					((= (getf msg +ev_msg_type) +ev_type_wheel)
						;nearer or further
						(setq away (max (n2r 1.5) (min (n2r 12) (+ away (* (n2r 0.2) (n2r (getf msg +ev_msg_wheel_y))))))))
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse))
						;a drag turns it, across about the up, down about the side
						(defq mx (getf msg +ev_msg_mouse_rx) my (getf msg +ev_msg_mouse_ry))
						(cond
							((= (getf msg +ev_msg_mouse_buttons) 0) (setq drag :nil))
							(drag (setq yaw (+ yaw (* (n2r 0.01) (n2r (- mx (first drag)))))
									pitch (max (n2r -1.5) (min (n2r 1.5) (+ pitch (* (n2r 0.01) (n2r (- my (second drag)))))))
									drag (list mx my)))
							((setq drag (list mx my)))))
					((. *window* :event msg))))
			(+select_picker
				;the model to open, or "" the picker closed
				(mail-send picker "")
				(setq picker :nil msg (trim msg))
				(cond
					((eql msg ""))
					((model-load msg)
						(say (cat name ", " (str triangles) " triangles, " (str (length parts)) " materials.")))
					((say (cat "Could not read a model from " msg ".")))))
			(:t ;the timer, a frame
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(draw-frame))))
	(if picker (mail-send picker ""))
	(each (# (if (elem-get %0 4) (canvas-mesh-destroy (elem-get %0 4)))) parts)
	(gui-sub-rpc *window*))
