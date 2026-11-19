(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "apps/system/files/picker.inc")
(import "lib/math/matrix.inc")
(import "lib/math/obj.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/files/zip.inc")
(import "lib/net/http.inc")

;A model of a .obj, with its .mtl and the pictures its materials have,
;turned with the mouse and drawn by a (Render-tris), lib/gpu/
;render_farm.inc, which has the GPU draw it, or with none the nodes of this
;machine, a strip of the frame each, or this task if there is only the
;one. A .zip of them is opened as well, the .obj in it found and the rest
;taken from it. It starts on a cube of its own, see data/

(enums +event +event_user
	(enum open mode url model))

;the timer is last, what is first in the list is what is read first
(enums +select 0
	(enum main fetch picker timer))

(defq +size 512 +timer_rate (/ 1000000 30) +near +double_1 +far (n2d 20)
	;the light is up, to the left, and round to our side
	sun (doubles (n2d -0.4) (n2d 0.5) (n2d 0.77))
	lens (mat4x4-frustum (n2d -0.5) (n2d 0.5) (n2d 0.5) (n2d -0.5) +near +far)
	select :nil id :t picker :nil
	;what a fetch, a task of its own, fetched: each address and its bytes,
	;read in place of the internet as a model is made of them, and what was
	;asked for, (name address picture), till it answers
	*fetched* :nil fetching :nil
	;how it is turned, how far away, and where the mouse was when pressed
	yaw (n2d 0.6) pitch (n2d 0.4) away (n2d 3.2) drag :nil
	;the parts of the model, a material each: (id canvas tint), what the
	;render knows its mesh by, a number that is never used twice, its
	;picture and the colour that is multiplied by
	parts (list) name "" triangles 0 mesh_ids 0
	;what draws the frames, and how many it has in the second
	render :nil frames 0 since (pii-time)
	;models to show it can, the cube of its own, and others fetched from
	;the internet as they are picked, over https, by lib/net/tls.inc, and
	;kept in downloads/models/ for the next time, fetch.lisp, each
	;an .obj, its .mtl and the pictures that names: examples of the three.js
	;project and test models of assimp, on GitHub, and Keenan Crane's model
	;repository at CMU. (name address [picture]),
	;the picture for a model that names none
	*models* (list
		(list "Cube" :nil)
		(list "Man" "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/models/obj/male02/male02.obj")
		(list "Woman" "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/models/obj/female02/female02.obj")
		(list "Spider" "https://raw.githubusercontent.com/assimp/assimp/master/test/models/OBJ/spider.obj")
		(list "Gun" "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/models/obj/cerberus/Cerberus.obj"
			"https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/models/obj/cerberus/Cerberus_A.jpg")
		;Keenan Crane's cow, a .zip of the .obj and its picture, from a
		;server of TLS 1.2
		(list "Spot" "https://www.cs.cmu.edu/~kmcrane/Projects/ModelRepository/spot.zip")))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Model" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *tools* ()
			(ui-buttons (+sym_open) +event_open))
		(. (ui-textfield *url* (:paper +argb_white :clear_text "" :min_width 240
					:hint_text "or an https:// address, of a .zip or an .obj")) :connect +event_url)
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font :font_body :paper :paper_toolbar2)) :connect +event_mode)
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *model* ("Cube" "Man" "Woman" "Spider" "Gun" "Spot") (:font :font_body :paper :paper_toolbar2))
			:connect +event_model)
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-label *status* (:text "..." :font :font_body))
	(ui-label *rate* (:text "..." :font :font_body))
	(ui-backdrop _ (:style :plain :paper +argb_black :min_width +size :min_height +size)
		;its pixels are in shared memory if the host has it, the nodes of
		;this machine then draw on them, a strip each
		(ui-element *canvas* (ifn (canvas-shared +size +size 1) (Canvas +size +size 1)))))

(ui-tool-tips *tools* '("open a model, an .obj or a .zip of one"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a model, from files or a zip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zip-text (data entry)
	(defq out (string-stream (cat "")))
	(if (zip-read data entry out) (str out)))

(defun zip-fetch (data entries folder name)
	; (zip-fetch data entries folder name) -> :nil | str
	;a file a model names, beside it in its zip, or anywhere in it
	(if (defq entry (or (zip-find entries (cat folder name)) (zip-find entries name))) (zip-text data entry)))

(defun model-fetch (url)
	; (model-fetch url) -> :nil | str
	;what is at an address, if it is there: of what the fetch task
	;fetched, if it has
	(if *fetched* (. *fetched* :find url)
		(and (defq resp (http-get url)) (= (pfind resp :status) 200) (http-body-str resp))))

(defun fetch-start (pick address picture)
	; (fetch-start pick address picture)
	;the files of a model fetched by a task of its own, the window drawn
	;meanwhile. A new mailbox each time, what a fetch asked before says is
	;not heard, the last picked is the one shown
	(. *status* :set_text (cat "Fetching " (ifn pick address) " ..."))
	(elem-set select +select_fetch (defq reply (mail-mbox)))
	(setq fetching (list pick address picture))
	(mail-send (open-child (cat *app_root* "fetch.lisp") +kn_call_pin)
		(cat reply address "\n" (ifn picture ""))))

(defun model-source (path)
	; (model-source path) -> :nil | (obj_text fetch pictures)
	;the text of the .obj, a function that gives the bytes of a file it
	;names: beside it, in the same zip, or at the same address, and in a
	;zip, the pictures there are, for a model that names none
	(defq remote (starts-with "https://" path) data (if remote (model-fetch path) (load path)))
	(cond
		((not data) :nil)
		((ends-with ".zip" path)
			;what a Mac puts in a zip of its own, beside each file, is not
			;the file
			(when (and (defq entries (filter (# (not (starts-with "__MACOSX/" (first %0)))) (ifn (zip-list data) (list))))
					(nempty? (defq objs (filter (# (ends-with ".obj" (to-lower (first %0)))) entries))))
				;of several, the most of it, the most detailed
				(defq obj (reduce (# (if (> (elem-get %1 +zip_entry_full) (elem-get %0 +zip_entry_full)) %1 %0)) objs (first objs))
					;(rfind) gives the place after the slash, the folder is up to it
					folder (slice (first obj) 0 (ifn (rfind "/" (first obj)) 0)))
				(list (zip-text data obj)
					`(lambda (name) (,zip-fetch ',data ',entries ,folder name))
					(filter (# (canvas-loader-of (to-lower %0))) (map (const first) entries)))))
		(:t (defq folder (slice path 0 (ifn (rfind "/" path) 0)))
			(list data (if remote `(lambda (name) (,model-fetch (cat ,folder name)))
					`(lambda (name) (load (cat ,folder name)))) (list)))))

(defun model-picture (name bytes)
	; (model-picture name bytes) -> :nil | canvas
	;a picture of a kind a canvas loads, from its bytes, its pixels in
	;shared memory if the host has it, the nodes that draw a strip each
	;all read the one copy of it
	(and bytes (defq kind (canvas-loader-of (to-lower name)))
		(defq canvas ((third kind) (string-stream bytes)))
		(. (shader-strip-picture canvas) :swap +swap_write)))

(defun model-plain ()
	; (model-plain) -> canvas
	;a picture of one white pixel, for a material that has none: its
	;colour is the tint
	(.-> (ifn (canvas-shared 1 1 1) (Canvas 1 1 1)) (:fill +argb_white) (:swap +swap_write)))

(defun model-load (path &optional picture)
	; (model-load path [picture]) -> :nil | :t
	;the model of that file, its parts, each a material, in place of what
	;there was. picture, an address, is the picture of a model that names
	;none
	(when (defq source (model-source path))
		(bind '(text named pictures) source)
		;each file it names had the once, two materials may name one picture
		(defq got (Fmap 11) fetch (lambda (file)
				(unless (. got :find file) (. got :insert file (list (named file))))
				(first (. got :find file)))
			;and each picture is made the once, and the plain one
			made (Fmap 11) painted (lambda (file)
				(unless (. made :find file) (. made :insert file (list (model-picture file (fetch file)))))
				(first (. made :find file)))
			plain :nil)
		(defq obj (obj-read (string-stream text)) materials (Fmap 11) fit (obj-fit obj) out (list) count 0)
		(each (lambda (library)
				(if (defq mtl (fetch library))
					(. (mtl-read (string-stream mtl)) :each (lambda (k v) (. materials :insert k v)))))
			(. obj :find :libraries))
		;a model that names no picture, in a zip that has one, the one whose
		;name begins as the model's does, or the first
		(defq stem (first (split (last (split path "/")) "._"))
			lone (and (nempty? pictures) (notany (# (and (defq m (. materials :find (first %0))) (. m :find :map))) (. obj :find :groups))
				(model-picture (defq lone_name (ifn (some (# (if (starts-with stem (last (split %0 "/"))) %0)) pictures) (first pictures)))
					(fetch lone_name)))
			;one given, for a model that names none
			lone (or lone (and picture (notany (# (and (defq m (. materials :find (first %0))) (. m :find :map))) (. obj :find :groups))
					(model-picture picture (model-fetch picture)))))
		(each (lambda ((material corners))
				(defq said (. materials :find material) picture lone
					;a picture is its colour, at full
					tint (cond (lone (list +double_1 +double_1 +double_1)) (said (. said :find :colour)) ((list (n2d 0.8) (n2d 0.8) (n2d 0.8)))))
				(when (and said (defq file (. said :find :map)))
					;a picture's name may have a folder of another machine in it
					(defq file (last (split file (const (char-class "/\\")))))
					(if (setq picture (painted file))
						(setq tint (list +double_1 +double_1 +double_1))))
				(defq verts (obj-flat obj corners fit) n (/ (length verts) 8))
				(setq count (+ count (/ n 3)))
				(push out (list (++ mesh_ids) (ifn picture (ifn plain (setq plain (model-plain))))
						(apply (const doubles) tint) verts)))
			(. obj :find :groups))
		(when (nempty? out)
			;the meshes of the last are forgotten, these are the render's now
			(. render :forget)
			(setq parts (map (lambda ((id canvas tint verts))
					(. render :mesh id verts)
					(list id canvas tint)) out)
				triangles count name (last (split path "/")))
			:t)))

(defun fetch-done (msg)
	; (fetch-done msg)
	;the files of the model, as the fetch task sends them, and the model
	;made of them
	(bind '(pick address picture) fetching)
	(setq fetching :nil *fetched* (Fmap 11))
	(defq at 0 nl "\n" why (if (starts-with "!" msg) (slice msg 1 -1)))
	(while (and (not why) (< at (length msg)))
		(defq e (find nl msg at) url (slice msg at e)
			e1 (find nl msg (inc e)) n (str-as-num (slice msg (inc e) e1)))
		(. *fetched* :insert url (slice msg (inc e1) (+ e1 1 n)))
		(setq at (+ e1 1 n)))
	(defq loaded (and (not why) (nempty? msg) (model-load address picture))
		why (if why (cat ": " why) "."))
	(setq *fetched* :nil)
	(cond
		((and loaded pick)
			(. *status* :set_text (cat pick ", " name ", " (str triangles) " triangles, " (str (length parts)) " materials, fetched from the internet.")))
		(loaded
			(. *status* :set_text (cat name ", " (str triangles) " triangles, " (str (length parts)) " materials, fetched.")))
		(pick (. *status* :set_text (cat "Could not fetch " pick why)))
		((. *status* :set_text (cat "Could not fetch a model from " address why)))))

;;;;;;;;;;;
; drawing
;;;;;;;;;;;

(defun model-matrix ()
	;how the model is turned, and it is away down the view
	(mat4x4-mul (mat4x4-translate +double_0 +double_0 (neg away))
		(mat4x4-mul (mat4x4-rotx pitch) (mat4x4-roty yaw))))

(defun draw-frame ()
	;the model as it is turned, a part a thing drawn, with its picture
	(unless (empty? parts)
		(defq vvals (list (list 'model (model-matrix)) (list 'lens lens)))
		(. render :frame (map (lambda ((id canvas tint))
				(list id vvals (list (list 'tint tint) (list 'sun sun)) (list canvas))) parts))
		(++ frames)))

(defun mode-show ()
	;the button as the render has it: the GPU is not to be had on a host
	;with none
	(. *mode* :set_selected (if (eql (. render :mode) :gpu) 1 0)))

(defun picker-open ()
	;the Files app's picker, for the model to open
	(if picker (mail-send picker ""))
	(setq picker (file-pick (elem-get select +select_picker) "Open model..." :nil :nil :nil :nil :nil '("apps/demos/model/data"))))

(defun main ()
	(setq select (task-mboxes +select_size)
		render (Render-tris *canvas* (cat *app_root* "model_vertex.shader") (cat *app_root* "model_lit.shader")))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(mode-show)
	(. *model* :set_selected 0)
	(if (model-load (cat *app_root* "data/cube.obj"))
		(. *status* :set_text (cat name ", " (str triangles) " triangles, " (str (length parts)) " materials. Drag to turn, the wheel for nearer.")))
	(. *window* :open)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with: a child has started, wants a part, or has drawn its
		;part of a frame
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_open) (picker-open))
					((= id +event_url)
						;a model fetched, over https
						(fetch-start :nil (trim (get :clear_text *url*)) :nil))
					((= id +event_mode)
						(. render :mode (if (= (. *mode* :get_selected) 1) :gpu :cpu))
						(mode-show))
					((= id +event_model)
						;one of the models it knows, the cube here, the rest fetched
						(bind '(pick address &optional picture) (elem-get *models* (. *model* :get_selected)))
						(cond
							((not address)
								(if (model-load (cat *app_root* "data/cube.obj"))
									(. *status* :set_text (cat name ", " (str triangles) " triangles, " (str (length parts)) " materials."))))
							((fetch-start pick address picture))))
					((= (getf msg +ev_msg_type) +ev_type_wheel)
						;nearer or further
						(setq away (max (n2d 1.5) (min (n2d 12) (+ away (* (n2d 0.2) (n2d (getf msg +ev_msg_wheel_y))))))))
					((and (= id (. *canvas* :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse))
						;a drag turns it, across about the up, down about the side
						(defq mx (getf msg +ev_msg_mouse_rx) my (getf msg +ev_msg_mouse_ry))
						(cond
							((= (getf msg +ev_msg_mouse_buttons) 0) (setq drag :nil))
							(drag (setq yaw (+ yaw (* (n2d 0.01) (n2d (- mx (first drag)))))
									pitch (max (n2d -1.5) (min (n2d 1.5) (+ pitch (* (n2d 0.01) (n2d (- my (second drag)))))))
									drag (list mx my)))
							((setq drag (list mx my)))))
					((. *window* :event msg))))
			((= idx +select_fetch)
				(fetch-done msg))
			((= idx +select_picker)
				;the model to open, or "" the picker closed
				(mail-send picker "")
				(setq picker :nil msg (trim msg))
				(cond
					((eql msg ""))
					((model-load msg)
						(. *status* :set_text (cat name ", " (str triangles) " triangles, " (str (length parts)) " materials.")))
					((. *status* :set_text (cat "Could not read a model from " msg ".")))))
			(:t ;the timer, a frame, if the last is not still out with a farm
				(defq start (pii-time))
				(unless (. render :busy) (draw-frame))
				;the next frame is a tick on, or four times as long as this
				;one took if that is more: on a slow machine drawing on its
				;own node it turns less smoothly, and never has more than a
				;quarter of the node, the apps beside it have the rest
				(defq took (- (pii-time) start))
				(mail-timeout (elem-get select +select_timer)
					(- (max +timer_rate (* took 4)) took) 0)
				(. render :tick)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(mode-show)
					(. *rate* :set_text (cat (str (/ (* frames 1000000) (- now since))) " frames a second, "
							(. render :how)))
					(setq frames 0 since now)))))
	(. render :close)
	(if picker (mail-send picker ""))
	(. *window* :close))
