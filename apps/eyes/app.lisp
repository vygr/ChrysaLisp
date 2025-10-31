;;;;;;;;;;;;;;;
; apps/eyes/app.lisp
;;;;;;;;;;;;;;;
(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")

;;;
;;; Configuration
;;;

(defq *config* :nil *config_version* 1
      *config_file* (cat *env_home* "eyes.tre"))

(defun get-default-config ()
    (scatter (Emap)
        :version *config_version*
        :width min_width
        :height min_height))

(defun load-config ()
    (if (defq stream (file-stream *config_file*))
        (setq *config* (tree-load stream)))
    (if (or (not *config*) (/= (. *config* :find :version) *config_version*))
        (setq *config* (get-default-config))))

(defun save-config ()
    (when (defq stream (file-stream *config_file* +file_open_write))
        (tree-save stream *config*)))

;;;
;;; UI and State
;;;

(enums +event 0
    (enum close max min))

(enums +select 0
    (enum main timer))

(defq +rate (/ 1000000 30) ; 30 FPS
    min_width 256 min_height 128
    max_width 512 max_height 256
    *running* :t)

(ui-window *window* ()
    (ui-title-bar _ "Eyes" (0xea19 0xea1b 0xea1a) +event_close)
    ; This initial canvas is a placeholder that will be replaced in main.
    (ui-canvas *canvas* 1 1 1))

;;;
;;; Drawing and Window Logic
;;;

(defun resize-window (w h)
    (defq parent (penv *canvas*))
    (. *canvas* :sub)
    (setq *canvas* (Canvas w h 1))
    (. parent :add_child *canvas*)
    (. *canvas* :set_canvas_flags +canvas_flag_antialias)

    ; Get current position and new preferred size
    (bind '(x y _ _) (. *window* :get_bounds))
    (bind '(pw ph) (. *window* :pref_size))

    ; Fit and apply change
    (bind '(x y w h) (view-fit x y pw ph))
    (. *window* :change_dirty x y w h))

(defun circle (r)
    ; Cached circle generation
    (memoize r (list (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path))) 11))

(defun redraw (mx my)
    (bind '(w h) (map (const n2f) (. *canvas* :pref_size)))
    (. *canvas* :fill +argb_black)

    ; Get absolute canvas position and calculate relative mouse coordinates
    (defq canvas_x (n2f (getf *canvas* +view_ctx_x 0))
          canvas_y (n2f (getf *canvas* +view_ctx_y 0))
          rel_mx (- (n2f mx) canvas_x)
          rel_my (- (n2f my) canvas_y))

    ; Calculate eye dimensions and positions
    (defq eye_radius (* h 0.4)
          pupil_radius (* eye_radius 0.4)
          max_pupil_dist (- eye_radius pupil_radius)
          left_eye_cx (* w 0.25)
          right_eye_cx (* w 0.75)
          eye_cy (* h 0.5))

    ; --- Left Eye ---
    (defq vec_to_mouse (Vec2-f (- rel_mx left_eye_cx) (- rel_my eye_cy)))
    (defq dist_to_mouse (vec-length vec_to_mouse))
    (defq pupil_offset (if (> dist_to_mouse 0.0)
        (vec-scale (vec-norm vec_to_mouse) (min dist_to_mouse max_pupil_dist))
        (Vec2-f 0.0 0.0)))
    (bind '(lpx lpy) (vec-add (Vec2-f left_eye_cx eye_cy) pupil_offset))

    ; Draw left eye
    (.-> *canvas*
        (:set_color +argb_white)
        (:fpoly left_eye_cx eye_cy +winding_odd_even (circle eye_radius))
        (:set_color +argb_black)
        (:fpoly lpx lpy +winding_odd_even (circle pupil_radius)))

    ; --- Right Eye ---
    (defq vec_to_mouse (Vec2-f (- rel_mx right_eye_cx) (- rel_my eye_cy)))
    (defq dist_to_mouse (vec-length vec_to_mouse))
    (defq pupil_offset (if (> dist_to_mouse 0.0)
        (vec-scale (vec-norm vec_to_mouse) (min dist_to_mouse max_pupil_dist))
        (Vec2-f 0.0 0.0)))
    (bind '(rpx rpy) (vec-add (Vec2-f right_eye_cx eye_cy) pupil_offset))

    ; Draw right eye
    (.-> *canvas*
        (:set_color +argb_white)
        (:fpoly right_eye_cx eye_cy +winding_odd_even (circle eye_radius))
        (:set_color +argb_black)
        (:fpoly rpx rpy +winding_odd_even (circle pupil_radius)))

    (. *canvas* :swap 0))

;;;
;;; Main Loop
;;;

(defun main ()
    (defq select (task-mboxes +select_size))
    (load-config)

    ; Get initial dimensions from config
    (defq w (ifn (. *config* :find :width) min_width)
          h (ifn (. *config* :find :height) min_height))

    ; Replace the placeholder canvas with the correctly sized one
    (defq parent (penv *canvas*))
    (. *canvas* :sub)
    (setq *canvas* (Canvas w h 1))
    (. parent :add_child *canvas*)
    (. *canvas* :set_canvas_flags +canvas_flag_antialias)

    ; Position and display the window for the first time
    (bind '(x y w h) (apply view-locate (. *window* :pref_size)))
    (gui-add-front-rpc (. *window* :change x y w h))
    (mail-timeout (elem-get select +select_timer) +rate 0)

    (while *running*
        (defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
        (cond
            ((= idx +select_timer)
                (mail-timeout (elem-get select +select_timer) +rate 0)
                (bind '(mx my _ _) (gui-info))
                (redraw mx my))

            ((= (defq id (getf msg +ev_msg_target_id)) +event_close)
                (setq *running* :nil))

            ((= id +event_min)
                (resize-window min_width min_height)
                (. *config* :insert :width min_width)
                (. *config* :insert :height min_height))

            ((= id +event_max)
                (resize-window max_width max_height)
                (. *config* :insert :width max_width)
                (. *config* :insert :height max_height))

            (:t (. *window* :event msg))))

    (save-config)
    (gui-sub-rpc *window*))