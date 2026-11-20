(report-header "GPU: a render's frames are the CPU's while its GPU gets its shader ready")

(import "gui/lisp.inc")
(import "lib/gpu/render_farm.inc")

;the GPU's first frame is waited for :gpu_warm, a second, then the frames
;are the CPU's till the GPU has drawn one: a driver can take seconds to
;build a shader the first time, 20 on the Pi. Here it is waited for not
;at all, and the render looked at at once, so it is set aside whatever
;the GPU, and the CPU's frames are drawn in this task, :farm :nil

(defq rp_canvas (canvas-shared 64 48 1)
	rp_vals (list (list 'resolution (list 64.0 48.0)) (list 'time 0.5)))
(cond
	((not rp_canvas) (test-skip "the CPU's frames while the GPU gets ready" "this host has no shared memory"))
	((= (gpu-format) 0) (test-skip "the CPU's frames while the GPU gets ready" "this host has no GPU"))
	(:t (defq render (Render-shade rp_canvas "apps/demos/surface/surface.shader")
			rp_select (list (mail-mbox)) rp_hows (list) rp_parked :nil rp_t0 (pii-time))
		(def render :gpu_warm 0 :farm :nil)
		(assert-eq "it starts on the GPU" :gpu (. render :mode))
		(assert-eq "the first frame is out with the GPU" :nil (. render :frame rp_vals))
		(. render :tick)
		(assert-true "looked at at once, the GPU is set aside" (nempty? (get :parked render)))
		(assert-eq "and the next frame may be given" :nil (. render :busy))
		(assert-eq "it is drawn by the CPU, here" :t (. render :frame rp_vals))
		(assert-eq "and said to be" :here (get :how render))
		;frames as an app gives them, till one is the GPU's: the one set
		;aside answers, and is the farm again, however long its shader took
		(until (or (find (get :how render) '(:gpu :gpu_copy)) (> (- (pii-time) rp_t0) 120000000))
			(mail-timeout (first rp_select) 20000 0)
			(bind '(idx msg) (. render :read rp_select 0))
			(if (get :parked render) (setq rp_parked :t))
			(unless (. render :busy) (. render :frame rp_vals))
			(. render :tick))
		(assert-true "then the GPU draws them" (find (get :how render) '(:gpu :gpu_copy)))
		(assert-eq "it is ready" :t (get :gpu_ready render))
		(assert-eq "and is not set aside" :nil (get :parked render))
		(assert-eq "nor set aside again" :nil (progn (. render :frame rp_vals) (. render :tick) (get :parked render)))
		(. render :close)))
