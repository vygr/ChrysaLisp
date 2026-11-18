;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A shader drawn by the GPU, a tile at a time, by a task.
;
; The GPU's side of what lib/gpu/tile.inc is the CPU's, and the same
; job: the app has a Jobs object, lib/task/jobs.inc, of the child here,
; a farm of one where the CPU's is a farm of many, and gives it a tile
; of the frame, (shader-tile), all of the frame as a rule. The child has
; the pixel shader as a kernel, (shader-pixel-kernel), runs it on the
; GPU over the pixels of the tile, reads them back, and writes them to
; the app's pixels in shared memory, by their key, as a tile child
; shades onto them. It answers with a +shade_reply, and the app shows
; the pixels as it does a frame of tiles, (. canvas :swap).
;
; The GPU is the node's own, lib/gpu/compute.inc, not the GUI's, so the
; child can be on any node of the machine, and is best kept off the
; app's. Nothing of the app's is lent to it: it holds only the shared
; pixels, which last till the last task on them has gone.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./tile.inc")

(defq +shader_shade_child "lib/gpu/shade_child.lisp")

;its answer to a tile: drawn 1, or 0 if this node has no GPU, the shader
;can not be made a kernel of, or the pixels can not be reached
(structure +shade_reply +job_reply_size
	(uint drawn))
