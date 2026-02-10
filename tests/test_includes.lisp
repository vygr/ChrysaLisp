;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_includes.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Includes Command Test")

(defq test_vp "tests/tmp_test_includes.vp")

; Function to run includes on a string and capture output
(defun test-includes (content)
    (save content test_vp)
    (defq out_lines (list))
    (pipe-run (cat "includes " test_vp) (lambda (l) (push out_lines l)))
    (pii-remove test_vp)
    (apply (const cat) out_lines))

; --- Test prefixes that are NOT implicit dependencies of lib/asm/func.inc ---
(assert-true "includes: canvas prefix" (found? (test-includes "(vp-add-cr +canvas_scale :r0)") "gui/canvas/class.inc"))
(assert-true "includes: vdu prefix"    (found? (test-includes "(vp-add-cr +vdu_chars :r0)") "gui/vdu/class.inc"))
(assert-true "includes: pixmap prefix" (found? (test-includes "(vp-add-cr +pixmap_type :r0)") "gui/pixmap/class.inc"))
(assert-true "includes: region prefix" (found? (test-includes "(vp-add-cr rect_x :r0)") "gui/region/class.inc"))
(assert-true "includes: heap prefix"   (found? (test-includes "(vp-add-cr +hp_heap_size :r0)") "sys/heap/class.inc"))
(assert-true "includes: host_gui prefix" (found? (test-includes "(vp-add-cr +ev_msg_mouse :r0)") "service/gui/class.inc"))
(assert-true "includes: statics prefix" (found? (test-includes "(assign statics_sys_load_host_os_funcs :r0)") "sys/statics/class.inc"))
(assert-true "includes: vec-set"       (found? (test-includes "(vec-set :r0 :r1)") "sys/math/class.inc"))
(assert-true "includes: audio prefix"  (found? (test-includes "(vp-add-cr +audio_rpc_size :r0)") "service/audio/class.inc"))
(assert-true "includes: clip prefix"   (found? (test-includes "(vp-add-cr +clip_rpc_size :r0)") "service/clipboard/class.inc"))
