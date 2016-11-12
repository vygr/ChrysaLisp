%ifndef CLASS_COMPONENT_1234
%define CLASS_COMPONENT_1234

(import 'class/class_ref.inc)

(def-class 'component 'ref)
(def-method 'init 'class/component/init)
(def-method 'connect 'class/component/connect 'static '(r0 r1 r2 r3))
(def-method 'emit 'class/component/emit 'static '(r0 r1))
(def-method 'disconnect_slot 'class/component/disconnect_slot 'static '(r0 r1))
(def-method 'disconnect_sig 'class/component/disconnect_sig 'static '(r0 r1))

(def-method 'deinit 'class/component/deinit 'override)

(def-struct 'component 'ref)
	(struct 'component_slot_list 'lh_list)
(def-struct-end)

%endif
