(include "sys/list/class.inc")
(include "class/hmap/class.inc")

(def-class 'component 'hmap)
(dec-method :vtable 'gui/component/vtable)
(dec-method :init 'gui/component/init :static '(r0 r1))
(dec-method :find_owner 'gui/component/find_owner :static '(r0) '(r0 r1))
(dec-method :get_prop 'gui/component/get_prop :static '(r0 r1) '(r0 r1))
(dec-method :ref_prop 'gui/component/ref_prop :static '(r0 r1) '(r0 r1))
(dec-method :get_long_prop 'gui/component/get_long_prop :static '(r0 r1) '(r0 r1))

(dec-method :deinit 'gui/component/deinit :override)

(def-struct 'component 'hmap)
	(ptr 'target_ids)
	(long 'id)
	(struct 'owner_id 'id)
(def-struct-end)
