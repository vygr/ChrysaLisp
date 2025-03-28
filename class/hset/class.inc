(include "././obj/class.inc")

(def-class hset obj
	(dec-method :vtable class/hset/vtable)
	(dec-method :create class/hset/create :static (:r0 :r1) (:r0))
	(dec-method :init class/hset/init :static (:r0 :r1 :r2 :r3))
	(dec-method :bucket class/hset/bucket :static (:r0 :r1) (:r0 :r1))
	(dec-method :clear class/hset/clear :static (:r0) (:r0))
	(dec-method :each class/hset/each :static (:r0 :r1 :r2) (:r0))
	(dec-method :find class/hset/find :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :insert class/hset/insert :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :flush class/hset/flush :static (:r0) (:r0))
	(dec-method :key_callback class/obj/null :static (:r0 :r1) (:r0 :r1))
	(dec-method :each_callback class/obj/null :static (:r0 :r1 :r2))
	(dec-method :deinit class/hset/deinit :override))

(def-struct hset obj_size
	(uint num_buckets)
	(ptr buckets key_callback))
