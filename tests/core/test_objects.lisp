(report-header "Collections & Objects")

; --- Hash Maps (Fmap) ---
(defq m (Fmap 11))
(. m :insert 'key1 100)
(. m :insert 'key2 200)

(assert-eq "Fmap Find" 100 (. m :find 'key1))

; Calculate size by iterating
; Fmap :each passes (key value) to the callback
(defq m_count 0)
(. m :each (lambda (k v) (++ m_count)))
(assert-eq "Fmap Count" 2 m_count)

(. m :erase 'key1)
(assert-eq "Fmap Erase" :nil (. m :find 'key1))

; --- Fast Sets (Fset) ---
(defq s (Fset 11))
(. s :insert "apple")
(. s :insert "banana")

(assert-eq "Fset Find" "apple" (. s :find "apple"))
(assert-eq "Fset Miss" :nil	(. s :find "cherry"))

; --- Object System ---

; Define a simple class
(defclass TestPoint (x y) :nil
	(def this :x x :y y)

	(defmethod :get_x ()
		(get :x this))

	(defmethod :add (other)
		(defq nx (+ (get :x this) (. other :get_x)))
		(defq ny (+ (get :y this) (get :y other)))
		(TestPoint nx ny))
)

(defq p1 (TestPoint 10 20))
(defq p2 (TestPoint 5 5))
(defq p3 (. p1 :add p2))

(assert-eq "Obj Get Method" 10 (. p1 :get_x))
(assert-eq "Obj Interaction" 15 (. p3 :get_x))

; Test Inheritance
; Inherit from TestPoint, initializing it with 0, 0
(defclass TestPoint3D (z) (TestPoint 0 0)
	(def this :z z)
	(defmethod :get_z () (get :z this))
)

(defq p3d (TestPoint3D 99))
; Inherited values (x=0, y=0 from super init)
(assert-eq "Inherited Method" 0 (. p3d :get_x))
(assert-eq "New Method" 99 (. p3d :get_z))

; --- weak-ref / obj-ref ---
(defq o_ref "test-obj")
(defq w_ref (weak-ref o_ref))
(assert-true "weak-ref number" (num? w_ref))
(assert-eq "obj-ref" o_ref (obj-ref w_ref))

(assert-true "hash" (num? (hash "test")))
(defq s_obj (str-alloc 10))
(obj-set s_obj +str_data +type_int 123456)
(assert-eq "obj-get/set" 123456 (obj-get s_obj +str_data +type_int))
