;;;;;;;;;;;;;;
; items widget
;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Items () (View)
	; (Items) -> items
	;a list of items one under another, as a flow down of all of them, of
	;which only those seen are views: put in a Scroll, it is the size of
	;the whole list, and as it is moved it has the views of the items in
	;the part of it its parent shows, asked for of :item_fn as they come
	;into it and let go of as they leave. Each item is the height of the
	;first, :item_h, and as wide as the list
	(def this :item_count 0 :item_fn :nil :items_held (list))

	(defmethod :set_count (count)
		; (. items :set_count count) -> items
		;how many items there are, the views it holds let go of, and
		;the height of an item, if not given, that of the first. Its
		;owner lays out what it is in, (.-> scroll :layout :dirty_all)
		(each (# (. (second %0) :sub)) (get :items_held this))
		(def this :item_count count :items_held (list))
		(when (and (> count 0) (not (def? :item_h this)))
			;in the list as it is measured, it may have its font of it
			(. this :add_child (defq first_item ((get :item_fn this) 0)))
			(def this :item_h (max 1 (second (. first_item :pref_size)))
				:items_held (list (list 0 first_item))))
		this)

	(defmethod :refresh ()
		; (. items :refresh) -> items
		;the items it shows asked for again, the list the same length
		(each (# (. (second %0) :sub)) (get :items_held this))
		(def this :items_held (list))
		(.-> this :show :dirty_all))

	(defmethod :show ()
		; (. items :show) -> items
		;the views of the items its parent shows, those it has kept, the
		;others asked for, and those it has that are out of sight let go
		(defq parent (penv this) held (get :items_held this) item_h (ifn (def? :item_h this) 1)
			count (get :item_count this) kept (list))
		(bind '(& y w &) (. this :get_bounds))
		(bind '(& ph) (if parent (. parent :get_size) '(0 0)))
		;the items of the part of it its parent shows
		(defq top (max 0 (/ (neg y) item_h))
			end (min count (/ (+ (neg y) ph item_h -1) item_h)))
		(each (lambda ((index view))
				(if (<= top index (dec end))
					(push kept (list index view))
					(. view :sub)))
			held)
		(defq got (map (const first) kept))
		(each! (lambda ()
				(unless (find (!) got)
					(push kept (list (!) (defq view ((get :item_fn this) (!)))))
					(. this :add_child view)))
			'() top end)
		(each (lambda ((index view)) (. view :change 0 (* index item_h) w item_h)) kept)
		(def this :items_held kept)
		this)

	(defmethod :set_bounds (x y width height)
		; (. items :set_bounds x y width height) -> items
		;moved by what it is in, a Scroll scrolled: what is seen of it
		;shown
		(.-> (.super this :set_bounds x y width height) :show))

	(defmethod :constraint ()
		; (. items :constraint) -> (width height)
		;as wide as asked, or as its first item, and as high as all its
		;items
		(defq w (ifn (def? :min_width this) 0))
		(if (nempty? (defq held (get :items_held this)))
			(setq w (max w (first (. (second (first held)) :pref_size)))))
		(list w (* (get :item_count this) (ifn (def? :item_h this) 0))))

	(defmethod :layout ()
		; (. items :layout) -> items
		;opaque only of a paper of its own, which it draws
		(defq col (def? :paper this))
		(. this :set_flags (if (and col (= (>> col 24) 0xff)) +view_flag_opaque 0) +view_flag_opaque))

	(defmethod :draw ()
		; (. items :draw) -> items
		;its paper behind the items, if it has its own
		(when (defq col (def? :paper this))
			(bind '(w h) (. this :get_size))
			(.-> this (:ctx_set_color col) (:ctx_filled_box 0 0 w h)))
		this))
