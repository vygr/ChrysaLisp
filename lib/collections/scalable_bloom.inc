;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scalable Bloom Filter - Dynamically grows to maintain FP rate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./bloom.inc")
(import "./set.inc")

;module
(env-push)

(defclass ScalableBloom (&optional initial_size target_fp_rate growth_factor) (Set)
	; (ScalableBloom [initial_size] [target_fp_rate] [growth_factor]) -> scalable_bloom
	; Create a Scalable Bloom filter that grows dynamically
	; initial_size: size of first Bloom filter (default: 1024)
	; target_fp_rate: target false positive rate (default: 0.01 = 1%)
	; growth_factor: how much to grow each filter (default: 2)
	(defq initial_size (ifn initial_size 1024)
		  target_fp_rate (ifn target_fp_rate 0.01)
		  growth_factor (ifn growth_factor 2))

	; Calculate initial hash count for target FP rate
	; k = -log2(P) where P is target FP rate
	(defq initial_hash_count (max 1 (floor (* (neg (/ (log target_fp_rate) (log 2.0))) 0.7))))

	(def this
		:filters (list (Bloom initial_size initial_hash_count))
		:initial_size initial_size
		:target_fp_rate target_fp_rate
		:growth_factor growth_factor
		:hash_count initial_hash_count
		:capacity initial_size
		:count 0
		:fill_ratio 0.7)  ; Trigger growth at 70% capacity

	(defmethod :_should_grow ()
		; Internal: Check if we should add a new filter
		(> (get :count this)
		   (* (get :capacity this) (get :fill_ratio this))))

	(defmethod :_add_filter ()
		; Internal: Add a new Bloom filter to the list
		(defq filters (get :filters this)
			  new_size (* (get :initial_size this)
						  (pow (get :growth_factor this) (length filters)))
			  hash_count (get :hash_count this))
		(push filters (Bloom new_size hash_count))
		(def this
			:capacity (+ (get :capacity this) new_size)))

	(defmethod :add (key)
		; (. scalable_bloom :add key) -> scalable_bloom
		; Add an element, growing if necessary
		(when (. this :_should_grow)
			(. this :_add_filter))

		; Add to the most recent filter
		(defq filters (get :filters this))
		(. (elem-get filters (- (length filters) 1)) :add key)
		(def this :count (+ (get :count this) 1))
		this)

	(defmethod :insert (key)
		; (. scalable_bloom :insert key) -> scalable_bloom
		; Alias for :add for compatibility with Set interface
		(. this :add key))

	(defmethod :contains? (key)
		; (. scalable_bloom :contains? key) -> :t | :nil
		; Check if element might be in any of the filters
		(defq result :nil)
		(some! (lambda (filter)
			(when (. filter :contains? key)
				(setq result :t)
				:t))
			(get :filters this))
		result)

	(defmethod :find (key)
		; (. scalable_bloom :find key) -> key | :nil
		; Return key if possibly in set, :nil if definitely not in set
		(if (. this :contains? key) key))

	(defmethod :clear ()
		; (. scalable_bloom :clear) -> scalable_bloom
		; Clear all filters and reset to single initial filter
		(defq initial_size (get :initial_size this)
			  hash_count (get :hash_count this))
		(def this
			:filters (list (Bloom initial_size hash_count))
			:capacity initial_size
			:count 0)
		this)

	(defmethod :empty ()
		; (. scalable_bloom :empty) -> scalable_bloom
		; Alias for :clear for compatibility with Set interface
		(. this :clear))

	(defmethod :empty? ()
		; (. scalable_bloom :empty?) -> :t | :nil
		(= (get :count this) 0))

	(defmethod :size ()
		; (. scalable_bloom :size) -> num
		; Return total bit capacity across all filters
		(defq total 0)
		(each (lambda (filter)
			(setq total (+ total (. filter :size))))
			(get :filters this))
		total)

	(defmethod :item_count ()
		; (. scalable_bloom :item_count) -> num
		; Return the number of items added
		(get :count this))

	(defmethod :filter_count ()
		; (. scalable_bloom :filter_count) -> num
		; Return the number of internal Bloom filters
		(length (get :filters this)))

	(defmethod :hash_count ()
		; (. scalable_bloom :hash_count) -> num
		; Return the number of hash functions used
		(get :hash_count this))

	(defmethod :capacity ()
		; (. scalable_bloom :capacity) -> num
		; Return total capacity before next growth
		(get :capacity this))

	(defmethod :false_positive_rate ()
		; (. scalable_bloom :false_positive_rate) -> real
		; Estimate the overall false positive probability
		; P_total = 1 - ∏(1 - P_i) for each filter i
		(defq result 1.0)
		(each (lambda (filter)
			(defq p_i (. filter :false_positive_rate))
			(setq result (* result (- 1.0 p_i))))
			(get :filters this))
		(- 1.0 result))

	(defmethod :copy ()
		; (. scalable_bloom :copy) -> scalable_bloom
		; Create a shallow copy of the Scalable Bloom filter
		(defq that ((get 'ScalableBloom) (get :initial_size this)
					(get :target_fp_rate this) (get :growth_factor this)))
		(def that
			:filters (map (lambda (f) (. f :copy)) (get :filters this))
			:hash_count (get :hash_count this)
			:capacity (get :capacity this)
			:count (get :count this)
			:fill_ratio (get :fill_ratio this))
		that)

	(defmethod :deep_copy ()
		; (. scalable_bloom :deep_copy) -> scalable_bloom
		; Deep copy is same as shallow copy
		(. this :copy))

	(defmethod :move ()
		; (. scalable_bloom :move) -> scalable_bloom
		; Move the filter's data to a new instance and clear this one
		(defq that ((get 'ScalableBloom) (get :initial_size this)
					(get :target_fp_rate this) (get :growth_factor this))
			  this_filters (get :filters this))
		(def that
			:filters this_filters
			:hash_count (get :hash_count this)
			:capacity (get :capacity this)
			:count (get :count this)
			:fill_ratio (get :fill_ratio this))
		(. this :clear)
		that)

	(defmethod :resize (new_size)
		; (. scalable_bloom :resize new_size) -> scalable_bloom
		; Not meaningful for scalable filters, just clears
		(. this :clear))

	(defmethod :inserted (key)
		; (. scalable_bloom :inserted key) -> scalable_bloom
		; Always returns scalable_bloom
		(. this :add key))

	(defmethod :intern (key)
		; (. scalable_bloom :intern key) -> key
		; Add key and return it
		(. this :add key)
		key)

	(defmethod :erase (key)
		; (. scalable_bloom :erase key) -> error
		; Scalable Bloom filters do not support deletion
		(throw "Scalable Bloom filters do not support element deletion" this))

	(defmethod :union (that)
		; (. scalable_bloom :union scalable_bloom) -> scalable_bloom
		; Not well-defined for Scalable Bloom filters
		(throw "Union operation not supported for Scalable Bloom filters" this))

	(defmethod :intersect (that)
		; (. scalable_bloom :intersect scalable_bloom) -> scalable_bloom
		; Not well-defined for Scalable Bloom filters
		(throw "Intersect operation not supported for Scalable Bloom filters" this))

	(defmethod :difference (that)
		; (. scalable_bloom :difference scalable_bloom) -> scalable_bloom
		; Not well-defined for Scalable Bloom filters
		(throw "Difference operation not supported for Scalable Bloom filters" this))

	(defmethod :not_intersect (that)
		; (. scalable_bloom :not_intersect scalable_bloom) -> scalable_bloom
		; Not well-defined for Scalable Bloom filters
		(throw "Not-intersect operation not supported for Scalable Bloom filters" this))

	(defmethod :each (fnc)
		; (. scalable_bloom :each lambda) -> scalable_bloom
		; Cannot iterate elements in a Scalable Bloom filter
		(throw "Cannot iterate elements in a Scalable Bloom filter" this))

	(defmethod :stats ()
		; (. scalable_bloom :stats) -> (filter_count total_size count capacity)
		; Get statistics about the scalable filter
		(list
			(. this :filter_count)
			(. this :size)
			(. this :item_count)
			(. this :capacity)))

	(defmethod :save (stream)
		; (. scalable_bloom :save stream) -> scalable_bloom
		; Serialize Scalable Bloom filter to a stream
		; Format: magic_number initial_size target_fp_rate growth_factor hash_count
		;         capacity count fill_ratio num_filters [filter1...filterN]
		(defq magic 0x53424C4D)  ; "SBLM" in ASCII
		(. stream :write_long magic)
		(. stream :write_long (get :initial_size this))
		(. stream :write_real (get :target_fp_rate this))
		(. stream :write_real (get :growth_factor this))
		(. stream :write_long (get :hash_count this))
		(. stream :write_long (get :capacity this))
		(. stream :write_long (get :count this))
		(. stream :write_real (get :fill_ratio this))
		(defq filters (get :filters this))
		(. stream :write_long (length filters))
		(each (lambda (filter)
			(. filter :save stream))
			filters)
		this)

	(defmethod :load (stream)
		; (. scalable_bloom :load stream) -> scalable_bloom
		; Deserialize Scalable Bloom filter from a stream
		(defq magic (. stream :read_long))
		(unless (= magic 0x53424C4D)
			(throw "Invalid Scalable Bloom filter file format" this))
		(defq initial_size (. stream :read_long)
			  target_fp_rate (. stream :read_real)
			  growth_factor (. stream :read_real)
			  hash_count (. stream :read_long)
			  capacity (. stream :read_long)
			  count (. stream :read_long)
			  fill_ratio (. stream :read_real)
			  num_filters (. stream :read_long)
			  filters (list))
		(each (lambda (_)
			(defq filter (Bloom))
			(. filter :load stream)
			(push filters filter))
			(range 0 num_filters))
		(def this
			:initial_size initial_size
			:target_fp_rate target_fp_rate
			:growth_factor growth_factor
			:hash_count hash_count
			:capacity capacity
			:count count
			:fill_ratio fill_ratio
			:filters filters)
		this)

	(defmethod :save_to_file (filename)
		; (. scalable_bloom :save_to_file filename) -> scalable_bloom
		; Save Scalable Bloom filter to a file
		(when (defq stream (file-stream filename +file_open_write))
			(. this :save stream)
			(. stream :close))
		this)

	(defmethod :load_from_file (filename)
		; (. scalable_bloom :load_from_file filename) -> scalable_bloom
		; Load Scalable Bloom filter from a file
		(when (defq stream (file-stream filename))
			(. this :load stream)
			(. stream :close))
		this)
	)

;module
(export-classes '(ScalableBloom))
(env-pop)
