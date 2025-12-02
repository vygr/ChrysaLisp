;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counting Bloom Filter - Supports element deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

; Private helper: compute hash with seed
(defun _hash_with_seed (key seed)
	; (hash key) combined with seed using simple mixing
	(logxor (hash key) (<< seed 13) (>> seed 7)))

; Private helper: get counter at position
(defun _get_counter (counters pos)
	; Get the counter value at position
	(elem-get (get :counters counters) pos))

; Private helper: set counter at position
(defun _set_counter (counters pos value)
	; Set the counter value at position
	(elem-set (get :counters counters) pos value))

; Private helper: increment counter at position
(defun _inc_counter (counters pos)
	; Increment counter at position, cap at max value
	(defq current (_get_counter counters pos)
		  max_val (get :max_count counters))
	(when (< current max_val)
		(_set_counter counters pos (+ current 1))))

; Private helper: decrement counter at position
(defun _dec_counter (counters pos)
	; Decrement counter at position, floor at 0
	(defq current (_get_counter counters pos))
	(when (> current 0)
		(_set_counter counters pos (- current 1))))

(defclass CountingBloom (&optional size hash_count max_count) (Set)
	; (CountingBloom [size] [hash_count] [max_count]) -> counting_bloom
	; Create a Counting Bloom filter with counter array
	; max_count: maximum counter value (default: 15, fits in 4 bits)
	(defq size (ifn size 1024)
		  hash_count (ifn hash_count 3)
		  max_count (ifn max_count 15))

	(def this
		:size size
		:hash_count hash_count
		:max_count max_count
		:counters (map (lambda (_) 0) (range 0 size))
		:count 0)

	(defmethod :add (key)
		; (. counting_bloom :add key) -> counting_bloom
		; Add an element to the Counting Bloom filter
		(defq size (get :size this)
			  hash_count (get :hash_count this))
		(each (lambda (seed)
			(defq hash_val (_hash_with_seed key seed)
				  pos (% (abs hash_val) size))
			(_inc_counter this pos))
			(range 0 hash_count))
		(def this :count (+ (get :count this) 1))
		this)

	(defmethod :insert (key)
		; (. counting_bloom :insert key) -> counting_bloom
		; Alias for :add for compatibility with Set interface
		(. this :add key))

	(defmethod :remove (key)
		; (. counting_bloom :remove key) -> counting_bloom
		; Remove an element from the Counting Bloom filter
		; Note: Safe to call even if element wasn't in filter
		(defq size (get :size this)
			  hash_count (get :hash_count this))
		(each (lambda (seed)
			(defq hash_val (_hash_with_seed key seed)
				  pos (% (abs hash_val) size))
			(_dec_counter this pos))
			(range 0 hash_count))
		(when (> (get :count this) 0)
			(def this :count (- (get :count this) 1)))
		this)

	(defmethod :erase (key)
		; (. counting_bloom :erase key) -> counting_bloom
		; Alias for :remove for Set interface compatibility
		(. this :remove key))

	(defmethod :contains? (key)
		; (. counting_bloom :contains? key) -> :t | :nil
		; Check if element might be in the set
		(defq size (get :size this)
			  hash_count (get :hash_count this)
			  result :t)
		(some! (lambda (seed)
			(defq hash_val (_hash_with_seed key seed)
				  pos (% (abs hash_val) size))
			(when (= (_get_counter this pos) 0)
				(setq result :nil)
				:t))
			(range 0 hash_count))
		result)

	(defmethod :find (key)
		; (. counting_bloom :find key) -> key | :nil
		; Return key if possibly in set, :nil if definitely not in set
		(if (. this :contains? key) key))

	(defmethod :clear ()
		; (. counting_bloom :clear) -> counting_bloom
		; Reset all counters to zero
		(defq size (get :size this))
		(def this
			:counters (map (lambda (_) 0) (range 0 size))
			:count 0)
		this)

	(defmethod :empty ()
		; (. counting_bloom :empty) -> counting_bloom
		; Alias for :clear for compatibility with Set interface
		(. this :clear))

	(defmethod :empty? ()
		; (. counting_bloom :empty?) -> :t | :nil
		(= (get :count this) 0))

	(defmethod :size ()
		; (. counting_bloom :size) -> num
		; Return the counter array size
		(get :size this))

	(defmethod :item_count ()
		; (. counting_bloom :item_count) -> num
		; Return the number of items added (approximate)
		(get :count this))

	(defmethod :hash_count ()
		; (. counting_bloom :hash_count) -> num
		; Return the number of hash functions used
		(get :hash_count this))

	(defmethod :max_count ()
		; (. counting_bloom :max_count) -> num
		; Return the maximum counter value
		(get :max_count this))

	(defmethod :false_positive_rate ()
		; (. counting_bloom :false_positive_rate) -> real
		; Estimate the false positive probability
		; Same formula as regular Bloom filter
		(defq k (i2f (get :hash_count this))
			  n (i2f (get :count this))
			  m (i2f (get :size this)))
		(if (= n 0.0)
			0.0
			(defq exp_part (f2r (exp (neg (/ (* k n) m))))
				  rate (pow exp_part k))
			rate)))

	(defmethod :copy ()
		; (. counting_bloom :copy) -> counting_bloom
		; Create a shallow copy of the Counting Bloom filter
		(defq that ((get 'CountingBloom) (get :size this) (get :hash_count this) (get :max_count this)))
		(def that
			:counters (map identity (get :counters this))
			:count (get :count this))
		that)

	(defmethod :deep_copy ()
		; (. counting_bloom :deep_copy) -> counting_bloom
		; Deep copy is same as shallow copy (no nested objects)
		(. this :copy))

	(defmethod :move ()
		; (. counting_bloom :move) -> counting_bloom
		; Move the filter's data to a new instance and clear this one
		(defq that ((get 'CountingBloom) (get :size this) (get :hash_count this) (get :max_count this))
			  this_counters (get :counters this))
		(def that
			:counters this_counters
			:count (get :count this))
		(def this
			:counters (map (lambda (_) 0) (range 0 (get :size this)))
			:count 0)
		that)

	(defmethod :resize (new_size)
		; (. counting_bloom :resize new_size) -> counting_bloom
		; Resize the counter array (warning: clears all data)
		(def this
			:size new_size
			:counters (map (lambda (_) 0) (range 0 new_size))
			:count 0)
		this)

	(defmethod :inserted (key)
		; (. counting_bloom :inserted key) -> counting_bloom
		; Always returns counting_bloom (can't detect if already present)
		(. this :add key))

	(defmethod :intern (key)
		; (. counting_bloom :intern key) -> key
		; Add key and return it
		(. this :add key)
		key)

	(defmethod :union (that)
		; (. counting_bloom :union counting_bloom) -> counting_bloom
		; Union by taking maximum counter at each position
		(unless (and (= (get :size this) (get :size that))
					 (= (get :hash_count this) (get :hash_count that)))
			(throw "Cannot union Counting Bloom filters with different parameters" this))
		(defq this_counters (get :counters this)
			  that_counters (get :counters that))
		(each (lambda (i)
			(elem-set this_counters i
				(max (elem-get this_counters i) (elem-get that_counters i))))
			(range 0 (length this_counters)))
		(def this :count (+ (get :count this) (get :count that)))
		this)

	(defmethod :intersect (that)
		; (. counting_bloom :intersect counting_bloom) -> counting_bloom
		; Intersect by taking minimum counter at each position
		(unless (and (= (get :size this) (get :size that))
					 (= (get :hash_count this) (get :hash_count that)))
			(throw "Cannot intersect Counting Bloom filters with different parameters" this))
		(defq this_counters (get :counters this)
			  that_counters (get :counters that))
		(each (lambda (i)
			(elem-set this_counters i
				(min (elem-get this_counters i) (elem-get that_counters i))))
			(range 0 (length this_counters)))
		(def this :count 0)
		this)

	(defmethod :difference (that)
		; (. counting_bloom :difference counting_bloom) -> counting_bloom
		; Not well-defined for Counting Bloom filters, raises error
		(throw "Difference operation not supported for Counting Bloom filters" this))

	(defmethod :not_intersect (that)
		; (. counting_bloom :not_intersect counting_bloom) -> counting_bloom
		; Not well-defined for Counting Bloom filters, raises error
		(throw "Not-intersect operation not supported for Counting Bloom filters" this))

	(defmethod :each (fnc)
		; (. counting_bloom :each lambda) -> counting_bloom
		; Cannot iterate elements in a Counting Bloom filter (lossy data structure)
		(throw "Cannot iterate elements in a Counting Bloom filter" this))

	(defmethod :counter_stats ()
		; (. counting_bloom :counter_stats) -> (min max avg)
		; Get statistics about counter values
		(defq counters (get :counters this)
			  min_val 999999
			  max_val 0
			  sum 0)
		(each (lambda (c)
			(setq min_val (min min_val c)
				  max_val (max max_val c)
				  sum (+ sum c)))
			counters)
		(list min_val max_val (/ sum (length counters))))

	(defmethod :save (stream)
		; (. counting_bloom :save stream) -> counting_bloom
		; Serialize Counting Bloom filter to a stream
		; Format: magic_number size hash_count max_count count num_counters [counters...]
		(defq magic 0x43424C4D)  ; "CBLM" in ASCII
		(. stream :write_long magic)
		(. stream :write_long (get :size this))
		(. stream :write_long (get :hash_count this))
		(. stream :write_long (get :max_count this))
		(. stream :write_long (get :count this))
		(defq counters (get :counters this))
		(. stream :write_long (length counters))
		(each (lambda (counter)
			(. stream :write_long counter))
			counters)
		this)

	(defmethod :load (stream)
		; (. counting_bloom :load stream) -> counting_bloom
		; Deserialize Counting Bloom filter from a stream
		(defq magic (. stream :read_long))
		(unless (= magic 0x43424C4D)
			(throw "Invalid Counting Bloom filter file format" this))
		(defq size (. stream :read_long)
			  hash_count (. stream :read_long)
			  max_count (. stream :read_long)
			  count (. stream :read_long)
			  num_counters (. stream :read_long)
			  counters (list))
		(each (lambda (_)
			(push counters (. stream :read_long)))
			(range 0 num_counters))
		(def this
			:size size
			:hash_count hash_count
			:max_count max_count
			:count count
			:counters counters)
		this)

	(defmethod :save_to_file (filename)
		; (. counting_bloom :save_to_file filename) -> counting_bloom
		; Save Counting Bloom filter to a file
		(when (defq stream (file-stream filename +file_open_write))
			(. this :save stream)
			(. stream :close))
		this)

	(defmethod :load_from_file (filename)
		; (. counting_bloom :load_from_file filename) -> counting_bloom
		; Load Counting Bloom filter from a file
		(when (defq stream (file-stream filename))
			(. this :load stream)
			(. stream :close))
		this)
	)

;module
(export-classes '(CountingBloom))
(env-pop)
