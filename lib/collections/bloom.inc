;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bloom Filter - Space-efficient probabilistic set membership
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

; Private helper: compute hash with seed
(defun _hash_with_seed (key seed)
	; (hash key) combined with seed using simple mixing
	(logxor (hash key) (<< seed 13) (>> seed 7)))

; Private helper: set bit at position in bit array
(defun _set_bit (bits pos)
	; bits is a list of integers, each representing 64 bits
	(defq word_idx (/ pos 64)
		  bit_idx (% pos 64)
		  words (get :words bits))
	(when (< word_idx (length words))
		(elem-set words word_idx
			(logior (elem-get words word_idx) (<< 1 bit_idx))))
	bits)

; Private helper: get bit at position in bit array
(defun _get_bit (bits pos)
	; Return :t if bit is set, :nil otherwise
	(defq word_idx (/ pos 64)
		  bit_idx (% pos 64)
		  words (get :words bits))
	(if (< word_idx (length words))
		(/= 0 (logand (elem-get words word_idx) (<< 1 bit_idx)))
		:nil))

(defclass Bloom (&optional size hash_count) (Set)
	; (Bloom [size] [hash_count]) -> bloom
	; Create a Bloom filter with specified bit array size and number of hash functions
	; Default: 1024 bits, 3 hash functions
	(defq size (ifn size 1024)
		  hash_count (ifn hash_count 3)
		  num_words (+ (/ size 64) 1))

	(def this
		:bit_size size
		:hash_count hash_count
		:words (map (lambda (_) 0) (range 0 num_words))
		:count 0)

	(defmethod :add (key)
		; (. bloom :add key) -> bloom
		; Add an element to the Bloom filter
		(defq size (get :bit_size this)
			  hash_count (get :hash_count this))
		(each (lambda (seed)
			(defq hash_val (_hash_with_seed key seed)
				  pos (% (abs hash_val) size))
			(_set_bit this pos))
			(range 0 hash_count))
		(def this :count (+ (get :count this) 1))
		this)

	(defmethod :insert (key)
		; (. bloom :insert key) -> bloom
		; Alias for :add for compatibility with Set interface
		(. this :add key))

	(defmethod :contains? (key)
		; (. bloom :contains? key) -> :t | :nil
		; Check if element might be in the set
		; Returns :t if possibly in set, :nil if definitely not in set
		(defq size (get :bit_size this)
			  hash_count (get :hash_count this)
			  result :t)
		(some! (lambda (seed)
			(defq hash_val (_hash_with_seed key seed)
				  pos (% (abs hash_val) size))
			(unless (_get_bit this pos)
				(setq result :nil)
				:t))
			(range 0 hash_count))
		result)

	(defmethod :find (key)
		; (. bloom :find key) -> key | :nil
		; Return key if possibly in set, :nil if definitely not in set
		(if (. this :contains? key) key))

	(defmethod :clear ()
		; (. bloom :clear) -> bloom
		; Reset all bits to zero
		(defq num_words (length (get :words this)))
		(def this
			:words (map (lambda (_) 0) (range 0 num_words))
			:count 0)
		this)

	(defmethod :empty ()
		; (. bloom :empty) -> bloom
		; Alias for :clear for compatibility with Set interface
		(. this :clear))

	(defmethod :empty? ()
		; (. bloom :empty?) -> :t | :nil
		(= (get :count this) 0))

	(defmethod :size ()
		; (. bloom :size) -> num
		; Return the bit array size
		(get :bit_size this))

	(defmethod :item_count ()
		; (. bloom :item_count) -> num
		; Return the number of items added (approximate)
		(get :count this))

	(defmethod :hash_count ()
		; (. bloom :hash_count) -> num
		; Return the number of hash functions used
		(get :hash_count this))

	(defmethod :false_positive_rate ()
		; (. bloom :false_positive_rate) -> real
		; Estimate the false positive probability
		; Formula: (1 - e^(-k*n/m))^k
		; where k = hash_count, n = item_count, m = bit_size
		(defq k (i2f (get :hash_count this))
			  n (i2f (get :count this))
			  m (i2f (get :bit_size this)))
		(if (= n 0.0)
			0.0
			(defq exp_part (f2r (exp (neg (/ (* k n) m))))
				  rate (pow exp_part k))
			rate)))

	(defmethod :optimal_hash_count (expected_items)
		; (. bloom :optimal_hash_count expected_items) -> num
		; Calculate optimal number of hash functions for given expected items
		; Formula: k = (m/n) * ln(2)
		(defq m (i2f (get :bit_size this))
			  n (i2f expected_items))
		(if (> n 0.0)
			(max 1 (floor (* (/ m n) 0.693147)))
			1))

	(defmethod :copy ()
		; (. bloom :copy) -> bloom
		; Create a shallow copy of the Bloom filter
		(defq that ((get 'Bloom) (get :bit_size this) (get :hash_count this)))
		(def that
			:words (map identity (get :words this))
			:count (get :count this))
		that)

	(defmethod :deep_copy ()
		; (. bloom :deep_copy) -> bloom
		; Deep copy is same as shallow copy for Bloom filters (no nested objects)
		(. this :copy))

	(defmethod :erase (key)
		; (. bloom :erase key) -> error
		; Bloom filters do not support deletion
		(throw "Bloom filters do not support element deletion" this))

	(defmethod :move ()
		; (. bloom :move) -> bloom
		; Move the filter's data to a new instance and clear this one
		(defq that ((get 'Bloom) (get :bit_size this) (get :hash_count this))
			  this_words (get :words this))
		(def that
			:words this_words
			:count (get :count this))
		(def this
			:words (map (lambda (_) 0) (range 0 (length this_words)))
			:count 0)
		that)

	(defmethod :resize (new_size)
		; (. bloom :resize new_size) -> bloom
		; Resize the bit array (warning: clears all data)
		(defq num_words (+ (/ new_size 64) 1))
		(def this
			:bit_size new_size
			:words (map (lambda (_) 0) (range 0 num_words))
			:count 0)
		this)

	(defmethod :inserted (key)
		; (. bloom :inserted key) -> bloom | :nil
		; Always returns bloom (can't detect if already present)
		; This is a limitation of Bloom filters
		(. this :add key))

	(defmethod :intern (key)
		; (. bloom :intern key) -> key
		; Add key and return it
		(. this :add key)
		key)

	(defmethod :union (that)
		; (. bloom :union bloom) -> bloom
		; Perform bitwise OR of two Bloom filters
		; Note: Only works if both filters have same size and hash count
		(unless (and (= (get :bit_size this) (get :bit_size that))
					 (= (get :hash_count this) (get :hash_count that)))
			(throw "Cannot union Bloom filters with different parameters" this))
		(defq this_words (get :words this)
			  that_words (get :words that))
		(each (lambda (i)
			(elem-set this_words i
				(logior (elem-get this_words i) (elem-get that_words i))))
			(range 0 (length this_words)))
		(def this :count (+ (get :count this) (get :count that)))
		this)

	(defmethod :intersect (that)
		; (. bloom :intersect bloom) -> bloom
		; Perform bitwise AND of two Bloom filters
		(unless (and (= (get :bit_size this) (get :bit_size that))
					 (= (get :hash_count this) (get :hash_count that)))
			(throw "Cannot intersect Bloom filters with different parameters" this))
		(defq this_words (get :words this)
			  that_words (get :words that))
		(each (lambda (i)
			(elem-set this_words i
				(logand (elem-get this_words i) (elem-get that_words i))))
			(range 0 (length this_words)))
		; Note: item count is not accurate after intersection
		(def this :count 0)
		this)

	(defmethod :difference (that)
		; (. bloom :difference bloom) -> bloom
		; Not well-defined for Bloom filters, raises error
		(throw "Difference operation not supported for Bloom filters" this))

	(defmethod :not_intersect (that)
		; (. bloom :not_intersect bloom) -> bloom
		; Not well-defined for Bloom filters, raises error
		(throw "Not-intersect operation not supported for Bloom filters" this))

	(defmethod :each (fnc)
		; (. bloom :each lambda) -> bloom
		; Cannot iterate elements in a Bloom filter (lossy data structure)
		(throw "Cannot iterate elements in a Bloom filter" this))
	)

;module
(export-classes '(Bloom))
(env-pop)
