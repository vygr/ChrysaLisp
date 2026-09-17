;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rosetta Computer Science Catalog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/searching.inc")

(defq *rosetta_catalog* (list
	; 1. Quicksort
	(list :quicksort "Quicksort" "Sorting" "O(n log n) | Space: O(log n)"
		"High-performance divide-and-conquer sorting using Lomuto/Hoare partitioning."
		'("O(n log n)" "O(n log n)" "O(n^2)" "O(log n)")
		'("Quicksort selects a pivot element and partitions the array into two sub-sequences: elements smaller than the pivot and elements greater than or equal to the pivot."
			"In ChrysaLisp, the built-in sort function implements an iterative Quicksort with an explicit work queue (_q). By managing partitions with a work queue rather than deep recursion, stack frames remain small and predictable on memory-constrained nodes.")
		'("(defun sort (_a &optional _f _l _h)"
			"	; (sort list [fcmp start end]) -> list"
			"	(setd _f (const cmp) _l 0 _h (length _a))"
			"	(defq _q (list _l _h))"
			"	(while (setq _h (pop _q) _l (pop _q))"
			"		(when (< _l _h)"
			"			(defq _p (pivot _f _a _l _h))"
			"			(push (push _q _l _p) (inc _p) _h))) _a)")
		"* **pivot**: High-speed VP assembly primitive that swaps the median candidate to avoid O(n^2) degraded behavior on sorted datasets.\n* **Work Queue**: Using `(list _l _h)` with `push` and `pop` replaces call stack frames with an explicit heap queue.")

	; 2. Binary Search
	(list :bsearch "Binary Search" "Sorting" "O(log n) | Space: O(1)"
		"Logarithmic item location in sorted contiguous sequences using binary division."
		'("O(1)" "O(log n)" "O(log n)" "O(1)")
		'("Binary Search repeatedly halves the search space by comparing the target with the middle element. If the target is smaller, the search continues in the lower half; otherwise in the upper half."
			"ChrysaLisp relies on binary search throughout its kernel and dictionary indexing (such as prefix completion in Dictionary).")
		'("(defun binary-search (vec target &optional cmp_fn)"
			"	(setd cmp_fn (const cmp))"
			"	(defq low 0 high (dec (length vec)) found -1)"
			"	(while (and (<= low high) (= found -1))"
			"		(defq mid (>> (+ low high) 1)"
			"			val (elem-get vec mid)"
			"			diff (cmp_fn target val))"
			"		(cond"
			"			((= diff 0) (setq found mid))"
			"			((< diff 0) (setq high (dec mid)))"
			"			(:t (setq low (inc mid)))))"
			"	found)")
		"* **Bitwise Shift**: `(>> (+ low high) 1)` computes the integer midpoint without floating-point division overhead.\n* **cmp_fn**: Higher-order comparator function defaults to `(const cmp)` for uniform comparison.")

	; 3. Fisher-Yates Shuffle
	(list :shuffle "Fisher-Yates Shuffle" "Sorting" "O(n) | Space: O(1)"
		"Unbiased in-place sequence permutation in linear time."
		'("O(n)" "O(n)" "O(n)" "O(1)")
		'("The Fisher-Yates (Knuth) shuffle generates a uniform random permutation of any finite sequence. It walks backward from the last element to the first, swapping each element with a randomly chosen element from the unshuffled prefix."
			"Every permutation is equally likely (probability exactly 1/n!).")
		'("(defun swap (seq i j)"
			"	(when (/= i j)"
			"		(defq tmp (elem-get seq i))"
			"		(elem-set (elem-set seq i (elem-get seq j)) j tmp)))"
			""
			"(defun shuffle (seq &optional start end)"
			"	(setd start 0 end (length seq))"
			"	(each! (lambda (x)"
			"		(swap seq (!) (random (inc (!)))))"
			"		(list seq) end start) seq)")
		"* **each!**: Bounded sequence iterator macro that provides the current index via `(!)`.\n* **random**: ChrysaLisp kernel random generator `(random (inc (!)))` selects in range `[0, index]`.")

	; 4. Hash Map (Fmap)
	(list :hashmap "Hash Map (Fmap)" "Structures" "O(1) Avg | Space: O(n)"
		"Dynamic key-value association with fast hashed bucket distribution."
		'("O(1)" "O(1)" "O(n)" "O(n)")
		'("A Hash Map maps keys to values using a hashing function that computes an index into an array of buckets. Collisions are handled via chaining or linear open addressing."
			"ChrysaLisp features `Fmap` (fast fixed-bucket map) and `Emap` (extensible associative tree map). `Fmap` provides near-instant lookup for known or bounded bucket sizes.")
		'("(defq my_map (Fmap 31))"
			""
			"; Insert key-value pairs"
			"(. my_map :insert \qname\q \qChrysaLisp\q)"
			"(. my_map :insert \qversion\q 1)"
			""
			"; Query and update"
			"(defq ver (. my_map :find \qversion\q))"
			"(. my_map :update \qcounter\q (# (if %0 (inc %0) 1)))"
			""
			"; Enumerate all entries"
			"(. my_map :each (lambda (k v)"
			"	(prin k \q: \q v) (print)))")
		"* **Fmap**: Bucket-based hash collection indexed with integer hash values.\n* **Lambda shorthand**: `(# ...)` generates concise inline callbacks.")

	; 5. Trie & Prefix Dictionary
	(list :trie "Trie & Dictionary" "Structures" "O(k) | Space: O(k*n)"
		"Prefix tree structure for instant auto-completion and dictionary searches."
		'("O(k)" "O(k)" "O(k)" "O(k*n)")
		'("A Trie (retrieval tree) stores strings as paths along connected character nodes. All descendants of a node share a common prefix."
			"ChrysaLisp provides the `Dictionary` class (`lib/text/dictionary.inc`) combining a set of inserted vocabulary words with sorted binary search bounds to provide sub-millisecond prefix completion for terminals, editors, and IDEs.")
		'("(defq dict (Dictionary))"
			""
			"; Insert vocabulary"
			"(. dict :insert_word \qalgorithm\q)"
			"(. dict :insert_word \qalgebra\q)"
			"(. dict :insert_word \qallocator\q)"
			""
			"; Prefix search: finds all words starting with \qalg\q"
			"(defq matches (. dict :find_matches \qalg\q))"
			"; -> (\qalgebra\q \qalgorithm\q)")
		"* **Dictionary**: Encapsulates `Fset` word insertion and sorted slice bounds.\n* **prefix search**: `find_matches` returns string matches in O(k + log n) time.")

	; 6. Ring Buffer (Circular FIFO)
	(list :ringbuf "Ring Buffer" "Structures" "O(1) Push/Pop | Space: O(n)"
		"Fixed-size circular buffer using wrap-around indices for streams and I/O."
		'("O(1)" "O(1)" "O(1)" "O(n)")
		'("A Ring Buffer uses a single, fixed-size memory buffer connected end-to-end. As data is written, the write pointer advances and wraps to 0 at the buffer boundary."
			"Ring buffers eliminate memory re-allocation and garbage collection churn, making them ideal for audio streams, UART serial drivers, and network sockets.")
		'("(defclass RingBuf (capacity) :nil"
			"	(def this :data (dim (list capacity) 0) :cap capacity"
			"		:head 0 :tail 0 :size 0)"
			""
			"	(defmethod :push (item)"
			"		(when (< (get :size this) (get :cap this))"
			"			(elem-set (get :data this) (get :tail this) item)"
			"			(def this :tail (% (inc (get :tail this)) (get :cap this))"
			"				:size (inc (get :size this)))"
			"			this))"
			""
			"	(defmethod :pop ()"
			"		(when (> (get :size this) 0)"
			"			(defq item (elem-get (get :data this) (get :head this)))"
			"			(def this :head (% (inc (get :head this)) (get :cap this))"
			"				:size (dec (get :size this)))"
			"			item)))")
		"* **dim**: Allocates fixed-dimension native arrays without heap resizing.\n* **Modulo wrap**: `(% (inc ptr) cap)` implements zero-branch circular wrapping.")

	; 7. Disjoint Set (Union-Find)
	(list :unionfind "Disjoint Set (Union-Find)" "Structures" "O(alpha(n)) | Space: O(n)"
		"Near constant-time set partition tracker with path compression."
		'("O(1)" "O(alpha(n))" "O(alpha(n))" "O(n)")
		'("Disjoint Set Union (DSU) tracks elements partitioned into disjoint subsets. It provides two operations: `find` (identifying which set an element belongs to) and `union` (merging two sets)."
			"With path compression and union-by-rank, both operations run in near-linear time proportional to the inverse Ackermann function.")
		'("(defclass UnionFind (size) :nil"
			"	(def this :parent (map (# %0) (range 0 size)))"
			""
			"	(defmethod :find (x)"
			"		(defq p (elem-get (get :parent this) x))"
			"		(if (= p x) x"
			"			(progn"
			"				(defq root (. this :find p))"
			"				(elem-set (get :parent this) x root)"
			"				root)))"
			""
			"	(defmethod :union (x y)"
			"		(defq rx (. this :find x) ry (. this :find y))"
			"		(unless (= rx ry)"
			"			(elem-set (get :parent this) rx ry))"
			"		this))")
		"* **Path Compression**: Flattening tree depths directly to root during `find`.\n* **Applications**: Kruskal's minimum spanning tree, circuit netlist routing in ChrysaLisp PCB app.")

	; 8. Run-Length Encoding (RLE)
	(list :rle "Run-Length Encoding (RLE)" "Compression" "O(n) | Space: O(n)"
		"Lossless compression replacing repetitive sequential symbols with counts."
		'("O(n)" "O(n)" "O(n)" "O(n)")
		'("Run-Length Encoding replaces contiguous identical characters or bytes with a single character and its repetition count."
			"RLE is particularly effective for bitmap graphics, icon masks, and sparse matrices where identical color pixels or zero-bytes repeat frequently.")
		'("(import \qlib/stream/rle.inc\q)"
			""
			"; Compress raw buffer"
			"(defq raw_stream (string-stream \qAAAABBBCCDAAAAA\q)"
			"	comp_stream (memory-stream))"
			"(stream-rle-write comp_stream raw_stream)"
			""
			"; Decompress stream"
			"(stream-seek comp_stream 0 0)"
			"(defq decomp_stream (memory-stream))"
			"(stream-rle-read decomp_stream comp_stream)"
			"(stream-seek decomp_stream 0 0)"
			"(defq result (read-line decomp_stream))")
		"* **stream-rle-write**: Native streaming RLE encoder built into ChrysaLisp.\n* **Zero overhead**: Non-repeating sequences are framed efficiently using literal escape codes.")

	; 9. Huffman Coding
	(list :huffman "Huffman Coding" "Compression" "O(n log k) | Space: O(k)"
		"Optimal entropy encoding assigning shorter prefix codes to frequent symbols."
		'("O(n)" "O(n log k)" "O(n log k)" "O(k)")
		'("Huffman coding builds a binary prefix tree by tallying symbol frequencies and pairing lowest-frequency nodes bottom-up. Symbols occurring most often receive the shortest binary bit sequences."
			"Because no code word is a prefix of another, decoding reads bit-by-bit unambiguously from root to leaf without delimiters.")
		'("(import \qlib/stream/huffman.inc\q)"
			""
			"; Static Huffman stream encoder"
			"(defq src (string-stream \qthe quick brown fox jumps over the lazy dog\q)"
			"	dest (memory-stream))"
			"(stream-huffman-write dest src)"
			""
			"; Read back decoded bytes"
			"(stream-seek dest 0 0)"
			"(defq out (memory-stream))"
			"(stream-huffman-read out dest)")
		"* **Static & Adaptive**: ChrysaLisp includes precomputed static trees and dynamic frequency models.\n* **Bit Pool**: Bits are read and written using `read-bits` and `write-bits` primitives.")

	; 10. LZ4 Sliding Window
	(list :lz4 "LZ4 Streaming Compression" "Compression" "O(n) | Space: O(1)"
		"Extremely fast dictionary compression using match lengths and offsets."
		'("O(n)" "O(n)" "O(n)" "O(1)")
		'("LZ4 is an LZ77-derivative focused on maximum decompression speed. It parses input streams into literal runs followed by match-copy references to earlier positions in a 64KB sliding window."
			"In ChrysaLisp, LZ4 is the primary compressor used for bytecode storage, asset packing, and rapid boot image loading.")
		'("(import \qlib/stream/lz4.inc\q)"
			""
			"; Compress memory buffer"
			"(defq in_s (string-stream (cat \qrepeat repeat repeat \q \qdata data data\q))"
			"	lz_s (memory-stream))"
			"(stream-lz4-write lz_s in_s)"
			""
			"; High-speed decode"
			"(stream-seek lz_s 0 0)"
			"(defq decoded (memory-stream))"
			"(stream-lz4-read decoded lz_s)")
		"* **Token format**: Byte tokens encode literal length (high 4 bits) and match length (low 4 bits).\n* **Performance**: Achieves hundreds of MB/s throughput on ChrysaLisp VP cores.")

	; 11. Base64 & Bitwise Codec
	(list :base64 "Base64 & Bitwise Codec" "Compression" "O(n) | Space: O(n)"
		"Binary-to-text radix-64 encoding using bitwise shifting and masking."
		'("O(n)" "O(n)" "O(n)" "O(n)")
		'("Base64 represents arbitrary binary data in an ASCII string format by translating groups of three 8-bit bytes (24 bits) into four 6-bit index characters from a standard 64-character alphabet."
			"It is fundamental for embedding images, keys, and MIME attachments in HTTP and JSON.")
		'("(defq +b64_chars \qABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\q)"
			""
			"(defun encode-triplet (b0 b1 b2)"
			"	(defq n (+ (<< b0 16) (<< b1 8) b2))"
			"	(cat"
			"		(elem-get +b64_chars (& (>> n 18) 0x3f))"
			"		(elem-get +b64_chars (& (>> n 12) 0x3f))"
			"		(elem-get +b64_chars (& (>> n 6) 0x3f))"
			"		(elem-get +b64_chars (& n 0x3f))))")
		"* **Bit Shifting**: `(<< val bits)` and `(>> val bits)` manipulate raw binary bit boundaries.\n* **Bitwise AND**: `(& val mask)` masks bits precisely to 6-bit chunks.")

	; 12. Fast Fourier Transform (FFT)
	(list :fft "Fast Fourier Transform (FFT)" "Math" "O(n log n) | Space: O(n)"
		"Discrete Fourier Transform divide-and-conquer decomposition."
		'("O(n log n)" "O(n log n)" "O(n log n)" "O(n)")
		'("The Cooley-Tukey Fast Fourier Transform decomposes a discrete signal from the time or spatial domain into its constituent frequencies in O(n log n) steps instead of O(n^2)."
			"FFT is the cornerstone of audio signal processing, radio spectrograms, image convolutions, and polynomial multiplication.")
		'("(defun dft-point (signal k n)"
			"	; Computes single frequency bin k for length n"
			"	(defq real_sum 0.0 imag_sum 0.0)"
			"	(each (lambda (t_idx)"
			"		(defq theta (/ (* -2.0 3.141592653589793 (n2f (* k t_idx))) (n2f n))"
			"			val (elem-get signal t_idx))"
			"		(setq real_sum (+ real_sum (* val (cos theta))))"
			"		(setq imag_sum (+ imag_sum (* val (sin theta)))))"
			"		(range 0 n))"
			"	(list real_sum imag_sum))")
		"* **n2f**: Converts integer indices into IEEE double floats for precision math.\n* **Trigonometry**: ChrysaLisp provides native `sin`, `cos`, and transcendental VP math primitives.")

	; 13. Mandelbrot Set Fractal
	(list :mandelbrot "Mandelbrot Fractal" "Math" "O(w * h * max_iter) | Space: O(1)"
		"Complex quadratic recurrence escape-time boundary visualization."
		'("O(1)" "O(iter)" "O(max_iter)" "O(1)")
		'("The Mandelbrot set consists of complex numbers c for which the function f_c(z) = z^2 + c does not diverge to infinity when iterated from z = 0."
			"Points are colored based on the number of iterations required before |z| exceeds 2. ChrysaLisp demonstrates this in `apps/science/mandelbrot/app.lisp`.")
		'("(defun mandelbrot-point (cr ci max_iter)"
			"	(defq zr 0.0 zi 0.0 iter 0)"
			"	(while (and (< iter max_iter) (< (+ (* zr zr) (* zi zi)) 4.0))"
			"		(defq new_zr (+ (- (* zr zr) (* zi zi)) cr)"
			"			new_zi (+ (* 2.0 zr zi) ci))"
			"		(setq zr new_zr zi new_zi iter (inc iter)))"
			"	iter)")
		"* **Complex arithmetic**: Represented as unrolled real/imaginary pairs `(zr, zi)`.\n* **Parallel acceleration**: Easily distributed across multi-core VP nodes using pipe farms.")

	; 14. 48.16 Fixed-Point Arithmetic
	(list :fixedpoint "48.16 Fixed-Point Math" "Math" "O(1) CPU Cycle | Space: O(1)"
		"High-speed deterministic sub-pixel coordinate geometry without FPU traps."
		'("O(1)" "O(1)" "O(1)" "O(1)")
		'("ChrysaLisp implements a native 48.16 fixed-point numeric type where the upper 48 bits represent the integer component and the lower 16 bits represent the fractional component (1/65536 per unit)."
			"Fixed-point guarantees bit-identical results across all hardware architectures and avoids non-deterministic floating-point rounding quirks in UI rendering.")
		'("; In ChrysaLisp fixed-point literals and math:"
			"(defq x (i2f 10)       ; 10.0 in 48.16 fixed-point"
			"	step 0x0000000000008000) ; 0.5 (half unit = 32768/65536)"
			""
			"; Fast coordinate scaling"
			"(defq half_x (>> x 1))  ; Exactly 5.0"
			""
			"; Conversion primitives:"
			"(defq int_val (f2i x))   ; Fixed to integer: 10"
			"(defq real_val (f2r x))  ; Fixed to IEEE double float: 10.0)")
		"* **48.16 type**: Integral to ChrysaLisp 2D canvas transforms and vector graphics.\n* **Determinism**: Guaranteed exact cross-platform rendering across VP emulator and native hardware.")

	; 15. Task-Farm Parallelism
	(list :taskfarm "Task-Farm Parallelism" "Systems" "O(n / cores) | Space: O(cores)"
		"Distributing independent computational work units across a cluster of VP nodes."
		'("O(n/c)" "O(n/c)" "O(n/c)" "O(c)")
		'("Task farming (scatter-gather) distributes a queue of independent work packets across worker nodes. As soon as a worker finishes a job, it returns the result to a central collector mailbox and receives the next job."
			"ChrysaLisp's actor/mailbox messaging system provides transparent node-to-node routing across physical CPU cores and network links.")
		'("(import \qlib/task/cmd.inc\q)"
			""
			"(defun run-parallel-jobs (tasks)"
			"	(defq nodes (mail-nodes) master_mbox (task-mbox))"
			"	; Scatter jobs across cluster nodes"
			"	(each (lambda (t)"
			"		(defq target (elem-get nodes (random (length nodes))))"
			"		(open-task (cat *app_root* \qworker.lisp\q) target master_mbox t))"
			"		tasks)"
			"	; Gather results from worker mailboxes"
			"	(defq results (list))"
			"	(while (< (length results) (length tasks))"
			"		(push results (mail-read master_mbox)))"
			"	results)")
		"* **mail-nodes**: Enumerates all active nodes in the ChrysaLisp cluster topology.\n* **open-task**: Spawns an asynchronous process onto a remote or local node with a reply mailbox.")

	; 16. Tail-Call Optimization
	(list :tailcall "Tail-Call Optimization" "Systems" "O(1) Stack Frame | Space: O(1)"
		"Recursive function execution in constant stack space via frame reuse."
		'("O(1)" "O(n)" "O(n)" "O(1) stack")
		'("Tail-Call Optimization (TCO) occurs when the last action of a function is a call to another function or to itself. Instead of pushing a new stack frame, the runtime overwrites the current frame and jumps."
			"In ChrysaLisp, recursive loops compile cleanly without danger of stack overflow. Together with the `loop` macro and `while` construct, iterations run at raw machine speed.")
		'("; Pure tail-recursive accumulator loop"
			"(defun sum-range (n acc)"
			"	(if (<= n 0)"
			"		acc"
			"		(sum-range (dec n) (+ acc n))))"
			""
			"; Evaluates in constant stack space:"
			"(sum-range 100000 0) ; -> 5000050000")
		"* **Frame reuse**: Arguments are updated in place before jumping to the function header.\n* **Zero stack growth**: Enables functional programming idioms without imperative mutation."))
)

(defun catalog-all ()
	*rosetta_catalog*)

(defun catalog-categories ()
	'("All" "Sorting" "Structures" "Compression" "Math" "Systems"))

(defun catalog-filter (cat_filter query_str)
	(defq q (to-lower (trim (if (str? query_str) query_str "")))
		c (or cat_filter "All"))
	(filter (lambda (entry)
		(bind '(id title category complexity summary table explanation code idioms) entry)
		(defq match_cat (or (eql c "All") (eql category c))
			match_query (if (eql q "") :t
				(or (found? (to-lower title) q)
					(found? (to-lower summary) q)
					(found? (to-lower category) q)
					(found? (to-lower complexity) q))))
		(and match_cat match_query))
		*rosetta_catalog*))

(defun catalog-find (target_id)
	(some (# (if (eql (first %0) target_id) %0 :nil)) *rosetta_catalog*))

(defun catalog-overview-markdown (entry)
	(defq id (first entry)
		title (second entry)
		category (third entry)
		complexity (elem-get entry 3)
		summary (elem-get entry 4)
		table (elem-get entry 5)
		explanation (elem-get entry 6))
	(defq lines (list (cat "# " title) ""
		(cat "**Category:** `" category "` | **Complexity:** `" complexity "`") ""
		(cat "*" summary "*") ""
		"---" ""
		"### How It Works" ""))
	(each (lambda (p) (push lines p "")) explanation)
	(push lines "---" "" "### Complexity Analysis" ""
		"| Metric | Best | Average | Worst | Space |"
		"| :--- | :--- | :--- | :--- | :--- |"
		(cat "| **Bound** | " (join table " | ") " |") ""
		"---" ""
		"### ChrysaLisp Implementation" "")
	lines)

(defun catalog-idioms-markdown (entry)
	(defq idioms (elem-get entry 8))
	(if (and idioms (nempty? (trim idioms)))
		(cat '("---" "" "### Key ChrysaLisp Idioms" "") (split idioms "\n") '(""))
		:nil))

(defun catalog-to-markdown (entry)
	(defq lines (catalog-overview-markdown entry)
		code (elem-get entry 7))
	(push lines "```lisp")
	(each (lambda (l) (push lines l)) code)
	(push lines "```" "")
	(if (defq idm (catalog-idioms-markdown entry))
		(each (lambda (l) (push lines l)) idm))
	lines)

