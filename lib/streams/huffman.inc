;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Huffman coding, canonical, of codes no longer than a limit
;
; (huffman-lengths freqs limit) -> lengths
; (huffman-codes lengths) -> codes
; (huffman-table lengths) -> table
; (huffman-symbol in bits table) -> symbol | -1
;
; A code is said by the length of each symbol's code, 0 for a symbol not
; used, as DEFLATE and JPEG say theirs: the codes of a length come one
; after the other in the order of their symbols, after the shorter ones.
; So the lengths are all that is stored, and the codes made from them.
;
; Bits go as (write-bits) and (read-bits) have them, the low bits of a
; byte first, and a code is written from its high bit, as DEFLATE has it,
; so (huffman-codes) gives each the other way round, ready to write.
;
; (huffman-compress in out [token_bits block]) codes a stream in blocks,
; each with its own code, made from what is in it, and its lengths ahead
; of it. (huffman-compress-static in out book) codes it with a code made
; before, a book, of what streams like it have: (huffman-freqs) of them,
; (huffman-book) of that, kept with (huffman-write-book) and read with
; (huffman-read-book). A token the book has no code for is not coded, and
; the result is :nil.
;
; The stream is a token_bits of 8, its tokens of that many bits. A token
; of 2^token_bits is the end of a block, or of the stream.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/fill.inc")

;module
(env-push)

(defq +huffman_block 65536)

(defun huffman-lengths (freqs limit)
	; (huffman-lengths freqs limit) -> lengths
	;freqs a nums of how often each symbol is, lengths a nums of the same
	;length, none more than limit, which must be enough for the symbols
	(defq used (filter (# (> (elem-get freqs %0) 0)) (range 0 (length freqs)))
		lengths (fill (nums) (length freqs) 0) n (length used))
	(cond
		((= n 0))
		((= n 1) (elem-set lengths (first used) 1))
		(:t ;the tree, the two least taken each time, from two queues, the
			;leaves by how often, and the nodes made, which come in order
			(sort used (# (if (/= (defq d (- (elem-get freqs %0) (elem-get freqs %1))) 0) d (- %0 %1))))
			(defq weight (cat (apply (const nums) (map (# (elem-get freqs %0)) used)) (fill (nums) (dec n) 0))
				parent (fill (nums) (dec (* 2 n)) 0)
				leaf 0 node n made n)
			(defun least ()
				(cond
					((and (< leaf n) (or (>= node made) (<= (elem-get weight leaf) (elem-get weight node))))
						(++ leaf) (dec leaf))
					(:t (++ node) (dec node))))
			(while (< made (dec (* 2 n)))
				(defq a (least) b (least))
				(elem-set weight made (+ (elem-get weight a) (elem-get weight b)))
				(elem-set parent a made)
				(elem-set parent b made)
				(++ made))
			;the depth of each, from the root down, a parent after its children
			(defq depth (fill (nums) (dec (* 2 n)) 0) most 0 i (- (* 2 n) 2))
			(while (> i 0)
				(-- i)
				(elem-set depth i (inc (elem-get depth (elem-get parent i)))))
			;how many of each length, and none longer than the limit, as JPEG's
			;K.3 does it: two of the longest are made one shorter, and one
			;shorter one made two of one longer
			(defq counts (fill (nums) (inc (max limit n)) 0))
			(each (# (defq d (elem-get depth %0)) (elem-set counts d (inc (elem-get counts d))) (setq most (max most d)))
				(range 0 n))
			(setq i most)
			(while (> i limit)
				(while (> (elem-get counts i) 0)
					(defq j (- i 2))
					(while (= (elem-get counts j) 0) (-- j))
					(elem-set counts i (- (elem-get counts i) 2))
					(elem-set counts (dec i) (inc (elem-get counts (dec i))))
					(elem-set counts (inc j) (+ (elem-get counts (inc j)) 2))
					(elem-set counts j (dec (elem-get counts j))))
				(-- i))
			;the shortest to the most taken
			(defq at (dec n) len 1)
			(while (>= at 0)
				(cond
					((= (elem-get counts len) 0) (++ len))
					(:t (elem-set lengths (elem-get used at) len)
						(elem-set counts len (dec (elem-get counts len)))
						(-- at))))))
	lengths)

(defun huffman-codes (lengths)
	; (huffman-codes lengths) -> codes
	;the code of each symbol, its bits the other way round, to be written
	;with (write-bits) of its length
	(defq most (reduce (const max) lengths 0)
		counts (fill (nums) (inc most) 0)
		next (fill (nums) (inc most) 0) code 0)
	(each (# (if (> %0 0) (elem-set counts %0 (inc (elem-get counts %0))))) lengths)
	(each (# (setq code (<< (+ code (elem-get counts (dec %0))) 1)) (elem-set next %0 code)) (range 1 (inc most)))
	(map (lambda (len)
			(cond
				((= len 0) 0)
				(:t (defq c (elem-get next len) r 0)
					(elem-set next len (inc c))
					(times len (setq r (logior (<< r 1) (logand c 1)) c (>> c 1)))
					r)))
		lengths))

(defun huffman-table (lengths)
	; (huffman-table lengths) -> (counts symbols)
	;to read a code: how many there are of each length, and the symbols in
	;the order of their codes
	(defq most (reduce (const max) lengths 0)
		counts (fill (nums) (inc most) 0)
		offsets (fill (nums) (inc most) 0)
		symbols (fill (nums) (length lengths) 0))
	(each (# (elem-set counts %0 (inc (elem-get counts %0)))) lengths)
	(elem-set counts 0 0)
	;where the codes of each length begin, after those shorter
	(each (# (elem-set offsets (inc %0) (+ (elem-get offsets %0) (elem-get counts %0)))) (range 1 most))
	(each (lambda (len)
			(when (> len 0)
				(elem-set symbols (elem-get offsets len) (!))
				(elem-set offsets len (inc (elem-get offsets len)))))
		lengths)
	(list counts symbols))

(defun huffman-symbol (in bits table)
	; (huffman-symbol in bits table) -> symbol | -1
	;the next symbol of a code, read a bit at a time, -1 at the end of
	;the stream or if the bits are no code
	(bind '(counts symbols) table)
	(defq at 0 lo 0 index 0 len 0 most (dec (length counts)) found -1)
	(while (and (< found 0) (< len most))
		(++ len)
		(defq bit (read-bits in bits 1))
		(cond
			((< bit 0) (setq len most))
			(:t (setq at (logior at bit))
				(defq count (elem-get counts len))
				(cond
					((< (- at lo) count) (setq found (elem-get symbols (+ index (- at lo)))))
					(:t (setq index (+ index count) lo (<< (+ lo count) 1) at (<< at 1)))))))
	found)

;the lengths of a code as it is stored: how many symbols have a code, and
;each of them, and its length, in 5 bits
(defun huffman-write-lengths (out bits lengths symbol_bits)
	(defq used (filter (# (> (elem-get lengths %0) 0)) (range 0 (length lengths))))
	(write-bits out bits (length used) (inc symbol_bits))
	(each (# (write-bits out bits %0 symbol_bits) (write-bits out bits (elem-get lengths %0) 5)) used))

(defun huffman-read-lengths (in bits symbols symbol_bits)
	; -> :nil | lengths
	(defq lengths (fill (nums) symbols 0)
		n (read-bits in bits (inc symbol_bits)) ok (>= n 0))
	(while (and ok (> n 0))
		(defq s (read-bits in bits symbol_bits) len (read-bits in bits 5))
		(if (and (>= s 0) (< s symbols) (> len 0)) (elem-set lengths s len) (setq ok :nil))
		(-- n))
	(if ok lengths))

(defun huffman-limit (symbols)
	;the longest a code may be, enough for every symbol, and no less than 15
	(defq l 15)
	(while (> symbols (<< 1 l)) (++ l))
	l)

;;;;;;;;;;;;;;;;;;;;;;;;
; a stream, block by block
;;;;;;;;;;;;;;;;;;;;;;;;

(defun huffman-compress (in_stream out_stream &optional token_bits block)
	; (huffman-compress in_stream out_stream [token_bits block])
	;the tokens a block of them at a time, default 65536, each with a code
	;of its own. Each block a bit of 1, its code's lengths, its tokens and
	;the end of it, and the stream a bit of 0 after the last
	(setd token_bits 8 block +huffman_block)
	(defq rbits (array 0 0) wbits (array 0 0) eob (<< 1 token_bits)
		symbol_bits (inc token_bits) limit (huffman-limit (inc eob)) going :t)
	(write-bits out_stream wbits token_bits 8)
	(while going
		(defq tokens (cap block (nums)) freqs (fill (nums) (inc eob) 0))
		(while (and (< (length tokens) block) (>= (defq token (read-bits in_stream rbits token_bits)) 0))
			(if (= (logand (length tokens) 0xfff) 0) (task-slice))
			(push tokens token)
			(elem-set freqs token (inc (elem-get freqs token))))
		(setq going (= (length tokens) block))
		(when (nempty? tokens)
			(task-slice)
			(elem-set freqs eob 1)
			(defq lengths (huffman-lengths freqs limit) codes (huffman-codes lengths))
			(write-bits out_stream wbits 1 1)
			(huffman-write-lengths out_stream wbits lengths symbol_bits)
			(each (# (if (= (logand (!) 0xfff) 0) (task-slice))
					(write-bits out_stream wbits (elem-get codes %0) (elem-get lengths %0)))
				tokens)
			(write-bits out_stream wbits (elem-get codes eob) (elem-get lengths eob))))
	(write-bits out_stream wbits 0 1)
	(flush-bits out_stream wbits))

(defun huffman-decompress (in_stream out_stream &optional _)
	; (huffman-decompress in_stream out_stream) -> :t | :nil
	;:nil if the stream stops before its end, or is not one, what was got
	;by then is written
	(defq rbits (array 0 0) wbits (array 0 0) token_bits (read-bits in_stream rbits 8)
		ok (<= 1 token_bits 24) going ok)
	(when ok
		(defq eob (<< 1 token_bits) symbol_bits (inc token_bits)))
	(while going
		(task-slice)
		(defq more (read-bits in_stream rbits 1))
		(cond
			((= more 0) (setq going :nil))
			((and (= more 1) (defq lengths (huffman-read-lengths in_stream rbits (inc eob) symbol_bits)))
				(defq table (huffman-table lengths) sym 0 n 0)
				(while (and (>= (setq sym (huffman-symbol in_stream rbits table)) 0) (/= sym eob))
					(if (= (logand (++ n) 0xfff) 0) (task-slice))
					(write-bits out_stream wbits sym token_bits))
				(if (< sym 0) (setq ok :nil going :nil)))
			(:t (setq ok :nil going :nil))))
	(flush-bits out_stream wbits)
	ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a stream by a book made from another
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun huffman-freqs (in_stream &optional token_bits freqs)
	; (huffman-freqs in_stream [token_bits freqs]) -> freqs
	;how often each token is in the stream, added to freqs if given, a
	;nums of 2^token_bits
	(setd token_bits 8 freqs (fill (nums) (<< 1 token_bits) 0))
	(defq rbits (array 0 0) n 0)
	(while (>= (defq token (read-bits in_stream rbits token_bits)) 0)
		(elem-set freqs token (inc (elem-get freqs token)))
		(if (= (logand (++ n) 0xfff) 0) (task-slice)))
	freqs)

(defun huffman-book (token_bits freqs)
	; (huffman-book token_bits freqs) -> book
	;a code of the tokens as often as freqs has them, and of the end. Each
	;is taken as once more than it was, so a token not seen has a code too
	(list token_bits (huffman-lengths (cat (map (# (+ %0 1)) freqs) (list 1)) (huffman-limit (inc (length freqs))))))

(defun huffman-write-book (out_stream book)
	; (huffman-write-book out_stream book)
	(bind '(token_bits lengths) book)
	(defq wbits (array 0 0))
	(write-bits out_stream wbits token_bits 8)
	(huffman-write-lengths out_stream wbits lengths (inc token_bits))
	(flush-bits out_stream wbits))

(defun huffman-read-book (in_stream)
	; (huffman-read-book in_stream) -> :nil | book
	(defq rbits (array 0 0) token_bits (read-bits in_stream rbits 8))
	(and (<= 1 token_bits 24)
		(defq lengths (huffman-read-lengths in_stream rbits (inc (<< 1 token_bits)) (inc token_bits)))
		(list token_bits lengths)))

(defun huffman-compress-static (in_stream out_stream book)
	; (huffman-compress-static in_stream out_stream book) -> :t | :nil
	;:nil if a token has no code in the book, the stream is coded up to it
	(bind '(token_bits lengths) book)
	(defq rbits (array 0 0) wbits (array 0 0) eob (<< 1 token_bits)
		codes (huffman-codes lengths) ok (> (elem-get lengths eob) 0) n 0)
	(while (and ok (>= (defq token (read-bits in_stream rbits token_bits)) 0))
		(if (= (logand (++ n) 0xfff) 0) (task-slice))
		(if (> (defq len (elem-get lengths token)) 0)
			(write-bits out_stream wbits (elem-get codes token) len)
			(setq ok :nil)))
	(if ok (write-bits out_stream wbits (elem-get codes eob) (elem-get lengths eob)))
	(flush-bits out_stream wbits)
	ok)

(defun huffman-decompress-static (in_stream out_stream book)
	; (huffman-decompress-static in_stream out_stream book) -> :t | :nil
	(bind '(token_bits lengths) book)
	(defq rbits (array 0 0) wbits (array 0 0) eob (<< 1 token_bits)
		table (huffman-table lengths) sym 0 n 0)
	(while (and (>= (setq sym (huffman-symbol in_stream rbits table)) 0) (/= sym eob))
		(if (= (logand (++ n) 0xfff) 0) (task-slice))
		(write-bits out_stream wbits sym token_bits))
	(flush-bits out_stream wbits)
	(= sym eob))

;module
(export-symbols '(huffman-lengths huffman-codes huffman-table huffman-symbol
	huffman-compress huffman-decompress huffman-freqs huffman-book huffman-write-book huffman-read-book
	huffman-compress-static huffman-decompress-static))
(env-pop)
