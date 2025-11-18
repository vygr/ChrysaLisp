;ELIZA pattern matching engine
;Classic symbolic processing and pattern transformation

(import "./patterns.inc")

;Helper: join list of words with separator
(defun join-words (words sep)
	(if (list? words)
		(reduce (lambda (acc word)
			(if (eql acc "")
				word
				(cat acc sep word))) words "")
		words))

;Helper: replace substring in string
(defun str-replace (str old new)
	(defq result "" idx 0)
	(while (< idx (length str))
		(if (and (<= (+ idx (length old)) (length str))
				 (eql (slice str idx (+ idx (length old))) old))
			(progn
				(setq result (cat result new))
				(setq idx (+ idx (length old))))
			(progn
				(setq result (cat result (slice str idx (inc idx))))
				(setq idx (inc idx)))))
	result)

;Helper: random number generator
(defun random (n)
	(% (abs (pii-time)) n))

;Normalize input: lowercase and split into words
(defun normalize (text)
	(defq text (to-lower text))
	;Remove punctuation
	(each! (# (if (or (eql %0 (ascii-char 33)) ;!
					  (eql %0 (ascii-char 46)) ;.
					  (eql %0 (ascii-char 44)) ;,
					  (eql %0 (ascii-char 63)) ;?
					  (eql %0 (ascii-char 59)) ;semicolon
					  (eql %0 (ascii-char 58))) ;colon
				  (elem-set text (!) (ascii-char 32)))) text)
	;Split into words
	(filter (# (> (length %0) 0)) (split text)))

;Reflect words: transform "i" to "you", "my" to "your" etc.
(defun reflect (words)
	(map (lambda (word)
		(defq reflection (some! (# (if (eql (first %0) word) %0)) *reflections*))
		(if reflection (second reflection) word)) words))

;Match pattern against input words
;Pattern uses * for wildcards
(defun match-pattern (pattern words)
	(defq matches (list) p-idx 0 w-idx 0)
	(while (and (<= p-idx (length pattern)) (<= w-idx (length words)))
		(cond
			((= p-idx (length pattern))
			 ;Pattern exhausted - must be at end of words too
			 (if (= w-idx (length words))
				 (setq p-idx (inc p-idx) w-idx (inc w-idx))
				 (setq p-idx 999999)))
			((eql (elem-get pattern p-idx) "*")
			 ;Wildcard - try to match rest of pattern
			 (if (= p-idx (dec (length pattern)))
				 ;* at end matches everything remaining
				 (progn
					(push matches (join-words (slice words w-idx -1) " "))
					(setq p-idx (inc p-idx) w-idx (length words)))
				 ;Try to find next pattern element
				 (progn
					(defq next-p (elem-get pattern (inc p-idx))
						  matched-words (list)
						  found :nil)
					;Scan forward for the next pattern word
					(while (and (< w-idx (length words)) (not found))
						(if (eql (elem-get words w-idx) next-p)
							(setq found :t)
							(progn
								(push matched-words (elem-get words w-idx))
								(setq w-idx (inc w-idx)))))
					(if found
						(progn
							(push matches (join-words matched-words " "))
							(setq p-idx (+ p-idx 2) w-idx (inc w-idx)))
						;No match found
						(setq p-idx 999999)))))
			((and (< p-idx (length pattern)) (< w-idx (length words))
				  (eql (elem-get pattern p-idx) (elem-get words w-idx)))
			 ;Exact match
			 (setq p-idx (inc p-idx) w-idx (inc w-idx)))
			(:t ;No match
				(setq p-idx 999999))))
	;Success if we consumed all pattern
	(if (and (> p-idx (length pattern)) (>= w-idx (length words)))
		matches))

;Find matching pattern and return response
(defun find-response (words)
	(defq best-pattern :nil best-rank 0)
	;Search through all patterns
	(each! (lambda (_)
		(defq keyword (first %0)
			  rank (second %0)
			  patterns (third %0))
		;Check if keyword appears in input
		(when (or (eql keyword "xnone")
				  (some! (# (eql %0 keyword)) words))
			(when (> rank best-rank)
				;Try each pattern for this keyword
				(each! (lambda (_)
					(defq pattern (normalize (first %0))
						  responses (second %0))
					(when (defq matches (match-pattern pattern words))
						;Found a match!
						(setq best-pattern (list matches responses)
							  best-rank rank))) patterns)))) *patterns*)
	;Return best match or default
	(if best-pattern
		best-pattern
		(list (list) (second (third (some! (# (if (eql (first %0) "xnone") %0)) *patterns*))))))

;Generate response from template and matches
(defun generate-response (template matches)
	(defq response template)
	;Replace %0, %1, %2 etc with reflected matches
	(each! (lambda (_)
		(defq placeholder (cat "%" (str (!)))
			  match-words (normalize (elem-get matches (!)))
			  reflected (join-words (reflect match-words) " "))
		(setq response (str-replace response placeholder reflected))) matches)
	;Clean up extra spaces
	(setq response (trim response))
	;First char to uppercase
	(if (> (length response) 0)
		(setq response (cat (to-upper (slice response 0 1)) (slice response 1 -1))))
	response)

;Main ELIZA response function
(defun eliza-response (input)
	(defq words (normalize input))
	;Check for quit words
	(if (some! (# (find %0 words)) *quit_words*)
		:nil
		;Find matching pattern and generate response
		(progn
			(defq result (find-response words)
				  matches (first result)
				  responses (second result)
				  template (elem-get responses (random (length responses))))
			(generate-response template matches))))
