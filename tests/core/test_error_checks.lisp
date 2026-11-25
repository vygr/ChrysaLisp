(report-header "Error Checks: bad arguments must throw")

; --- index out of range ---
(assert-error "elem-get past end str" (elem-get "abc" 3) {(elem-get seq idx) not_valid_index})
(assert-error "elem-get before start str" (elem-get "abc" -5) {(elem-get seq idx) not_valid_index})
(assert-error "elem-get past end list" (elem-get (list 1 2 3) 3) {(elem-get seq idx) not_valid_index})
(assert-error "elem-get empty list" (elem-get (list) 0) {(elem-get seq idx) not_valid_index})
(assert-error "elem-get past end nums" (elem-get (nums 1 2) 2) {(elem-get seq idx) not_valid_index})
(assert-error "elem-set past end" (elem-set (list 1 2 3) 3 9) {(elem-set array idx elem) not_valid_index})
(assert-error "slice past end str" (slice "hello" 0 6) {(slice seq start end) not_valid_index})
(assert-error "slice before start str" (slice "hello" -7 0) {(slice seq start end) not_valid_index})
(assert-error "slice past end list" (slice (list 1 2) 0 3) {(slice seq start end) not_valid_index})
(assert-error "slice past end nums" (slice (nums 1 2) 3 0) {(slice seq start end) not_valid_index})

; --- wrong types to sequence functions ---
(assert-error "slice num" (slice 5 0 1) {(slice seq start end) wrong_types})
(assert-error "length num" (length 5) {(length seq) wrong_types})
(assert-error "first num" (first 5) {(first seq) not_a_sequence})
(assert-error "cat str list" (cat "a" (list 1)) {(cat seq ...) wrong_types})
(assert-error "cat str num" (cat "a" 5) {(cat seq ...) wrong_types})
(assert-error "cat list nums" (cat (list 1) (nums 2)) {(cat seq ...) wrong_types})
(assert-error "cat no args" (cat) {(cat seq ...) wrong_types})
(assert-error "find in num" (find 1 5) {(find elem seq [idx]) wrong_types})
(assert-error "reverse num" (reverse 5) {(slice seq start end) wrong_types})
(assert-error "push num" (push 5 1) {(push array elem ...) wrong_types})
(assert-error "push str" (push "abc" "d") {(push array elem ...) wrong_types})
(assert-error "push no elem" (push (list 1)) {(push array elem ...) wrong_types})
(assert-error "pop num" (pop 5) {(pop array) wrong_types})
(assert-error "pop str" (pop "abc") {(pop array) wrong_types})
(assert-error "partition zero" (partition (list 1 2) 0) {(partition seq [cnt]) wrong_types})

; --- arithmetic ---
(assert-error "add str" (+ 1 "a") {(+ num num ...) wrong_types})
(assert-error "add nil" (+ 1 :nil) {(+ num num ...) wrong_types})
(assert-error "add fixed and num" (* 1.5 2) {(* num num ...) wrong_types})
(assert-error "compare str" (< 1 "a") {(< num num ...) wrong_types})
(assert-error "logand str" (logand 1 "a") {(logand [num] ...) wrong_types})
(assert-error "neg str" (neg "a") {(neg num) wrong_types})
(assert-error "abs nil" (abs :nil) {(abs num) wrong_types})
(assert-error "add no args" (+) {(+ num num ...) wrong_types})
(assert-error "add one arg" (+ 5) {(+ num num ...) wrong_types})
(assert-error "sub one arg" (- 5) {(- num num ...) wrong_types})
(assert-error "compare one arg" (= 1) {(= num num ...) wrong_types})
(assert-error "min one arg" (min 3) {(min num num ...) wrong_types})
(assert-error "shl one arg" (<< 1) {(<< num cnt) wrong_types})
(assert-error "shr one arg" (>> 1) {(>> num cnt) wrong_types})
(assert-error "asr one arg" (>>> 1) {(>>> num cnt) wrong_types})
(assert-error "shl three args" (<< 1 2 3) {(<< num cnt) wrong_types})
(assert-error "shl str" (<< "a" 1) {(<< num cnt) wrong_types})
(assert-error "shl no args" (<<) {(<< num cnt) wrong_types})
(assert-error "nums str" (nums 1 "a") {(nums [num ...]) wrong_types})
(assert-error "nums-add lengths" (nums-add (nums 1 2) (nums 1)) {(nums-add nums nums [nums]) wrong_types})

; --- strings ---
(assert-error "sym num" (sym 5) {(sym str) not_a_string})
(assert-error "char str" (char "a") {(char num [width]) wrong_types})
(assert-error "code num" (code 5) {(code str [width idx]) wrong_types})
(assert-error "code empty" (code "") {(code str [width idx]) wrong_types})
(assert-error "str-as-num num" (str-as-num 5) {(str-to-num str) wrong_types})

; --- binding and calling ---
(assert-error "bind too few" (bind '(a b) (list 1)) {(bind (param ...) seq) wrong_num_of_args !})
(assert-error "bind too many" (bind '(a) (list 1 2)) {(bind (param ...) seq) wrong_num_of_args !})
(assert-error "bind num" (bind '(a b) 5) {(bind (param ...) seq) not_a_sequence !})
(assert-error "lambda too few" ((lambda (a) a)) {(bind (param ...) seq) wrong_num_of_args !})
(assert-error "apply num" (apply + 5) {(apply lambda seq) wrong_types})
(assert-error "eval no args" (eval) {(eval form [env]) wrong_types})
;check off, a call of nothing is the very thing
(assert-error "unbound function" (test_no_such_function 1) {(eval form [env]) symbol_not_bound})
(assert-error "unbound symbol" test_no_such_symbol {(eval form [env]) symbol_not_bound})
(assert-error "setq unbound" (setq test_no_such_symbol 1) {(setq sym val [sym val] ...) symbol_not_bound})

; --- environments ---
(assert-error "def odd args" (def (env)) {(def env sym val [sym val] ...) wrong_num_of_args})
(assert-error "def num" (def 5 'a 1) {(def env sym val [sym val] ...) not_an_environment})
(assert-error "get num" (get 5) {(get sym [env]) wrong_types})
(assert-error "env-push num" (env-push 5) {(env-push [env]) wrong_types})

; --- an error from inside a function that has a frame on the stack ---
;obj-set made its frame before its checks, and its error went with the
;frame still on, returning through the wrong word: it is said now, and the
;task goes on
(assert-error "obj-set before its object" (obj-set (str-alloc 16) -8 +type_long 0) {(obj-set obj offset type) not_valid_index})
(assert-error "obj-set past its object" (obj-set (str-alloc 16) 4096 +type_long 0) {(obj-set obj offset type) not_valid_index})
(assert-eq "and the task goes on after" 3 (+ 1 2))
;a field not at its natural alignment, docs/vm/vp_vm.md
(assert-error "obj-set of a long not on a word" (set-long (str-alloc 32) 7 0) {(obj-set obj offset type) not_valid_index})
(assert-error "of an int not on 4" (set-int (str-alloc 32) 3 0) {(obj-set obj offset type) not_valid_index})
(assert-error "of a short not on 2" (set-short (str-alloc 32) 1 0) {(obj-set obj offset type) not_valid_index})

;a struct written is that many bytes of what has raw data, a str, an array
;of numbers or a raw array, whole: fewer than that, or a list, is refused
(defq raw_s (str-alloc 16))
(test-cases
	(map (# (get-uint (obj-set raw_s +str_data 16 (ints 1 2 3 4)) (* %0 4))) '(0 1 2 3)) '(1 2 3 4)
	(get-uint (obj-set raw_s +str_data 4 (bytes 1 2 3 4)) 0) 67305985
	(get-uint (obj-set raw_s +str_data 4 (shorts 1 2)) 0) 131073
	(get-long (obj-set raw_s +str_data 8 (nums 77)) 0) 77
	(get-uint (obj-set raw_s +str_data 4 (nums-convert (nums 1) (floats))) 0) 1065353216)
(assert-error "obj-set of too few bytes" (obj-set raw_s +str_data 16 (ints 1 2)) {(obj-set obj offset type val) wrong_types})
(assert-error "of a list" (obj-set raw_s +str_data 8 (list 1)) {(obj-set obj offset type val) wrong_types})
(assert-error "of a number for a struct" (obj-set raw_s +str_data 8 77) {(obj-set obj offset type val) wrong_types})
(assert-error "and obj-get of a long not on a word" (get-long (str-alloc 32) 4) {(obj-get obj offset type) not_valid_index})

; --- a macro that throws as it is expanded, in a body ---
;its error took the place of the form it was expanded from, and in a body
;that is not run as it is read it sat there till that body was called: a
;file loaded, and a block of the docs passed, with it in them
(assert-error "a macro's error at the top"
	(repl (string-stream "(const (+ 1 :a))") "error_checks") {(+ num num ...) wrong_types})
(assert-error "and in the body of a function not called"
	(repl (string-stream "(defun error_checks_f () (const (+ 1 :a)))") "error_checks") {(+ num num ...) wrong_types})
(assert-error "and of a macro not used"
	(repl (string-stream "(defmacro error_checks_m () (static-qq (list (const (+ 1 :a)))))") "error_checks") {(+ num num ...) wrong_types})
(assert-eq "and a body with no error is still read" 3
	(repl (string-stream "(defun error_checks_g () (const (+ 1 2))) (error_checks_g)") "error_checks"))

;an assert-error passes only on the error it expects: another is not caught,
;and goes on up as it would anywhere, here to this catch
(assert-eq "an assert-error given another error lets it go on" :t
	(catch (progn (assert-error "inner" (elem-get (list) 0) {not this one}) :nil)
		(found? (str _) {(elem-get seq idx) not_valid_index})))

;a splice's error of an index went with the args lost, what asked the
;lengths had taken their register, and the node with it
(assert-error "splice of an index past its sequence" (splice (nums 1 2 3 4) (nums 9 8 7) (nums 0 2 0 2 3 1 4 2)) {(splice seq1 seq2 idxs) not_valid_index})
(assert-error "and before it" (splice "abcd" "xyz" (nums 0 2 -6 3)) {(splice seq1 seq2 idxs) not_valid_index})
(assert-eq "and one inside it is taken" "(1 2 9 8 3 2 7 8)" (str (splice (nums 1 2 3 4) (nums 9 8 7) (nums 0 2 0 2 3 1 3 1))))
