# ChrysaLisp Lisp Disciplines

Part of the `chrysalisp` skill. These rules are mandatory for any Lisp code you
write or review. Each has its own heading, with the reason for it and worked
examples. `SKILL.md` carries a one line summary of each.

## Contents

*	[Tab Style](#tab-style)

*	[Indentation and the `fmt` Command](#indentation-and-the-fmt-command)

*	[Sensible `defq` and `setq` Line Wrapping](#sensible-defq-and-setq-line-wrapping)

*	[Type-Dependent Equality with `eql`](#type-dependent-equality-with-eql)

*	[Multi-Argument (N-ary) Comparisons & Range Checks (`<=`, `=`, `>=`, `<`, `>`, `/=`)](#multi-argument-n-ary-comparisons--range-checks------)

*	[Dynamic Scoping (No Lexical Closures)](#dynamic-scoping-no-lexical-closures)

*	[Manual Scope Control (`env-push` / `env-pop`)](#manual-scope-control-env-push--env-pop)

*	[Destructuring & Tuple Unpacking (`bind` vs Manual `elem-get`)](#destructuring--tuple-unpacking-bind-vs-manual-elem-get)

*	[DO NOT Shadow Built-in Function Names](#do-not-shadow-built-in-function-names)

*	[Sensible Line Wrapping for `defq`, `setq`, and `bind`](#sensible-line-wrapping-for-defq-setq-and-bind)

*	[Static `'()` vs Independent `(list)`](#static--vs-independent-list)

*	[Control Flow and Error Prevention](#control-flow-and-error-prevention)

*	[Conditionals & Branching (`if`, `ifn`, `when`, `unless`)](#conditionals--branching-if-ifn-when-unless)

*	[Lists as LIFO Stacks & The Rocinante Collector Pattern](#lists-as-lifo-stacks--the-rocinante-collector-pattern)

*	[Pragmatic Lambda Usage (`#` vs `lambda`)](#pragmatic-lambda-usage--vs-lambda)

*	[Variable Binding and Shadowing](#variable-binding-and-shadowing)

*	[Object Syntax & Sensible Wrapping](#object-syntax--sensible-wrapping)

*	[Anaphoric Loop Index `(!)`](#anaphoric-loop-index-)

*	[Short Anaphoric Lambdas `(# ...)` vs `(lambda ...)`](#short-anaphoric-lambdas---vs-lambda-)

*	[Compile-Time Constants](#compile-time-constants)

*	[Top-Level `defun` Definitions (Prebinder Rule)](#top-level-defun-definitions-prebinder-rule)

*	[GUI Application Event Loop Pattern](#gui-application-event-loop-pattern)

*	[NEVER Run GUI Code from TUI or Test Environment](#never-run-gui-code-from-tui-or-test-environment)

*	[Short-Circuiting, Embedded Binding, and Branching (`and` / `or`)](#short-circuiting-embedded-binding-and-branching-and--or)

*	[Spans and Parts of Sequences (`each!` and Friends, No `range`)](#spans-and-parts-of-sequences-each-and-friends-no-range)

*	[No Recursion, a `(list)` Is the Stack](#no-recursion-a-list-is-the-stack)

*	[Characters Are Literals](#characters-are-literals)

*	[Time with `time-it`, Never by Hand](#time-with-time-it-never-by-hand)

*	[Traps](#traps)

*	[Use What Is There, and Grow the Tools](#use-what-is-there-and-grow-the-tools)

*	[Ownership Is a Tree, Back Links Are Weak](#ownership-is-a-tree-back-links-are-weak)

*	[Mail Is Unordered, Mailboxes Are Cheap](#mail-is-unordered-mailboxes-are-cheap)

*	[Messages Drive, Inactivity Dissolves](#messages-drive-inactivity-dissolves)

*	[A Matrix Is Made from Its Raw Variables](#a-matrix-is-made-from-its-raw-variables)

## Tab Style

Always use leading 4-space tab characters for indentation
in source code and documentation, with spaces afterwards if needed.

## Indentation and the `fmt` Command

The indent of a line comes from the structure alone. These are the rules the
`fmt` command applies, and the rules most of the tree already follows, so
write new code this way.

*	A line is one tab in from the line its enclosing form opened on. Not
	lined up under an argument.

	```vdu
	(defun f (a)
		(print a)
		(if a
			(print 1)
			(print 2)))
	```

*	Where several forms open on the one line, each is a tab further in than
	the one around it. The indent then shows which form owns a line. Here
	the body of the `lambda` is two tabs in, and the rest of the arguments
	of the `reduce` one tab in.

	```vdu
	(defq moves (reduce (lambda (out (dx dy key))
			(defq nx (+ x dx) ny (+ y dy))
			(push out (list nx ny key)))
		(list (list -1 0 +fkey_left) (list 1 0 +fkey_right))
		(list)))

	(when (and (first-test a)
			(second-test b))
		(print a))
	```

*	VP block forms, `(vpif)` `(loop-start)` `(switch)` and the like, indent
	the lines between them. `(else)` `(vpcase)` `(vp-label)` sit one back.
	`(validatecase)` adds no indent, what is in it sits where it would
	without it.

*	No line starts with a close bracket, they gather on the line above.

The `fmt` command, `lib/text/format.inc`, is an experiment and is parked.
Do not run `fmt -w` or `make fmt` on the tree, and do not treat its output
as the standard. The indent rules above are how the source is written by
hand, and stand without it.

*	The source scanners and the doc builder read a line at a time, and take
	a line that starts with `defun`, `defmethod`, `ffi`, `dec-method` and
	such to be one. So never start a line of data, or of a string, with one
	of those words.

## Sensible `defq` and `setq` Line Wrapping

Do not place only one variable setting per line when defining multiple
variables with `(defq ...)` or mutating with `(setq ...)`. Putting one
variable-value pair per line is excessive and inflates line count.
Pack related variable-value pairs onto lines up to a reasonable column width
(~80-100 characters) and flow across lines cleanly:

	; Good: Flowed, logically grouped variable settings
	(defq +font_title (create-font "fonts/OpenSans-Bold.ctf" 14) +font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
		+font_bold (create-font "fonts/OpenSans-Bold.ctf" 13) +font_small (create-font "fonts/OpenSans-Regular.ctf" 11)
		*config* :nil *config_version* 1 *config_file* (cat (get :home *env*) "news.tre")
		*selected_category* "top" *selected_id* 0 *current_stories* (list))

	; Avoid: 1 variable pair per line bloating vertical space
	(defq
		+font_title (create-font "fonts/OpenSans-Bold.ctf" 14)
		+font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
		*config* :nil
		*config_version* 1
		*selected_category* "top")

Combine local variable bindings within functions rather than writing multiple
consecutive `(defq ...)` calls:

	; Good: Combined and flowed
	(defq idx (!) is_selected (= id sel_id) card_flow (Flow) title_btn (Button)
		meta_flow (Flow) score_lbl (Label) meta_lbl (Label))

## Type-Dependent Equality with `eql`

*	`eql` performs deep content comparison on scalar numbers, strings,
	and typed numeric vectors (`nums`, `fixeds`, `doubles`).

*	`eql` performs pointer identity comparison on general containers
	(`list`, `hmap`). To test list content equivalence, use:

	(and (= (length l1) (length l2)) (every eql l1 l2))

## Multi-Argument (N-ary) Comparisons & Range Checks (`<=`, `=`, `>=`, `<`, `>`, `/=`)

*	All comparison operators accept an arbitrary number of arguments:
	`(= a b c ...)`, `(<= a b c ...)`, `(< a b c ...)`, `(>= a b c ...)`.

*	*Range & "Within" Tests (Never `(and (>= ...) (<= ...))`):*
	Never write multiple comparisons joined by `and`:
	```lisp
	;; ANTI-PATTERN (evaluates id twice, allocates and executes 'and'):
	(and (>= id +event_city_0) (<= id +event_city_5))
	```
	Always use a single chained comparison:
	```lisp
	;; THE CHRYSALISP WAY (single primitive call, evaluates id once):
	(<= +event_city_0 id +event_city_5)
	```

*	*Named Constants for Event Ranges & Capacity Limits:*
	Never scatter magic numbers (e.g. `50`, `20`) inside event range checks or loops.
	Always declare an explicit named constant for the capacity limit (e.g. `+max_stories 50`),
	and evaluate the upper bound at compile time with flat $N$-ary addition `(const (+ +event_story_0 +max_stories -1))`:
	```lisp
	(defq +max_stories 50)
	...
	(<= +event_story_0 id (const (+ +event_story_0 +max_stories -1)))
	```

*	*Monotonic Ordering:*
	*	`(<= a b c)` tests `a <= b <= c`.
	*	`(< a b c)` tests `a < b < c`.
	*	`(= a b c)` tests `a = b = c`.
	*	`(< -1 idx (length items))` tests `0 <= idx < (length items)` in one step!

## Dynamic Scoping (No Lexical Closures)

Functions and lambdas
execute strictly inside the environment of their caller. They do not
capture defining scopes. All context must be supplied explicitly via
arguments or pre-exist in the caller's environment.

## Manual Scope Control (`env-push` / `env-pop`)

*	`(env-push)` pushes a new empty environment as the current scope.
	This is the module pattern: `(env-push)` ... `(export-symbols ...)`
	`(env-pop)`.

*	`(env-push env)` pushes the given environment instead, linking its
	parent to the current scope. The environment must be parentless: a
	fresh `(env 1)`, or one previously popped. Its bindings then read and
	`setq` as plain variables. There is no `env-tuck`; this replaces it.

*	`(env-pop)` takes no arguments. It restores the parent scope, clears
	the popped environment's parent link, and returns the popped
	environment, ready to be pushed again.

*	Every push must be paired with a pop in the same function or file.
	An unbalanced push leaves the wrong environment current when the
	enclosing function returns.

*	While pushed, `defq` and `bind` define into the pushed environment.
	Declare outer variables before the push and update them with `setq`:

	(defq state (env 1) total :nil)
	(def state 'count 0)
	(env-push state)
	(setq total (++ count))
	(env-pop)

## Destructuring & Tuple Unpacking (`bind` vs Manual `elem-get`)

Always use `(bind '(var1 var2 ...) seq)` to unpack lists, tuples, or function return sequences.
Never write multi-line ladders of `(elem-get seq 0)`, `(elem-get seq 1)` in a `defq`:
```lisp
;; ANTI-PATTERN (manual elem-get unpacking ladder in defq):
(defq sym (elem-get selected_coin 0)
	name (elem-get selected_coin 1)
	price (elem-get selected_coin 2)
	change (elem-get selected_coin 3)
	rank (elem-get selected_coin 4)
	spark (elem-get selected_coin 5))

;; THE CHRYSALISP WAY (single bind statement):
(bind '(coin_sym name price change rank spark) selected_coin)
```

*	**`&` (Single Skip) vs `&ignore` (Trailing Discard):**
	- `&` skips **exactly one** element (1-to-1 placeholder).
	- `&ignore` terminates binding immediately and discards **all remaining** elements in the sequence.
	If a sequence has trailing elements not accounted for in the pattern, omitting `&ignore` causes an `Error: (bind (param ...) seq) wrong_num_of_args !`.
	```lisp
	;; WRONG: (date) returns 7 elements (sec min hr day mo yr dotw),
	;; so (& cmin chr &) only consumes 4 elements -> wrong_num_of_args!
	(bind '(& cmin chr &) (date city_sec))

	;; CORRECT: &ignore consumes and discards all remaining elements:
	(bind '(& cmin chr &ignore) (date city_sec))
	```

## DO NOT Shadow Built-in Function Names

Never use built-in function or primitive names as variable or argument identifiers.
Symbols such as `sym`, `str`, `num`, `char`, `type`, `list`, `first`, `rest`,
`last`, `find`, `slice`, `map`, `each`, `eval`, `read`, `print`, `length`,
`format`, `min`, `max`, `abs`, `sort`, `filter`, `path`, etc., are
core language primitives. Shadowing them breaks lexical/global lookups and
introduces subtle bugs. Always use descriptive names (e.g. `coin_sym`,
`item_name`, `val_str`).

## Sensible Line Wrapping for `defq`, `setq`, and `bind`

Keep code readable within ~80-100 columns. Avoid both extreme vertical
ladders (one pair per line) and sprawling multi-variable lines:
```lisp
;; IDIOMATIC (sensible groupings by category, ~80-100 columns):
(defq *config* :nil *config_version* 1
	*config_file* (cat (get :home *env*) "crypto.tre")
	*selected_symbol* "BTC" *coins_data* (list)
	*canvas_width* 360 *canvas_height* 110
	*btn_coin_0* :nil *btn_coin_1* :nil *btn_coin_2* :nil
	*btn_coin_3* :nil *btn_coin_4* :nil *btn_coin_5* :nil)

;; Wrap long list constructors across multiple lines:
(defq btns (list
	*btn_coin_0* *btn_coin_1* *btn_coin_2*
	*btn_coin_3* *btn_coin_4* *btn_coin_5*))
```

## Static `'()` vs Independent `(list)`

`'()` evaluates to a shared,
static empty list instance. Mutating `'()` (e.g. via `push`) corrupts
every reference in the system. Always use `(list)` to create a fresh,
independent, mutable empty list.

## Control Flow and Error Prevention

There is no `return` keyword;
a function yields the result of its last evaluated expression.
Non-local exits (`catch`, `throw`) work in every build, there is no build
without them, but they are for faults and debugging, not a way to steer
normal code. Code must prioritize input validation to prevent invalid
states from ever occurring, rather than catching errors after the fact.

## Conditionals & Branching (`if`, `ifn`, `when`, `unless`)

*	*Implicit Progn on Else Clauses:* Both `(if tst then else_1 else_2 ...)` and
	`(ifn tst then else_1 else_2 ...)` treat the `then` branch as a single form,
	but treat all subsequent expressions in the `else` position as an **implicit `progn`**
	(evaluated sequentially via `:lisp :repl_progn`).

*	*NEVER Wrap Else Clauses in `(progn ...)`:*
	Wrapping the `else` clause in `(progn ...)` is completely redundant:
	```lisp
	;; ANTI-PATTERN: (if (not ...)) + redundant (progn ...) in the else clause:
	(if (not entry)
		(progn
			(defq md (Md))
			(def md :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md)
			(. md :populate_lines '("# Select an algorithm" "" "*No algorithm selected.*")))
		(progn
			(defq md_top (Md))
			(def md_top :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md_top)
			(. md_top :populate_lines (catalog-overview-markdown entry))
			...))
	```
	Instead, use `ifn` with the fallback in the single-form `then` branch, and let the entire main block flow into the `else` clause with zero `progn` wrapper:
	```lisp
	;; THE CHRYSALISP WAY: ifn + implicit progn in else clause + (def (defq ...)):
	(ifn entry
		(progn
			(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md)
			(. md :populate_lines '("# Select an algorithm" "" "*No algorithm selected.*")))
		;; ELSE clause has implicit progn - no (progn ...) wrapper!
		(def (defq md_top (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
		(. *right_container* :add_child md_top)
		(. md_top :populate_lines (catalog-overview-markdown entry))
		(defq code_vdu (create-code-vdu (elem-get entry 7) page_w))
		(. *right_container* :add_child code_vdu)
		(when (defq idm (catalog-idioms-markdown entry))
			(def (defq lbl_space (Label)) :min_height 8 :border 0)
			(. *right_container* :add_child lbl_space)
			(def (defq md_bot (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md_bot)
			(. md_bot :populate_lines idm))
		(def (defq lbl_pad (Label)) :min_height 16 :border 0)
		(. *right_container* :add_child lbl_pad))
	```

*	*Test-Result Passthrough:* When no `else` clause is provided:

	*	`(if tst then)` returns `:nil` when `tst` evaluates to falsy.

	*	`(ifn tst then)` returns `tst` (the non-nil test result) when `tst`
		evaluates to truthy.

*	*Macro Architecture of `when` and `unless`:*

	*	Single-form `(when tst form)` expands directly to `(if tst form)`
		for zero-overhead evaluation.

	*	Multi-form `when` expands to `(ifn tst :nil body ...)`, routing the
		body through the else-clause implicit `progn`.

	*	`(unless tst body ...)` expands to `(if tst :nil body ...)` for both
		single- and multi-form bodies.

	*	Both `when` and `unless` strictly guarantee returning `:nil`
		whenever the body is not executed, completely avoiding the
		fallback overhead of intermediate `cond` or `condn` structures.

*	*Idiomatic Negative Conditionals (Never `(if (not ...))`):*
	Never write `(if (not cond) ...)`. Always use ChrysaLisp's native negative
	conditional forms:
	*	Use `(ifn cond then [else ...])` when branching on a falsy condition.
	*	Use `(unless cond body ...)` for single-branch side effects or guard clauses without an `else`.

*	*Short True Branch on Same Line (Else Flows Below):*
	A very common ChrysaLisp idiom is to place a short `then` branch of `if`/`ifn` on the same line as the test, with the `else` (implicit `progn`) block flowing below:
	```lisp
	(ifn (str? price_str) "$0.00"
		(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
			(defq int_part (slice price_str 0 dot)
				frac_part (slice price_str (+ dot 1) -1))
			(if (eql int_part "0")
				(cat "$0." (slice (cat frac_part "0000") 0 4))
				(cat "$" int_part "." (slice (cat frac_part "00") 0 2)))))

	(if (starts-with "-" chg_str) (cat chg_str "%")
		(cat "+" chg_str "%"))
	```

*	*Don't Waste Statements When Return Values Can Be Used Directly:*
	In ChrysaLisp, binding and evaluation forms (`defq`, `setq`, `push`, etc.) evaluate directly to their resulting value. Never waste a separate binding statement immediately before a conditional check:
	```lisp
	;; ANTI-PATTERN (wasting statements):
	(defq dot (find "." price_str))
	(ifn dot ...)

	(defq resp (http-get url))
	(when resp ...)

	;; THE CHRYSALISP WAY (embed return value directly in condition):
	(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
		...)

	(when (defq resp (http-get url))
		...)

	(when (defq stream (file-stream path))
		...)
	```

*	*Use Built-in `min` and `max` Primitives:*
	ChrysaLisp has built-in `(min num num ...)` and `(max num num ...)` primitives that accept variable arguments:
	```lisp
	;; ANTI-PATTERN (manual comparison branching):
	(each (#
		(if (< %0 min_val) (setq min_val %0))
		(if (> %0 max_val) (setq max_val %0)))
		flt_pts)

	;; THE CHRYSALISP WAY:
	(each (# (setq min_val (min min_val %0) max_val (max max_val %0))) flt_pts)
	```

*	*Single-Line `if` / `ifn` / `when` / `unless` for Simple Forms:*
	When a conditional has a short test and concise form, keep it on a **single line**:
	```lisp
	;; IDIOMATIC (single-line for simple guard/init):
	(ifn *config* (setq *config* (Emap)))
	(unless *syntax* (setq *syntax* (Syntax)))
	(if (nempty? line) (push lines line))

	;; ANTI-PATTERN (vertical sprawl for a trivial branch):
	(ifn *config*
		(setq *config* (Emap)))
	```

## Lists as LIFO Stacks & The Rocinante Collector Pattern

*	`(push list elem ...)` appends to the end of `list` **and returns the list** as its return value!

*	*Avoid Imperative `(each ... (push l ...))` Accumulation:*
	Never pre-allocate an empty list and use an imperative loop to populate it:
	```lisp
	;; ANTI-PATTERN (redundant variables and environment lookups):
	(defq results (list) items '((1 2) (3 4)))
	(each (lambda ((a b)) (push results (calc a b))) items)
	```
	Instead, use the Rocinante collector pattern with `reduce`:
	```lisp
	;; THE ROCINANTE WAY (zero temp bindings, direct pipeline expression):
	(reduce (lambda (p (a b)) (push p (calc a b))) '((1 2) (3 4)) (list))
	```
	Because `(push p ...)` returns `p` directly, the accumulator flows seamlessly through each iteration of `reduce` with no intermediate variables, returning the completed list as the expression's value.

*	*Variadic `push` (Multiple Values in One Statement):*
	`push` accepts multiple elements: `(push list elem0 elem1 ...)`.
	Never write consecutive `(push l a) (push l b)` statements; write `(push l a b)`.
	In `reduce` collectors, you can push multiple items into the accumulator in a single step:
	`(push p item1 item2)`.

*	`(pop list)` removes and returns the final element.

*	`(first list)` is `(elem-get list 0)`.

*	`(last list)` is `(elem-get list -2)`.

*	`(slice list 1 -2)` returns the list minus first and last items.

## Pragmatic Lambda Usage (`#` vs `lambda`)

*	Use `(# ...)` for compact, performance-critical callbacks. Positional
	symbols `%0`, `%1`, etc., are globally interned with permanent
	`str_hashslot` cache indices.

*	Use `(lambda ...)` when destructuring arguments or when explicit
	naming clarifies complex logic.

*	**NEVER** use `(bind ...)` inside an anaphoric `#` lambda
	(anti-pattern: `(# (bind '(k v) %0) ...)`). Doing so introduces
	local symbols that destroy the cache benefits of `%0` while adding
	`bind` overhead. Use `(lambda ((k v)) ...)` instead.

## Variable Binding and Shadowing

*	Do not nest `let` or `let*`. Use `defq` to declare and bind multiple
	variables simultaneously in scope:

	(defq a 1 b 2 c (list))

*	Never use built-in function names as variables (e.g. `path`, `str`).
	Variables, functions, and macros share a single symbol environment.

*	Use `bind` for destructuring sequences and return tuples:

	(bind '(w h) (. view :get_size))
	(bind '(x y &rest tail) my_list)
	(bind '(x y &ignore) my_list)
	(bind '((x0 x1 &ignore) (y0 y1 &ignore) &ignore) nested_list)

	Note: `&` is a 1-to-1 placeholder skipping a single element. `&ignore` terminates binding immediately and ignores all remaining trailing elements.

*	**`bind` Semantics (`(def (env) ...)` vs `(set (env) ...)`):**
	`(bind '(var1 var2 ...) seq)` directly inserts new bindings into the current local environment `(env)`. It performs `(def (env) ...)`, **NOT** `(set (env) ...)`.
	Because it defines variables in the local frame, `bind` does **not** update or mutate existing outer bindings or global variables (`*...*`).
	To update pre-existing outer or global variables, always use `setq`:
	```lisp
	(setq *selected_id* (. *config* :find :selected_id)
		*selected_cat* (. *config* :find :selected_cat)
		*search_query* (. *config* :find :search_query))
	```

*	**Extracting Map Values with `gather`:**
	To extract multiple values from a map or tree in a single call, use `(gather map :key1 :key2 ...)`. It returns a list of the resolved values:
	```lisp
	;; Extract multiple values from a map directly into local bindings:
	(bind '(x y width height) (gather *config* :x :y :width :height))
	(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
	```
	This eliminates repetitive individual `(. map :find :key)` calls when populating local variables.

## Object Syntax & Sensible Wrapping

*	Method call: `(. obj :method arg1 arg2)`.

*	Chained method call returning `this`: `.->` macro:

	(.-> *canvas* (:set_color +argb_black) (:fill 0))

*	Property access: `(get :prop obj)`.

*	Property create or mutate: `(def obj :prop val)`.

*	Property mutate only: `(set obj :prop val)`.

*	**Sensible Wrapping for `(def)` and `(set)`:**
	Avoid the vertical ladder anti-pattern where every single property and value occupies its own indented line. Instead, flow property pairs sensibly across lines, grouping 2 to 3 related pairs per line within ~80–100 columns, or keep the form inline on a single line if compact:
	```lisp
	;; ANTI-PATTERN: Excessive vertical sprawl (1 pair per line):
	(def (defq vdu (Vdu))
		:font +font_code
		:vdu_width 80
		:vdu_height h
		:paper 0
		:ink +argb_black)

	;; IDIOMATIC: Sensible wrapping (2-3 pairs per line, ~80-100 columns):
	(def (defq vdu (Vdu))
		:font +font_code :vdu_width 80 :vdu_height h
		:paper 0 :ink +argb_black)

	;; IDIOMATIC: Compact forms kept on a single line:
	(def (defq backdrop (Backdrop)) :paper +argb_grey1 :min_width (max pad_w page_w) :min_height rh)
	(def (defq scroll (Scroll +scroll_flag_horizontal)) :min_width page_w :min_height rh)
	```

## Anaphoric Loop Index `(!)`

In `each`, `each!`, `map`, and `lines!`,
the form `(!)` evaluates to the current zero-based loop index:

	(each (# (print "Item " (!) ": " %0)) my_list)

## Short Anaphoric Lambdas `(# ...)` vs `(lambda ...)`

Always use `(# ...)` when using positional arguments (`%0`, `%1`, etc.):

	(map (# (mat3x2-vec2-mul m %0 (cat %0))) paths)

NEVER write `(lambda (%0) ...)`. The `lambda` form is strictly reserved for
explicitly named parameter lists: `(lambda (item) ...)`.

## Compile-Time Constants

Force compile-time arithmetic or lookups
using `(const ...)`:

	(* x (const (/ 180.0 +fp_pi)))

## Top-Level `defun` Definitions (Prebinder Rule)

**NEVER** nest `defun` inside another `defun`, `progn`, `catch`, or other expressions!
The ChrysaLisp prebinder scans top-level forms to discover functions, prebind symbols, assign frames, and optimize call sites.
If a `defun` is placed inside `(progn ...)`, `(catch ...)`, or another function, the prebinder cannot see it, causing `symbol_not_bound`, `wrong_num_of_args`, or compilation failure.
Always define all functions with `defun` at the top level of your file.

## GUI Application Event Loop Pattern

	(defun main ()
		(defq select (task-mboxes +select_size) *running* :t)
		; ... setup widgets / window ...
		(. *window* :open :here)
		(while *running*
			(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
			(cond
				((= idx +select_main)
					(if (= (getf msg +ev_msg_target_id) +event_close)
						(setq *running* :nil)
						(. *window* :event msg)))
				(:t (. *window* :event msg))))
		(. *window* :close))

## NEVER Run GUI Code from TUI or Test Environment

GUI classes (`View`, `Window`, `Vdu`, `Flow`, `Button`, `Label`, `Md`, etc.)
and desktop apps (`apps/desktop/`, etc.) require full system mode with the
GUI subsystem, compositor, and window manager (`./run.sh -f`). They are NOT
present in the headless test environment or the TUI boot image
(`./run_tui.sh`); attempting to reference or instantiate GUI classes from
test scripts or TUI causes immediate `symbol_not_bound` errors.

To run GUI dependent code from an agent, pipe a `lisp -r` snippet to
`./run.sh -n 1 -f`, which gives the GUI boot image with a TUI attached to
the host. A widget tree can be built this way and dumped with
`(ui-save stream view)` for inspection, or a live window opened for the
user to interact with, see the `chrysalisp-gui-apps` skill.

## Short-Circuiting, Embedded Binding, and Branching (`and` / `or`)

*	`and` expands to `condn` (testing for `:nil`), and `or` expands to
	`cond`. Write predicates for zero wasted interpreter evaluations.

*	*Embed `defq` Inside Consumer Predicates:* Do not write standalone
	`(defq ...)` clauses in an `and` chain. Bind variables directly
	inside the comparison that uses them:

	(eql (third %1) (defq d (third %0)))

*	*Order Clauses for Fastest Failure:* Place the most discriminative
	test earliest to lazily abort execution before further bindings.

*	*Leverage `cond` Branch Conditions:* Put `(and ...)` directly as
	the `cond` branch condition; do not wrap the body in `(when ...)`.

*	*Use `when` Naturally for Multi-Statement Blocks:* Inside `case` or
	unconditional blocks, use `(when tst a1 a2)` directly.

*	*Anti-Pattern:*

	(cond
		((eql op0 'cpy-rr)
			(when (and (defq cpy_info (pfind +map (first %1)))
					(defq d (third %0))
					(eql (third %1) d)
					(defq s (second %0))
					(nql s d))
				(case (first cpy_info)
					(:cr
						(elem-set emit_list (!) (list (second cpy_info) (second %1) s d))
						(elem-set emit_list (inc (!)) '(nop)))))))

*	*Correct Pattern:*

	(cond
		((and (eql op0 'cpy-rr)
				(defq cpy_info (pfind +map (first %1)))
				(eql (third %1) (defq d (third %0)))
				(nql d :rsp)
				(nql (defq s (second %0)) d)
				(nql s :rsp))
			(case (first cpy_info)
				(:cr
					(elem-set emit_list (!) (list (second cpy_info) (second %1) s d))
					(elem-set emit_list (inc (!)) '(nop))))))

## Spans and Parts of Sequences (`each!` and Friends, No `range`)

The `!` iterators take their sequences as a list, and a start and end:
`(each! f seqs [start end])`, `(map! f seqs [start end out])`,
`(filter! f seq [start end out])`, `(reduce! f seqs init [start end])`,
`(some! f seqs [mode start end])`. `docs/ai_digest/rocinante.md` is their
doc.

*	Part of a sequence, or backwards, is a start and end, never a `slice`,
	`rest`, `most` or reversed copy made only to walk. A start past the end
	walks down. `(!)` is the index in what is walked, not counted from the
	start given. Keep a `slice` where the copy itself is wanted.

*	Two sequences side by side go in the one list, `(each (lambda (a b) ...)
	as bs)`, never `(elem-get bs (!))` inside a walk of `as`. `check` flags
	that.

*	Results of several walks collect into one `out` list of `map!` or
	`filter!`, not a `cat` of several. A walk that stops early is `some!`,
	whose mode comes before its span: `:nil` stops at the first that is
	not `:nil`, `:t` at the first that is, as `every`. A span of numbers is
	`(some! f '() :nil a b)`; `(some! f '() a b)` takes `a` as the mode.

*	There is no `range`. A span of numbers is an iterator with an empty
	list of sequences, `(each! (lambda () ... (!) ...) '() start end)`: the
	lambda takes nothing, `(!)` is the number, the end is not done, a start
	past the end counts down, an end not given is 0. `(map! (const !) '() a
	b)` is a list of the numbers where a list is the thing wanted. A count
	is `(times n ...)` or `(for start end ...)`. A step is a count of steps
	and `(* (!) step)`. `test-output` code can hold no `!`, so a span there
	is a `while`.

*	Bytes or text built a piece at a time go into a `(string-stream
	(str-alloc size))`, written in place, then `(str out)`, not a list of
	parts and `(apply cat ...)`.

*	An expression of only constants and literals inside a function is
	`(const ...)`. A value used once is put where it is used, not named.

*	A function that ignores what it is given is `(lambda (&ignore) ...)`,
	of any number of arguments, none among them. `&` skips one place before
	names that bind, `(bind '(& b c) seq)`, `(lambda (& y) ...)`, and takes
	exactly one there. Never `(lambda (_) ...)`, which binds a variable
	named `_`. `&` is the wildcard of `lmatch?`'s pattern too, `(lmatch?
	inst '(cpy-rr & :r1))`. A `_` that is read, the error `catch` binds,
	a macro's body spliced as `~_`, is a name and stays.
	`(const identity)` passes its argument on.

## No Recursion, a `(list)` Is the Stack

No function calls itself, or a ring of others. The stack runs out, and
each call shadows the hash slots of its names. A tree is walked with a
`(list)` of work items, popped and pushed, as `flatten` in `root.inc`
does. A code generator lays the tree out as a flat list of steps first,
then runs over the steps with a value stack.

## Characters Are Literals

A character known when the file is written is a string literal: `"\n"`
`"\t"` `"\r"` `"\v"` `"\f"` `"\b"` `"\q"` (a double quote) `"\\"`, the
character itself, or `"\xNN"`. Never `(ascii-char 10)` or `(ascii-char
+char_lf)`. The `+char_` names are the codes, for comparing and key codes.
`check` has a `char` rule for it.

## Time with `time-it`, Never by Hand

Every measurement uses the macros of `class/lisp/root.inc`: `(time-it
heading body)` prints the time of the body and nests; `(time-add heading
body)` adds the time to a total, and `(time-report)` prints the totals,
counts and longest, for what runs too often to print. A repeat count goes
inside the body, `(times n ...)`. Never write `pii-time` loops, best of N
or sums by hand: a missing feature is added to those macros, in that one
place.

Where a function can be written several ways with the iterators, time the
variants and keep the fastest. A session carries an offset of its own of
up to about 20 per cent, so compare variants across several fresh
sessions, interleaved, against the old code's own spread.

## Traps

`docs/ai_digest/lisp_traps.md` is the list of known traps, each pinned by
`tests/core/test_traps.lisp`. Read it before writing new Lisp, and add a
new trap there with its test. Others to know:

*	A `+name` is put in the code by value. A list made by a call, `(cat
	...)` or `(list ...)`, is then run as a form: store it `(list quote
	...)` or give it a `*name*`. `(const (list ...))` is the same. A
	`+name` used in the `defq` that makes it is not yet the constant; use
	it in a later `defq`. `check` has `early`.

*	A function given as a value, a callback or an entry of data, is looked
	up when used unless written `(const f)`, and defined above. A local
	bound to a lambda, `(defq f (lambda ...))` then `(f ...)`, is a lookup
	at every call: make it a real function defined above its caller.

*	`>>` is the logical shift, `>>>` the arithmetic one. A shift of a value
	that can be negative wants `>>>`.

*	`(# ...)` takes exactly the `%n` it names: with only `%1` it takes one
	argument, not two. Use a `lambda` for the second of two.

*	`list?` is true of anything made of a list, an env, an `Fmap`, an
	`Fset`, a `pset`, and gives the type depth. `list??` is a plain list.
	`(str? :nil)` is true, a symbol is a string: `str??` for a string.

*	`(find x lists)` finds only the same object: match by contents with
	`some` and `eql`. `eql` of two lists compares big numbers as objects,
	so compare lists of keys with `(every (const =) a b)`. `eql` of two
	vectors compares their elements, it is not identity.

*	`(found? text substr)` tests for a substring, `find` takes one element.
	`(rfind x seq)` is the index just past the match. `(str-to-num
	"word")` is 0, not an error.

*	`(apply (const pset) lst)` makes `lst` itself the pset, give it `(cat
	lst)`. `(pfind pset key)` asks if a key is in a pset.

*	`()` given to a function is run: write `(list)` or `'()`. `&ignore` ends
	a pattern, nested ones too, so name every part before it.

*	A value held in no name is let go of at once: hold an object whose
	letting go does something, `(defq stdio (create-stdio))`.

*	`(exec form)` and `(macrobind)` rewrite the form in place; `(exec (copy
	form))` keeps the text to send on. A lambda made by `eval` of a
	backquote is not macro expanded.

*	A macro of a library imported in the same top level form is not a
	macro yet when the form is made, and gives its expansion back as a
	list, silently. In a task's text put the import a form apart.

*	A module's own `defq` globals are gone once it pops: a function of it
	that names one at run time fails.

*	A release build has no checks: an error is a value that the next form
	walks over, and a `catch` sees only the very form it is round. A fault
	a caller must see goes in a value or the object's state, never a
	throw. Run a new stream or service on the `-e` emulator. A test of
	what is wrong on purpose is skipped unless `*test_checked*`.

*	`(print ...)` is buffered: a second `(print)` flushes it to the host.
	`stream-flush` of a `string-stream` sends it to stdout; capture what a
	writer writes with a `lisp-stream`.

*	A structure's name is its fields' prefix: name an app's structures for
	the app, `+pcb_job`, or two files clash.

*	`(reduce f seq :nil)` is `(reduce f seq)`: a start of `:nil` is no
	start, the first element is, and an empty seq is an error,
	`not_valid_index`. A search for the last that fits, `:nil` if none, is
	a walk that keeps it, `(each (# (if (fits %0) (setq found %0))) seq)`,
	or `reduce!` given its `:nil` and span. The Jukebox's seek met it with
	no order starts known yet.

*	A mailbox at the head of a mail is bytes, `+net_id_size` of them, any
	of which can be a newline or a space. A separator of the text after it
	is found from there, `(find "\n" msg +net_id_size)`: the Jukebox's
	measure child found the first of all, and a window whose mailbox had a
	newline in it never had a song's length.

## Use What Is There, and Grow the Tools

*	Before a new part, a watcher, a registry, a check, a tool, ask what
	already does it: service names and their scopes, mail, `open-task`,
	`open-child`, `node-spawn`, and the commands of `cmd/`. Grep `cmd/` and
	these skills first, and offer the small version.

*	A library facility is written once. Where a library or an app carries
	its own copy of what a library does, or two libraries do one job, merge
	them. It is not a drive to cut lines: alike frame code in apps is fine.
	A library many tasks import may be proposed for `root.inc`, measured
	first; files keep their `(import)` lines. Check `includes` and
	`imports` after.

*	Work on the tree is done by the tree's own commands, `files`, `grep`,
	`check`, `forward`, `brackets`, `lint`, `imports`, `tests`, not host
	shell or Python scripts. A host script used twice is a candidate for
	`cmd/`.

*	Run `check` on new or changed Lisp before its first run. It reads paths
	from stdin, one folder at a time: `files <folder> | check -j 40`,
	or the paths of files and folders as its arguments. `check`, `forward` and
	`brackets` need the Lock service and say nothing without it: a
	`run_tui.sh -s` script starts it first.

*	A strange error after an edit, a `symbol_not_bound` of an assembler
	name, a form run at the top level, is first a `brackets <file>`, which
	gives the line and column of a stray or missing bracket.

*	A fault the source text alone would have shown gets a rule in
	`cmd/check.lisp` in the same change as its fix, with a case in
	`tests/text/test_tree.lisp`. Only what is decidable from the text, and
	tried on the tree first. A line that does it on purpose carries `;check
	off`.

## Ownership Is a Tree, Back Links Are Weak

Letting go of the last reference to a structure, a window, a board, a
farm, frees all of it, with no release call to remember. A link back
towards an owner is a `(weak-ref owner)`, claimed with `obj-ref` only while
it is used, on a path that came through the owner. A `:close` of a farm or
jobs is a courtesy to the children; if memory ever needs it, that is a
logic bug. `(view-loops view)` finds cycles, and `tests -e` fails a module
that leaves its node holding more.

## Mail Is Unordered, Mailboxes Are Cheap

*	Two messages between the same two tasks can arrive in either order
	where there is more than one route. That is by design. What needs order
	uses the `:in` and `:out` streams, or waits for one step before sending
	the next.

*	Late or stale replies are not filtered with flags or counters. Make a
	new mailbox, `(mail-mbox)`, put it where the old one was, the select
	list or `(. jobs :reply_to mbox)`, and let the old one go: mail to it
	goes nowhere.

*	A str sent is the receiver's. Mail to a task on the same node is not
	copied, what arrives is the str that was sent, so the sender does not
	write into it after. One made for the send, `(str-alloc)` and `setf->`,
	needs nothing more. One kept to be sent again, a job a farm may give
	out twice, is sent as a fresh `(cat msg)` each time, and the one kept
	is never sent. `Jobs :give` wrote the key of
	the next child into a job it had already mailed, a child on the farm's
	node still at work on it read the new key, and its answer was taken for
	another job's. Only a child on the one node can see it, so a farm of
	many nodes seldom shows it and one of few does.

## Messages Drive, Inactivity Dissolves

Read this before designing anything two tasks do together, an app and its
child, a farm, a service and its clients. The full rule is in the
`chrysalisp-net-testing` skill, "Rules Of Distributed Code". In one line:
assume the other is there and can be reached, and never depend on it. It
is always best intentions, with a fall back. A far task is not launched
till its launch reply comes, and may never be; a job sent to it then may
not arrive; an answer may not come back.

*	Messages drive the states of an application, and fast: a job, its
	answer, the next. That is the ideal path, and all of the usual one.

*	Acks, stops and exits are courtesies, sent to make a change quick: the
	app's empty message that ends a child at once, a child's word that it
	is going. Nothing may depend on one arriving.

*	No side can know the other is there, so no protocol tries to make it
	certain. Inactivity dissolves what is left: a child given nothing for
	its idle time takes its parent to have gone and goes; a parent with
	work out and no answer for its patience takes the child to have gone
	and starts again. Every task has such a time, none blocks on a mail
	read for ever.

*	A start is a message too: `open-task` with a mailbox of the select
	list for the word that it started, never `open-child` or
	`open-remote` where the node may not be there to answer, they wait
	for ever, and in an app with the window frozen. What is for the task
	is kept till it says it has started.

*	Never keep track of whether a child may have gone. A farm lasts only
	while its work goes on: when the work stops, or what is out is no
	longer wanted, let it go, the courtesy that ends its children at once,
	and when work starts again make a new farm with new mailboxes. `(setq
	jobs (jobs-end jobs select +select_task +select_reply))`,
	`lib/task/jobs.inc`, is the letting go, and every app with a farm
	uses it. Chess does it for each game, Digits for each run, Surface at
	each change of mode, Mandelbrot and Mesh when the picture has stood
	still a few seconds, less than the 5 a child waits for work. A farm
	kept across a pause gives its next jobs to children that have gone,
	and waits out its patience for each. No `:restart`, and no time kept
	to guess by whether they went.

*	One model for work that the CPU or the GPU may do: a farm either way,
	of many children for the CPU, of one for the GPU, the same job and
	the same answer, each child writing shared memory, a `:pixmap` or a
	`:buf` by its key. One farm at a time, the other's let go of at the
	switch. `lib/gpu/tile.inc` and `lib/gpu/shade.inc` are the two sides
	for a pixel shader; Surface, Raymarch and Mandelbrot are made so.
	`docs/ai_digest/render_farm.md` is the whole of it, with a skeleton
	of an app and of a child: read it before writing either.

*	An answer drives the next job. An app that gives a frame only on a
	tick of its timer draws every other tick when a frame takes a little
	over one: give the next on the answer when the last took a tick or
	more, and leave a faster child to the timer.

*	Long work is a child's job, not a slice of the app's loop. An app that
	does a part of something in each frame, so many ms of it, to keep its
	loop free (Motion's dry run and seek, Molecule's build) is to give it
	to a child as a farmed job instead, the child calling `(task-slice)`
	as it goes, its answer the result (Chris, 2026-11-21). What the child
	makes that the app shows goes in a `:buf` by its key, a shared canvas
	for pixels, never a pointer handed by `weak-ref`.

*	Use what is there: `Jobs`, `Farm` and `Local` of `lib/task/` are this
	protocol, the start by message, the queue, the patience, the reply
	mailbox. A task of one is a farm of one. Do not write it by hand.

*	So a design is three lists: the messages of the ideal path, the
	courtesies, and the time each side dissolves by. One with no time on
	a side is not done. `lib/task/jobs.inc` is a farm's,
	`apps/science/digits/gpu.lisp` a task's that holds state.

## A Matrix Is Made from Its Raw Variables

A transform is always made afresh from its raw variables, position,
angles, scales, never by applying a change to the matrix kept, which
drifts. A world matrix is the parent's world times the local. Keeping the
made matrix until a variable changes is fine; an inverse is made from the
fresh one.

